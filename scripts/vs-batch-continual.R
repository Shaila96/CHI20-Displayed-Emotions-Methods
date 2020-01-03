#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(data.table)
library(reshape2)
library(ggnewscale)
library(cowplot)
library(nonpar)
library(cooccur)
library(EMT)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)


geom_text_size <- 4.5

emotion_cols <- c('F_Angry',
                  'F_Disgusted',
                  'F_Afraid',
                  'F_Happy',
                  'F_Sad',
                  'F_Surprised',
                  'F_Neutral')

plot_emotion_cols <- c('Angry',
                       'Disgusted',
                       'Afraid',
                       'Happy',
                       'Sad',
                       'Surprised',
                       'Neutral')

group_list <- c('B', 'C')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_stats <- function(facs_df) {
  temp_facs_df <- facs_df %>% 
    select(Participant_ID, Group) %>% 
    group_by(Participant_ID, Group) %>%
    summarize(n()) %>%
    group_by(Group) %>%
    summarise(Total_Group=n())
  print(temp_facs_df)
  
  subj_df <- facs_df %>% 
    select(Participant_ID) %>% 
    group_by(Participant_ID) %>% 
    summarize(n())
  convert_to_csv(subj_df, file.path(current_dir, curated_data_dir, "subject_list.csv"))
  
  # View(subj_df)
  # View(ques_df)
  
  ques_df <- ques_df %>%
    merge(subj_df, by="Participant_ID")
  
  View(ques_df)
  
  
  print(paste0("Mean age: ", mean(ques_df$Age)))
  print(paste0("SD age: ", sd(ques_df$Age)))
  
  
  ques_df_1 <- ques_df %>% 
    group_by(Gender) %>% 
    summarize(n())
  print(ques_df_1)
  # View(ques_df_1)
  
  
  ques_df_2 <- ques_df %>% 
    mutate(email_grp=ifelse(Group %in% c("BH", "BL"), "B", "C")) %>% 
    group_by(email_grp, Gender) %>% 
    summarize(grp_gender=n())
  print(ques_df_2)
  # View(ques_df_2)
  
  
  ques_df_3 <- ques_df %>% 
    mutate(email_grp=ifelse(Group %in% c("BH", "BL"), "B", "C")) %>% 
    group_by(email_grp) %>% 
    summarize(grp_mean_age=mean(Age),
              grp_sd_age=sd(Age))
  print(ques_df_3)
  # View(ques_df_3)
}

draw_heat_map_plot <- function(heat_map_df, type, plot_title) {
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    # geom_text(aes(label=diagonal_val)) +
    # geom_text(aes(label=diagonal_percentage)) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), 
                         name = "") +
    # guides(fill = guide_legend(order = 0)) +

    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0)) +
    scale_fill_gradientn(colours = c("white", "yellow", "pink"), 
                         name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), 
              data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), show.legend = FALSE) +
    scale_fill_gradientn(colours = c("white")) +
    
    ggtitle(plot_title) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size=24)) +
    labs(fill="") +
    scale_x_discrete(position = "top")
  

  if (type=='summative') {
    heatmap_plot <- heatmap_plot + 
      geom_text(aes(label=diagonal_val), size=geom_text_size) +
      geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0), size=geom_text_size)
  } else if (type=='percentage') {
    heatmap_plot <- heatmap_plot + 
      geom_text(aes(label=diagonal_percentage), size=geom_text_size) +
      geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0), size=geom_text_size)
  } 
  
  return(heatmap_plot)
}



# get_heat_map_df <- function(subj_facs_df, group='no_group') {
#   final_matrix=get_heat_map_matrix(subj_facs_df, group)
#   dimnames(final_matrix) = list(plot_emotion_cols, plot_emotion_cols)
#   
#   heat_map_df <- melt(final_matrix, varnames=c('row_name', 'col_name')) %>% 
#     mutate(row_name=as.factor(row_name),
#            col_name=as.factor(col_name),
#            diagonal_val = ifelse(row_name==col_name, value, NA),
#            non_diagonal_upper_matrix_val = ifelse(row_name==col_name, -1, value),
#            non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), -1, 0),
#            diagonal_percentage=round(100*diagonal_val/sum(value, na.rm=T), 2),
#            non_diagonal_upper_matrix_percentage=round(100*non_diagonal_upper_matrix_val/sum(value, na.rm=T), 2))
#   
#   # View(heat_map_df)
#   # convert_to_csv(heat_map_df, paste0('heat_map_dual_task_', group, '.csv'))
#   return(heat_map_df)
# }



draw_session_group_plots <- function(facs_df, type, test=F) {
  for (treatment in c('DT')) {
  # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
    
    plot_list <- list()
    
    for (group in group_list) {
      treatment_facs_df <- facs_df %>%
        filter(Treatment==treatment & Group %in% paste0(group, c('H', 'L'))) 
      
      if (test==T) {
        treatment_facs_df <- treatment_facs_df %>% 
          slice(1:100)
      }
  
      heat_map_df <- get_heat_map_df(treatment_facs_df, group=group)
      heatmap_plot <- draw_heat_map_plot(heat_map_df, type, paste0('Dual Task - ', group))
      plot_list[[length(plot_list)+1]] <- heatmap_plot
    } 
    
    combined_dt_plot <- plot_grid(plotlist=plot_list, 
                            labels=c('a', 'b'), 
                            label_size=36,
                            ncol=2)
    
    save_plot(file.path(all_session_plots_dir , 
                        treatment, 
                        paste0(treatment, '_group_', type)), 
              combined_dt_plot, 
              width=20)
  }
}



#-------------------------#
#-------Main Program------#
#-------------------------#
facs_df <<- read_new_data()


# get_stats(facs_df)


draw_session_group_plots(facs_df, 'summative')
#### draw_session_group_plots(facs_df, 'percentage')



# draw_session_group_plots(facs_df, 'summative', test=T)
#### draw_session_group_plots(facs_df, 'percentage', test=T)





#---------------- TEST CO-OCCURANCE MATRIX
# get_heat_map_df(facs_df[1:1,], Test=T)










