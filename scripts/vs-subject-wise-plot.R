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
library(cowplot)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)


geom_text_size <- 6

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

panorama_emotion_cols <- c('An',
                       'Di',
                       'Af',
                       'Ha',
                       'Sa',
                       'Su',
                       'Ne')

group_list <- c('B', 'C')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  facs_df <- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  
  return(facs_df)
}

get_group_abbr <- function(group_name_full) {
  if (group_name_full %in% c('CH', 'CL')) {
    return('Continual')
  } else if (group_name_full %in% c('BH', 'BL')) {
    return('Batch')
  }
}

get_full_group_name <- function(group_name) {
  if (group_name=='B') {
    return('Batch')
  } else if (group_name=='C') {
    return('Continual')
  }
}

draw_heat_map_plot <- function(heat_map_df, type, plot_title) {
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    # geom_text(aes(label=diagonal_val)) +
    # geom_text(aes(label=diagonal_percentage)) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0)) +
    scale_fill_gradientn(colours = c("white", "yellow", "pink"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), 
              data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), show.legend = FALSE) +
    scale_fill_gradientn(colours = c("white")) +
    
    # ggtitle(plot_title) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'left',
          plot.margin=unit(c(t = 0, r = 1.8, b = 2, l = 0), "lines"),
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



# get_heat_map_df <- function(subj_facs_df, 
#                             group='no_group', 
#                             subj='none', 
#                             plot_type='none', 
#                             treatment='none',
#                             export_file=T) {
#   
#   # final_matrix=get_heat_map_matrix(subj_facs_df, group)
#   
#   if (plot_type=='panorama') {
#     dimnames(final_matrix) = list(panorama_emotion_cols, panorama_emotion_cols)
#   } else {
#     dimnames(final_matrix) = list(plot_emotion_cols, plot_emotion_cols)
#   }
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
#   if (export_file==T) {
#     file_path=file.path(current_dir,
#                         curated_data_dir,
#                         'Subj Data',
#                         get_full_group_name(group),
#                         treatment,
#                         paste0(subj, '.csv'))
#     
#     write.table(final_matrix,
#                 file = file_path,
#                 row.names=T,
#                 col.names=NA,
#                 sep = ',')
#   }
#   
#   
#   return(heat_map_df)
# }



draw_signal_plot <- function(subj_facs_df, subj, treatment) {
  signal_plot <- subj_facs_df %>% 
    select(Treatment_Time_New, F_Angry, F_Disgusted, F_Afraid, F_Happy, F_Sad, F_Surprised, F_Neutral) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
    mutate(Expression = recode_factor(Expression,
                                      'F_Angry'='Angry',
                                      'F_Disgusted'='Disgusted',
                                      'F_Afraid'='Afraid',
                                      'F_Happy'='Happy',
                                      'F_Sad'='Sad',
                                      'F_Surprised'='Surprised',
                                      'F_Neutral'='Neutral'
                                      )) %>% 
    
    ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression)) +
    geom_line(alpha = 0.5, size=1.2) +
    
    # ggtitle(paste0(subj, ' - ', treatment, ' - ', subj_facs_df$Group)) +
    xlab("Time [s]") +
    ylab("") +
    
    scale_color_manual(values = c("Neutral"="White", 
                                  "Surprised"="Cyan",
                                  "Sad"="Blue",
                                  "Happy"="Green",
                                  "Afraid"="Orange",
                                  "Disgusted"="Brown",
                                  "Angry"="Red"
                                  )) +
    
    theme_bw() +
    theme(text=element_text(size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='left',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=20,
                                    margin=margin(t=0, r=0, b=15, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  
    
  return(signal_plot)
}


draw_signal_heatmap_plots <- function(facs_df, type, test=F) {
  for (group in group_list) {
    group_facs_df <- facs_df %>%
      filter(Group %in% paste0(group, c('H', 'L'))) 
    
    for (subj in levels(factor(group_facs_df$Participant_ID))) {
    # for (subj in c('T051', 'T064', 'T083', 'T085', 'T132', 'T178')) {
    # for (subj in c('T016', 'T064')) {
      
      # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
      for (treatment in c('RB')) {
        
        subj_facs_df <- group_facs_df %>%
          filter(Participant_ID==subj & Treatment==treatment) 
        
        if (test==T) {
          subj_facs_df <- subj_facs_df %>% 
            slice(1:10000)
        }
        
        signal_plot <- draw_signal_plot(subj_facs_df, subj, treatment)
    
        
        heat_map_df <- get_heat_map_df(subj_facs_df)
        heatmap_plot <- draw_heat_map_plot(heat_map_df, type, paste0(subj, ' - ', treatment))
        # save_plot(paste0(subj, '_', treatment), heatmap_plot)
        
        
        title <- ggdraw() + 
          draw_label(paste0(subj, ' - ', treatment, ' - ', get_group_abbr(subj_facs_df$Group[1])), 
                     fontface='bold',
                     size=24)
        
        
        combined_dt_plot <- plot_grid(title,
                                      # NULL,
                                      signal_plot,
                                      heatmap_plot,
                                      # labels=c('', '', 'A', 'B'),
                                      # label_size=18,
                                      rel_heights=c(0.2, 1, 1),
                                      ncol=1)
        
        save_plot(file.path(all_session_plots_dir, 
                            treatment, 
                            get_group_abbr(subj_facs_df$Group[1]), 
                            paste0(subj, '_', treatment, '_signal_combined_', type)), 
                  combined_dt_plot, width=20)
      }
    }
  }
}


draw_area_plot <- function(subj_facs_df, subj, treatment, area_plot_type) {
  area_plot <- subj_facs_df %>% 
    select(Treatment_Time_New, 
           F_Angry, 
           F_Disgusted, 
           F_Afraid, 
           F_Happy, 
           F_Sad, 
           F_Surprised, 
           F_Neutral, 
           Task) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New, -Task) %>% 
    mutate(Expression = recode_factor(Expression,
                                      'F_Angry'='Angry',
                                      'F_Disgusted'='Disgusted',
                                      'F_Afraid'='Afraid',
                                      'F_Happy'='Happy',
                                      'F_Sad'='Sad',
                                      'F_Surprised'='Surprised',
                                      'F_Neutral'='Neutral'
                                      )) %>% 
    
    # ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression))
    ggplot()
    # geom_point(aes(x=Treatment_Time_New, y=1.2, colour=Task), shape=15, size=2) +
    # scale_color_manual(values = c("Email" = "green",  "Report" = "white"))
    
  
    if (area_plot_type=='area') {
      area_plot <- area_plot +
        geom_area(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                  alpha = 0.5)
    } else if (area_plot_type=='bar') {
      area_plot <- area_plot +
        geom_bar(aes(x=Treatment_Time_New, y=Value, color=Expression, fill=Expression),
                 alpha = 0.5, 
                 stat = "identity")
    }

    
  area_plot <- area_plot +
    xlab("Time [s]") +
    ylab("") +

    scale_color_manual(values = c("Neutral"="White",
                                  "Surprised"="Cyan",
                                  "Sad"="Blue",
                                  "Happy"="Green",
                                  "Afraid"="Orange",
                                  "Disgusted"="Brown",
                                  "Angry"="Red"
    )) +

    scale_fill_manual(values = c("Neutral"="White",
                                  "Surprised"="Cyan",
                                  "Sad"="Blue",
                                  "Happy"="Green",
                                  "Afraid"="Orange",
                                  "Disgusted"="Brown",
                                  "Angry"="Red"
    )) +

    theme_bw() +
    theme(text=element_text(size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='left',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=20,
                                    margin=margin(t=0, r=0, b=15, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  # 
  
  return(area_plot)
}

draw_task_plot <- function(subj_facs_df) {
  task_plot <- subj_facs_df %>% 
    select(Treatment_Time_New, Task) %>% 
    # gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
    # mutate(Expression = recode_factor(Expression,
    #                                   'F_Angry'='Angry',
    #                                   'F_Disgusted'='Disgusted',
    #                                   'F_Afraid'='Afraid',
    #                                   'F_Happy'='Happy',
    #                                   'F_Sad'='Sad',
    #                                   'F_Surprised'='Surprised',
    #                                   'F_Neutral'='Neutral'
    # )) %>% 
    
    ggplot(aes(x=Treatment_Time_New)) +
    geom_point(aes(y=1, colour=Task), shape=15, size=2) +
    # xlab("") +
    # ylab("") +
    scale_color_manual(values = c("Email" = "black",  "Report" = "white")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          
          #############################################
          #       FOR VALIDATING TIMELINE SYNC        #
          #############################################
          panel.border = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line = element_line(colour = "white"),
          # axis.line = element_line(colour = "black"),
          
          
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position='none',
          legend.title = element_blank(),
          # plot.title = element_text(hjust = 0.5,
          #                           size=20,
          #                           margin=margin(t=0, r=0, b=15, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 11.8), "lines")) ##top, right, bottom, left

  
  return(task_plot)
}

draw_area_heatmap_plots <- function(facs_df, heat_map_type, area_plot_type, test=F) {
  # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
  # for (treatment in c('RB', 'ST', 'PM', 'PR')) {
  for (treatment in c('DT')) {
    
    for (group in group_list) {
    # for (group in c('C')) {
      group_facs_df <- facs_df %>%
        filter(Group %in% paste0(group, c('H', 'L'))) 
        
      # for (subj in levels(factor(group_facs_df$Participant_ID))) {
      # for (subj in c('T016', 'T064')) {
      for (subj in c('T064')) {
        
        print(subj)
        
        subj_facs_df <- facs_df %>%
          filter(Participant_ID==subj & Treatment==treatment) 
        
        if (test==T) {
          subj_facs_df <- subj_facs_df %>% 
            slice(1:100)
        }
        
        
        task_plot <- draw_task_plot(subj_facs_df)
        area_plot <- draw_area_plot(subj_facs_df, subj, treatment, area_plot_type)
  
        file_path=file.path(current_dir,
                            curated_data_dir,
                            'Subj Data',
                            get_full_group_name(group),
                            treatment,
                            paste0(subj, '.csv'))
        
        heat_map_df <- get_heat_map_df(subj_facs_df, file_path=file_path)
        heatmap_plot <- draw_heat_map_plot(heat_map_df, heat_map_type, paste0(subj, ' - ', treatment))
        
        
        title <- ggdraw() + 
          draw_label(paste0(subj, ' - ', treatment, ' - ', get_group_abbr(subj_facs_df$Group[1])), 
                     fontface='bold',
                     size=24)
        
        
        combined_dt_plot <- plot_grid(title,
                                      # NULL,
                                      task_plot,
                                      area_plot,
                                      heatmap_plot,
                                      # labels=c('', '', 'A', 'B'),
                                      # label_size=18,
                                      rel_heights=c(0.2, 0.2, 1, 1),
                                      ncol=1)
        
        save_plot(file.path(all_session_plots_dir, 
                            treatment, 
                            get_group_abbr(subj_facs_df$Group[1]), 
                            paste0(subj, '_', treatment, '_', area_plot_type, '_combined_', heat_map_type)), 
                  combined_dt_plot, 
                  width=20)
      }
    }
  }
}


get_subj_co_occurance_matrices <- function(facs_df, test=F) {
  for (group in group_list) {
    
    grp_facs_df <- facs_df %>%
      filter(Group %in% paste0(group, c('H', 'L')))
    
    for (subj in levels(factor(grp_facs_df$Participant_ID))) {
      # for (subj in c('T016', 'T064')) {
      # for (subj in c('T064')) {
      
      print(subj)
      
      # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
      # for (treatment in c('PR')) {
      for (treatment in c('DT')) {
        
        subj_facs_df <- grp_facs_df %>%
          filter(Participant_ID==subj & Treatment==treatment) 
        
        if (test==T) {
          subj_facs_df <- subj_facs_df %>% 
            slice(1:100)
        }
        
        heat_map_df <- get_heat_map_df(subj_facs_df, subj=subj, group=subj_facs_df$Group)
      }
    }
  }
}


get_grp_co_occurance_matrices <- function(facs_df, type, test=F) {
  for (group in group_list) {
    dt_facs_df <- facs_df %>%
      filter(Treatment=='DT' & Group %in% paste0(group, c('H', 'L'))) 
    
    if (test==T) {
      dt_facs_df <- dt_facs_df %>% 
        slice(1:100)
    }
    
    heat_map_df <- get_heat_map_df(dt_facs_df, subj=group)
  } 
}



draw_panaroma_heat_map_plot <- function(heat_map_df, subj_facs_df, type, plot_title) {
  temp_subj_facs_df <- subj_facs_df %>% 
    select(Treatment_Time_New,
           F_Angry, 
           F_Disgusted, 
           F_Afraid, 
           F_Happy, 
           F_Sad, 
           F_Surprised, 
           F_Neutral) %>% 
    na.omit()
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    # geom_text(aes(label=diagonal_val)) +
    # geom_text(aes(label=diagonal_percentage)) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0)) +
    scale_fill_gradientn(colours = c("white", "yellow", "pink"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), 
              data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), 
              show.legend = FALSE) +
    scale_fill_gradientn(colours = c("white")) +
    
    # ggtitle(plot_title) +
    ggtitle(paste0(plot_title, " / ", nrow(temp_subj_facs_df))) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'none',
          plot.margin=unit(c(t = 1, r = 1.8, b = 2, l = 0), "lines"),
          plot.title = element_text(
            margin=margin(t = 0, r = 0, b = -2, l = 0, unit = "pt"),
            hjust = 0.5,
            # font_face="Bold",
            size=20)) +
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


draw_panorama_heatmap <- function(facs_df, type, test=F) {
  # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
  for (treatment in c('DT')) {
    plot_list <- list()
    
    for (group in group_list) {
      group_facs_df <- facs_df %>%
        filter(Group %in% paste0(group, c('H', 'L')))
 
      for (subj in levels(factor(group_facs_df$Participant_ID))) {
        print(subj)
        
        subj_facs_df <- group_facs_df %>%
          filter(Participant_ID==subj & Treatment==treatment) 
        
        if (test==T) {
          subj_facs_df <- subj_facs_df %>% 
            slice(1:10)
        }
        
        heat_map_df <- get_heat_map_df(subj_facs_df, 
                                       plot_type='panorama', 
                                       group=group, 
                                       subj=subj, 
                                       treatment=treatment)
        heatmap_plot <- draw_panaroma_heat_map_plot(heat_map_df, subj_facs_df, type, subj)
        plot_list[[length(plot_list)+1]] <- heatmap_plot
      }
    }
    
    
    panorama_batch_plot <- plot_grid(plotlist=plot_list[1:13], ncol=5) 
    
    panorama_continual_plot <- plot_grid(plotlist=plot_list[14:26], ncol=5)
    
    panorama_plot <- plot_grid(NULL,
                               panorama_batch_plot,
                               NULL,
                               panorama_continual_plot,
                               labels=c('', 'A', '', 'B'),
                               # label_y = -0.2,
                               vjust=1,
                               label_size=36,
                               rel_heights=c(0.04, 1, 0.2, 1),
                               ncol=1)
    
    save_plot(file.path(all_session_plots_dir, treatment, paste0('panorama_heatmap_', treatment,'_', type)), 
              panorama_plot, width=20, height=24)
  }
}



#-------------------------#
#-------Main Program------#
#-------------------------#
#### facs_df <<- read_data()
# facs_df <<- read_new_data()


#### draw_signal_heatmap_plots(facs_df, heat_map_type='summative', test=T)
# draw_area_heatmap_plots(facs_df, heat_map_type='summative', area_plot_type='bar', test=T)


#### draw_signal_heatmap_plots(facs_df, heat_map_type='summative')
draw_area_heatmap_plots(facs_df, heat_map_type='summative', area_plot_type='bar')








# draw_panorama_heatmap(facs_df, 'no_text', test=T)
# draw_panorama_heatmap(facs_df, 'no_text')


#### draw_panorama_heatmap(facs_df, 'summative')
#### draw_panorama_heatmap(facs_df, 'percentage')


#### draw_panorama_heatmap(facs_df, 'summative', test=T)
#### draw_panorama_heatmap(facs_df, 'percentage', test=T)












#### draw_signal_heatmap_plots(facs_df, 'percentage')
#### draw_signal_heatmap_plots(facs_df, 'percentage', test=T)



#### draw_area_heatmap_plots(facs_df, 'summative', 'area', test=T)
#### draw_area_heatmap_plots(facs_df, 'summative', 'area')



#### get_subj_co_occurance_matrices(facs_df, test=T)
#### get_subj_co_occurance_matrices(facs_df)


#### get_grp_co_occurance_matrices(facs_df, test=T)
#### get_grp_co_occurance_matrices(facs_df)



