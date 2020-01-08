scripts_dir <- 'scripts'
plots_dir <- 'plots'
all_session_plots_dir <- 'all_session_plots'
log_dir <- 'log-files'

data_dir <- 'data'
final_data_dir <- 'final-data'
curated_data_dir <- 'curated-data'

# subj_data_dir <- 'subject-data'
# utility_data_dir <- 'utility-data'
# physiological_data_dir <- 'physiological-data'
# questionnaire_data_dir <- 'questionnaire-data'
# performance_data_dir <- 'performance-data'
# index_data_dir <- 'index-data'
# final_data_dir <- 'final-data'

default_plot_width <- 12
default_plot_height <- 10

one_hour_sec <- 3600

facs_file_name <- 'Physiology+FACS+Gaze+Speech+Judges.csv'
facs_gaze_speech_file_name <- 'facs_gaze_speech_data.csv'
facs_judges_file_name <- 'facs_judges.csv'




b_dummy_file_name <- 'b_dummy.csv'
c_dummy_file_name <- 'c_dummy.csv'

b_facs_raw_file_name <- 'Batch_FACS_RAW.csv'
c_facs_raw_file_name <- 'Continuous_FACS_RAW.csv'

b_facs_new_file_name <- 'Batch_FACS_data.csv'
c_facs_new_file_name <- 'Continuous_FACS_data.csv'


font_style <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)


brown <- 'rgb(202, 111, 30)'
violet <- 'rgb(91, 44, 111)'
light_red <- 'rgb(231, 76, 60)'
light_violet <- 'rgb(210, 180, 222)'
light_green <- 'rgb(39, 174, 96)'
light_blue <- 'rgb(133, 193, 233)'
light_grey <- 'rgb(204, 209, 209)'



facs_size <- 7

# subj_list_file_name <- 'subj_list.csv'


# grp_dir <- 'Group1'
# session_list <- c('Baseline', 'WorkingSession')
# signal_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')
# 
# 
# pp_file_pattern <- '.*_pp.csv'
# nr_pp_file_pattern <- '.*_nr.csv'
# rb_marker_file_pattern <- '.*Baseline_sessionmarkers.csv'
# ws_marker_file_pattern <- '.*WorkingSession_sessionmarkers.csv'
# activity_file_pattern <- '.*Activity.csv'
# mac_app_usage_file_pattern <- '.*Monitor.*log'
# win_app_usage_file_pattern <- '.*MonitorLog.csv'
# e4_file_pattern <- 'HR.csv|EDA.csv'
# iWatch_file_pattern <- '.*iWatch.csv'



# computer_usage_pattern = '^CR$|^CW$|^Computer - Reading$|^Computer - Writing$|^C - Reading$|^C - Writing$|^C - Writing/Reading$'
# computer_usage_pattern = 'CR|CW|Computer - Reading|Computer - Writing|C - Reading|C - Writing|C - Writing/Reading|Working'
# computer_usage_pattern = 'CR|CW'



# qc0_file_name <- 'qc0_all_subj.csv'
# qc1_file_name <- 'qc1_all_subj.csv'
# qc2_file_name <- 'qc2_all_subj.csv'
# 
# 
# qc0_session_mean_file_name <- 'qc0_session_mean.csv'
# qc1_session_mean_file_name <- 'qc1_session_mean.csv'
# qc2_session_mean_file_name <- 'qc2_session_mean.csv'
# 
# qc0_activity_mean_file_name <- 'qc0_activity_mean.csv'
# qc1_activity_mean_file_name <- 'qc1_activity_mean.csv'
# qc2_activity_mean_file_name <- 'qc2_activity_mean.csv'


## We don't know until now how many filtering we will do :P
## Hope it doesn't exixts 99 filtering
# qc99_file_name <- 'qc99_all_subj.csv'




## 1. Timestamp -->    Extract whatever inside []                 -->   \\[(.*)\\]
## 2. Application -->  Extract whatever after the space of []     -->   (.*)
# mac_data_pattern <- '\\[(.*)\\] (.*)'

# s_interface_date_format <- '%a %b %d %H:%M:%S'


decorator_hash <- '###########################################################'


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


custom_read_csv <- function(file_name) {
  return(read.csv(file_name, stringsAsFactors=F))
}

convert_to_csv <- function(df, file_path, row_names=F) {
  write.table(df, file = file_path, row.names=row_names, sep = ',')
}

save_plot <- function(plot_name, plot, width=default_plot_width, height=default_plot_height) {
  plot_path <- file.path(plots_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(plots_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}

new_display <- function(os) {
  if(os=="osx") {
    quartz()
  } else if (os=="windows") {
    windows()
  }
}




#--------------------------#
#-------   String   -------#
#--------------------------#
print_msg <- function(msg) {
  print(msg)
  message(msg)
}

write_log_msg <- function(msg, file_name) {
  message(msg)
  write(msg, file=file_name, append=TRUE)
}

is_match <- function(str, pattern) { 
  return(grepl(pattern, str)) 
} 

replace_to_underscore <- function(str) {
  gsubfn('.', list('.' = '_', ' ' = '_', '-' = '_'), tolower(str))
}

replace_to_space <- function(str) {
  gsubfn('.', list('_' = ' ', '-' = ' '), str)
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

is_null <- function(cell_val) {
  print(cell_val)
  if (is.na(cell_val)) {
    return(T)
  } else if (length(trim(cell_val))==0) {
    return(T)
  } 
  
  # else if (cell_val=="") {
  #   return(T)
  # }
  
  return(F)
}



#---------------------------------#
#-------   Date and Time   -------#
#---------------------------------#
convert_date <- function(data, date_format) {
  return(as.POSIXct(data, format=date_format))
}

convert_s_interface_date <- function(data) {
  convert_date(data, s_interface_date_format)
}


#--------------------------#
#--- File and Directory ---#
#--------------------------#
is_empty <- function(item) {
  return(length(item)==0)
}

get_dir_list <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

get_matched_file_names <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

get_matched_file_names_recursively <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}


#--------------------------#
#---   Miscellaneous    ---#
#--------------------------#
get_axis_label <- function(label) {
  return(list(
    title = label,
    titlefont = font_style
  ))
}












format_dummy_data <- function() {
  b_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, b_facs_raw_file_name)) %>% 
    filter(Participant_ID %in% c('T016', 'T064'))
  c_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, c_facs_raw_file_name)) %>% 
    filter(Participant_ID %in% c('T016', 'T064'))
    # slice(1:100000)
  

  convert_to_csv(b_facs_df, file.path(current_dir, data_dir, final_data_dir, b_dummy_file_name))
  convert_to_csv(c_facs_df, file.path(current_dir, data_dir, final_data_dir, c_dummy_file_name))
}



format_each_raw_data <- function(df, file_name) {
  df <- df %>% 
    select(Participant_ID, 
           Group, 
           Treatment, 
           Treatment_Time, 
           Task, 
           F_Seconds, 
           F_Angry,
           F_Disgusted,
           F_Afraid,
           F_Happy,
           F_Sad,
           F_Surprised,
           F_Neutral
           ) %>% 
    filter(Treatment=='DT')
  
  # print(str(df))
  convert_to_csv(df, file.path(current_dir, data_dir, final_data_dir, file_name))
}


format_raw_data <- function(dummy=T) {
  ## b_facs_raw_file_name
  ## c_facs_raw_file_name
  
  if (dummy==T) {
    b_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, b_dummy_file_name))
    c_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, c_dummy_file_name))
  } else {
    b_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, b_facs_raw_file_name))
    c_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, c_facs_raw_file_name))
  }
  
  format_each_raw_data(b_facs_df, b_facs_new_file_name)
  format_each_raw_data(c_facs_df, c_facs_new_file_name)
}


read_new_data <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  b_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, b_facs_new_file_name))
  c_facs_df <- custom_read_csv(file.path(current_dir, data_dir, final_data_dir, c_facs_new_file_name))
  
  # print(unique(b_facs_df$Participant_ID))
  # print(unique(c_facs_df$Participant_ID))
  
  facs_df <- rbind(b_facs_df, c_facs_df) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  
  # print(str(facs_df))
  # View(facs_df)
  
  
  # ques_df <<- custom_read_csv(file.path(current_dir, data_dir, "Questionnaire Data.csv"))
  
  return(facs_df)
}




##########################################################################################
# read_data <- function() {
#   ## Making the df as global variable, as it takes a lot of time to load.
#   ## So, during one R session we load this df once
#   facs_df <- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>%
#     mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
#   # print(str(facs_df))
#   
#   ques_df <<- custom_read_csv(file.path(current_dir, data_dir, "Questionnaire Data.csv"))
#   
#   return(facs_df)
# }
##########################################################################################





get_heat_map_df <- function(facs_df, 
                                group='no_group', 
                                subj='none', 
                                plot_type='none', 
                                treatment='none',
                                Test=F,
                                file_path=NULL) {
  ## Initializing matrix with all 0
  final_matrix = matrix(0, facs_size, facs_size)
  
  # print(facs_df[c(1:10), emotion_cols])
  # convert_to_csv(facs_df[c(1:10), emotion_cols], 'facs_test.csv')
  
  for(i in 1:nrow(facs_df)){
    # for(i in 1:2){
    
    emotion_vals_by_row <- as.vector(unlist(facs_df[i, emotion_cols]))
    
  
    ##################################################################
    if (Test==T) {
      emotion_vals_by_row <- as.vector(c(0, 0.2, 0, 0.5, 0.2, 0, 0.1))
    }
    ##################################################################
    
    if (!any(is.na(emotion_vals_by_row))) {
      ## Outer product
      current_matrix <- outer(emotion_vals_by_row, emotion_vals_by_row)
      # print(current_matrix)
      
      ## Convert into upper triangle matrix
      current_matrix[lower.tri(current_matrix)] <- 0
      # print(current_matrix)
      
      #############################################################################
      #               NEW METHOD
      #############################################################################
      ## Make double of the upper traingle matrix
      ## Add the upper and lower triagnle value
      current_matrix[upper.tri(current_matrix, diag=F)] <- current_matrix[upper.tri(current_matrix, diag=F)]*2
      # print(current_matrix)
      #############################################################################
      
      
      
      #############################################################################
      #               OLD METHOD
      #############################################################################
      ## Normalize with the sum of the elements of the matrix
      # current_matrix <- current_matrix/sum(current_matrix)
      # print(current_matrix)
      #############################################################################
      
      ## Add to the final matrix
      final_matrix <- final_matrix + current_matrix
      # print(paste0('Step ', i, ': Sum Matrix -->'))
      # print(final_matrix)
    }
  }
  
  ## Dividing matrix using 1000 and taking until 2 decimal
  final_matrix = round(final_matrix, 2)
  final_matrix[lower.tri(final_matrix)] <- NA
  
  
  if (Test==T) {
    print(final_matrix)
  }
  
  if (plot_type=='panorama') {
    dimnames(final_matrix) = list(panorama_emotion_cols, panorama_emotion_cols)
  } else {
    dimnames(final_matrix) = list(plot_emotion_cols, plot_emotion_cols)
  }
  
  heat_map_df <- melt(final_matrix, varnames=c('row_name', 'col_name')) %>% 
    mutate(row_name=as.factor(row_name),
           col_name=as.factor(col_name),
           diagonal_val = ifelse(row_name==col_name, value, NA),
           non_diagonal_upper_matrix_val = ifelse(row_name==col_name, -1, value),
           non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), -1, 0),
           diagonal_percentage=round(100*diagonal_val/sum(value, na.rm=T), 2),
           non_diagonal_upper_matrix_percentage=round(100*non_diagonal_upper_matrix_val/sum(value, na.rm=T), 2))
  
  
  if (!is.null(file_path)) {
    # file_path=file.path(current_dir,
    #                     curated_data_dir,
    #                     'Subj Data',
    #                     get_full_group_name(group),
    #                     treatment,
    #                     paste0(subj, '.csv'))
    # 
    # print(paste(current_dir, curated_data_dir))
    # print(file_path)
    
    write.table(final_matrix,
                file = file_path,
                row.names=T,
                col.names=NA,
                sep = ',')
  }
  
  
  return(heat_map_df)
}

