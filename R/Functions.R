#----  importing_data_from_NSQIP ----

# ICD.CPT
ICD.CPT <- function(files, dataOuput, commonFile, CPT.CODES, ICD.10.CODES, ICD.9.CODES){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    df <-  df %>% filter( cpt %in% CPT.CODES)
    df <-  df %>% filter(podiag %in% ICD.9.CODES |podiag10 %in% ICD.10.CODES)

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}





# ICD
ICD9.10 <- function(files, dataOuput, commonFile, ICD.10.CODES, ICD.9.CODES){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    df <-  df %>% filter(podiag %in% ICD.9.CODES |podiag10 %in% ICD.10.CODES  )

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}





# CPT
CPT <- function(files, dataOuput, commonFile,CPT.CODES){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    df <-  df %>% filter(cpt %in% CPT.CODES)

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}







# ICD10
ICD10 <- function(files, dataOuput, commonFile, ICD.10.CODES){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    df <-  df %>% filter(podiag10 %in% ICD.10.CODES)

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}





# ICD9
ICD9 <- function(files, dataOuput, commonFile, ICD.9.CODES){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    df <-  df %>% filter(podiag %in% ICD.9.CODES)

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}



# PRNCPTx
PRNCPTx <- function(files, dataOuput, commonFile,words){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    pattern <- paste(words, collapse = "|")
    df <-  df %>% filter(str_detect(prncptx, pattern))

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}


# PODIAGx
PODIAGx <- function(files, dataOuput, commonFile,words){

  # loading libraries
  library(tidyverse)
  library(janitor)

  # extract common column names
  df <- read.delim(commonFile) %>% clean_names()

  common_cols <- colnames(df)

  for (file  in files) {
    df <- read.delim(file)  %>% clean_names()

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    common_cols <- intersect(common_cols, colnames(df))
  }


  common_cols <- c(common_cols,"race","file_name","dopertod","doptodis")







  data <- data.frame()

  for (file  in files) {

    df <- read.delim(file) %>% clean_names()
    df$file_name <- file

    if ("race_new" %in% colnames(df)) {
      colnames(df)[colnames(df) == "race_new"] <- "race"
    }
    if ("d_operto_d" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_operto_d"] <- "dopertod"
    }
    if ("d_opto_dis" %in% colnames(df)) {
      colnames(df)[colnames(df) == "d_opto_dis"] <- "doptodis"
    }
    if (!("podiag10" %in% colnames(df))) {
      df$podiag10 <- NA
    }
    if (!("podiagtx10" %in% colnames(df))) {
      df$podiagtx10 <- NA
    }

    names(df) <- make.unique(names(df))

    pattern <- paste(words, collapse = "|")
    df <-  df %>% filter(str_detect(podiagtx, pattern)|str_detect(podiagtx10, pattern))

    df <-  df[,common_cols]
    data <- rbind(data, df)

    # Remove all the "+" signs in any column
    data[] <- lapply(data, function(x) gsub("\\+", "", x))

    # Replace -99 in any column with NA
    data[] <- lapply(data, function(x) ifelse(x == -99, NA, x))

    # Replace NULL in any column with NA
    data[] <- lapply(data, function(x) gsub("NULL", NA, x))



  }

  write.csv(data, dataOuput)

  cat(paste("Done","Your data file has been saved in this folder:", dataOuput,
            "Author's email: ahmeds1999haheen@gmail.com",
            "Please don't forget to cite us!"))

}


#----  importing_data_from_NIS ----

# NIS.COLUMNS.CODES

NIS.COLUMNS.CODES <- function(file_paths, codes_to_filter,
                              columns_to_filter, output_dir,
                              output_name="_",chunk_size = 100000) {
  library(haven)
  library(tidyverse)
  library(pbapply)

  chunk_size <- chunk_size
  dat <- data.frame()
  tttime <- 0
  # Initialize the progress bar
  pboptions(type = 1, style = 3)

  # Start the timer
  start_time <- Sys.time()

  for (file_path in file_paths) {
    total_cases <- 0  # Reset total cases for each file

    dat <- data.frame()

    repeat {
      # Capture the start time of the loop
      loop_start_time <- Sys.time()

      data_chunk <- read_sas(file_path, skip = total_cases, n_max = chunk_size)
      if (nrow(data_chunk) == 0) {
        break
      }

      data_chunk <- data_chunk %>% mutate_if(is.character, ~as.factor(as.character(.)))

      df <- data_chunk %>%
        filter_at(vars(all_of(columns_to_filter)), any_vars(. %in% codes_to_filter))

      dat <- rbind(dat, df)

      total_cases <- total_cases + nrow(data_chunk)

      # Capture the end time of the loop and calculate the duration
      loop_end_time <- Sys.time()
      cat("File: ", basename(file_path) , "\n")
      cat("Number of total cases: ", total_cases , "\n")
      cat("Number of filtered cases: ", nrow(dat) , "\n")
      loop_time = difftime(loop_end_time, loop_start_time, units = "secs")
      cat("Time for this loop: ",loop_time,"secs","\n")
      tttime = tttime + loop_time
      cat("Total time: ",tttime/60,"min","\n")
    }

    # Save the filtered data to a separate file
    output_file <- file.path(output_dir, paste0(basename(file_path),output_name, "_filtered.csv"))
    write.csv(dat, output_file, row.names = FALSE)
  }

  # Calculate the total time
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "secs")
  cat("Total time: ", total_time/60,"minutes", "\n")

}



NIS.COLUMNS.PATTERN <- function(file_paths, words,
                                columns_to_filter, output_dir,
                                output_name="_",chunk_size = 100000) {
  library(haven)
  library(tidyverse)
  library(pbapply)

  chunk_size <- chunk_size
  dat <- data.frame()
  tttime <- 0
  pattern <- paste(words, collapse = "|")
  # Initialize the progress bar
  pboptions(type = 1, style = 3)

  # Start the timer
  start_time <- Sys.time()

  for (file_path in file_paths) {
    total_cases <- 0  # Reset total cases for each file

    dat <- data.frame()

    repeat {
      # Capture the start time of the loop
      loop_start_time <- Sys.time()

      data_chunk <- read_sas(file_path, skip = total_cases, n_max = chunk_size)
      if (nrow(data_chunk) == 0) {
        break
      }

      data_chunk <- data_chunk %>% mutate_if(is.character, ~as.factor(as.character(.)))

      df <- data_chunk %>%
        filter_at(vars(all_of(columns_to_filter)), any_vars(str_detect(., pattern)))

      dat <- rbind(dat, df)

      total_cases <- total_cases + nrow(data_chunk)

      # Capture the end time of the loop and calculate the duration
      loop_end_time <- Sys.time()
      cat("File: ", basename(file_path) , "\n")
      cat("Number of total cases: ", total_cases , "\n")
      cat("Number of filtered cases: ", nrow(dat) , "\n")
      loop_time = difftime(loop_end_time, loop_start_time, units = "secs")
      cat("Time for this loop: ",loop_time,"secs","\n")
      tttime = tttime + loop_time
      cat("Total time: ",tttime/60,"min","\n")
    }

    # Save the filtered data to a separate file
    output_file <- file.path(output_dir, paste0(basename(file_path),output_name, "_filtered.csv"))
    write.csv(dat, output_file, row.names = FALSE)
  }

  # Calculate the total time
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "secs")
  cat("Total time: ", total_time/60,"minutes", "\n")

}




