import_tripleA_data <- function() {
  
  ## ImportData returns the list object `l_imp` back to the `l_df` object.
  
# Init --------------------------------------------------------------------
  library(tidyverse)     ## https://github.com/tidyverse/tidyverse
  library(zip)
  
  l_imp <- list()

  read_zip_data <- function(zip_fpn, data_fn = "") {
    # browser()
    
    ## names of files in zip are constructed in a similar fashion as 
    ## the zip-file itself, where the type of file is appended to the file name
    data_fn <- 
      str_c(str_sub(basename(zip_fpn), end = 17),
            data_fn
            )
    
    ## prevent errors when archive does not contain the specified file
    file_in_zip <- data_fn %in% zip_list(zip_fpn)$filename

    df <- 
      NULL %>%   ## shorthand for df <- NULL
      when(file_in_zip & str_detect(data_fn, ".csv$")   ~ read_csv(file = unz(zip_fpn, data_fn)),
           file_in_zip & str_detect(data_fn, ".Rdata$") ~ dget(file = unz(zip_fpn, data_fn)),
           TRUE                                         ~ str_c("Missing ", data_fn)
           )

  }
  
# Select file -------------------------------------------------------------
  # browser()
  
  zip_fpn  <- file.choose()
  zip_dir  <- zip_fpn %>% dirname()
  zip_bn   <- zip_fpn %>% basename()
  zip_type <- "AWB"
  zip_awb  <- zip_bn %>% str_sub(end = 12)
  zip_env  <- zip_bn %>% str_sub(start = 15, end = 17)
  
  ## show details of local file
  l_imp$meta_local_fn <- str_c(zip_type, " ", zip_awb, " (", zip_env, ")")
  l_imp$meta_local_fp <- str_c(str_replace(zip_dir, pattern = "C:/Users/pijnapla", replacement = ".."), "/")
  l_imp$meta_local_bn <- zip_bn
  

# Read CSV files ----------------------------------------------------------

  l_imp$df_bkg_h      <- read_zip_data(zip_fpn, "_bkg-hist.csv")
  l_imp$df_bkg_info_h <- read_zip_data(zip_fpn, "_bkg-info-hist.csv")
  l_imp$df_leg_h      <- read_zip_data(zip_fpn, "_leg-hist.csv")
  l_imp$df_line_h     <- read_zip_data(zip_fpn, "_line-hist.csv")
  l_imp$df_cust_h     <- read_zip_data(zip_fpn, "_cust-hist.csv")
  l_imp$df_remarks    <- read_zip_data(zip_fpn, "_bkg-rmrk.csv")
  l_imp$df_edi        <- read_zip_data(zip_fpn, "_edi-msg.csv")
  l_imp$df_sod2df     <- read_zip_data(zip_fpn, "_sod2df.Rdata")
  
# PSH Suffix --------------------------------------------------------------

  ## BKG_PSH_SUFFIX: convert to ordered factor (move P-level to the start)
  x <- factor(l_imp$df_bkg_h$BKG_PSH_SUFFIX)
  x <- c("P", levels(x)[!str_detect(levels(x), "P")])
  l_imp$df_bkg_h$BKG_PSH_SUFFIX <- factor(l_imp$df_bkg_h$BKG_PSH_SUFFIX, levels = x, ordered = T)
  
  
# Return results ----------------------------------------------------------

  l_imp
  
  }