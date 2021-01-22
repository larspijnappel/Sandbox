## source(str_c(here::here(), "/r_functions/show_summaries.R"))
## `here` package > http://jenrichmond.rbind.io/post/how-to-use-the-here-package/

# mpg %>% show_summaries()
## call function w/ providing a short description of the dataframe
# mpg %>% show_summaries(df_name = "mpg tidyverse dataset")

show_summaries <- function(df, df_name = "<not provided>") {
  require(tidyverse)
  require(skimr)
  require(Hmisc)
  # browser()
  
  get_section_header <- function(section_name, df_name = "") {
    ## determine first part of section header
    x <- str_glue("=== {section_name} {df_name} ") %>% str_trim()
    x <- str_glue("\n\n{x} ")
    
    ## complete the section header
    str_c(
      str_pad(
        string = x,
        width = 144,
        side = "right",
        pad = "="
      ),
      "\n\n"
    )
  }
  
  cat(get_section_header("summaries for dataframe:", df_name))
  
  cat(get_section_header("glimpse"))
  df %>% glimpse()
  
  cat(get_section_header("skim"))
  df %>% 
    skim() %>% 
    ## skim won't return output when used in a function, unless..
    print()
  
  cat(get_section_header("describe"))
  df %>% describe(df_name)
}
