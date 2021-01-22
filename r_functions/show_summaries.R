## source(str_c(here::here(), "/r_functions/show_summaries.R"))
## `here` package > http://jenrichmond.rbind.io/post/how-to-use-the-here-package/

# mpg %>% show_summaries()
## call function w/ providing a short description of the dataframe
# mpg %>% show_summaries(df_name = "mpg tidyverse dataset")

show_summaries <- function(df, df_name = "<not provided>") {
  require(tidyverse)     ## https://github.com/tidyverse/tidyverse
  require(skimr)         ## https://github.com/ropensci/skimr
  require(Hmisc)         ## https://github.com/harrelfe/Hmisc
  # require(summarytools)  ## https://github.com/dcomtois/summarytools
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

  ## following won't work when used in a function - probably because of the 
  ## result being written to an Output file before it's shown in the Viewer.
  # cat(get_section_header("summarytools"))
  # df %>% 
  #   ## see summarytools section 2.4
  #   dfSummary() %>% 
  #   ## show results in Viewer
  #   view()
}
