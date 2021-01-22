# How to include external function(s) -------------------------------------

# library(here)  ## https://github.com/r-lib/here | http://jenrichmond.rbind.io/post/how-to-use-the-here-package/
# fpn <- str_c(
#   ## generic R functions directory are placed on same level as project directories
#   here() %>% dirname(),
#   "/r_functions/show_summaries.R"
# )
# source(fpn)

# Example usage show_summaries --------------------------------------------

# mpg %>% show_summaries()
# ## call function w/ providing a short description of the dataframe
# mpg %>% show_summaries(df_name = "mpg tidyverse dataset")


# Function show_summaries -------------------------------------------------

show_summaries <- function(df, df_name = "<not provided>") {
  require(tidyverse)     ## https://github.com/tidyverse/tidyverse
  require(skimr)         ## https://github.com/ropensci/skimr
  require(Hmisc)         ## https://github.com/harrelfe/Hmisc
  # require(summarytools)  ## https://github.com/dcomtois/summarytools
  # browser()
 
  section_width <- 144
  
  get_section_header <- function(section_name, df_name = "") {
    ## determine first part of section header
    x <- str_glue("=== {section_name} {df_name} ") %>% str_trim()
    x <- str_glue("\n\n{x} ")
    
    ## complete the section header
    str_c(
      str_pad(
        string = x,
        width = section_width,
        side = "right",
        pad = "="
      ),
      "\n\n"
    )
  }
  
  cat(str_c("\n\n", strrep("=", section_width)))
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
