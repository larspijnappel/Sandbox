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
  
  get_col_values <- function(df) {
    cn    <- df %>% names()                   ## column names
    out_n <- vector("character", length(cn))  ## number of unique values
    out_v <- vector("character", length(cn))  ## unique values
    
# get unique values -------------------------------------------------------

    for (i in seq_along(cn)) {
      uv <- df[i] %>%
        unique() %>% 
        pull() %>% 
        replace_na("<NA>")
      
      out_n[i] <- uv %>% length()
      out_v[i] <- uv %>% sort() %>% str_c(collapse = ", ")
    }
    
# format output result ----------------------------------------------------

    ## add padding + separator to column names
    cn <- str_c(
      str_pad(cn,
              max(str_length(cn)),
              side = "right"
              ),
      "|"
    )
    ## add padding before + separator after number of distinct values
    out_n <- str_c(
      str_pad(out_n,
              max(str_length(out_n))
              ),
      "|"
      )
    
    ## create final ouput having: column names | nr. of distinct values | distinct values
    out <- str_c(cn, out_n, out_v)
    
    ## take into account the required space for the indices
    i_width <- str_length(length(cn)) + 5
    
    ## remove stuff if output is linger then specified section_width
    out <- if_else((str_length(out) + i_width) > section_width,
                   ## cutoff values
                   str_c(str_sub(out, end = section_width - i_width), ".."),
                   out
                   )

# return result -----------------------------------------------------------

    out %>% sort()
  }

# run + return different summaries ----------------------------------------

  cat(str_c("\n\n", strrep("=", section_width)))
  cat(get_section_header("summaries for", df_name))
  
  cat(get_section_header("glimpse"))
  df %>% glimpse()
  
  cat(get_section_header("unique values"))
  df %>% 
    get_col_values() %>% 
    ## this function won't return output when used in a function, unless ..
    print(quote = FALSE)
  
  cat(get_section_header("skim"))
  df %>% 
    skim() %>% 
    ## skim won't return output when used in a function, unless ..
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