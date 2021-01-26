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
# ## call function w/ providing a short description for the section titles
# mpg %>% show_summaries(description = "mpg tidyverse dataset")
# ## use name of object as description
# mpg %>% show_summaries(description = substitute(.))


# Function show_summaries -------------------------------------------------

show_summaries <- function(df, description = "<not provided>") {
  # browser()
  
# Init --------------------------------------------------------------------
  require(tidyverse)     ## https://github.com/tidyverse/tidyverse
  require(skimr)         ## https://github.com/ropensci/skimr
  require(Hmisc)         ## https://github.com/harrelfe/Hmisc
  # require(summarytools)  ## https://github.com/dcomtois/summarytools

  section_width <- 144
  ## add the object's class(es) to the description
  description <- 
    str_c(description,
          " [class: ",
          df %>% class() %>% str_c(., collapse = "|"),
          "]"
          )
  
  ## (pipeline w/ conditionally steps: `purrr::when()` vs. `if()` statements)
  ## to prevent processing errors, convert to a dataframe class where needed
  df <-
    df %>% 
    when(
      any(str_detect(class(df), "array|table|ts")) ~ as_tibble(.),
      ~ .
      )
  # ## old fashion way w. `if()` statement
  # df <-
  # if (any(str_detect(class(df), "array|table"))) {
  #   df %>% as_tibble()
  # } else {
  #   df
  # }
  
# helper functions --------------------------------------------------------

  get_section_header <- function(section_name, description = "") {
    ## determine first part of section header
    x <- 
      str_glue("=== {section_name} {description} ") %>% 
      str_squish() %>% 
      str_c("\n\n", ., " ")
    
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
      ## skip processing when column is of type list ..
      if (is.list(df[[i]])) { 
        out_v[i] <- "<column type: list>"
      }
      
      ## .. else do process it
      if (!is.list(df[[i]])) {
        ## `sort()` must be done last, while
        ## numeric values should NOT be converted to characters
        ## (which is needed for sorting values in factoral columns)
        uv <- 
          df[i] %>%
          unique() %>%
          ## only use `pull()` on data.frame objects
          when(
            any(class(df) == "data.frame") ~ pull(.),
            ~ .
            ) %>% 
          ## conditionally convert column type factor to character, so that 
          ## `sort()` works as expected
          when(
            any(class(df[[i]]) == "factor") ~ as.character(.),
            ~ .
            ) %>% 
          sort()

        out_n[i] <- uv %>% length()
        out_v[i] <- uv %>% str_c(collapse = ", ")
      }
    }
    
# format output result ----------------------------------------------------

    ## add padding to column names
    cn <- 
      str_pad(cn,
              max(str_length(cn)),
              side = "right"
              )
    
    ## add padding before number of distinct values
    out_n <- 
      str_pad(out_n,
              max(str_length(out_n))
              )
    
    ## create final ouput having: column names|nr. of distinct values|distinct values
    out <- str_c(cn, "|", out_n, "|", out_v)
    
    ## take into account the required space for the indices
    i_width <- str_length(length(cn)) + 5
    
    ## remove stuff if output is linger then specified section_width
    out <- 
      if_else((str_length(out) + i_width) > section_width,
              ## cutoff values
              str_c(str_sub(out, end = section_width - i_width), ".."),
              out
              )

# return result -----------------------------------------------------------

    out %>% sort()
  }


# execute show_summaries --------------------------------------------------

  cat(str_c("\n", strrep("=", section_width)))
  cat(get_section_header("summaries for", description))
  
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
  
  cat(str_c("\n\n", strrep("=", section_width)))
  cat(get_section_header("describe"))
  ## remove any column of type list
  df[,sapply(df, class) != "list"] %>% describe(description)
  
  ## following won't work when used in a function - probably because of the 
  ## result being written to an Output file before it's shown in the Viewer.
  # cat(get_section_header("summarytools"))
  # df %>% 
  #   ## see summarytools section 2.4
  #   dfSummary() %>% 
  #   ## show results in Viewer
  #   view()
}