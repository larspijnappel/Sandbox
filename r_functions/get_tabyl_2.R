get_tabyl_2 <- function(df, by_row, by_col, sort_by = Total, sort_desc = TRUE) {
  suppressPackageStartupMessages(library(janitor))
  
  ## embrace promised arguments
  ## see vignette("programming") section Indirection
  
  x <- df %>%
    tabyl({{by_row}}, {{by_col}}) %>% 
    # adorn_totals(where = c("row", "col")) %>%
    adorn_totals("col") %>%
    adorn_percentages() %>% 
    adorn_pct_formatting() %>% 
    adorn_ns() %>%
    as_tibble()
  
  ## when no sorting options are provided (i.e. `sort_by`, `sort_desc`), 
  ## then output is in descending order of the Total column.
  x %>%
    when(sort_desc == TRUE ~ x %>% arrange(desc({{sort_by}})),
         ~ x %>% arrange({{sort_by}})
         )

}

# storms %>% get_tabyl_2(name, status)
# storms %>% get_tabyl_2(by_row = category, by_col = status, sort_by = category)
# storms %>% get_tabyl_2(category, status)
# mtcars %>% get_tabyl_2(cyl, am, cyl)
# mtcars %>% get_tabyl_2(carb, cyl, carb)
# mtcars %>% get_tabyl_2(gear, cyl)
