get_tabyl_2 <- function(df = df, by_row = by_row, by_col = by_col, sort_by = Total) {
  library(janitor)
  
  ## embrace promised arguments
  ## see vignette("programming") section Indirection
  
  df %>%
    tabyl({{by_row}}, {{by_col}}) %>% 
    # adorn_totals(where = c("row", "col")) %>%
    adorn_totals("col") %>%
    adorn_percentages() %>% 
    adorn_pct_formatting() %>% 
    adorn_ns() %>%arrange(desc({{sort_by}})) %>%
    as_tibble()
}

# storms %>% get_tabyl_2(name, status)
# storms %>% get_tabyl_2(by_row = category, by_col = status, sort_by = category)
# storms %>% get_tabyl_2(category, status)
# mtcars %>% get_tabyl_2(cyl, am, cyl)
# mtcars %>% get_tabyl_2(carb, cyl, carb)
# mtcars %>% get_tabyl_2(gear, cyl)
