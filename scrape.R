packages <- c("rvest", "magrittr", "dplyr", "tidyr", "forcats", "ggplot2", "stringr")
required_packages <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(required_packages)) {
  install.packages(required_packages, dependencies = TRUE)
}

lapply(packages, require, character.only = TRUE)

scrape <- function(url = "https://www.thegazette.com/salaries/data-kirkwood-community-college-public-employee-salaries-for-fiscal-year-2023/")
{
  html <- rvest::read_html(url)
  
  html %>%
    rvest::html_elements("table#salaries") %>%
    rvest::html_table() %>%
    `[[`(1) %>%
    rename(
      #"name_first" = 1,
      #"name_last" = 2,
      "title" = 3,
      "salary" = 4) %>%
    
    # Original data combines the title, division, and department into one column. Split these up.
    separate_wider_delim(
      title,
      ", ",
      names = c("title", "division", "department"),
      too_few = "align_start",
      too_many = "merge") %>%
    
    # If there is no specified department, use the division value
    mutate(department = coalesce(department, division)) %>%
    
    # Combine "Allied Health Programs" and "Allied Health" departments
    mutate(department = case_match(
      department,
      "Allied Health Programs" ~ "Allied Health",
      .default = department)) %>%
  
    # Use a factor for the title
    mutate(title = fct_infreq(title)) %>%
    
    # Use a factor for the department
    mutate(department = fct_infreq(department)) %>%
    
    # Convert salary into a number
    mutate(salary = as.numeric(gsub("\\$|,", "", salary)))
}