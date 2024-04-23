library(tidyverse)
library(rvest)
library(glue)


program <- "MADAN"
program <- "MSTAT"
url <- glue("https://programsandcourses.anu.edu.au/program/{program}")
html_page <- read_html(url)

title <- html_page |> 
  html_element(".intro__degree-title") |> 
  html_text() |> 
  str_remove_all("\\r\\n") |> 
  str_trim()

studytable <- html_page |> 
  html_element(".table-container") |> 
  html_table() 

studytable |> 
  mutate(across(everything(), ~str_replace_all(., "\\r\\n.+", ""))) |> 
  rename(year = X1) |> 
  pivot_longer(cols = -year, names_to = "delete", values_to = "course_code") |> 
  select(-delete) |> 
  filter(course_code != "", 
         str_detect(course_code, "[A-Z]{4}[0-9]{4}")) |> 
  mutate(program_code = program,
         program_name = title, 
         .before = year)
  
