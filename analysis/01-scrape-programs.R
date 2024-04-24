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
title

studytable <- html_page |> 
  html_element(".table-container") |> 
  html_table() 

studytable |> 
  mutate(across(-X1, ~str_extract_all(., "[A-Z]{4}[0-9]{4}"))) |> 
  mutate(X1 = str_replace_all(X1, "\\r\\n.+", "")) |> 
  rename(year = X1) |> 
  pivot_longer(cols = -year, names_to = "delete", values_to = "course_code") |> 
  select(-delete) |> 
  unnest_longer(course_code) |> 
  filter(str_detect(course_code, "^[A-Z]{4}[0-9]{4}$")) |> 
  mutate(program_code = program,
         program_name = title, 
         .before = year)
  
