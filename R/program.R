

program_course_list <- function(program, has_study_table = TRUE) {
  html_page <- read_html(glue::glue("https://programsandcourses.anu.edu.au/program/{program}"))
  
  title <- html_page |> 
    html_element(".intro__degree-title") |> 
    html_text() |> 
    str_remove_all("\\r\\n") |> 
    str_trim()
  
  if(has_study_table) {
    studytable <- html_page |> 
      html_element(".table-container") |> 
      html_table() 
    
    res <- studytable |> 
      mutate(across(-X1, ~str_extract_all(., "[A-Z]{4}[0-9]{4}"))) |> 
      mutate(X1 = str_replace_all(X1, "\\r\\n.+", "")) |> 
      rename(year = X1) |> 
      pivot_longer(cols = -year, names_to = "delete", values_to = "course_code") |> 
      select(-delete) |> 
      unnest_longer(course_code) |> 
      filter(str_detect(course_code, "^[A-Z]{4}[0-9]{4}$"))
  } else {
    courses <- html_page |> 
      html_element("#study") |> 
      html_elements(".ql-indent-1") |> 
      html_elements("a") |> 
      html_text()
    
    res <- tibble(year = NA,
                  course_code = courses)
  }
  

  
 res <- res |> 
    mutate(program_code = program,
           program_name = title, 
           .before = year)
  
  write_csv(res, here::here(glue::glue("data/data-raw/program-{program}.csv")))
  
  res
}