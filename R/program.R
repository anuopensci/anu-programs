program_courses <- function(program_course_list, overwrite = FALSE) {
  map2_dfr(program_course_list$course_code, 
          program_course_list$type,
          ~course_info(.x, overwrite = overwrite) |> 
            mutate(type = .y)) 
}

program_study_table <- function(program) {
  html_page <- read_html(glue::glue("https://programsandcourses.anu.edu.au/program/{program}"))
  html_page |> 
    html_element(".table-container") |> 
    as.character() |> 
    str_replace_all("href=\"", "target=\"_blank\" href=\"https://programsandcourses.anu.edu.au") |>
    htmltools::HTML()
}


program_course_list <- function(program, has_study_table = TRUE) {
  html_page <- read_html(glue::glue("https://programsandcourses.anu.edu.au/program/{program}"))
  
  title <- html_page |> 
    html_element(".intro__degree-title") |> 
    html_text() |> 
    str_remove_all("\\r\\n") |> 
    str_trim()
  
  courses <- html_page |> 
    html_element("#study") |> 
    html_elements("p") |> 
    html_elements("a") |> 
    html_text()
  
  res <- tibble(year = NA,
                course_code = courses,
                type = "listed")
  
  if(has_study_table) {
    studytable <- html_page |> 
      html_element(".table-container") |> 
      html_table() 
    
    tidyst <- studytable |> 
      mutate(across(-X1, ~str_extract_all(., "[A-Z]{4}[0-9]{4}"))) |> 
      mutate(X1 = str_replace_all(X1, "\\r\\n.+", "")) |> 
      rename(year = X1) |> 
      pivot_longer(cols = -year, names_to = "delete", values_to = "course_code") |> 
      select(-delete) |> 
      unnest_longer(course_code) |> 
      filter(str_detect(course_code, "^[A-Z]{4}[0-9]{4}$")) |> 
      mutate(type = "studytable")
    
    res <- bind_rows(tidyst, res)
  } 

 res <- res |> 
    mutate(program_code = program,
           program_name = title, 
           .before = year)
  
  write_csv(res, here::here(glue::glue("data/data-raw/program-{program}.csv")))
  
  res
}
