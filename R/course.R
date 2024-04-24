course_info <- function(course, overwrite = FALSE) {
  fn <- here::here(glue::glue("data/data-raw/course-{course}.csv"))
  if(!overwrite & file.exists(fn)) return(read_csv(fn))
  
  url <- glue::glue("https://programsandcourses.anu.edu.au/course/{course}")
  html_page <- read_html(url)
  title <- html_page |> 
    html_element(".intro__degree-title") |> 
    html_text() |> 
    str_remove_all("\\r\\n") |> 
    str_trim()

  requisites_section <- html_page |> 
    html_element(".requisite") |> 
    html_text()
  
  # assumes incompatible statement at the end!
  requisites <- requisites_section |> 
    str_replace("Incompatible with (.+)", "")  |> 
    str_extract_all("[A-Z]{4}[0-9]{4}") |> 
    pluck(1)

  incompatible <- requisites_section |> 
    str_extract("Incompatible with (.+)") |> 
    str_extract_all("[A-Z]{4}[0-9]{4}") |> 
    pluck(1)
  
  incompatible2 <- requisites_section |> 
    str_extract("You are not able to enrol in this course if you have .+ completed .+") |> 
    str_replace("Incompatible with (.+)", "")  |> 
    str_extract_all("[A-Z]{4}[0-9]{4}") |> 
    pluck(1)
  
  incompatible <- na.omit(c(incompatible, incompatible2))
  requisites <- setdiff(requisites, incompatible)

  schedule <- html_page |> 
    html_element(".degree-summary-inner") |> 
    html_elements(".degree-summary__code") %>%
    magrittr::extract(str_detect(html_text(.), "Offered in")) |> 
    html_elements(".degree-summary__code-text") |> 
    html_text() |> 
    str_remove_all("\\r\\n") |> 
    str_trim() |> 
    str_subset("See Future Offerings", negate = TRUE)
  
  na_if_empty <- function(x) ifelse(any(is.na(x)) | any(x == "") | length(x)==0L, NA, paste0(x, collapse=";"))

  res <- tibble(course_code = course, 
                course_name = title, 
                requisites = na_if_empty(requisites), 
                incompatible = na_if_empty(incompatible),
                schedule = na_if_empty(schedule)) 

  write_csv(res, fn)
  
  res
}

course_reqs <- function(courses = NULL, nlevel = Inf) {
  if(is.null(courses)) {
    # get all if NULL
    fns <- dir("data/data-raw", pattern = "course-", full.names = TRUE)
    courses <- map_dfr(fns, read_csv)
  }
  
  course_reqs <- courses |> 
    select(-incompatible, -schedule) |> 
    separate_longer_delim(requisites, ";") |> 
    filter(!requisites %in% courses$course_code) |> 
    filter(!is.na(requisites)) |> 
    pull(requisites) |> 
    unique()
  
  course_reqs_df <- map_dfr(course_reqs, course_info)
  if(nlevel != 1 & nrow(course_reqs_df)) {
    course_reqs_df <- bind_rows(course_reqs_df,
                                course_reqs(bind_rows(courses, course_reqs_df), 
                                            nlevel - 1))
  }
  course_reqs_df
}