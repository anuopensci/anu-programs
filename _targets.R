library(targets)
tar_option_set(
  packages = c("tidyverse", "rvest") 
)

tar_source()

list(
  tar_target(madan, program_course_list("MADAN")),
  tar_target(mstat, program_course_list("MSTAT")),
  tar_target(bacts, program_course_list("BACTS")),
  tar_target(madan_courses, map_dfr(madan$course_code, ~course_info(.x, overwrite = FALSE))),
  tar_target(mstat_courses, map_dfr(mstat$course_code, ~course_info(.x, overwrite = FALSE)))
)