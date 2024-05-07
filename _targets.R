library(targets)
tar_option_set(
  packages = c("tidyverse", "rvest", "visNetwork") 
)

tar_source()

list(
  tar_target(madan, program_course_list("MADAN")),
  tar_target(mstat, program_course_list("MSTAT")),
  tar_target(msda, program_course_list("MSDA", has_study_table = FALSE)),
  tar_target(bacts, program_course_list("BACTS")),
  tar_target(madan_courses, program_courses(madan)),
  tar_target(msda_courses, program_courses(msda)),
  tar_target(mstat_courses, program_courses(mstat)),
  #tar_target(bacts_courses, program_courses(bacts)),
  tar_target(course_required_all, course_reqs()),
  tar_target(mstat_required, course_reqs(mstat_courses)),
  tar_target(madan_required, course_reqs(madan_courses)),
  tar_target(msda_required, course_reqs(msda_courses)),
  #tar_target(bacts_required, course_reqs(bacts_courses)),
  NULL
)
