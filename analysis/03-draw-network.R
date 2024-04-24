library(tidyverse)
library(visNetwork)
targets::tar_source()

# get current courses and required resquisites ----------------------------

fns <- dir("data/data-raw", pattern = "course-", full.names = TRUE)
courses <- map_dfr(fns, read_csv)
courses |> 
  mutate(across(c(requisites, incompatible, schedule), ~str_split(., ";")))

course_reqs <- courses |> 
  select(-incompatible, -schedule) |> 
  separate_longer_delim(requisites, ";") |> 
  filter(!requisites %in% courses$course_code) |> 
  filter(!is.na(requisites)) |> 
  pull(requisites) |> 
  unique()

course_reqs_df <- map_dfr(course_reqs, course_info)


# combine the reqs + mains ------------------------------------------------

course_df <- bind_rows(mutate(courses, type = "main"), 
                       mutate(course_reqs_df, main = "req")) |> 
  separate_longer_delim(requisites, ";") |> 
  #mutate(schedule = map(schedule, ~str_split(., ";"))) |> 
  select(-incompatible)

course_nodes <- course_df |> 
  select(label = course_code, title = course_name, schedule, type) |> 
  mutate(id = 1:n(), .before = label) |> 
  mutate(schedule = str_replace_all(schedule, ";", ","),
         schedule = ifelse(is.na(schedule), "Missing", schedule)) |> 
  mutate(shape = "box",
         shadow = TRUE,
         #color.background = case_when(str_detect(label, "COMP") ~ "dodgerblue",
        #                              str_detect(label, "STAT") ~ "orange",
         #                             TRUE ~ "lightpink"),
         color.background = case_match(type,
                                       "main" ~ "dodgerblue",
                                       "req" ~ "orange",
                                       .default = "lightpink"),
         color.border = "black",
         color.highlight = "yellow",
         url = paste0("https://programsandcourses.anu.edu.au/course/", label))

course_edges <- course_df |>
  mutate(from = match(requisites, course_nodes$label),
         to = match(course_code, course_nodes$label)) |> 
  select(from, to) |> 
  filter(!is.na(from)) |> 
  mutate(arrows = "to")

visNetwork(course_nodes, course_edges, width = "100%", height = "500px") %>% 
  visEvents(selectNode = 
              "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") |> 
  visOptions(selectedBy = list(variable = "schedule", multiple = TRUE),
             nodesIdSelection = TRUE) |> 
  visLayout(improvedLayout = TRUE, clusterThreshold = 40)
  