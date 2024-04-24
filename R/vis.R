

plot_network <- function(courses, required = NULL) {
  if(is.null(required)) {
    course_df <- courses |> 
      #separate_longer_delim(requisites, ";") |> 
      select(-incompatible) |> 
      mutate(type = "main")
  } else {
    required_filtered <- required |> 
      filter(!course_code %in% courses$course_code)
    
    course_df <- bind_rows(mutate(courses, type = "main"), 
                           mutate(required_filtered, type = "req")) |> 
      #separate_longer_delim(requisites, ";") |> 
      select(-incompatible)
  }
  
  course_nodes <- course_df |> 
    select(label = course_code, title = course_name, schedule, type) |> 
    mutate(id = 1:n(), .before = label) |> 
    mutate(schedule = str_replace_all(schedule, ";", ","),
           schedule = ifelse(is.na(schedule), "Missing", schedule)) |> 
    mutate(shape = "box",
           shadow = TRUE,
           color.background = case_when(title == "The page you are looking for doesn't exist" ~ "red",
                                        type == "main" ~ "dodgerblue",
                                        type == "req" ~ "orange",
                                         .default = "lightpink"),
           color.border = "black",
           color.highlight = "yellow",
           url = paste0("https://programsandcourses.anu.edu.au/course/", label))
  
  course_edges <- course_df  |> 
    separate_longer_delim(requisites, ";") |> 
    #filter(requisites %in% course_code) |> 
    mutate(from = match(requisites, course_nodes$label),
           to = match(course_code, course_nodes$label)) |> 
    select(from, to) |> 
    filter(!is.na(from)) |> 
    mutate(arrows = "to")
  
  #browser()
  
  
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
}