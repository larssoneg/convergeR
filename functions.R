#' Generate sequences of a single variable
generate_sequence <- function(form, range) {
  
  # Parse input for validity
  detected_variables <- stringr::str_replace_all(string = form, pattern = "cos|sin|tan|\\(|\\)|\\^", replacement = "") %>%
                        stringr::str_extract_all(pattern = "[a-z]|[A-Z]") %>%
                        unlist() %>% 
                        unique()

  is_single_variable <- length(detected_variables) == 1
  
  if (!is_single_variable) {
    stop("Multiple or no variables detected. Specify a closed form of one variable.")
  }
  
  # Modify formula to be a function of x
  form %<>% stringr::str_replace_all(pattern = detected_variables, replacement = "x")
  x <- range

  # Evaluate
  out <- eval(parse(text = form))

  return(list(data = out, form = form))
  
}



#' Plot sequences of a single variable
#' Generate sequence using generate_sequence()
plot_sequence <- function(sequence_data, a, epsilon, range) {
  
  # Create data.frame from the vector sequence_data
  plot_data <- data.frame(n = range[1]:range[2], 
                          value = sequence_data$data,
                          a = a
                          )
  
  # Plot title
  plot_title <- paste("(a<sub>n</sub>) =", sequence_data$form)
  
  # Create plot
  p <- plotly::plot_ly(data = plot_data) %>%
    
       # Sequence
       plotly::add_markers(x = ~n, y = ~value, name = "a<sub>n</sub>") %>%
    
       # Value of a_n
       plotly::add_lines(x = ~n, y = ~a, color = "red", name = "Candidate limit") %>%

       # Epsilon neighborhood    
       plotly::add_ribbons(x = ~n, ymin = ~a - epsilon, ymax = ~a + epsilon, opacity = 0.6, name = "&#x0190;-neighborhood") %>%
    
       plotly::layout(title = plot_title,
                      yaxis = list(title = "a<sub>n</sub>"),
                      xaxis = list(title = "n", range = c(range[1], range[2])),
                      legend = list(orientation = "h", xanchor = "left", x = 0)
                     ) 


  return(p)  
}





