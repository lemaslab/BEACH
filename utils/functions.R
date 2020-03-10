# useful functions

# data inventory(): 
data_inventory <- function(data = NULL, group_var, row_match, col_match, question,response){
  if(!missing(data)) {
    
    #Variable create
    col <- sym(col_match)
    row <- row_match
    group <-sym(group_var)
    
    # function 
    matching <- data %>%
      filter(!!col == row) %>%
      select(test_id,study_visit,question,response) %>%
      drop_na(!! response) %>%
      group_by(!! group) %>%
      summarize(count=length(response)) %>%
      mutate(percent=(count/max(count)*100)) %>%
      mutate(study_visit=row) %>%
      select(test_id,study_visit, everything())
  } else {
    message("You must specify data frame!")
  }
} # END FUNCTION
