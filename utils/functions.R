# useful functions

data_inventory=function(df,group_var,event,question,response)
                {df %>%
                group_by(!! group_var) %>%
                dplyr::filter(df, study_visit== event) 
} # end function
  
  %>%
                select(group_var,event,question,response) %>%
                drop_na(response) %>%
                summarize(count=length(response)) %>%
                mutate(percent=(count/max(count)*100)) %>%
                mutate(study_visit=event) %>%
                select(group_var,event, everything())
                } # end function