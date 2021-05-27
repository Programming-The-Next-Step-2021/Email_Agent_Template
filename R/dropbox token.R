library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")



library(dplyr)
drop_acc() %>% data.frame()

list('Monday'= list("morning","afternoon","evening"),
     'Tuesday'= list("morning","afternoon","evening"),
     'Wednesday'= list("morning","afternoon","evening"),
     'Thursday'= list("morning","afternoon","evening"),
     'Friday'= list("morning","afternoon","evening"),
     'Saturday'= list("morning","afternoon","evening"),
     'Sunday'= list("morning","afternoon","evening"))),