library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")

library(dplyr)
drop_acc() %>% data.frame()

library(Email_Agent_Template)

context('core Email Template functionality')

test_check("Email_Agent_Template")

test_that('functionality of load data function'{
  
})


##if a certain input is given, function should produce a certain ##
##excel file which is save to the dropbox account##
