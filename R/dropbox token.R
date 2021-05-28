library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")


