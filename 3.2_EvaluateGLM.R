##

library(readr)

ppdata_test <- read_csv("Data/ppdata_test.csv")
View(ppdata_test)

revalue(ppdata_test$NCD, c("4+" = "4")) -> ppdata_test$NCD
revalue(ppdata_test$NCD, c("Unknown" = "3.5")) -> ppdata_test$NCD

ppdata_test$NCD <- as.numeric(as.character(ppdata_test$NCD))

unique(ppdata_simple$NCD, incomparables = FALSE)

ppdata_test$Predicted <- predict(glm_simple,newdata=ppdata_test,type='response')

write.csv(ppdata_test, file = "ppdata_test2.csv")

mean((ppdata_test$PurePrem-ppdata_test$Predicted)^2)

summary(glm_simple)










