# https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset

covid <- read.csv("C:/Users/ps3aj/Desktop/COVID19_line_list_data.csv", header = TRUE)

# clearCovid <- covid[c("summary","location","country","age","visiting.Wuhan","from.Wuhan","death","recovered","symptom")]
# clearCovid[c("visiting.Wuhan", "from.Wuhan", "death", "recovered")] <- 
#   lapply(clearCovid[c("visiting.Wuhan", "from.Wuhan", "death", "recovered")], as.numeric) 

# Arreglar esto
# discCOVID <- discretizeDF(clearCovid, methods = 
#                          list( age = 
#                                  list(method = "frequency", breaks = 3, labels = c("joven", "adulto", "viejo"))
#                                ,
#                                from.Wuhan =
#                                  list(method = "frequency", breaks = 2, labels = c("Si", "No"))
#                                ,
#                                visiting.Wuhan =
#                                  list(method = "frequency", breaks = 2, labels = c("Si", "No"))
#                                ,
#                                death = 
#                                  list(method = "frequency", breaks = 2, labels = c("Si", "No"))
#                                ,
#                                recovered =
#                                  list(method = "frequency", breaks = 2, labels = c("Si", "No"))
#                          )
# )

clearCovid <- covid[c("location","country","age","visiting.Wuhan","from.Wuhan","symptom")]
clearCovid[c("visiting.Wuhan", "from.Wuhan")] <- 
  lapply(clearCovid[c("visiting.Wuhan", "from.Wuhan")], as.numeric) 

# Contemplando simtomas, info pequeña
clearCovid2 <- clearCovid
clearCovid2 <- clearCovid[complete.cases(clearCovid), ]
clearCovid2 <- clearCovid2[clearCovid2$symptom != "", ]
clearCovid2 <- separate_rows(clearCovid2,symptom,sep = ", ")
discCOVID <- clearCovid2
discCOVID$age <- ordered(cut(clearCovid2$age, c(0,33,66,200)),
                               labels = c("joven", "adulto", "viejo"))
discCOVID$from.Wuhan <- ordered(cut(clearCovid2$from.Wuhan, c(-1,0.5,1.1)),
                         labels = c("No", "Si"))
discCOVID$visiting.Wuhan <- ordered(cut(clearCovid2$visiting.Wuhan, c(-1,0.5,1.1)),
                                labels = c("No", "Si"))
rules <- apriori(discCOVID, parameter = list(support=0.1, confidence=0.01))
rulesPruned <- rules[!is.redundant(rules)]
inspect(rulesPruned)


#Sin sintomas
noSymptomsCOVID <- clearCovid[c("location","country","age","visiting.Wuhan","from.Wuhan")]
noSymptomsCOVID$age <- ordered(cut(noSymptomsCOVID$age, c(0,33,66,200)),
                         labels = c("joven", "adulto", "viejo"))
noSymptomsCOVID$from.Wuhan <- ordered(cut(noSymptomsCOVID$from.Wuhan, c(-1,0.5,1.1)),
                                labels = c("No", "Si"))
noSymptomsCOVID$visiting.Wuhan <- ordered(cut(noSymptomsCOVID$visiting.Wuhan, c(-1,0.5,1.1)),
                                    labels = c("No", "Si"))
rules2 <- apriori(noSymptomsCOVID, parameter = list(support=0.03, confidence=0.01))
inspect(subset(rules2, lhs %ain% "country=Spain"))
inspect(subset(rules2, lhs %ain% "country=Japan"))
inspect(subset(rules2, lhs %ain% "country=Germany"))


