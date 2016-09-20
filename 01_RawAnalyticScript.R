library(tidyverse) ## loadtidyverse library commands without reference

raw_data <- read_csv(file = "rawData.csv", na=c("","NA","-999","-888")) #load raw data while making NA & extreme values as not available
str(raw_data) ## See data structure frame

categorical_variables <- select(raw_data,group,gender) #define categorical variables as group & gender columns

categorical_variables$group <- as.factor(categorical_variables$group) ## sets categorical variable specific for group

categorical_variables$gender <- as.factor(categorical_variables$gender) ## seets categorical variable specific for gender
levels(categorical_variables$gender) <- list("Male" =1, "Female" =2) ## define numerical values/categories for strings

affective_commitment_items <- select (raw_data, AC1, AC2, AC3, AC4, AC5) ##defines columns for affective commitment
agreeableness_items <- select (raw_data, A1, A2, A3, A4, A5) ##defines columns for agreeableness
extroversion_items <- select (raw_data, E1,E2,E3,E4,E5) ##defines columns for extroversion

psych::describe(extroversion_items) ##psych command to describe data set(mean,sd,se,kurtosis,etc)
psych::describe(agreeableness_items)
agreeableness_items
is_bad_value <- agreeableness_items<1 | agreeableness_items>5 ##define variable as the range of items for agreeableness that exceed boundary
agreeableness_items[is_bad_value] <- NA ##substute all agreeableness values in bad range to NA

psych::describe(affective_commitment_items) ##substitution for affective
is_bad_value2 <- affective_commitment_items<1 | affective_commitment_items>7
affective_commitment_items[is_bad_value2] <- NA

agreeableness_items <- mutate(agreeableness_items, A5=6-A5) ##reverse code for agreeableness items, add one to max of scale

affective_commitment_items <- mutate(affective_commitment_items,AC4=8-AC4) ##reverse code for affective items
affective_commitment_items<- mutate(affective_commitment_items, AC5=8-AC5)

agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)$scores ##generate averaged agreeablenessmeasure
extroversion <- psych::alpha(as.data.frame(extroversion_items), check.keys = FALSE)$scores ##generate extroversion average measure
affective_commitment <- psych::alpha(as.data.frame(affective_commitment_items), check.keys = FALSE)$scores ##generate affective average

analytic_data <- cbind(categorical_variables,agreeableness,extroversion,affective_commitment) ##generate new data set that is stiched categorical measures with new averaged measures. Uses Alpha command with $scores to generate new values

save(analytic_data,file="study1_analytic_data.RData") ##save as a R file
write_csv(analytic_data,path ="study2_analytic_data.csv") ##save as a csv file
library(haven) ##load haven commands without reference
write_sav(analytic_data,path="study1_analytic_data.sav") ##save as a spss .sav