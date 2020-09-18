#Advance Econometric Project

# 0. Neccesary packages:

library(dplyr) 
library(ggplot2)

# 1. Cleaning the data:

setwd("/Users/lajobu/Desktop/Projects/Econ/survey/") #Set folder
survey <- read.csv("survey_results_public.csv")
survey

dim(survey) 

# The original file has 98855 rows (number of people that replied the survey) and 192 columns (number of feautures, mostly the questions of the survey)

# Selected variables:  
      # - Dependent variable: CareerSatisfaction
      # - Independent variables: Country, FormalEducation, CompanySize, DevType, YearsCoding, JobSatisfaction, ConvertedSalary, Hobby, RaceEthnicity & Age.

colnames(survey)

survey_sel <- survey %>%
  dplyr::select(CareerSatisfaction, Country, FormalEducation, CompanySize, YearsCoding, JobSatisfaction, ConvertedSalary, Hobby, Age)

dim(survey_sel)
# Our selection contains 9 variables (one of them is a dependent variable)

any(is.na(survey_sel)) #It is TRUE, hence there are some NA values in our data
any(survey_sel$ConvertedSalary == 0) #There are zero salaries

survey_sel <- survey_sel %>% #To remove 0
  filter(ConvertedSalary!=0) %>% 
  na.omit(survey_sel)

any(is.na(survey_sel)) #It is FALSE, hence there is not any NA value in our data
any(survey_sel$ConvertedSalary <= 0) #It is FALSE, hence there is not any 0 value in our data

survey_sel <- survey_sel %>% #Filter some eastern european countries
  filter(Country %in% c("Poland", "Czech Republic", "Hungary", "Slovakia", "Romania", "Bulgaria", "Turkey", "Moldova", "Belarus", "Ukraine") ) 

dim(survey_sel) #Our final dataset will have 2270 respondents, and 9 variables

unique(survey_sel[,1]) 

#saveRDS(survey_sel,'survey_sel_an.Rdata')

#Variables analysis:

survey_sel_or <- survey_sel %>% 
  filter(survey_sel$ConvertedSalary < 250000) 

saveRDS(survey_sel_or, "survey_sel_or.Rdata")

#Target variable:

unique(survey_sel$CareerSatisfaction)

# [1] "Slightly satisfied"                 "Slightly dissatisfied"              "Moderately dissatisfied"           
# [4] "Extremely satisfied"                "Moderately satisfied"               "Neither satisfied nor dissatisfied"
# [7] "Extremely dissatisfied"  

#As we can see above, the levels are not numeric, hence the first step will be to transform the levels to numbers.
      # -Extremely dissatisfied - 1
      # -Moderately dissatisfied - 2
      # -Slightly dissatisfied - 3
      # -Neither satisfied nor dissatisfied - 4
      # -Slightly satisfied - 5
      # -Moderately satisfied - 6
      # -Extremely satisfied - 7


as.character(survey_sel$CareerSatisfaction) <-
plyr::revalue(survey_sel$CareerSatisfaction,
    c(
      "1" = 1,
      "2" = 1,
      "3" = 1,
      "4" = 2,
      "5" = 3,
      "6" = 3,
      "7" = 3
    )
  ) %>% as.numeric()

survey_sel$CareerSatisfaction <- factor(survey_sel$CareerSatisfaction, levels = (1:7)) #To order the levels in ascending order

#Country:

unique(survey_sel$Country)

# [1] "Poland"         "Romania"        "Turkey"         "Slovakia"       "Bulgaria"       "Belarus"        "Czech Republic" "Ukraine"       
# [9] "Hungary" 

survey_sel$Country <- survey_sel$Country %>% 
  as.factor()

#FormalEducation:

unique(survey_sel$FormalEducation)

# [1] "Some college/university study without earning a degree"                            
# [2] "Master’s degree (MA, MS, M.Eng., MBA, etc.)"                                       
# [3] "Bachelor’s degree (BA, BS, B.Eng., etc.)"                                          
# [4] "Professional degree (JD, MD, etc.)"                                                
# [5] "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"
# [6] "Other doctoral degree (Ph.D, Ed.D., etc.)"                                         
# [7] "Primary/elementary school"                                                         
# [8] "Associate degree"                                                                  
# [9] "I never completed any formal education" 

survey_sel$FormalEducation <-
  plyr::revalue(survey_sel$FormalEducation,
                c(
                  "I never completed any formal education"= 1,
                  "Primary/elementary school" = 1,
                  "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)" = 1,
                  
                  "Professional degree (JD, MD, etc.)" = 2,
                  "Some college/university study without earning a degree"  = 2,
                  "Associate degree" = 2,
                  
                  "Bachelor’s degree (BA, BS, B.Eng., etc.)"  = 3,
                  "Master’s degree (MA, MS, M.Eng., MBA, etc.)" = 3,
                  "Other doctoral degree (Ph.D, Ed.D., etc.)" = 3
                )
  ) %>% as.numeric()

#CompanySize:

unique(survey_sel$CompanySize)

# [1] "20 to 99 employees"       "10,000 or more employees" "1,000 to 4,999 employees" "5,000 to 9,999 employees" "100 to 499 employees"    
# [6] "500 to 999 employees"     "Fewer than 10 employees"  "10 to 19 employees"    

survey_sel$CompanySize <-
  plyr::revalue(survey_sel$CompanySize,
                c(
                  "Fewer than 10 employees" = 1,
                  "10 to 19 employees" = 1,
                  "20 to 99 employees" = 1,
                  
                  "100 to 499 employees" = 2,
                  "500 to 999 employees" = 2,
                  
                  "1,000 to 4,999 employees"  = 3,
                  "5,000 to 9,999 employees" = 3,
                  "10,000 or more employees"  = 3
                )
  ) %>% as.numeric()

#YearsCoding:

unique(survey_sel$YearsCoding)

# [1] "3-5 years"        "6-8 years"        "0-2 years"        "12-14 years"      "9-11 years"       "24-26 years"      "18-20 years"     
# [8] "15-17 years"      "30 or more years" "21-23 years"      "27-29 years"     

survey_sel$YearsCoding <-
  plyr::revalue(survey_sel$YearsCoding,
                c(
                  "0-2 years" = 1, #amateur
                  "3-5 years" = 1,
                  
                  "6-8 years" = 2, #middle
                  "9-11 years" = 2,
                  "12-14 years" = 3, #senior
                  "15-17 years"  = 3,
                  
                  "18-20 years" = 4, #upper-senior
                  "21-23 years"  = 4,
                  "24-26 years"  = 4,
                  "27-29 years" = 4,
                  "30 or more years" = 4
                )
  ) %>% as.numeric()

#JobSatisfaction:

unique(survey_sel$JobSatisfaction)

# [1] "Slightly satisfied"                 "Moderately satisfied"               "Slightly dissatisfied"             
# [4] "Neither satisfied nor dissatisfied" "Extremely satisfied"                "Extremely dissatisfied"            
# [7] "Moderately dissatisfied" 

survey_sel$JobSatisfaction <-
  plyr::revalue(survey_sel$JobSatisfaction,
                c(
                  "Extremely dissatisfied" = 1,
                  "Moderately dissatisfied" = 1,
                  "Slightly dissatisfied" = 1,
                  
                  "Neither satisfied nor dissatisfied" = 2,
                  
                  "Slightly satisfied" = 3,
                  "Moderately satisfied" = 3,
                  "Extremely satisfied" = 3
                )
  ) %>% as.numeric()

#Hobby:

unique(survey_sel$Hobby)

# [1] "No"  "Yes"

survey_sel$Hobby <-
  plyr::revalue(survey_sel$Hobby,
                c(
                  "No" = 0,
                  "Yes" = 1
                )
  ) %>% as.numeric()

#Age:

unique(survey_sel$Age)

#[1] "25 - 34 years old"  "45 - 54 years old"  "35 - 44 years old"  "18 - 24 years old"  "Under 18 years old" "55 - 64 years old"         

survey_sel$Age <-
  plyr::revalue(survey_sel$Age,
                c(
                  "Under 18 years old" = 1, #young
                  "18 - 24 years old" = 1,
                  
                  "25 - 34 years old" = 2, #medium
                  "35 - 44 years old" = 2,
                  
                  "45 - 54 years old" = 3, #elderly
                  "55 - 64 years old" = 3
                )
  ) %>% as.numeric()

survey_sel$CareerSatisfaction <- survey_sel$CareerSatisfaction %>% 
  as.numeric()

#saveRDS(survey_sel,'survey_an1.Rdata')

#saveRDS(survey_sel,'survey_sel.Rdata')

################################################################################################################################################

# 2. Data desciption:

survey_sel_an <- readRDS("survey_sel_an.Rdata") #As factors
survey_an1 <- readRDS("survey_an1.Rdata") #As numerical

#Descriptioon of data:

psych::describe(survey_an1[,c(1:9)],) %>% 
  knitr::kable(align = "l",digits = 2)

# [1] Poland         Romania        Turkey         Slovakia       Bulgaria       Belarus        Czech Republic Ukraine        Hungary       
# Levels: Belarus Bulgaria Czech Republic Hungary Poland Romania Slovakia Turkey Ukraine

survey_an1$Country <- 
  plyr::revalue(survey_an1$Country,
                c(
                  "Poland" = 1,
                  "Romania" = 2,
                  "Turkey" = 3,
                  "Slovakia" = 4,
                  "Bulgaria" = 5,
                  "Belarus" = 6,
                  "Czech Republic" = 7,
                  "Ukraine" = 8,
                  "Hungary" = 9
                )
  ) %>% as.numeric()

#GRAPHS:

#Target variable:
par(mfrow=c(1, 1))
hist(survey_an1[,1], xlab= NULL, ylab= NULL,
     main = names(survey_an1[1]), col="steelblue")

counts <- table(survey_an1[1])
barplot(counts, main= names(survey_an1[1]), ylab= NULL, col="steelblue") 


# require(scales)
# ggplot(survey_sel_an, aes(CareerSatisfaction)) +
#   geom_bar(fill = "steelblue") +
#   theme_minimal() +
#   scale_y_continuous(labels = comma) +
#   theme(axis.text.x = element_text(angle = 50, hjust = 0.95)) +
#   theme(text = element_text(size=12)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank())

#Multinomial variables

a <- c(2, 3,4,5,6,9)
par(mar=c(2,2,2,2), mfrow=c(2,4), cex= 0.7)
for (i in a) {
  hist(survey_an1[,i], xlab= NULL, ylab= NULL,
       main = names(survey_an1[i]), col="steelblue")
}

#Binomial variables:

a <- sum(survey_an1$Hobby == 1)
b <- sum(survey_an1$Hobby == 0)
c <- a + b
x <- c(a/c, b/a)

labels <- c("Yes", "No")
pie(x, labels, col= c("steelblue", "grey"),  main = "Hobby")

#Numerical variables:

d <- density(survey_an1$ConvertedSalary)
plot(d, main="ConvertedSalary", xlab= '', ylab= '', yaxt='n')
polygon(d, col="steelblue", border="black") 

################################################################################################################################################

#Hypothesis:

#H1: Salary increases career satisfaction
#H2: Salary and years are jointly significant in the model


survey_sel <- readRDS("/Users/lajobu/Downloads/survey_sel.Rdata")

colnames(survey_sel)

survey_sel$CareerSatisfaction = as.factor(survey_sel$CareerSatisfaction) #To transfor the dependent variable to factor

options(max.print = 10000)

ologit.survey <- oglmx::ologit.reg(CareerSatisfaction~Country+FormalEducation+CompanySize+DevType+YearsCoding+
                                     ConvertedSalary+Age, data= survey_sel) #Just selected variables, as it is not possible to run summary() with 11
View(summary(ologit.survey))
summary(ologit.survey)[["loglikelihood"]] #-3056.943
summary(ologit.survey)[["AIC"]] #7453.887
summary(ologit.survey)[["McFaddensR2"]] #0.1384801

