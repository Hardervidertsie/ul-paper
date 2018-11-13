http://www.biostathandbook.com/twowayanova.html

if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}

Input = ("
Instruction        Student  Month   Calories.per.day
         'Curriculum A'     a        1       2000
         'Curriculum A'     a        2       1978
         'Curriculum A'     a        3       1962
         'Curriculum A'     a        4       1873
         'Curriculum A'     a        5       1782
         'Curriculum A'     a        6       1737
         'Curriculum A'     b        1       1900
         'Curriculum A'     b        2       1826
         'Curriculum A'     b        3       1782
         'Curriculum A'     b        4       1718
         'Curriculum A'     b        5       1639
         'Curriculum A'     b        6       1644
         'Curriculum A'     c        1       2100
         'Curriculum A'     c        2       2067
         'Curriculum A'     c        3       2065
         'Curriculum A'     c        4       2015
         'Curriculum A'     c        5       1994
         'Curriculum A'     c        6       1919
         'Curriculum A'     d        1       2000
         'Curriculum A'     d        2       1981
         'Curriculum A'     d        3       1987
         'Curriculum A'     d        4       2016
         'Curriculum A'     d        5       2010
         'Curriculum A'     d        6       1946
         'Curriculum B'     e        1       2100
         'Curriculum B'     e        2       2004
         'Curriculum B'     e        3       2027
         'Curriculum B'     e        4       2109
         'Curriculum B'     e        5       2197
         'Curriculum B'     e        6       2294
         'Curriculum B'     f        1       2000
         'Curriculum B'     f        2       2011
         'Curriculum B'     f        3       2089
         'Curriculum B'     f        4       2124
         'Curriculum B'     f        5       2199
         'Curriculum B'     f        6       2234
         'Curriculum B'     g        1       2000
         'Curriculum B'     g        2       2074
         'Curriculum B'     g        3       2141
         'Curriculum B'     g        4       2199
         'Curriculum B'     g        5       2265
         'Curriculum B'     g        6       2254
         'Curriculum B'     h        1       2000
         'Curriculum B'     h        2       1970
         'Curriculum B'     h        3       1951
         'Curriculum B'     h        4       1981
         'Curriculum B'     h        5       1987
         'Curriculum B'     h        6       1969
         'Curriculum C'     i        1       1950
         'Curriculum C'     i        2       2007
         'Curriculum C'     i        3       1978
         'Curriculum C'     i        4       1965
         'Curriculum C'     i        5       1984
         'Curriculum C'     i        6       2020
         'Curriculum C'     j        1       2000
         'Curriculum C'     j        2       2029
         'Curriculum C'     j        3       2033
         'Curriculum C'     j        4       2050
         'Curriculum C'     j        5       2001
         'Curriculum C'     j        6       1988
         'Curriculum C'     k        1       2000
         'Curriculum C'     k        2       1976
         'Curriculum C'     k        3       2025
         'Curriculum C'     k        4       2047
         'Curriculum C'     k        5       2033
         'Curriculum C'     k        6       1984
         'Curriculum C'     l        1       2000
         'Curriculum C'     l        2       2020
         'Curriculum C'     l        3       2009
         'Curriculum C'     l        4       2017
         'Curriculum C'     l        5       1989
         'Curriculum C'     l        6       2020
         ")

Data = read.table(textConnection(Input),header=TRUE)

Data$Instruction = factor(Data$Instruction,
                          levels=unique(Data$Instruction))

library(psych)

headTail(Data)

str(Data)

summary(Data)

rm(Input)

require(ggplot2)
ggplot(data = Data, aes(x = Month, y = Calories.per.day, color = Student)) + geom_point() +
  geom_smooth( method = "loess") +
  facet_wrap(~ Instruction, ncol= 1)

model = lme(Calories.per.day ~ Instruction + Month + Instruction*Month, 
            random = ~1|Student,
            correlation = corAR1(form = ~ Month | Student,
                                 value = 0.4287),
            data=Data,
            method="REML")

Anova(model)


model.fixed = gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
                  data=Data,
                  method="REML")
anova(model, model.fixed)


library(rcompanion)

model.null = lme(Calories.per.day ~ 1,
                 random = ~1|Student,
                 data = Data)

nagelkerke(model, 
           model.null)



#50 individuals
12*2 *50
set.seed(1251)
SymptomScore <- melt(replicate(50, 5 + 2 * rnorm(1) + # random start point
                                 (0 + rnorm(1))* # mult factor of slope of pos or neg. improvement
                                 (0.1*(1:25))^0.1  +# pos. improvement term
                                  0.3*rnorm(25) # random meas error term
                               , simplify = "vector" ))[,3] 

ICT_data1 <- data.frame( Individual = paste("p",rep(1:50, each = 25 ), sep =""),
                        MeasurementDate = c("pre", 1:24),
                        BMI = rep(25 + 5*rnorm(25), each = 50) + 0.5*rnorm(50*25),
                        AgeGroup = "Adult",
                        Sex = rep(sample(c("M", "F"), 50, replace = TRUE), each = 25),
                        SymptomScore =  SymptomScore,
                        treatment = "treatmentA"
)


set.seed(12233)
SymptomScore <- melt(replicate(50, 4 + 2 * rnorm(1) + # random start point
                                 (1 + rnorm(1))* # mult factor of slope of pos or neg. improvement
                                 (0.2*(1:25))^0.5  +# pos. improvement term
                                 0.3*rnorm(25) # random meas error term
                               , simplify = "vector" ))[,3] 

ICT_data2 <- data.frame( Individual = paste("p",rep(51:100, each = 25 ), sep =""),
                        MeasurementDate = c("pre", 1:24),
                        BMI = rep(25 + 5*rnorm(25), each = 50) + 0.5*rnorm(50*25),
                        AgeGroup = "Adult",
                        Sex = rep(sample(c("M", "F"), 50, replace = TRUE), each = 25),
                        SymptomScore =  SymptomScore,
                        treatment = "treatmentB"
)


set.seed(6322861)
SymptomScore <- melt(replicate(50, 4 + 2 * rnorm(1) + # random start point
                                 (1 + rnorm(1))* # mult factor of slope of pos or neg. improvement
                                 (0.3*(1:25))^0.9  +# pos. improvement term
                                 0.3*rnorm(25) # random meas error term
                               , simplify = "vector" ))[,3] 

ICT_data3 <- data.frame( Individual = paste("p",rep(101:150, each = 25 ), sep =""),
                         MeasurementDate = c("pre", 1:24),
                         BMI = rep(25 + 5*rnorm(25), each = 50) + 0.5*rnorm(50*25),
                         AgeGroup = "Adult",
                         Sex = rep(sample(c("M", "F"), 50, replace = TRUE), each = 25),
                         SymptomScore =  SymptomScore,
                         treatment = "treatmentC"
)


ICT_data<- rbind(ICT_data1, ICT_data2, ICT_data3)
ICT_data$MeasurementDate <- factor(ICT_data$MeasurementDate, levels = unique(ICT_data$MeasurementDate))

veryhigh <- ICT_data$BMI <50
high <- ICT_data$BMI <30 
good <- ICT_data$BMI <25 
low <- ICT_data$BMI <20 
verylow <- ICT_data$BMI < 17 

ICT_data$BMI_cat[veryhigh] <- "veryhigh"
  ICT_data$BMI_cat[high] <- "high"
  ICT_data$BMI_cat[good] <- "good"
  ICT_data$BMI_cat[low] <- "low"
  ICT_data$BMI_cat[verylow] <- "verylow"
table(ICT_data$BMI_cat)





sum(is.na(ICT_data))
headTail(ICT_data)
summary(ICT_data$SymptomScore)


require(ggplot2)
ggplot(data = ICT_data, aes(x = MeasurementDate, y = SymptomScore, color = BMI_cat)) + 
  geom_point(aes(group = Individual)) + geom_line(aes(group = Individual)) +
  facet_grid(treatment~Sex, scales = "free") + theme_minimal()




plot(density(ICT_data$SymptomScore))
plot(density(ICT_data$BMI))
plot(ICT_data$SymptomScore)
plot(ICT_data$BMI)



ICT_data[ICT_data$Individual == "p1",]

head(ICT_data)

ICT_data$MeasurementDateI <- ICT_data$MeasurementDate
ICT_data$MeasurementDateI <- gsub("pre", 0, ICT_data$MeasurementDateI)
ICT_data$MeasurementDateI <- as.integer(ICT_data$MeasurementDateI)
model = lme(SymptomScore ~ BMI + Sex + treatment, 
            random = ~1|Individual,
            correlation = corAR1(form = ~ MeasurementDateI | Individual),
            data=ICT_data,
            method="REML")

model0 = gls(SymptomScore ~ BMI + Sex + treatment , 
            data=ICT_data,
            method="REML")

Anova(model)

anova(model, model0)

summary(model0)


summary(model)


# MeasurementDate (from just before the start of treatment to up to two years)
# Individual (person id)
# AgeGroup (Children/ Adolescent/ Adult/ Elderly) 
# Sex (M/F)
# BMI (length normalized weight)
# Ethnicity (by continent)
# SymptomScore







