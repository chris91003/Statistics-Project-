library(dplyr)
library(ggplot2)
library(ggpubr)
library(psych)

install.packages("psych")
#Power Test to calculate sample size

power.t.test(n= NULL, delta= 8, sd= 5, sig.level= 0.05,power= 0.8, type="two.sample")
# use boxplot to make comparisons of means              
#regress pulse against sex and maybe collect other information (weight, height, age)             


#Load in data and convert to dataframe

heartrate <- read.csv("HeartRate.csv")
heartrate$Sex <- as.factor(heartrate$Sex)
View(heartrate)

#Summary Statistics

describeBy(heartrate, group=heartrate$Sex, fast=TRUE)



#Subset by Sex

men <- heartrate[heartrate$Sex == "M", ]
women <- heartrate[heartrate$Sex == "F", ]


#Boxplot of heart rates by Sex, weights by Sex, and Heights by Sex

box_sex <- ggplot(heartrate, aes(Heart.Rate, Sex, fill= Sex))+
  geom_boxplot()+ coord_flip()

box_weight <- ggplot(heartrate, aes(Weight,Sex , fill= Sex))+
  geom_boxplot()+ coord_flip()

box_analysis <- ggplot(heartrate, aes(Height,Sex , fill= Sex))+
  geom_boxplot()+ coord_flip()

ggarrange(box_sex, box_weight, box_analysis, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


#Histogram of data and t.test
hist(men$`Heart.Rate`)
hist(women$`Heart.Rate`)
nonorm <- t.test(men$`Heart.Rate`, women$`Heart.Rate`)


#Mann-Whitney Test
women_hr <- women$Heart.Rate
men_hr <- men$Heart.Rate

wilcox.test(Heart.Rate~Sex, data=heartrate, paired= TRUE, exact= FALSE)
whitney <- wilcox.test(men_hr,women_hr, paired= TRUE, exact= FALSE)


#Visualizaton of heartrate and sex
ggplot(heartrate, aes(x= Sex, y= Heart.Rate)) +geom_jitter(width= 0.1, aes(colour = Sex))

#Regressing Heart Rate using Sex y= 70-1.429(Sex)
heartrate <- read.csv("HeartRate.csv")

model <- lm(heartrate$Heart.Rate~heartrate$Sex)
summary(lm(heartrate$Heart.Rate~heartrate$Sex))


summ(model)


#Plot residuals against fitted values
plot(model$residuals, model$fitted.values)
plot(model)

#Regressing Heartrate and weight heartrate= 83.0565-0.1042(Weight)
model2 <- lm(heartrate$Heart.Rate~heartrate$Weight)
plot(heartrate$Weight, heartrate$Heart.Rate, xlab= ("Height"),
     ylab= ("Heartrate"))
abline(model2)
summary(model2)


plot <- ggplot(heartrate, aes(x= Weight, y= Heart.Rate))+
  geom_smooth(method= "lm", se= FALSE)+ geom_point()+
  stat_regline_equation(label.x= 160,label.y= 90)


#Regressing Heartrate and height heartrate= 192.9354-1.9086(Height)
model3 <- lm(heartrate$Heart.Rate~heartrate$Height)
plot(heartrate$Height, heartrate$Heart.Rate)
abline(model3)
summary(model3)


plot2 <- ggplot(heartrate, aes(x= Height, y= Heart.Rate))+
  geom_smooth(method= "lm", se= FALSE)+ geom_point()+
  stat_regline_equation(label.x= 60,label.y= 85)


#Multiple Coefficient Regression heartrate, height, weight, sex
#heartrate= 378.05484-5.08316(Height)+0.06274(Weight)+24.51767(Sex)

heartrate$Sex <- ifelse(heartrate$Sex == "M", 1, 0)

model4 <- lm(Heart.Rate~ Height+ Weight+ Sex, data=heartrate)
summary(model4)


plot <- ggplot(heartrate, aes(x= Weight, y= Heart.Rate))+
  geom_smooth(method= "lm", se= FALSE)+ geom_point()+
  stat_regline_equation(label.x= 160,label.y= 90)

model5 <- lm(Heart.Rate~ Height+ Sex, data=heartrate)
summary(model5)


#Predict values

newdata<- heartrate[, c("Heart.Rate", "Height", "Sex")]
heartrate$predict <- predict(model5, data= newdata)
heartrate$model4 <- predict(model4, data= newdata)


#Plot predicted vs original 

ggplot(heartrate,                                     
       aes(x = predict,
           y = Heart.Rate)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 1) + labs(x= "Predicted", y= "Observed")




#Log Transform Data
newheartrate <- heartrate
newheartrate$Heart.Rate <- log10(heartrate$Heart.Rate)
log_men <- newheartrate[heartrate$Sex == "M", ]
log_women <- newheartrate[heartrate$Sex == "F", ]

hist(log_men$Heart.Rate)
hist(log_women$Heart.Rate)

norm <- t.test(Heart.Rate~Sex, data= newheartrate)

#Merging t test statistics of Normalized and Non normalized

library(broom)
nonorm <- tidy(t.test(men$`Heart.Rate`, women$`Heart.Rate`))
whitney <- tidy(wilcox.test(men_hr,women_hr))
norm <- tidy(t.test(Heart.Rate~Sex, data= newheartrate))

#Removing specific columns
merge <- rbind(nonorm, norm)
merge <- merge[-c(1, 2, 3, 6)]
merge <- as.data.frame(merge)
rownames(merge) <- c("Non-Normalized", "Normalized")

merge
