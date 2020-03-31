setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")
library(tidyverse)
ukbqc <- read.delim("UKB34137_QC_and_24HR.txt", header = TRUE, 
                    sep="\t")

ukbqc <- as_tibble(ukbqc)
colnames(ukbqc)

#Categorical vs. categorical
#1369= beef intake (categorical) 
#2080 = Frequency of tiredness / lethargy in last 2 weeks(categorical)

table(ukbqc$f.1369.0.0)
table(ukbqc$f.2080.0.0)

beeftiredness<-ukbqc%>%select(f.eid,f.1369.0.0, f.2080.0.0)
beeftiredness    

colnames(beeftiredness) <- c("FID", "Beef_intake", "Tiredness_in_2_wk")

beeftiredness[!is.na(beeftiredness$Tiredness_in_2_wk),]
beeftiredness[complete.cases(beeftiredness),]
nbf<-nrow(beeftiredness[complete.cases(beeftiredness),])

png(filename="testplot")

cat<-ggplot(data = beeftiredness, mapping = aes(x = Beef_intake, 
fill = Tiredness_in_2_wk)) +
    geom_bar() + labs(title= 
            paste("Tiredness vs Beef Intake\n n=", nbf, sep=" "))

cat + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Save as png- for pdf sub png to pdf

dev.off()

#Categorical vs. Discrete
#94=Diastolic blood pressure, manual reading (discrete)
#1558= Alcohol intake frequency (categorical)

table(ukbqc$f.94.0.0)
table(ukbqc$f.1558.0.0)


bpalcohol<-ukbqc%>%select(f.eid,f.1558.0.0, f.94.0.0)

bpalcohol  

colnames(bpalcohol) <- c("FID", "Alcohol_intake", "Blood_Pressure")

bpalcohol[!is.na(bpalcohol$Blood_Pressure),]
bpalcohol[complete.cases(bpalcohol),]
nba<-nrow(bpalcohol[complete.cases(bpalcohol),])

catd<-ggplot(data = bpalcohol, mapping = aes(x =Alcohol_intake, 
    y = Blood_Pressure)) +
    geom_boxplot() + labs(title = 
paste("Blood Pressure vs. Alcohol Intake\n n=", nba, sep= " ")) + 
    theme(plot.title=element_text(family="Times", size=12)) + 
    theme(axis.title.x = element_text(family="Times", size=8)) + 
    theme(axis.title.y = element_text(family= "Times", size = 8))

catd + 
theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                family = "Times", size = 6)) + 
    theme(axis.text.y = element_text(family = "Times", size = 6))


#Categorical vs. continuous
#30780= LDL (continuous) 
#1920= mood swings (categorical)

table(ukbqc$f.30780.0.0)
table(ukbqc$f.1920.0.0)


ldlmood<-ukbqc%>%select(f.eid,f.30780.0.0, f.1920.0.0)
ldlmood 

colnames(ldlmood) <- c("FID", "LDL", "mood_swings")

catc <- ggplot(data = ldlmood, mapping = aes(x =mood_swings, y = LDL)) +
    geom_boxplot()

catc + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Discrete vs discrete 
#1070= Time spent watching television (TV) (discrete)
#914= Duration of vigorous activity (discrete) 

table(ukbqc$f.1070.0.0)
table(ukbqc$f.914.0.0)


tvexercise<-ukbqc%>%select(f.eid,f.1070.0.0, f.914.0.0)
tvexercise

colnames(tvexercise) <- c("FID", "TV_daily", "vigorous_activity_daily")


tvexercise2<-tvexercise[tvexercise$TV_daily>=0,]
tvexercise3<-tvexercise2[tvexercise2$vigorous_activity_daily<=750,]
tvexercise4<-tvexercise3[tvexercise3$vigorous_activity_daily>=0,]

tvexercise4[!is.na(tvexercise4$vigorous_activity_daily),]
tvexercise4[complete.cases(tvexercise4),]
n<-nrow(tvexercise4[complete.cases(tvexercise4),])

ggplot(tvexercise4, aes(x = vigorous_activity_daily, y = TV_daily)) +
    geom_point(alpha=0.01) +
    labs(x = "Vigorous Activity (min)",
         y = "TV Watched per Day (hrs)",
         title = paste("TV Watched per Day vs. Vigorous Activity 
         \n n=", n, sep=" "))




#subset to remove -10 to -1 and report how many were excluded 

#Continuous vs discrete
#904=Number of days/week of vigorous physical activity 10+ minutes 
#(discrete)
#30760=HDL (continuous)

table(ukbqc$f.904.0.0)
table(ukbqc$f.30760.0.0)

hdlexercise<-ukbqc%>%select(f.eid,f.904.0.0, f.30760.0.0)
hdlexercise

colnames(hdlexercise) <- c("FID", "days_activity", "HDL")

ggplot(hdlexercise, aes(x = days_activity, y = HDL)) +
    geom_point(alpha = 0.5) +
    labs(x = "Vigorous Activity (days/week)",
         y = "HDL",
         title = "Vigorous Activity vs HDL")
#remove the -3 and -1, change the transparentcy of dats, aplha to .5


#Continuous vs continuous
#30690= Cholesterol (continuous)
#100004= total fat intake (continuous)

table(ukbqc$f.30690.0.0)
table(ukbqc$f.100004.0.0)

fatcholesterol<-ukbqc%>%select(f.eid,f.100004.0.0, f.30690.0.0)
fatcholesterol

colnames(fatcholesterol) <- c("FID", "Total_Fat", "Cholesterol")

ggplot(data = fatcholesterol, mapping = aes(x = Total_Fat, 
    y = Cholesterol)) + geom_point() +
    labs(x = "Total Fat (g)",
         y = "Cholesterol",
         title = "Relationship of Cholesterol and Total Fat") +   
    geom_smooth(method = "lm", y ~ x) #added a regression line 






#example of paste
#added sample size



fatchol<- ggplot(data = fatcholesterol, mapping = aes(x = Total_Fat, 
                     y = Cholesterol))  + geom_point() +
    labs(x = "Total Fat (g)",
         y = "Cholesterol",
         title = paste("Relationship of Cholesterol and Total Fat \n n=", nfc, sep=" ")) +   
    geom_smooth(method = "lm", se=FALSE, color = "blue", formula = y ~ x) 

fatchol

# lm_eq <- function(fatcholesterol)
#     {
#     m <- lm(Cholesterol ~ Total_Fat, fatcholesterol);
#     eq <- substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2, 
#                      list(a = format(unname(coef(m)[1]), digits = 2),
#                           b = format(unname(coef(m)[2]), digits = 2),
#                           r2 = format(summary(m)$r.squared, digits = 3)))
#     as.character(as.expression(eq));
# }


a<- paste("y = ", format(m$coefficients[[1]], digits = 3), " + ", 
            format(m$coefficients[[2]], digits = 3), "x", ", r^2= " 
      ,format(summary(m)$r.squared, digits =3), sep = "") 

fatchol1 <- fatchol + geom_text(x = 200, y = 12, label = a)
fatchol1

date<-"02-20-2020"

fatcholesterol
fatcholesterol[fatcholesterol$Cholesterol>=4,]
fatcholesterol[!is.na(fatcholesterol$Total_Fat),]
fatcholesterol[complete.cases(fatcholesterol),]

nfc<-nrow(fatcholesterol[complete.cases(fatcholesterol),])

#report Sample size
#r-sq and linear eq. -(--is it linear?)
#units for axes
#point transparency
#y vs. x
#remove extreme ouliers
#how to label one point
#how to insert a message (test statistic)
#change font
#change colors

