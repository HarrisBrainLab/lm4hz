# Set Up

# A. Install Packages
library(lme4)
library(readxl)

# B. Prepare Data

df1 <-read_excel("Data.xlsx")
  
# If needed, remove incomplete rows
df2 <- df1[complete.cases(df1),]
  
# i. Isolate Variables of Interest
    
  # User Specified Outcome Variable
  y  <- df2$tNAA
  y.z <- (y-mean(y))/sd(y)
  
  # Predictor Variables
  v <- df2$vendor     # Vendor
  s <- df2$site        # Site
  p <- df2$subj        # Subject ID
  GM <- df2$GM_fraction
  GM.z <- (GM-mean(GM))/sd(GM)
  gender <- df2$Gender # Gender
  age <- df2$Age       # Age
  age.z <- (age-mean(age))/sd(age)
  
# ii. Dummy Code Variables Create Factprs
     
  # Dummy Coding
  
  v <- gsub("GE",1,v)               # Dummy Code GE as 1
  v <- gsub("Philips", 2, v)        # Dummy Code Philips as 3
  v <- gsub("Siemens", 3, v)        # Dummy Code Siemans as 2
  v <- as.factor(as.numeric(v))     # Turns v from text into numbers, and makes it a factor
      
  s <- gsub("G1",1,s)             
  s <- gsub("G4",2,s)              
  s <- gsub("G5",3,s)             
  s <- gsub("G6",4,s)
  s <- gsub("G7",5,s)
  s <- gsub("G8",6,s)
  s <- gsub("P1",7,s)
  s <- gsub("P3",8,s)
  s <- gsub("P4",9,s)
  s <- gsub("P5",10,s)
  s <- gsub("P6",11,s)
  s <- gsub("P7",12,s)
  s <- gsub("P8",13,s)
  s <- gsub("P9",14,s)
  s <- gsub("P10",15,s) # For some reason rather than replacing "P10" with 15 it will only replace it with 70
  s <- gsub("70",15,s)  # Rather than finding out why I just then replaced 70 with 15 here
  s <- gsub("S1",16,s)
  s <- gsub("S3",17,s)
  s <- gsub("S5",18,s)
  s <- gsub("S6",19,s)
  s <- gsub("S8",20,s)
  s <- as.factor(as.numeric(s))
      
  p <- as.factor(p)
      
# iii. Create Dataframe
      
  # Input Factors into Dataframe
  df <- data.frame(v, s, p, gender, age, age.z, GM, GM.z, y, y.z)
  
  # Get Summary (If Desired)  
  summary(df)  
      
# C. Calculate Means
  
# i. Grand Mean   
  M <- mean(df$y) # Get the grand mean for outcome
  df$M <- M
  
# ii. Other Mean Values (If Desired)
  
  ms <- with(df,tapply(y,s,mean)) # Mean outcome at each site level
  mv <- with(df,tapply(y,v,mean)) # Mean outcome at each vendor level
  m2 <- with(df,tapply(y,list(s,v),mean)) # Mean outcome at each site:vendor combination

  ds <- diff(ms) # Difference between site levels
  dv <- diff(mv) # Difference between vendor levels
  
  
# D. Produce Model
      
# Vendor only model 0a 
model.z.v <- lmer(y.z ~ M + (1|v), data = df)
summary(model.z.v)
logLik(model.z.v)

# Site only model 0b
model.z.s <- lmer(y.z ~ M + (1|s), data = df)
summary(model.z.s)
logLik(model.z.s)

#All model 1
model.z <- lmer(y.z ~ M + (1|v) + (1|s), data = df)
summary(model.z)
logLik(model.z.s)

#ANOVAs

anova1a <- anova(model.z.v, model.z)
anova1a
anova1b <- anova(model.z.s, model.z)
anova1b

# GM Model 4

model.z.GM<- lmer(y.z ~ M + GM.z + (M|v) + (M|s), data = df)

#Model with fixed effects only
#model.z.GM<- lm(y.z ~ M + GM.z, data = df)

summary(model.z.GM) 
logLik(model.z.GM)

anova4<-anova(model.z, model.z.GM)
anova4

# Age Model 5

model.z.age<- lmer(y.z ~ M + age.z + (M|v) + (M|s), data = df)

#Model with fixed effexts only
#model.z.age<- lm(y.z ~ M + age.z, data = df)

summary(model.z.age) 
logLik(model.z.age)

anova5<-anova(model.z, model.z.age)
anova5

# Sex Model 6 Vendor only

model.z.gender <- lmer(y.z ~ M + gender + (M|v) + (M|s), data = df)

#Model with fixed effects only
#model.z.gender <- lm(y.z ~ M + gender, data = df)

summary(model.z.gender) 
logLik(model.z.gender)

anova6<-anova(model.z, model.z.gender)
anova6

