#####Welcome to a Star Wars introduction to Statistics and Plotting in R
###This was originally created by Dom J. Acri for educational purposes

#################################
## Build the toolbox (packages)##
#################################
install.packages(c("tidyverse","psych","skimr","broom","jmv","ggstatsplot"))

library(tidyverse)
library(psych)
library(skimr)
library(broom)
library(jmv)
library(ggstatsplot)

#################################
#######Let's Get Started!!#######
#################################
#In a galaxy far far away...
starwars
View(starwars)
glimpse(starwars)
summary(starwars)
#Descriptive statistics can be hard to visualize, can we make it easier?
skim(starwars)
#Height & Weight are normally correlated is this true in the star wars universe?
cor.test(starwars$mass, starwars$height)
  #I need to make this plotted!
plot(starwars$mass, starwars$height)
  #Is this the best
starwars$mass #Print the mass, NOTICE ANYONE??
starwars$name[16] #They are in row 16, WHO IS IT?
sw_regular <- filter(starwars, name != "Jabba Desilijic Tiure") #Princess Leia to the rescue!
#Now that we have a dataset without Episode VIII's villian, let's try again!
cor.test(sw_regular$mass, sw_regular$height)
plot(sw_regular$mass, sw_regular$height)
#My readout is stuck in the console, can we free it?
corr1 <- tidy(cor.test(sw_regular$mass, sw_regular$height))#notice it's the same test
View(corr1)#we can see it, can we take it to excel?
write.csv(corr1, file="Height_Weight_JabbaFree.csv")
?corrMatrix
corrMatrix(sw_regular, vars=vars(mass,height))
  #Let's fill it in!
corrMatrix(sw_regular, vars=vars(mass,height),
           ci=T,
           plots=T,
           plotDens=T)
#Let's publish what we find, but don't spend time on it!
    ########take a pause, show Good Place gif
ggstatsplot::ggscatterstats(sw_regular,
                            x = height,
                            y = mass)
##Customization
?ggscatterstats
ggstatsplot::ggscatterstats(sw_regular,
                            x = height,
                            y = mass,
                            title = "Does Height & Weight Correlate in Space?",
                            xlab = "Height (cM)",
                            ylab = "Mass (kg)",
                            point.color = "black",
                            point.alpha = 1)
#Is there an effect of gender??
t.test(mass ~ gender, data=sw_regular)
#ohh no what's going on?????
sw_regular$gender #how is that possible?
sw_regular$name[2:3]#who are they?
gender_test <- filter(sw_regular, gender == c("male","female"))
t.test(mass~gender, data = gender_test)
######let's color some dots
gender_plot=filter(gender_test,mass != "NA")
for (i in c(1:length(gender_plot$name))){
  gender_plot$col[gender_plot$gender == "male"] = "blue";
  gender_plot$col[gender_plot$gender == "female"] = "red"
}
ggstatsplot::ggscatterstats(gender_plot,
                            x = height,
                            y = mass,
                            title = "Does Height & Weight Correlate in Space?",
                            xlab = "Height (cM)",
                            ylab = "Mass (kg)",
                            point.color = gender_plot$col,
                            point.alpha = 1)
