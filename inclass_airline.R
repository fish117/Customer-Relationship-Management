
#set working directory

#Load the data
rm(list=ls())
load("airline.data.Rdata")


#Visualize the data in a scatterplot where frequent flyers are shown with an X and non-frequent flyers as O
plot(all.data$minutes.spent,all.data$spend, pch = ifelse(all.data$frequentflyerprogram.numeric > 0, "x", "o"))



library(visreg)

#main effects model

maineffects <- lm(spend ~ frequentflyerprogram.numeric + minutes.spent, data=all.data)
summary(maineffects)
visreg(maineffects, "minutes.spent", by="frequentflyerprogram.numeric", overlay ="TRUE")


#interaction model

interaction <- lm(spend ~ frequentflyerprogram.numeric + minutes.spent + frequentflyerprogram.numeric:minutes.spent, data=all.data)
summary(interaction)
visreg(interaction, "minutes.spent", by="frequentflyerprogram.numeric", overlay ="TRUE")



#Interaction model with frequent flyer coded the other way

all.data$frequentflyerprogram.recoded<-abs(all.data$frequentflyerprogram.numeric-1)
interaction.recoded <- lm(spend ~ frequentflyerprogram.recoded + minutes.spent + frequentflyerprogram.recoded:minutes.spent, data=all.data)
summary(interaction.recoded)
visreg(interaction.recoded, "minutes.spent", by="frequentflyerprogram.recoded", overlay ="TRUE")


#Interaction model with mean-centered time spent

all.data$minutes.spent.centered<-all.data$minutes.spent-mean(all.data$minutes.spent)
interaction.centered <- lm(spend ~ frequentflyerprogram.numeric + minutes.spent.centered + frequentflyerprogram.numeric:minutes.spent.centered, data=all.data)
summary(interaction.centered)
visreg(interaction.centered, "minutes.spent.centered", by="frequentflyerprogram.numeric", overlay ="TRUE")
mean(all.data$minutes.spent)


  