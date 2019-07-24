cdc<- merge(inc, medexp14, by="state")

#explore
explore <- summarySE(cdc, measurevar="crude", groupvars=c("year", "exp_2014", "age", "site", "race"), na.rm=TRUE)

ggplot(data=explore) +
  geom_boxplot(mapping=aes(x=exp_2014, y=crude, color=race, alpha=1/100)) + facet_wrap(~ year)

ggplot(explore, aes(x=year, y=crude, colour=interaction(race, exp_2014))) + facet_wrap(~ age) +
  geom_errorbar(aes(ymin=crude-se, ymax=crude+se), width=.1) +
  geom_line() +
  geom_point() 
  

ggplot(subset(explore, site=="Breast"), aes(x=year, y=crude, colour=interaction(race, exp_2014))) + facet_wrap(~ age) +
  geom_errorbar(aes(ymin=crude-se, ymax=crude+se), width=.1) +
  geom_line() +
  geom_point()

ggplot(subset(explore, site=="Cervix Uteri"), aes(x=year, y=crude, colour=interaction(race, exp_2014))) + facet_wrap(~ age) +
  geom_errorbar(aes(ymin=crude-se, ymax=crude+se), width=.1) +
  geom_line() +
  geom_point()


ggplot(subset(explore, site=="Colon and Rectum"), aes(x=year, y=crude, colour=interaction(race, exp_2014))) + facet_wrap(~ age) +
  geom_errorbar(aes(ymin=crude-se, ymax=crude+se), width=.1) +
  geom_line() +
  geom_point()

#explore 2
cdc2 <- subset(cdc, vote_y=="yes")

explore2 <- summarySE(cdc2, measurevar="crude", groupvars=c("year", "exp_2014", "age", "site", "race"), na.rm=TRUE)






#treat 1: 2011 v. 2016, no specification. expy v no | <65, <65 v. 65+ | expy
treat11 <- summarySE(cdc4t1, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)


ggplot(treat11, aes(x=year, y=dr, colour=exp_2014)) + facet_wrap(~ age) + 
  geom_line() +
  geom_point() #clean up graph, solid graph showing result

ggplot(treat11, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) + facet_wrap(~ age) +
geom_line() +
  geom_point()

#reg models

cdc4t1$time = ifelse(cdc4t1$year >= 2014, 1, 0) #create dummy variable for treated years
cdc4t1$treated = ifelse(cdc4t1$exp_2014 == "yes", 1, 0) #create dummy variable for treated groups
cdc4t1$did = cdc4t1$time * cdc4t1$treated #create interaction dummy
t1 = lm(dr ~ treated + time + did, data = subset(cdc4t1,age=="60-64")) #regression model
summary(t1) #insig, close, med r2

cdc4t1$treated1 = ifelse(cdc4t1$age == "60-64", 1, 0) #create dummy variable for treated groups
cdc4t1$did1 = cdc4t1$time * cdc4t1$treated1 #create interaction dummy
t1 = lm(dr ~ treated1 + time + did1, data = subset(cdc4t1,exp_2014=="yes")) #regression model
summary(t1) #insig, close, med r2

#treat 2: switch michigan to exp-y
