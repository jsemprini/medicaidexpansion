cdc <- merge(black, white, by=c("state", "year", "age"))

cdc <- merge(cdc, medexp14, by="state")

cdc$dr <- cdc$crude_black / cdc$crude_white

cdc1 <- subset(cdc, dr>0)
cdc1 <- subset(cdc1, dr<10)

#treat 1: exp_y v. no | <65 

treat1 <- explore1 <- summarySE(cdc1, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(treat1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(treat1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(treat1, aes(x=exp_2014, y=dr)) + 
  geom_boxplot() + facet_wrap(~ age)


#show trend
ggplot(subset(treat1, year<2014), aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(subset(treat1, year<2014), aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(subset(treat1, year<2014), aes(x=year, y=dr, colour=exp_2014)) + 
geom_smooth() + geom_line() + facet_wrap(~ age)

#regression: treatment 1: expanding v. not - 2012-13 v. 2015-16
cdc_t1a <- subset(cdc1, year>2011)
cdc_t1a <- subset(cdc_t1a, year!=2014)

explore_t1 <- summarySE(cdc_t1a, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(explore_t1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(explore_t1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_smooth() + facet_wrap(~ age)

#create DID
cdc_t1a$time = ifelse(cdc_t1a$year >= 2014, 1, 0) #create dummy variable for treated years
cdc_t1a$treated = ifelse(cdc_t1a$exp_2014 == "yes", 1, 0) #create dummy variable for treated groups
cdc_t1a$did = cdc_t1a$time * cdc_t1a$treated #create interaction dummy

#regression - treatment1: exp_y v. exp_n | <65

did_t1fa = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t1fa)

did_t1fb = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t1fb)

did_t1f2a = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t1f2a)

did_t1f2b = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t1f2b)

did_t1f3a = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t1f3a)

did_t1f3b = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t1f3b)

did_t1f4a = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t1f4a)

did_t1f4b = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t1f4b)

did_t1f5a = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="65-69"), index=c("state", "year"), model="within") #regression model
summary(did_t1f5a)

did_t1f5b = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="65-69"), index=c("state", "year"), model="random") #regression model
summary(did_t1f5b)

did_t1f6a = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="70-74"), index=c("state", "year"), model="within") #regression model
summary(did_t1f6a)

did_t1f6b = plm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="70-74"), index=c("state", "year"), model="random") #regression model
summary(did_t1f6b)


#treatment 2: exp_y v. exp_n | vote_y & <65
cdc_t2a <- subset(cdc_t1a, vote_y=="yes")

explore_t2 <- summarySE(cdc_t2a, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(explore_t2, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(explore_t2, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(explore_t2, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_smooth() + facet_wrap(~ age)


#fixed (younger ages are SIG +)
did_t2fa = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fa)

did_t2ra = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra)

did_t2fa2 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2fa2)

did_t2ra2 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra2)

did_t2fa3 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2fa3)

did_t2ra3 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra3)

did_t2fa4 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fa4)

did_t2ra4 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra4)

did_t2f5a = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="65-69"), index=c("state", "year"), model="within") #regression model
summary(did_t2f5a)

did_t2f5b = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="65-69"), index=c("state", "year"), model="random") #regression model
summary(did_t2f5b)

did_t2f6a = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="70-74"), index=c("state", "year"), model="within") #regression model
summary(did_t2f6a)

did_t2f6b = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="70-74"), index=c("state", "year"), model="random") #regression model
summary(did_t2f6b)
