cdc <- merge(black, white, by=c("state", "year", "age"))

cdc <- merge(cdc, medexp14, by="state")

cdc$dr <- cdc$crude_black / cdc$crude_white

#explore

explore <- summarySE(cdc, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(data=explore) +
  geom_point(mapping=aes(x=year, y=dr, color=exp_2014, alpha=1/100)) + facet_wrap(~ age)

ggplot(explore, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(explore, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

#explore 2
cdc1 <- subset(cdc, vote_y=="yes")

explore2 <- summarySE(cdc1, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(data=explore2) +
  geom_point(mapping=aes(x=year, y=dr, color=exp_2014, alpha=1/100)) + facet_wrap(~ age)

ggplot(explore2, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(explore2, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

#treat 1: exp_y v. no | <65 
cdc_t1 <- subset(cdc, age!="65-69" | age!="70-74")

treat1 <- explore2 <- summarySE(cdc, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

ggplot(treat1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_line() +
  geom_point() + facet_wrap(~ age)

ggplot(treat1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)

#regression: treatment 1: expanding v. not - 2012-13 v. 2015-16
cdc_t1a <- subset(cdc_t1, year>2011)
cdc_t1a <- subset(cdc_t1a, year!=2014)

cdc_t1a$time = ifelse(cdc_t1a$year >= 2014, 1, 0) #create dummy variable for treated years
cdc_t1a$treated = ifelse(cdc_t1a$exp_2014 == "yes", 1, 0) #create dummy variable for treated groups
cdc_t1a$did = cdc_t1a$time * cdc_t1a$treated #create interaction dummy
did_t1a = lm(dr ~ treated + time + did, data = cdc_t1a) #regression model
summary(did_t1a) #insig

#by age
did_t1a1 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="45-49")) #regression model
summary(did_t1a1) #insig

did_t1a2 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="50-54")) #regression model
summary(did_t1a2) #insig

did_t1a3 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="55-59")) #regression model
summary(did_t1a3) #insig

did_t1a4 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="60-64")) #regression model
summary(did_t1a4) #insig

did_t1a = lm(dr ~ treated + time + did, data = cdc_t1a) #regression model
summary(did_t1a) #insig

#by age
did_t1a1 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="45-49")) #regression model
summary(did_t1a1) #insig

did_t1a2 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="50-54")) #regression model
summary(did_t1a2) #insig

did_t1a3 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="55-59")) #regression model
summary(did_t1a3) #insig

did_t1a4 = lm(dr ~ treated + time + did, data = subset(cdc_t1a, age=="60-64")) #regression model
summary(did_t1a4) #insig

#fixed (younger ages are SIG +)
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

#treat 1b, 2011 v. 2016, exp_y v. no | <65
cdc_t1b <- subset(cdc_t1a, year!=2012)
cdc_t1b <- subset(cdc_t1b, year!=2015)

#fixed (younger ages are SIG +)
did_t1fb = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t1fb)

did_t1rb = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t1rb)

did_t1f2b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t1f2b)

did_t1r2b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t1r2b)

did_t1f3b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t1f3b)

did_t1r3b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t1r3b)

did_t1f4b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t1f4b)

did_t1r4b = plm(dr ~ treated + time + did, data = subset(cdc_t1b, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t1r4b)

#treat 2: 2011-12 and 2015-16, exp_y v. no | <65 & vote_y
cdc_t2a <- subset(cdc_t1a, vote_y=="yes")

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

#treat 1b, 2011 v. 2016, exp_y v. no | <65 & vote=yes
cdc_t2b <- subset(cdc_t1b, vote_y=="yes")

#fixed (younger ages are SIG +)
did_t2fb = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fb)

did_t2rb = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2rb)

did_t2f2b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2f2b)

did_t2r2b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2r2b)

did_t2f3b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2f3b)

did_t2r3b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2r3b)

did_t2f4b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2f4b)

did_t2r4b = plm(dr ~ treated + time + did, data = subset(cdc_t2b, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2r4b)

#treat 3: 55-59 & 60-64 v. 65-69 & 70-74 (2011-12 v. 2015-16) | exp_y
cdc_t3a <- subset(cdc, exp_2014=="yes")
cdc_t3a <- subset(cdc_t3a, age!="50-54")
cdc_t3a <- subset(cdc_t3a, age!= "40-44")
cdc_t3a <- subset(cdc_t3a, age!= "45-49")
cdc_t3a <- subset(cdc_t3a, age!="35-39")

cdc_t3a <- subset(cdc_t3a, year>2011)
cdc_t3a <- subset(cdc_t3a, year!=2014)

cdc_t3a$time = ifelse(cdc_t3a$year >= 2014, 1, 0) #create dummy variable for treated years
cdc_t3a$treated = ifelse(cdc_t3a$age == "65-69" | cdc_t3a$age=="70-74", 0, 1) #create dummy variable for treated groups
cdc_t3a$did = cdc_t3a$time * cdc_t3a$treated #create interaction dummy

did_t3a = lm(dr ~ treated + time + did, data = cdc_t3a) #regression model
summary(did_t3a) #insig

#drop 55, 70
cdc_t3b <- subset(cdc_t3a, age!="55-59")
cdc_t3b <- subset(cdc_t3b, age!="70-74")

did_t3a = lm(dr ~ treated + time + did, data = cdc_t3a) #regression model
summary(did_t3a) 

cdc_t3ba <- subset(cdc_t3b, dr>0)
explore3b <- summarySE(cdc_t3ba, measurevar="dr", groupvars=c("year", "age"), na.rm=TRUE)

ggplot(explore3b, aes(x=year, y=dr, colour=age)) + 
  geom_line() +
  geom_point() 

ggplot(explore3b, aes(x=year, y=dr, colour=age)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() 

did_t3ba = lm(dr ~ treated + time + did, data = cdc_t3ba) #regression model
summary(did_t3ba)

did_t3aa = lm(dr ~ treated + time + did, data = cdc_t3aa) #regression model
summary(did_t3aa)

#explore3 
cdc_t3aa <- subset(cdc_t3a, dr>0)
explore3 <- summarySE(cdc_t3aa, measurevar="dr", groupvars=c("year", "age"), na.rm=TRUE)

ggplot(explore3, aes(x=year, y=dr, colour=age)) + 
  geom_line() +
  geom_point() 

ggplot(explore3, aes(x=year, y=dr, colour=age)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() 

#fixed (younger ages are SIG +)
did_t3fa3 = plm(dr ~ treated + time + did, data = cdc_t3a, index=c("year", "age"), model="within") #regression model
summary(did_t3fa3)

did_t2ra3 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra3)

did_t2fa4 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fa4)

did_t2ra4 = plm(dr ~ treated + time + did, data = subset(cdc_t2a, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2ra4)


#treat 1 and 2 without zero dr
cdc_t1bb <- subset(cdc_t1b, dr>0)

#fixed (younger ages are SIG +)
did_t1fbb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t1fbb)

did_t1rbb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t1rbb)

did_t1f2bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t1f2bb)

did_t1r2bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t1r2bb)

did_t1f3bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t1f3bb)

did_t1r3bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t1r3bb)

did_t1f4bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t1f4bb)

did_t1r4bb = plm(dr ~ treated + time + did, data = subset(cdc_t1bb, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t1r4bb)

#treat 2: 2011-12 and 2015-16, exp_y v. no | <65 & vote_y
cdc_t2a <- subset(cdc_t1a, vote_y=="yes")
cdc_t2ab <- subset(cdc_t2a, dr>0)

#fixed (younger ages are SIG +)
did_t2fab = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fab)

did_t2rab = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2rab)

did_t2fab2 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2fab2)

did_t2rab2 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2rab2)

did_t2fab3 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2fab3)

did_t2rab3 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2rab3)

did_t2fab4 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fab4)

did_t2rab4 = plm(dr ~ treated + time + did, data = subset(cdc_t2ab, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2rab4)

#treat 3, fixed effect
cdc_t3c <- cdc_t3ba

cdc_t3c$state_age <- with(cdc_t3c, paste0(state, age))

did_t3F = plm(dr ~ treated + time + did, data = cdc_t3c, index=c("state_age", "year"), model="within") #regression model
summary(did_t3F)

cdc_t3c$age_year <- with(cdc_t3c, paste0(age, year))

did_t3F = plm(dr ~ treated + time + did, data = cdc_t3c, index=c("state", "age_year"), model="within") #regression model
summary(did_t3F)

cdc_t3c$state_year <- with(cdc_t3c, paste0(state,year))

#state specific

did_t3F = plm(dr ~ treated + time + did, data = subset(cdc_t3c, state=="Illinois"), index=c("year", "age"), model="within") #regression model
summary(did_t3F)

did_t3F = plm(dr ~ treated + time + did, data = subset(cdc_t3c, state=="California"), index=c("year", "age"), model="within") #regression model
summary(did_t3F)

did_t3F = plm(dr ~ treated + time + did, data = subset(cdc_t3c, state=="New York"), index=c("year", "age"), model="within") #regression model
summary(did_t3F)

did_t3F = plm(dr ~ treated + time + did, data = subset(cdc_t3c, state=="Ohio"), index=c("year", "age"), model="within") #regression model
summary(did_t3F)

did_t3F = plm(dr ~ treated + time + did, data = subset(cdc_t3c, state=="Maryland"), index=c("year", "age"), model="within") #regression model
summary(did_t3F)

#drop california from treatement 2

cdc_t2abc <- subset(cdc_t2ab, state!="California")
#fixed (younger ages are SIG +)
did_t2fabc = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabc)

did_t2rabc = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabc)

did_t2fabc2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabc2)

did_t2rabc2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabc2)

did_t2fabc3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabc3)

did_t2rabc3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabc3)

did_t2fabc4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabc4)

did_t2rabc4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abc, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabc4)

#remove NY
cdc_t2abcd <- subset(cdc_t2abc, state!="New York")

#fixed (younger ages are SIG +)
did_t2fabcd = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcd)

did_t2rabcd = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcd)

did_t2fabcd2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcd2)

did_t2rabcd2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcd2)

did_t2fabcd3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcd3)

did_t2rabcd3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcd3)

did_t2fabcd4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcd4)

did_t2rabcd4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcd, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcd4)

#drop nj & mich & LA & MD
cdc_t2abcde <- subset(cdc_t2abcd, state!="New Jersey")
cdc_t2abcde <- subset(cdc_t2abcde, state!="Michigan")
cdc_t2abcde <- subset(cdc_t2abcde, state!="Maryland")
cdc_t2abcde <- subset(cdc_t2abcde, state!="Louisiana")

#fixed (younger ages are SIG +)
did_t2fabcde = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="45-49"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcde)

did_t2rabcde = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="45-49"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcde)

did_t2fabcde2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="50-54"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcde2)

did_t2rabcde2 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="50-54"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcde2)

did_t2fabcde3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="55-59"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcde3)

did_t2rabcde3 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="55-59"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcde3)

did_t2fabcde4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="60-64"), index=c("state", "year"), model="within") #regression model
summary(did_t2fabcde4)

did_t2rabcde4 = plm(dr ~ treated + time + did, data = subset(cdc_t2abcde, age=="60-64"), index=c("state", "year"), model="random") #regression model
summary(did_t2rabcde4)


treat2abc <- summarySE(cdc_t2abc, measurevar="dr", groupvars=c("year", "exp_2014", "age"), na.rm=TRUE)

treat2abc <- subset(treat2abc, age!="40-44")

ggplot(treat2abc, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_errorbar(aes(ymin=dr-se, ymax=dr+se), width=.1) +
  geom_line() +
  geom_point() + facet_wrap(~ age)
