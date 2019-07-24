cdcG1 <- subset(cdc1, year<2014)

ggplot(cdcG1, aes(x=year, y=dr, colour=exp_2014)) + 
  geom_point() + facet_wrap(~ age) + geom_smooth() #Graph1

ggplot(cdcG1, aes(x=interaction(vote_y, exp_2014), y=dr)) + 
  geom_boxplot() + facet_wrap(~ age) #Graph2

#table 1,2
table(medexp14$vote_y, medexp14$exp_2014)

#create summary statistic tables
#table 3 (DR summary: medex_y v. non)

cdc1T3 <- subset(cdc1, year<2015)

Table3 <- describeBy(cdc1T3[c(18)], interaction(cdc1T3$age, cdc1T3$exp_2014),mat=TRUE)

Table3cru <- describeBy(cdc1T3[c(18)], cdc1T3$exp_2014,mat=TRUE)

#table 4 (DR summary: medexp_y v. vote_y,exp_n)
cdc1T4 <- subset(cdc1T3, vote_y=="yes")
Table4 <- describeBy(cdc1T4[c(18)], interaction(cdc1T4$age, cdc1T4$exp_2014),mat=TRUE)
Table4cru <- describeBy(cdc1T4[c(18)], cdc1T4$exp_2014,mat=TRUE)


