
#install.packages("sqldf")
library(sqldf)
library(dplyr)
library(reshape)
library(ggplot2)



# Import files
visits <- read.csv('Visits.csv')
signups <- read.csv('Signups.csv')





# 1) Frequency distribution of users by different 

# a.	Relative frequency distribution for Devices
devices <- sqldf('select device,count(*) as cnt from signups
                 group by device')
devices$relative_frequency <- devices$cnt / sum(devices$cnt)
devices

# Bar plot of frequency for devices
ggplot(devices,aes(device,relative_frequency)) + 
  geom_bar(stat = 'identity') +
  ggtitle('Relative frequency distribution for Devices')



# b. Relative frequency distribution for Auth type
auth_type <- sqldf('select auth_type,count(*) cnt from signups
                 group by auth_type')
auth_type$relative_frequency <- auth_type$cnt / sum(auth_type$cnt)
auth_type

# Bar plot of frequency for Auth type
ggplot(auth_type,aes(auth_type,relative_frequency)) + 
  geom_bar(stat = 'identity') +
  ggtitle('Relative frequency distribution for Auth type')



# c.	Relative frequency distribution for Device, auth type combination
comb <- sqldf('select device, auth_type, count(*) as cnt from signups
                 group by device, auth_type')
comb$relative_frequency <- comb$cnt / sum(comb$cnt)
comb

# Bar plot of frequency for Device, auth type combination
comb$combination = paste(comb$device,comb$auth_type)

ggplot(comb,aes(combination,relative_frequency)) + 
  geom_bar(stat = 'identity') +
  ggtitle('Relative frequency for Device, auth type combination')






# 2)
# Let’s build a table, and generate a heatmap!
# For users who signed up on Jun 1 2016, what proportion of them came back in the 1st week (Jun 2 – Jun 8 2016), 
# in the 2nd week (Jun 9 – Jun 15 2016), in the 3rd week (Jun 16 – Jun 22 2016) etc. all the way upto 24 weeks. 
# (I.e.)If I signup on Jun 1, and visit on Jun 12, Jun 13, Jun 18 and Jul 15, I would be counted as visited in the 2nd week 
# (Note: Jun 12 and 13 are part of the same week, so I get counted only once),visited in the 3rd week and visited in the 7th week. 


# Join visit and signup tables
sign_visit <- sqldf('select s.uid, s.signup_dt, v.dt from signups s
                   join visits v on s.uid = v.uid')

# convert date column to date type
sign_visit$dt <- as.Date(sign_visit$dt)
sign_visit$signup_dt <- as.Date(sign_visit$signup_dt)

# calculate the number of week for visiting back
sign_visit$week <- ceiling(difftime(sign_visit$dt,sign_visit$signup_dt,units = 'days')/7)
sign_visit$week <- as.numeric(sign_visit$week)


# build the table
visit_table <- sqldf('select signup_dt, week, count(*) as num from 
              (select signup_dt,uid, week 
              from sign_visit
              group by signup_dt,uid, week)
              group by signup_dt,week')

# desired date and week
visit_table <- visit_table%>%
  filter(week <= 24,signup_dt < as.Date('2016-10-31'))

# reshape the dataframe for the desired table
cast_visit_table <- cast(visit_table,signup_dt~week,value = 'num')

cast_visit_table <- rename(cast_visit_table,c(`0` = '# signed up'))

# same as spread in tidyr
# a <-  spread(visit_table,week,num)

# convert number of visiting to percentage of visiting
for (i in seq(3,26)){
  cast_visit_table[,i] = round((cast_visit_table[,i]/cast_visit_table[,2]),2)
}

# Result for the desired table
cast_visit_table

# reshape the table for heatmap
melt_heatmap_table <- melt(cast_visit_table,id = 'signup_dt')%>%
  filter(week > 0)

# same as gather() in tidyr
# b <- gather(cast_visit_table,week,value,-signup_dt,-`# signed up`)

ggplot(melt_heatmap_table,aes(x=week,y=signup_dt,fill = value))+
    geom_tile() +
    labs(x = "week of revisiting", y = "sign up date", title = "Percentage of visiting by signup date and week") +
    scale_fill_gradient(low = "white", high = "RED")

# It reaches a steady state at the 10th week, before that time, the retention rate keeps decreasing (color becomes lighter) as week goes by.
# However, after the 10th week, we stop losing customer, those loyalty customers keep visiting us since then, and the number of them is relative steady. 

# In addition, there is a white slash in the heat map, which means the percentage of customers decreased sharply on that specific two weeks. 
# The time of the white slash are Christmas week and it’s following week, which explains the decrease of visiting.






# 3) Similar to the table you created in Q2, what if we were to build it for just two days – 24th July 2016 and 18th Aug 2016, but segmented by auth type ? 

# For 2016-08-18
# Join visit and signup tables
sign_visit_type <- sqldf('select s.uid, s.signup_dt,s.auth_type, v.dt from signups s
                   join visits v on s.uid = v.uid')


sign_visit_type <- sign_visit_type%>%
  filter(signup_dt == '2016-07-24')

# convert date column to date type
sign_visit_type$dt <- as.Date(sign_visit_type$dt)
sign_visit_type$signup_dt <- as.Date(sign_visit_type$signup_dt)

# calculate the number of week for visiting back
sign_visit_type$week <- ceiling(difftime(sign_visit_type$dt,sign_visit_type$signup_dt,units = 'days')/7)
sign_visit_type$week <- as.numeric(sign_visit_type$week)


visit_type_table <- sqldf('select auth_type,week, count(*) as num from 
              (select auth_type,uid, week 
              from sign_visit_type
              group by auth_type,uid, week)
              group by auth_type,week')

# desired week
visit_type_table <- visit_type_table%>%
  filter(week <= 24)

# reshape the dataframe for the desired table
cast_visit_type_table <- cast(visit_type_table,auth_type~week,value = 'num')

cast_visit_type_table <- rename(cast_visit_type_table,c(`0` = '# signed up'))

# convert number of visiting to percentage of visiting
for (i in seq(3,26)){
  cast_visit_type_table[,i] = round((cast_visit_type_table[,i]/cast_visit_type_table[,2]),2)
}

# Result table
cast_visit_type_table

# reshape the table for heatmap
melt_auth_type <- melt(cast_visit_type_table,id = 'auth_type')
melt_auth_type <- melt_auth_type%>%
  filter(week > 0)

ggplot(melt_auth_type,aes(x=week,y=auth_type,fill = value))+
    geom_tile() +
    labs(x = "week of revisiting", y = "auth_type", title = "Percentage of visiting by week and auth_type on 2016-07-24") +
    scale_fill_gradient(low = "white", high = "RED")






# THE CODE BELOW IS EXACTLY THE SAME AS ABOVE, EXCEPT FOR THE DATE.
# For 2016-07-24

# Join visit and signup tables
sign_visit_type <- sqldf('select s.uid, s.signup_dt,s.auth_type, v.dt from signups s
                   join visits v on s.uid = v.uid')

# for 2016-07-24
sign_visit_type <- sign_visit_type%>%
  filter(signup_dt == '2016-08-18')

# convert date column to date type
sign_visit_type$dt <- as.Date(sign_visit_type$dt)
sign_visit_type$signup_dt <- as.Date(sign_visit_type$signup_dt)

# calculate the number of week for visiting back
sign_visit_type$week <- ceiling(difftime(sign_visit_type$dt,sign_visit_type$signup_dt,units = 'days')/7)
sign_visit_type$week <- as.numeric(sign_visit_type$week)


visit_type_table <- sqldf('select auth_type,week, count(*) as num from 
              (select auth_type,uid, week 
              from sign_visit_type
              group by auth_type,uid, week)
              group by auth_type,week')

# desired week
visit_type_table <- visit_type_table%>%
  filter(week <= 24)

# reshape the dataframe for the desired table
cast_visit_type_table <- cast(visit_type_table,auth_type~week,value = 'num')

cast_visit_type_table <- rename(cast_visit_type_table,c(`0` = '# signed up'))

# convert number of visiting to percentage of visiting
for (i in seq(3,26)){
  cast_visit_type_table[,i] = round((cast_visit_type_table[,i]/cast_visit_type_table[,2]),2)
}

# Result table
cast_visit_type_table

# reshape the table for heatmap
melt_auth_type <- melt(cast_visit_type_table,id = 'auth_type')
melt_auth_type <- melt_auth_type%>%
  filter(week > 0)

ggplot(melt_auth_type,aes(x=week,y=auth_type,fill = value))+
    geom_tile() +
    labs(x = "week of revisiting", y = "auth_type", title = "Percentage of visiting by week and auth_type on 2016-08-18") +
    scale_fill_gradient(low = "white", high = "RED")


# Conclusion
#  On 2016-07-04, type C achieves the highest retention rate, especially for the first 20 weeks
#  On 2016-08-18, type B achieves the highest retention rate, especially for the first 20 weeks
# Therfore, the retention vary by different auth types 







# 4) For users who signed up on Jun 1 2016, what proportion of them came back after signing up, for the first time within 1 week (Jun 2 – Jun 8 2016),
# first time within 2 weeks (Jun 2 – Jun 15 2016) , first time within 3 weeks (Jun 2 – 22 2016) etc. 

# Use table 'sign_visit' we computed in question 2
first_time_visit <- sign_visit%>%
  filter(week > 0)

first_time_visit <- sqldf('select signup_dt, first_week,count(*) as count from 
                    (select signup_dt,min(week) as first_week from first_time_visit
                    group by uid,signup_dt)
                    group by signup_dt,first_week')

# desired week and date
first_time_visit <- first_time_visit%>%
  filter(first_week <= 24,signup_dt < as.Date('2016-10-31'))

# reshape the dataframe for the desired table
cast_first_time_visit <- cast(first_time_visit,signup_dt~first_week,value = 'count')

cast_first_time_visit$`# signed up` <- cast_visit_table$`# signed up`

# Convert NA to 0
cast_first_time_visit[is.na(cast_first_time_visit)] <- 0

# convert number of visiting to percentage of visiting
for (i in seq(2,25)){
  cast_first_time_visit[,i] = round(cast_first_time_visit[,i]/cast_first_time_visit[,26],2)
}

# result table
cast_first_time_visit

# compute the proportion of users don’t come back even after 24 weeks time 
cast_first_time_visit <- cast_first_time_visit%>%
  mutate(percent_no_return = 1-rowSums(.[2:25]))

mean(cast_first_time_visit$percent_no_return)
