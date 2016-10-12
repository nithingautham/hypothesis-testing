#Hypotheses Testing


# Reading Input file
articles <- read.csv("D:\\education\\pgdda\\Course - 2\\assignment\\Article popularity dataset.csv")


#Treating Outliers in Shares data

IQR <- boxplot.stats( articles$shares)$stats[4] - boxplot.stats( articles$shares)$stats[2]

UpperLimit <- boxplot.stats( articles$shares)$stats[4] + (IQR * 1.5)

LowerLimit <- boxplot.stats( articles$shares)$stats[2] - + (IQR * 1.5)

articles$shares <- as.numeric(  articles$shares )

articles <- articles[ !(articles$shares < LowerLimit |  articles$shares > UpperLimit) , ] 


# way to extract Day of publishing for each article from the URL 
# Using str_locate using stringr library
library(stringr)

articles$date_yyyy_mm_dd <-sapply(articles$URL, function(x)  substr(x ,    str_locate(x, "[0-9]+[/][0-9]+[/][0-9]+")[1] ,
                                                                    str_locate(x, "[0-9]+[/][0-9]+[/][0-9]+")[2])    )

articles$day_of_week <-  weekdays(  as.Date(  articles$date_yyyy_mm_dd ) )

# Creating new meaningful columns

articles$Subject_type[ as.numeric(articles$data_channel_is_socmed) == 1  ] <- "Social_Media"

articles$Subject_type[ as.numeric(articles$data_channel_is_socmed) != 1  ] <- "Others"


articles$Weekday_type[ as.character(articles$day_of_week) == "Satruday" | as.character(articles$day_of_week) =="Sunday"  ] <- "Weekends"

articles$Weekday_type[ as.character(articles$day_of_week) != "Satruday" & as.character(articles$day_of_week) !=
                         "Sunday"  ] <- "Weekdays"


# Initial Insight

library(gdata)

tapply(articles$shares, list( articles$Subject_type,articles$Weekday_type), mean)


# Questions - 1
# Confirm at 1% significance level if the average number of shares for each article differ significantly for
# articles published over weekdays and the weekend (Z1 versus Z2).


t1 <- t.test(  articles$shares[ articles$Weekday_type =="Weekdays"],
          articles$shares[ articles$Weekday_type =="Weekends"  ]  ,
         conf.level = 0.99 , alternative = "two.sided" , paired = FALSE
        )

t1$p.value

t1$statistic


# Question - 2
# Confirm at 1% significance level if the average number of shares for each article published over the
# weekend differ significantly for articles on Social media channel and other channels (X12 versus X22).



t2 <- t.test(  articles$shares[ articles$Weekday_type =="Weekends" & articles$Subject_type == "Others"]
         ,articles$shares[ articles$Weekday_type =="Weekends" & articles$Subject_type == "Social_Media"]
         ,conf.level = 0.99 , alternative = "two.sided" , paired = FALSE
         )

t2$statistic

t2$p.value

# Question - 3
# Confirm at 1% significance level if the average number of shares for each article published over
# weekdays differ significantly for 
# articles on Social media channel and other channels (X11 versus X21).


t3 <- t.test(  articles$shares[ articles$Weekday_type =="Weekdays" & articles$Subject_type == "Social_Media"]
         ,articles$shares[ articles$Weekday_type =="Weekdays" & articles$Subject_type == "Others"]
         ,conf.level = 0.99 , alternative = "two.sided" , paired = FALSE
      )

t3$statistic

t3$p.value


# Question - 4
# Confirm at 5% significance level if the average number of shares for Social Media articles published over weekdays 
# and weekends differ significantly from each other (X11 versus X12).


t4 <- t.test(  articles$shares[ articles$Weekday_type =="Weekdays" & articles$Subject_type == "Social_Media"]
         ,articles$shares[ articles$Weekday_type =="Weekends" & articles$Subject_type == "Social_Media"]
         ,conf.level = 0.95 , alternative = "two.sided" , paired = FALSE
            )

t4$statistic

t4$p.value