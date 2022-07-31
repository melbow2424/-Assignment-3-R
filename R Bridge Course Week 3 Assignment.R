setwd("c:/Users/Melissa/OneDrive/Documents/CUNY/Bridge/R/Homework 3")

df <- read.csv('baseball.csv')
View(df)

#df_ba = transform(df, batting_average = r/ab*100) # wrong calculation for batting average
#View(df_ba)


df_h = transform(df, batting_average = h/ab)
summary(df_h)

without_outliers_all_years = subset(df_h, !is.infinite(batting_average) 
                          & !is.na(batting_average) & batting_average < 40 
                          & batting_average > 0 & ab > 99)

df_97_07_ba = subset(without_outliers_all_years, year > 1996)
View(df_97_07_ba)

summary(df_97_07_ba)


#summary(df_97_07_ba)

without_outliers = subset(df_97_07_ba, !is.infinite(batting_average) 
                             & !is.na(batting_average) & batting_average < 0.50 
                             & batting_average > 0 & ab > 99)
View(without_outliers)
summary(without_outliers)


'''
*My goal here was to find the best baseball hitter from 1997-2007. 
*Added the batting average of a player. 
*Excluded batting averages of NA, Infinty, 1000-400, 0 (Outliers)
*Need to be up at bat about 100 times (if excluding this factor you get players with high
averages because they hit 1 but batted 3 time)
'''
#BMI<-rnorm(n=1000, m=24.2, sd=2.2) 
#hist(BMI)

#hist(without_outliers$batting_average) 

# Historgram: Batting Average from 1997-2007 
hist(without_outliers$batting_average, 
     xlab="Batting Average", 
     main="Distribution of Batting Average from 1997-2007", 
     col="darkgreen")

'''
curve(dnorm(x, 
            mean=mean(without_outliers$batting_average), 
            sd=sd(without_outliers$batting_average)), add=TRUE, col="lightblue", lwd=2)
'''

# Scatter Plot: Batting Average from 1997-2007

plot(x = without_outliers$hr,y = without_outliers$bb,
     xlab = "Home Runs",
     ylab = "Walks",
     xlim = c(10, 75),
     ylim = c(10,235),		 
     main = "Home Runs vs Walks from 1997- 2007"
) + abline(reg = lm(without_outliers$bb ~ without_outliers$hr),
           col = "red", 
           lwd = 2)

boxplot(df_ba$batting_average ~ df_ba$year)

boxplot(df_ba$batting_average ~ df_ba$year, outline = FALSE)

boxplot(without_outliers_all_years$batting_average ~ without_outliers_all_years$year, 
        outline = FALSE) + abline(h=0.27306, 
                                  col="blue", 
                                  lwd = 2)

boxplot(without_outliers$batting_average ~ without_outliers$year) + abline(h=13, col="blue")

summary(without_outliers_all_years)

Summary(df_h)

