## Name - Sankalp Susil Kumar Biswal 
## Date - 28/10/2023 
## Class - ALY6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
library(ggplot2)
library(tidyverse)
library(janitor)
library(lubridate)

#Project6----

#Q1-Q10 ----

p<- 0.65
n <- 7

#1----
prob1_result <- dbinom(5,n,p)

#2----
win <- c(0:7)
prob2_result <-  data.frame(
  wins = win,
  probability = dbinom(win,n,p)
  
)

#3----
#Using pbinom for cummulative probability
prob3_result <- pbinom(4,n,p) 

#4----
prob4_result <- pbinom(5,n,p) - pbinom(2,n,p)

#5----
prob5_result <- pbinom(7,n,p) - pbinom(4,n,p)

#6----
# Theoretical Expected value = n*p
prob6_result <- p*n

#7----
#For Binomial Distribution, Variance = n*p*(1-p) {n= sample size, p= prob. of winning}
prob7_result <- n*p*(1-p)

#8----

set.seed(10)
#generating 1000 random values for no. of wins using 'rbinom'
q8 <- rbinom(1000, n,p)

#9----
prob9_result <- mean(q8)

#10----

prob10_result <- var(q8)

#Poisson Distribution
#Q11-Q18----
#11----
lambda <- 7
prob11_result <-dpois(6,(lambda/60)*60)

#12----
#Removing the 60/60 used in the previous step as they get cancelled
prob12_result <- ppois(40, lambda*8)

#13----
#Using the complementary rule. Multiplying lambda *5 because there are 5 employee working now.
#Used '274' since we need 275 or greater.
prob13_result <- 1 - ppois(274, lambda*5*8)

#14----
#Multiplying lambda*4 since there are 4 employees now
prob14_result <- 1 - ppois(274, lambda*4*8)

#15----
#qpois(p,lambda) p = percentile value. 
#p =0.90 since we want to calculate top 10% which is 90th percentile

prob15_result <- qpois(0.90,lambda*8 )

#16----
set.seed(15)
random <- rpois(1000,lambda*8)

#17----
prob17_result<- mean(random)

#18----Doubt
prob18_result <- var(random)

#Normal Distribution----
avg <- 2000
sd <- 100
#19----
#Using pnorm(x,mean,sd)
prob19_result <- pnorm(2200,avg,sd) - pnorm(1800,avg,sd)

#20----
prob20_result <- 1 - pnorm(2500,avg,sd)

#21----
#Used qnorm() and round(). Used 0.10 in qnorm since we have to find bottom 10%

prob21_result<- round(qnorm(0.10,avg,sd), 0)

#22----

set.seed(25)
random_lights <- rnorm(10000,avg,sd)

#23----
prob23_result<- mean(random_lights)

#24----
prob24_result <- sd(random_lights)

#25----
set.seed(1)
#Using replicate() to make 1000 samples and sample() to create each sample with 100 values
samples <- replicate(1000, sample(random_lights, 100), simplify = FALSE)
#Using saaply() to find mean for each row
row_mean <- sapply(samples,mean)

prob25_result <- data.frame( mean = row_mean)

#26----

# Create the histogram plot
ggplot(prob25_result, aes(x = mean)) +
  geom_histogram(binwidth = 4, fill = "blue", color = "black") +
  labs(
    title = "Histogram of mean of samples",
    x = "Values",
    y = "Frequency"
  )+scale_y_continuous(
    breaks = seq(0,200, by=50),
    limits = c(0,200)
  )
#27----
prob27_result <- mean(prob25_result$mean)

#Penguins----
#Analysing flipper length of penguins
library(palmerpenguins)

#28----

adelie_penguis<- filter(penguins, species == 'Adelie')
adelie_penguis <- na.omit(adelie_penguis)

ggplot(adelie_penguis, aes(x = adelie_penguis$flipper_length_mm)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    title = "Histogram of Adelie Penguin's Flipper Length",
    x = "Flipper Length in mm",
    y = "Frequency"
  )
# A Shapiro-Wilk test is used to check the normality of the above distribution
shapiro.test(adelie_penguis$flipper_length_mm)

#29----

#Creating dataset for Gentoo penguins
gentoo_penguin <- filter(penguins,species == 'Gentoo')
gentoo_penguin <- na.omit(gentoo_penguin)

#Creating a scatter-plot in order to compare the two features of Gentoo Penguins

ggplot(gentoo_penguin, aes(x = flipper_length_mm, y = bill_depth_mm, color = flipper_length_mm)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Gentoo Penguin Flipper Length vs. Bill Depth",
    x = "Flipper Length (mm)",
    y = "Bill Depth (mm)"
  )+geom_text(x = 210, y = 17, label = paste("Correlation-", round(correlation_coefficient, 3)))

#Finding the correlation coefficient to understand the relation b/w X and Y axis

correlation_coefficient <- cor(gentoo_penguin$flipper_length_mm, gentoo_penguin$bill_depth_mm)


#Test Run----
p_load(testthat)
test_file("project6_tests.R")

