##Problem Set 1 

#Loading libraries (I will be using the tidy approach)
library(readxl)
library(tidyverse)

#Reading in the dataset
France1985 <- read_csv("ps1_data_F2023.csv")
View(France1985)

#1a) I first create France's 1985 abridged life table for males. 
#To calculate the length of each interval, I create
#a new vector n which is x shifted up, and then I 
#subtract n - x to produce the interval-length vector, N.

#Each new vector is then created following the well-known life-table formulas,
#with the replace_na used to insert particular values for open-age categories. 

France1985LT <- France1985 %>% 
  mutate(n = lead(x),
         N = (n - x),
         nmx = nDx/nNx,
         nqx = (N * nmx)/(1 + ((N - nax)*nmx)),
         nqx = replace_na(nqx, 1),
         npx = 1 - nqx,
         npx = replace_na(npx, 1),
         lx = (cumprod(npx)),
         lx = lag(lx),
         lx = replace_na(lx, 1),
         ndx = lx - lead(lx),
         ndx = replace_na(ndx, tail(lx, n = 1)),
         nLx = (N * lead(lx)) + (nax * ndx),
         nLx = replace_na(nLx, France1985LT$lx[France1985LT$x == 85]/(tail(nmx, n = 1))),
         Tx = rev(cumsum(France1985LT$nLx[length(France1985LT$nLx):1])),
         ex = Tx/lx)

#1b) Create plots with ggplot

#lx
lx <- ggplot(data = France1985LT, 
       mapping = aes(x = x, y = lx)) +
  geom_line() +
  labs(x = "Age") 

lx

#ndx
ndx <- ggplot(data = France1985LT, 
             mapping = aes(x = x, y = ndx)) +
  geom_line() +
  labs(x = "Age") 
ndx

#nmx
nmx <- ggplot(data = France1985LT, 
              mapping = aes(x = x, y = nmx)) +
  geom_line() +
  labs(x = "Age") 
nmx

#1c)
40 + France1985LT$ex[France1985LT$x == 40]
#The life expectancy at age 40 for French males is 46.21. This means that conditional on surviving
#to age 40, the average individual is expected to live another 46.21 years if 
#he is subjected to France's 1985 mortality regime.

#1d) 
France1985LT$lx[France1985LT$x == 30]
#The probability of surviving from birth to age 30 is 0.965 or 96.5% for French males subject to 1985
#mortality conditions.

#1e)
France1985LT$lx[France1985LT$x == 65]/France1985LT$lx[France1985LT$x == 30]
#The probability of surviving to age 65 for those who survived to age 30 
#is 0.753 or 75.3% for French males subject to 1985 mortality conditions.

#1f) 
France1985LT$ndx[France1985LT$x == 50]/France1985LT$lx[France1985LT$x == 0]
#The probability that a French male newborn subject to 1985 mortality conditions
#would die between 50 and 55 is 0.041 or 4.1%

#1g)
France1985LT$Tx[France1985LT$x == 15] - France1985LT$Tx[France1985LT$x == 65]
#A French male newborn could expect to live 44.4 years in the interval 15 to 65 if he were 
#subject to French 1985 mortality conditions.

#1h)
#High-mortality populations are often characterized by high infant mortality. 
#In low-mortality populations, newborns often die within the very first hours or days
#of life, implying biological/genetic causes. High-mortality populations differ in that
#there are often social causes of mortality (e.g. famine, increased vulnerability to 
#disease due to weak health-care systems), which implies that the nax could be more 
#evenly distributed in the first years of life. I would therefore expect the first nax 
#value to be much larger than the standard nax values of low-mortality populations which
#reflect the clustering of death within the first day/week/month of the year.

#1i) A stationary population implies a constant flow of births, 
#constant age-specific death rates, and a closed population. In this model, 
#CBR = 1/e0 = CDR. 
1/France1985LT$ex[France1985LT$x == 0]
1/France1985LT$ex[France1985LT$x == 0] * 1000
#The CDR would be 0.014 or 14.02 per 1,000 individuals if France's male population 
#were stationary in 1985. 

#1k
install.packages("LifeTables")
library(LifeTables)

lt_nax <- lt.mx(nmx = France1985LT$nmx, 
                 sex = "male", 
                 age = c(0, 1, seq(5, 85, 5)), 
                 nax = France1985LT$nax)
lt_nax[["lt"]]

lt_null <- lt.mx(nmx = France1985LT$nmx, 
                 sex = "male", 
                 age = c(0, 1, seq(5, 85, 5)), 
                 nax = NULL)
lt_null[["lt"]]
