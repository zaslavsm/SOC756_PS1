#### Reading in libraries ####

library(kableExtra)
library(readxl)
library(tidyverse)
library(HMDHFDplus)

#### Loading data ####
# HMD Females USA 2005
BLTUSA2005 <- readHMDweb("USA", "bltper_1x1")

#Next, I create my multi-decrement lifetable. 
#I pull the age-specific probability of dying in the US for 16 to 31 year olds,
#assuming that they map on well to Wisconsinites age-specific probabilities of 
#dying in 2005. 

#The `D` suffix refers to all columns concerned with just death, the `A` suffix with
#columns just concerned with accidents, and the `tot` suffix with columns concerned
#with both.

# Set radix 
r <- 85000

Deaths2005 <- BLTUSA2005 %>% 
  filter(Year == "2005",
         Age %in% c(16:31)) %>% 
  select(Age, qx) %>% 
  rename(qxD = qx) %>% 
  mutate(qxA = 0.062 - (0.000053 * Age^2),
         qxtot = (qxD + qxA),
         pxD = 1 - qxD,
         pxA = 1 - qxA,
         pxtot = 1 - qxtot,
         lxD = c(r, r * cumprod(pxD[1:(length(pxD) - 1)])),
         lxA = c(r, r * cumprod(pxA[1:(length(pxA) - 1)])),
         lxtot = c(r, r * cumprod(pxtot[1:(length(pxtot) - 1)])),
         ndxD = lxtot * qxD,
         ndxA = lxtot * qxA,
         ndxtot = lxtot * qxtot)

# 1A)
Deaths2005$lxtot[16] / Deaths2005$lxtot[1]

# 0.5956642

# 1B)
(Deaths2005$lxA[Deaths2005$Age == 25] - Deaths2005$lxA[Deaths2005$Age == 31]) / Deaths2005$lxtot[Deaths2005$Age == 25]

# 0.1247539

# 1C) 
sum(Deaths2005$ndxD) / Deaths2005$lxtot[Deaths2005$Age == 16]

# 0.01087141

