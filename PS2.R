
##Problem Set 1 

#Loading libraries (I will be using the tidy approach)
#install.packages("HMDHFDplus")
library(readxl)
library(tidyverse)
library(HMDHFDplus)
print(getHMDcountries(), n = 40) 

FemalesLT <- readHMDweb("USA", "fltper_5x1")
Females2004 <- FemalesLT %>% 
  filter(Year == "2004",
         Age < 105)

MalesLT <- readHMDweb("USA", "mltper_5x1")
Males2004 <- MalesLT %>% 
  filter(Year == "2004",
         Age < 105)

#Reading in GSS poverty estimates provided by Prof. Nobles
GSSpoverty <- read_csv("ps2data_2023.csv")

#Splitting by sex and adding "Age" variable that matches HMD data 
#and will permit merging

#Males
GSSpovm <- GSSpoverty %>% 
  filter(sex == "male") %>% 
  mutate(Age = c(0, 1, seq(5, 110, 5)))

#Females
GSSpovf <- GSSpoverty %>% 
  filter(sex == "female") %>% 
  mutate(Age = c(0, 1, seq(5, 110, 5)))

#Merging Male HMD data with poverty data and Female HMD data with poverty data
#Males
HMD_GSS_M <- left_join(
  Males2004,
  GSSpovm,
  by = join_by(Age)
)
#Females 
HMD_GSS_F <- left_join(
  Females2004,
  GSSpovf,
  by = join_by(Age)
)

#1) Estimate expected number of years lived in poverty above age x
#I rely on equation 1 in Molla et al. 2001
#Lxpr = Lx prime, Txpr = Tx prime, epr = e prime

#2) Variance of prevalence rates (varprev)

#3) Variance of life expectancy lived in poverty (varepr)

HMD_GSS_M <- HMD_GSS_M %>% 
  rename(npix = `proportion_poverty_1-n_x` ) %>% 
  mutate(Lxprm = Lx * npix,
         Txprm = rev(cumsum(Lxprm[length(Lxprm):1])),
         eprm = Txprm/lx,
         varprevm = ((1 - npix) * npix) / number_sampled_N,
         Lx2m = Lx^2,
         Lx2s2m = Lx2m * varprevm,
         vareprm = (1/lx^2) * rev(cumsum(Lx2s2m[length(Lx2s2m):1])))

HMD_GSS_F <- HMD_GSS_F %>% 
  rename(npix = `proportion_poverty_1-n_x` ) %>% 
  mutate(Lxprf = Lx * npix,
        Txprf = rev(cumsum(Lxprf[length(Lxprf):1])),
        eprf = Txprf / lx,
        varprevf = ((1 - npix) * npix) / number_sampled_N,
        Lx2f = Lx^2,
        Lx2s2f = Lx2f * varprevf,
        vareprf = (1 / lx^2) * rev(cumsum(Lx2s2f[length(Lx2s2f):1])))
#a) 
df_a <- data.frame(HMD_GSS_M$Age, HMD_GSS_M$eprm, HMD_GSS_F$eprf)
df_a
#According to the data, females at every age group until 100 have a life expectancy in poverty
#that is greater than males'.

#b 
zscore <- (HMD_GSS_M$eprm - HMD_GSS_F$eprf)/(sqrt(HMD_GSS_M$vareprm) + sqrt(HMD_GSS_F$vareprf))
df_bc <- tibble(zscore)
df_a$HMD_GSS_M.eprm[1] - df_a$HMD_GSS_F.eprf[1]
#At birth, women are expected to live 5.67 years more in poverty than men. This is statistically
#significant with a z-score of -13.5.
df_bc[1,]

#c) What about the rest of the age intervals? Are the differences in life expectancy statistically
#significant?
#I set my threshold at alpha = 0.05, which is equivalent to t = 1.96.
statsig <- case_when(abs(zscore) > 1.96 ~ T,
                     abs(zscore) < 1.96 ~ F)
statsig
#The difference in expected life lived in poverty is no longer significant between males and females
#from ages 95 and above. 

