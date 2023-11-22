#### Problem Set 6 ####
#### Michael Zaslavsky ####
#### Last updated: 11/22/23 ####
#### https://github.com/zaslavsm ####

#### Load libraries ####

library(kableExtra)
library(tidyverse)
library(readxl)
library(pdftools)
library(ggpubr)

#### Data preparation ####
nLx <- read.csv("nLx_values.csv")
surFile <- read.csv("nLx_values.csv")

#Black educational categories, split
nLb1 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lb1")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLb2 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lb2")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLb3 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lb3")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLb4 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lb4")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLb5 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lb5")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

#White educational categories, split
nLw1 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lw1")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLw2 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lw2")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLw3 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lw3")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLw4 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lw4")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

nLw5 <- nLx %>% 
  filter(str_detect(cat_race_ed_age, "Lw5")) %>% 
  mutate(lead = lead(nLx)) %>% 
  mutate(Sratio = lead/nLx)

#### Step 1A: Creating S submatrices ####

#Blacks 
S0tb <- matrix(0, nrow = 5, ncol = 5)
S5tb <- matrix(0, nrow = 5, ncol = 5)
S10tb <- matrix(0, nrow = 5, ncol = 5)
S15tb <- matrix(0, nrow = 5, ncol = 5)
S20tb <- matrix(0, nrow = 5, ncol = 5)
S25tb <- matrix(0, nrow = 5, ncol = 5)
S30tb <- matrix(0, nrow = 5, ncol = 5)
S35tb <- matrix(0, nrow = 5, ncol = 5)
S40tb <- matrix(0, nrow = 5, ncol = 5)

nLbs <- list(nLb1, nLb2, nLb3, nLb4, nLb5)

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[1]
  S0tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[2]
  S5tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[3]
  S10tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[4]
  S15tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[5]
  S20tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[6]
  S25tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[7]
  S30tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[8]
  S35tb[i, i] <- value
}

for (i in 1:5) {
  value <- nLbs[[i]]$Sratio[9]
  S40tb[i, i] <- value
}

#### Step 1B: Creating S submatrices ####


#Whites
S0tw <- matrix(0, nrow = 5, ncol = 5)
S5tw <- matrix(0, nrow = 5, ncol = 5)
S10tw <- matrix(0, nrow = 5, ncol = 5)
S15tw <- matrix(0, nrow = 5, ncol = 5)
S20tw <- matrix(0, nrow = 5, ncol = 5)
S25tw <- matrix(0, nrow = 5, ncol = 5)
S30tw <- matrix(0, nrow = 5, ncol = 5)
S35tw <- matrix(0, nrow = 5, ncol = 5)
S40tw <- matrix(0, nrow = 5, ncol = 5)

nLws <- list(nLw1, nLw2, nLw3, nLw4, nLw5)

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[1]
  S0tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[2]
  S5tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[3]
  S10tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[4]
  S15tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[5]
  S20tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[6]
  S25tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[7]
  S30tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[8]
  S35tw[i, i] <- value
}

for (i in 1:5) {
  value <- nLws[[i]]$Sratio[9]
  S40tw[i, i] <- value
}

#### Step 2A: Creating B submatrices ####

#Blacks
B15tb <- matrix(0, nrow = 5, ncol = 5)
B20tb <- matrix(0, nrow = 5, ncol = 5)
B25tb <- matrix(0, nrow = 5, ncol = 5)
B30tb <- matrix(0, nrow = 5, ncol = 5)
B35tb <- matrix(0, nrow = 5, ncol = 5)
B40tb <- matrix(0, nrow = 5, ncol = 5)

#First, important mobility matrix and transpose it so that the calculations go in the right direction
mBlack <- t(matrix(c(0.289, 0.079, 0.025, 0.033, 0, 
                     0.268, 0.35, 0.19, 0.038, 0.032, 
                     0.243, 0.278, 0.386, 0.243, 0.163, 
                     0.126, 0.164, 0.212, 0.496, 0.371, 
                     0.073, 0.129, 0.188, 0.189, 0.434), nrow = 5, ncol = 5))

#Next, create 5 by 1 L matrices
Lblack <- matrix(0, nrow = 5, ncol = 1)

#Insert 5L0s for each education category for blacks
Lblack[1, 1] <- nLb1[1, 2]
Lblack[2, 1] <- nLb2[1, 2]
Lblack[3, 1] <- nLb3[1, 2]
Lblack[4, 1] <- nLb4[1, 2]
Lblack[5, 1] <- nLb5[1, 2]

#Divide by 2 * l0. According to life tables, radix is 100000
Lblack <- Lblack/200000

#Next, create 5 by 1 Fertility matrices for Blacks assuming constant ASFRs 
grrblack <- matrix(c(2.18, 2.18, 1.67, 1.43, 1.01)/6)#because 6 age cateogries

#Now, we have all of the components to generate Bijat = 5Li0t * (Fiat + Siat * Fi,a + 5t) * Mijt
B15tb <- Lblack[ , 1] * (grrblack[ , 1] + S15tb * grrblack[ , 1]) * mBlack
B20tb <- Lblack[ , 1] * (grrblack[ , 1] + S20tb * grrblack[ , 1]) * mBlack
B25tb <- Lblack[ , 1] * (grrblack[ , 1] + S25tb * grrblack[ , 1]) * mBlack
B30tb <- Lblack[ , 1] * (grrblack[ , 1] + S30tb * grrblack[ , 1]) * mBlack
B35tb <- Lblack[ , 1] * (grrblack[ , 1] + S35tb * grrblack[ , 1]) * mBlack
B40tb <- Lblack[ , 1] * (grrblack[ , 1] + S40tb * grrblack[ , 1]) * mBlack

#### Step 2B: Creating B submatrices ####

#Whites
W15tb <- matrix(0, nrow = 5, ncol = 5)
W20tb <- matrix(0, nrow = 5, ncol = 5)
W25tb <- matrix(0, nrow = 5, ncol = 5)
W30tb <- matrix(0, nrow = 5, ncol = 5)
W35tb <- matrix(0, nrow = 5, ncol = 5)
W40tb <- matrix(0, nrow = 5, ncol = 5)

#First, important mobility matrix and transpose it so that the calculations go in the right direction

mWhite <- t(matrix(c(0.132, 0.034, 0.014, 0.01, 0.001, 
                     0.179, 0.15, 0.061, 0.027, 0.033,
                     0.485, 0.427, 0.457, 0.243, 0.143, 
                     0.13, 0.208, 0.25, 0.338, 0.259, 
                     0.075, 0.18, 0.217, 0.381, 0.564), nrow = 5, ncol = 5)) 

#Next, create 5 by 1 L matrices
Lwhite <- matrix(0, nrow = 5, ncol = 1)

#Insert 5L0s for each education category for blacks
Lwhite[1, 1] <- nLw1[1, 2]
Lwhite[2, 1] <- nLw2[1, 2]
Lwhite[3, 1] <- nLw3[1, 2]
Lwhite[4, 1] <- nLw4[1, 2]
Lwhite[5, 1] <- nLw5[1, 2]

#Divide by 2 * l0. According to life tables, radix is 100000
Lwhite <- Lwhite/200000

#Next, create 5 by 1 Fertility matrices for Blacks assuming constant ASFRs 
grrwhite <- matrix(c(1.76, 1.81, 1.62, 1.54, 1.27)/6)#because 6 age cateogries

#Now, we have all of the components to generate Bijat = 5Li0t * (Fiat + Siat * Fi,a + 5t) * Mijt
W15tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S15tw * grrwhite[ , 1]) * mWhite
W20tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S20tw * grrwhite[ , 1]) * mWhite
W25tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S25tw * grrwhite[ , 1]) * mWhite
W30tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S30tw * grrwhite[ , 1]) * mWhite
W35tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S35tw * grrwhite[ , 1]) * mWhite
W40tb <- Lwhite[ , 1] * (grrwhite[ , 1] + S40tw * grrwhite[ , 1]) * mWhite

#### Step 3: combining everything into one big M matrix for Blacks and Whites ####
null <- matrix(0, 5, 5)

# Blacks 

r1_B = cbind(null, null, null, B15tb, B20tb, B25tb, B30tb, B35tb, B40tb, null)
r2_B = cbind(S0tb, null, null, null, null, null , null, null, null, null)
r3_B = cbind(null, S5tb, null, null, null,  null, null, null, null, null)
r4_B = cbind(null, null, S10tb, null, null, null, null, null, null, null)
r5_B = cbind(null, null, null, S15tb, null, null, null, null, null, null)
r6_B = cbind(null, null, null, null, S20tb, null, null, null, null, null) 
r7_B = cbind(null, null, null, null, null, S25tb, null, null, null, null)
r8_B = cbind(null, null, null, null, null, null, S30tb, null, null, null)
r9_B = cbind(null, null, null, null, null, null, null, S35tb, null, null)
r10_B = cbind(null, null, null, null, null, null, null, null, S40tb, null)

M_B <- rbind(r1_B, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Whites

r1_W = cbind(null, null, null, W15tb, W20tb, W25tb, W30tb, W35tb, W40tb, null)
r2_W = cbind(S0tw, null, null, null, null, null , null, null, null, null)
r3_W = cbind(null, S5tw, null, null, null,  null, null, null, null, null)
r4_W = cbind(null, null, S10tw, null, null, null, null, null, null, null)
r5_W = cbind(null, null, null, S15tw, null, null, null, null, null, null)
r6_W = cbind(null, null, null, null, S20tw, null, null, null, null, null) 
r7_W = cbind(null, null, null, null, null, S25tw, null, null, null, null)
r8_W = cbind(null, null, null, null, null, null, S30tw, null, null, null)
r9_W = cbind(null, null, null, null, null, null, null, S35tw, null, null)
r10_W = cbind(null, null, null, null, null, null, null, null, S40tw, null)

M_W <- rbind(r1_W, r2_W, r3_W, r4_W, r5_W, r6_W, r7_W, r8_W, r9_W, r10_W)

#### Step 4: Initial P Matrix

# First, I import the percentage distribution of Educational Attainment for Black and 
# White women (Table 1)

edu_B <- matrix(c(0.376, 0.29, 0.233, 0.063, 0.038), nrow = 5, ncol = 1) 

edu_W <- matrix(c(0.172, 0.219, 0.421, 0.119, 0.069), nrow = 5, ncol = 1) 

# Next, population counts. As the distributions provided are for the year 1960, I
# import 1960 US population counts for black and white females

# From : https://www2.census.gov/library/publications/decennial/1960/pc-s1-supplementary-reports/pc-s1-11.pdf
# Note that these are actually white/non-white, not white/black counts, but so be it...

# Black
pop_0_b <- 1481801
pop_5_b <- 1302027
pop_10_b <- 1066884
pop_15_b <- 814446
pop_20_b <- 703464
pop_25_b <- 702247
pop_30_b <- 732320
pop_35_b <- 707589
pop_40_b <- 618380
pop_45_b <- 564577

P_B1960 <- matrix(c(pop_0_b * edu_B,
                    pop_5_b * edu_B,
                    pop_10_b * edu_B,
                    pop_15_b * edu_B,
                    pop_20_b * edu_B,
                    pop_25_b * edu_B,
                    pop_30_b * edu_B,
                    pop_35_b * edu_B,
                    pop_40_b * edu_B,
                    pop_45_b * edu_B))

# White 
pop_0_w <- 8509371
pop_5_w <- 7885385
pop_10_w <- 7182319
pop_15_w <- 5771136
pop_20_w <- 4824957
pop_25_w <- 4833802
pop_30_w <- 5370642
pop_35_w <- 5694008
pop_40_w <- 5305982
pop_45_w <- 4956983

P_W1960 <- matrix(c(pop_0_w * edu_W,
                    pop_5_w * edu_W,
                    pop_10_w * edu_W,
                    pop_15_w * edu_W,
                    pop_20_w * edu_W,
                    pop_25_w * edu_W,
                    pop_30_w * edu_W,
                    pop_35_w * edu_W,
                    pop_40_w * edu_W,
                    pop_45_w * edu_W))

#### Question 2 ####

# Equilibrium education distribution for Blacks

t <- 1960 
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_B %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_b <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_b <- tibble::rownames_to_column(eq_dis_b, "Years")

ggplot(eq_dis_b, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_b_1.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Equilibrium education distribution for Whites

t <- 1960
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- 
    M_W %*% 				
    P_Wt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Wt[seq(i, 50, 5), 1])/sum(P_Wt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_w <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_w <- tibble::rownames_to_column(eq_dis_w, "Years")

ggplot(eq_dis_w, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_w_1.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

#### Question 3 ####

# Substitute white fertility patterns into Black birth matrices
B15tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S15tb * grrwhite[ , 1]) * mBlack
B20tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S20tb * grrwhite[ , 1]) * mBlack
B25tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S25tb * grrwhite[ , 1]) * mBlack
B30tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S30tb * grrwhite[ , 1]) * mBlack
B35tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S35tb * grrwhite[ , 1]) * mBlack
B40tbw <- Lblack[ , 1] * (grrwhite[ , 1] + S40tb * grrwhite[ , 1]) * mBlack

r1_Bw = cbind(null, null, null, B15tbw, B20tbw, B25tbw, B30tbw, B35tbw, B40tbw, null)

M_Bw <- rbind(r1_Bw, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Equilibrium education distribution for Blacks with White fertility rates

t <- 1960
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_Bw %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_bw <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_bw <- tibble::rownames_to_column(eq_dis_bw, "Years")

ggplot(eq_dis_bw, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_bw_2.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Calculate difference in equilibrium distribution by focusing on highest education category. 
# First, calculate difference in proportion projected to be in category 5 for Whites and Blacks

a <- eq_dis_w[5 , 2] - eq_dis_b[5 , 2]

# Next, calculate difference with new Black equilibrium distribution which has white ASFRs

b <- eq_dis_w[5 , 2] - eq_dis_bw[5 , 2]

(a - b)/a

#### Question 4 ####
# Substitute White survival sub-matrices into Black Leslie matrix. 
M_BWs <- rbind(r1_B, r2_W, r3_W, r4_W, r5_W, r6_W, r7_W, r8_W, r9_W, r10_W)

# Equilibrium education distribution for Blacks with White maternal mortality rates

t <- 1960
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_BWs %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_bws <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_bws <- tibble::rownames_to_column(eq_dis_bws, "Years")

ggplot(eq_dis_bws, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_bws_3.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Calculate difference in equilibrium distribution by focusing on highest education category. 
# First, calculate difference in proportion projected to be in category 5 for Whites and Blacks

a <- eq_dis_w[5 , 2] - eq_dis_b[5 , 2]

# Next, calculate difference with new Black equilibrium distribution which has white ASFRs

b <- eq_dis_w[5 , 2] - eq_dis_bws[5 , 2]

(a - b)/a

#### Question 5 ####
# Substitute White birth sub-matrices into Black Leslie Matrix 
M_BWBs <- rbind(r1_W, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Equilibrium education distribution for Blacks with White maternal mortality rates

t <- 1960
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_BWBs %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_bwbs <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_bwbs <- tibble::rownames_to_column(eq_dis_bwbs, "Years")

ggplot(eq_dis_bwbs, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_bwbs_4.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Calculate difference in equilibrium distribution by focusing on highest education category. 
# First, calculate difference in proportion projected to be in category 5 for Whites and Blacks

a <- eq_dis_w[5 , 2] - eq_dis_b[5 , 2]

# Next, calculate difference with new Black equilibrium distribution which has white ASFRs

b <- eq_dis_w[5 , 2] - eq_dis_bwbs[5 , 2]

(a - b)/a

#### Question 6 ####

#Substitute white intergenerational mobility rates into submatrices 
B15tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S15tb * grrblack[ , 1]) * mWhite
B20tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S20tb * grrblack[ , 1]) * mWhite
B25tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S25tb * grrblack[ , 1]) * mWhite
B30tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S30tb * grrblack[ , 1]) * mWhite
B35tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S35tb * grrblack[ , 1]) * mWhite
B40tb_wmob <- Lblack[ , 1] * (grrblack[ , 1] + S40tb * grrblack[ , 1]) * mWhite

r1_bwmob = cbind(null, null, null, B15tb_wmob, B20tb_wmob, 
                 B25tb_wmob, B30tb_wmob, B35tb_wmob, B40tb_wmob, null)

M_Bwmob <- rbind(r1_bwmob, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Educational distribution for Whites after 3 generations

t <- 1960
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

for (l in 1:90) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- M_W %*% P_Wt
  
  for (i in 1:5) {
    Ed_dis[i] <-
      sum(P_Wt[seq(i, 50, 5), 1]) / sum(P_Wt)
  }
  
  t = t + 1
  
  print(paste("Iteration:", l))
  print(Ed_dis)
  eq_dis_3genw <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
}

eq_dis_3genw <- tibble::rownames_to_column(eq_dis_3genw, "Years")

ggplot(eq_dis_3genw, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_3genw.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Educational distribution for Blacks after 3 generations

t <- 1960
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

for (l in 1:90) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- M_B %*% P_Bt
  
  for (i in 1:5) {
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1]) / sum(P_Bt)
  }
  
  t = t + 1
  
  print(paste("Iteration:", l))
  print(Ed_dis)
  eq_dis_3genb <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
}

eq_dis_3genb <- tibble::rownames_to_column(eq_dis_3genb, "Years")

ggplot(eq_dis_3genb, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_3genb.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Calculate difference in equilibrium distribution by focusing on highest education category. 
# First, calculate difference in proportion projected to be in category 5 for Whites and Blacks

a <- eq_dis_3genw[5 , 2] - eq_dis_3genb[5 , 2]

# Equilibrium education distribution for Blacks with White intergenerational mobility
# rates after 3 generations

t <- 1960
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

for (l in 1:90) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- M_Bwmob %*% P_Bt
  
  for (i in 1:5) {
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1]) / sum(P_Bt)
  }
  
  t = t + 1
  
  print(paste("Iteration:", l))
  print(Ed_dis)
  eq_dis_bwmob <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
}

eq_dis_bwmob <- tibble::rownames_to_column(eq_dis_bwmob, "Years")

ggplot(eq_dis_bwmob, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_bwmob_4.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Calculate difference in equilibrium distribution by focusing on highest education category. 
# First, calculate difference in proportion projected to be in category 5 for Whites and Blacks

a <- eq_dis_3genw[5 , 2] - eq_dis_3genb[5 , 2]

# Next, calculate difference with new Black distribution after 3 generations with
# white intergenerational mobility rates.

b <- eq_dis_3genw[5 , 2] - eq_dis_bwmob[5 , 2]

(a - b)/a

#### Question  7 ####

# Generate matrix for full intergenerational "rigidity"
nointm <- (diag(1, nrow = 5, ncol = 5))

# Substitute into birth sub-matrices for Blacks
B15tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S15tb * grrblack[ , 1]) * nointm
B20tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S20tb * grrblack[ , 1]) * nointm
B25tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S25tb * grrblack[ , 1]) * nointm
B30tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S30tb * grrblack[ , 1]) * nointm
B35tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S35tb * grrblack[ , 1]) * nointm
B40tb_nointm <- Lblack[ , 1] * (grrblack[ , 1] + S40tb * grrblack[ , 1]) * nointm

r1_nointmb = cbind(null, null, null, B15tb_nointm, B20tb_nointm, 
                 B25tb_nointm, B30tb_nointm, B35tb_nointm, B40tb_nointm, null)

M_Bnointm <- rbind(r1_nointmb, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Substitute into birth sub-matrices for Whites
W15tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S15tw * grrwhite[ , 1]) * nointm
W20tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S20tw * grrwhite[ , 1]) * nointm
W25tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S25tw * grrwhite[ , 1]) * nointm
W30tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S30tw * grrwhite[ , 1]) * nointm
W35tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S35tw * grrwhite[ , 1]) * nointm
W40tb_nointm <- Lwhite[ , 1] * (grrwhite[ , 1] + S40tw * grrwhite[ , 1]) * nointm

r1_nointmw = cbind(null, null, null, W15tb_nointm, W20tb_nointm, 
                  W25tb_nointm, W30tb_nointm, W35tb_nointm, W40tb_nointm, null)

M_Wnointm <- rbind(r1_nointmw, r2_W, r3_W, r4_W, r5_W, r6_W, r7_W, r8_W, r9_W, r10_W)

# Equilibrium education distribution for Blacks with no intergenerational mobility

t <- 1960 
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

for (l in 1:2837) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_Bnointm %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  print(paste("Iteration:", l))
  print(Ed_dis)
  eq_dis_bnointm <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
}

eq_dis_bnointm

#Does not converge -- or rather, at the 2838th iteration everyone is in category 4. 

# Equilibrium education distribution for Whites with no intergenerational mobility 

t <- 1960
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

for (l in 1:2252) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- 
    M_Wnointm %*% 				
    P_Wt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Wt[seq(i, 50, 5), 1])/sum(P_Wt)
  }		
  
  t = t + 1
  
  print(paste("Iteration:", l))
  print(Ed_dis)
  eq_dis_wnointm <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
}

eq_dis_wnointm


# Does not converge -- or rather, at the 2252nd iteration everyone is in category 4. 

#### Question 8 ####
# Generate matrix for independent intergenerational mobility 
mcon <- matrix(rep(0.2, times = 25), nrow = 5, ncol = 5)

# Substitute into birth sub-matrices for Blacks
B15tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S15tb * grrblack[ , 1]) * mcon
B20tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S20tb * grrblack[ , 1]) * mcon
B25tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S25tb * grrblack[ , 1]) * mcon
B30tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S30tb * grrblack[ , 1]) * mcon
B35tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S35tb * grrblack[ , 1]) * mcon
B40tb_mcon <- Lblack[ , 1] * (grrblack[ , 1] + S40tb * grrblack[ , 1]) * mcon

r1_mconb = cbind(null, null, null, B15tb_mcon, B20tb_mcon, 
                   B25tb_mcon, B30tb_mcon, B35tb_mcon, B40tb_mcon, null)

M_Bmcon <- rbind(r1_mconb, r2_B, r3_B, r4_B, r5_B, r6_B, r7_B, r8_B, r9_B, r10_B)

# Substitute into birth sub-matrices for Whites
W15tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S15tw * grrwhite[ , 1]) * mcon
W20tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S20tw * grrwhite[ , 1]) * mcon
W25tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S25tw * grrwhite[ , 1]) * mcon
W30tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S30tw * grrwhite[ , 1]) * mcon
W35tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S35tw * grrwhite[ , 1]) * mcon
W40tb_mcon <- Lwhite[ , 1] * (grrwhite[ , 1] + S40tw * grrwhite[ , 1]) * mcon

r1_mconw = cbind(null, null, null, W15tb_mcon, W20tb_mcon, 
                   W25tb_mcon, W30tb_mcon, W35tb_mcon, W40tb_mcon, null)

M_Wmcon <- rbind(r1_mconw, r2_W, r3_W, r4_W, r5_W, r6_W, r7_W, r8_W, r9_W, r10_W)

# Equilibrium education distribution for Blacks with independent intergenerational mobility

t <- 1960 
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- 
    M_Bmcon %*% 				
    P_Bt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Bt[seq(i, 50, 5), 1])/sum(P_Bt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_bmcon <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_bmcon <- tibble::rownames_to_column(eq_dis_bmcon, "Years")

ggplot(eq_dis_bmcon, aes(x = Years, y = Ed_dis)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Proportion") 

ggsave("eq_dis_bmcon.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)

# Equilibrium education distribution for Whites with independent intergenerational mobility 

t <- 1960 
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0,0,5), ncol = 1, nrow = 5)

while(TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- 
    M_Wmcon %*% 				
    P_Wt
  
  for (i in 1: 5){
    Ed_dis[i] <-
      sum(P_Wt[seq(i, 50, 5), 1])/sum(P_Wt)
  }		
  
  t = t + 1
  
  if(all(Ed_dis == Ed_dis_p)){
    print(t - 1960)
    print(Ed_dis)
    eq_dis_wmcon <- data.frame(Ed_dis, row.names = c("1", "2", "3", "4", "5"))
    break			
  }	
  
}

eq_dis_wmcon <- tibble::rownames_to_column(eq_dis_wmcon, "Years")

# Converges to 0 -- very strange

#### Question 9 -- Extra credit #### 



# Equilibrium education distribution for Blacks plotted


# At what iteration is convergence reached? Set at 4 decimal points. 

# Blacks

t <- 1960
P_Bt <- P_W1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

while (TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- M_B %*% P_Bt
  
  for (i in 1:5) {
    Ed_dis[i] <- sum(P_Bt[seq(i, 50, 5), 1]) / sum(P_Bt)
  }
  
  if (all(round(Ed_dis, 4) == round(Ed_dis_p, 4))) {
    print(paste("Convergence reached at iteration:", t - 1960))
    break
  }
  
  t = t + 1
}

# Whites 

t <- 1960
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

while (TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- M_W %*% P_Wt
  
  for (i in 1:5) {
    Ed_dis[i] <- sum(P_Wt[seq(i, 50, 5), 1]) / sum(P_Wt)
  }
  if (all(round(Ed_dis, 4) == round(Ed_dis_p, 4))) {
    print(paste("Convergence reached at iteration:", t - 1960))
    break
  }
  
  t = t + 1
}

t <- 0
P_Bt <- P_B1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

result_dfb <- data.frame(matrix(ncol = 0, nrow = 5))

while (TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Bt <- M_B %*% P_Bt
  
  for (i in 1:5) {
    Ed_dis[i] <- sum(P_Bt[seq(i, 50, 5), 1]) / sum(P_Bt)
  }
  
  col_names <- paste0("Ed_dis_", t)
  
  colnames(result_dfb)[ncol(result_dfb)] <- col_names
  
  result_dfb <- cbind(result_dfb, Ed_dis)
  
  t = t + 1
  
  if (all(Ed_dis == Ed_dis_p)) {
    print(t - 1960)
    print(result_dfb)
    break
  }
}

result_dfb$ed_cat = c(1:5)

result_dfb1 <- result_dfb %>% 
  rename(Ed_dis_588 = Ed_dis) %>% 
  pivot_longer(cols = !ed_cat,
               names_to = "Iteration", 
               values_to = "Proportion") %>% 
  mutate(Iteration = as.numeric(str_extract(Iteration, "\\d+")))

plot1 <- ggplot(result_dfb1, aes(x = Iteration, y = Proportion, color = factor(ed_cat))) +
  geom_line() +
  theme_BSSD +
  labs(title = "Black educational distribution after number of iterations") +
  guides(color = guide_legend(title = "Education categories")) +
  geom_vline(xintercept = 69, linetype = "dashed", color = "red")


# Equilibrium education distribution for Whites plotted

t <- 0
P_Wt <- P_W1960
Ed_dis <- matrix(seq(0, 0, 5), ncol = 1, nrow = 5)

result_dfw <- data.frame(matrix(ncol = 0, nrow = 5))

while (TRUE) {
  Ed_dis_p <- Ed_dis
  
  P_Wt <- M_W %*% P_Wt
  
  for (i in 1:5) {
    Ed_dis[i] <- sum(P_Wt[seq(i, 50, 5), 1]) / sum(P_Wt)
  }
  
  col_names <- paste0("Ed_dis_", t)
  
  colnames(result_dfw)[ncol(result_dfw)] <- col_names
  
  result_dfw <- cbind(result_dfw, Ed_dis)
  
  t = t + 1
  
  if (all(Ed_dis == Ed_dis_p)) {
    print(t - 1960)
    print(result_dfb)
    break
  }
}

result_dfw$ed_cat = c(1:5)

result_dfw1 <- result_dfw %>% 
  rename(Ed_dis_1056 = Ed_dis) %>% 
  pivot_longer(cols = !ed_cat,
               names_to = "Iteration", 
               values_to = "Proportion") %>% 
  mutate(Iteration = as.numeric(str_extract(Iteration, "\\d+")))

plot2 <- ggplot(result_dfw1, aes(x = Iteration, y = Proportion, color = factor(ed_cat))) +
  geom_line() +
  theme_BSSD + 
  labs(title = "White educational distribution after number of iterations") +
  guides(color = guide_legend(title = "Education categories")) +
  geom_vline(xintercept = 69, linetype = "dashed", color = "red")

combined_plot <- ggarrange(plot1, plot2,
                           common.legend = T,
                           legend = "bottom")

ggsave("combined_plot_PS6.png", 
       scale = 1,
       height = 7,
       width=13, 
       dpi = 300)







