setwd("U:/soc754/")

df <- read.csv("ps1_data_F2023.csv")

df$nNx <- as.numeric(gsub(",", "", df$nNx, fixed=TRUE))
df$nDx <- as.numeric(gsub(",", "", df$nDx, fixed=TRUE))

df$nqx <- df$nDx/df$nNx
df$nqx[nrow(df)] <- 1

df$lx <- NA
df$lx[1] <- 100000
for(i in 2:nrow(df)) {
  df$lx[i] <- df$lx[i-1]*(1-df$nqx[i-1])
}

df$ndx <- NA
for(i in 1:nrow(df)) {
  df$ndx[i] <- df$lx[i]*(df$nqx[i])
  if(i==nrow(df)) {
    df$ndx[i] <- df$lx[i]
  }
}

df$nLx <- NA
for(i in 1:nrow(df)) {
  if(i<nrow(df)) {
    addon <- df$lx[i+1]*(df$x[i+1]-df$x[i])
  }
  if(i==nrow(df)) {
    addon <- 0
  }
  
    df$nLx[i] <- df$nax[i]*df$ndx[i]+addon
  
}

df$nmx <- df$ndx/df$nLx

df$Tx <- NA
df$Tx[1] <- sum(df$nLx)
for(i in 2:nrow(df)) {
  df$Tx[i] <- df$Tx[i-1]-df$nLx[i-1]
}

df$ex <- df$Tx/df$lx

plot(df$x, df$lx, xlab="Age at beginning of interval", ylab="lx", main="x and lx")

plot(df$x, df$ndx, xlab="Age at beginning of interval", ylab="ndx", main="x and ndx")

plot(df$x, df$nmx, xlab="Age at beginning of interval", ylab="nmx", main="x and nmx")