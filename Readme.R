ttraw <- scan("tests.txt", character(), sep="\n")
tttime <- substr(ttraw,1,8)
vaisala <- strptime(substr(ttraw,27,48), format = "%b %d %T %Y", tz = "UTC")
vaidate <- as.Date(vaisala)
time <- as.POSIXct(paste(vaidate, tttime))
kulonbs <- vaisala - time
## Adjustments identification and date
clkadjust <- kulonbs > -1
## Exclude clock adjustment for aggregation
vaidate[clkadjust] <- vaidate[clkadjust] + 1
aggreg <- tapply(kulonbs, vaidate, mean)
okvaidate <- as.Date(names(aggreg))
nrnewadj <- which(diff(okvaidate) == 1) + 1
okvaidate[nrnewadj] <- okvaidate[nrnewadj] - 1
## Process dataframe differences from real differences
res.df <- data.frame(Readdiff = diff(okvaidate),
                     Clockdiff = diff(aggreg)
                     )
res.df <- res.df[res.df$Readdiff > 0, ]

plot(res.df, xlab = "Readdiff [days]",
     ylab = "clockdiff [secs]")
res.lm <- lm(Clockdiff ~ Readdiff, res.df)
abline(res.lm)
## Napi diff secben -2.54
coef(res.lm)[2]
## Havi diff percben -1.27
(coef(res.lm)[2] * 30) / 60
