ttraw <- scan("tests.txt", character(), sep="\n")
tttime <- substr(ttraw,1,8)
vaisala <- strptime(substr(ttraw,27,48), format = "%b %d %T %Y", tz = "UTC")
vaidate <- as.Date(vaisala)
time <- as.POSIXct(paste(vaidate, tttime))
kulonbs <- vaisala - time
aggreg <- tapply(kulonbs, vaidate, mean)
res.df <- data.frame(Readdiff = diff(as.Date(names(aggreg))),
                     Clockdiff = diff(aggreg)
                     )

plot(res.df, xlab = "Readdiff [days]",
     ylab = "clockdiff [mins]")
res.lm <- lm(Clockdiff ~ Readdiff, res.df)
abline(res.lm)
## Napi diff secben -2.54
coef(res.lm)[2] * 60
## Havi diff percben -1.27
coef(res.lm)[2] * 30
