prop <- 0.5
xxx <- NULL
for (i in 1:3030){
  sss <- c(xxx, dbinom(i, 3030, prop))
}
plot(xxx, xlim=c(1515-100, 1515+100 ))
abline(v=1515)
dbinom(1515, 3030, prop)
qbinom(0.025, 3030, prop)
qbinom(0.975, 3030, prop)

http://stattrek.com/online-calculator/binomial.aspx

