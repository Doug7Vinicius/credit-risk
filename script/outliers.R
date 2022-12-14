library(outliers)
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)

dixon.test(x)



dixon.test(dados$IDADE)

grubbs.test(dados.padr.cr)

grubbs.test(dados$RENDA)

library(EnvStats)



boxplot(dados$RENDA, horizontal = TRUE)

summary(dados$RENDA)

IQR(dados$RENDA)

TminRenda = 2000 - (1.5*IQR(dados$RENDA))
TmaxRenda = 16890 + (1.5*IQR(dados$RENDA))

TminPatr = 0 - (1.5*IQR(dados$PATRIMONIO))
TmaxPatr = 593106 + (1.5*IQR(dados$PATRIMONIO))

dados$RENDA[which(dados$RENDA < Tmin | dados$RENDA > Tmax)]

dt$RENDA <- dados$RENDA[which(dados$RENDA > Tmin & dados$RENDA < Tmax)]


boxplot(renda, horizontal = TRUE)
hist(renda)
boxplot(dados)

dt$PATRIMONIO <- dados$PATRIMONIO[which(dados$PATRIMONIO > TminPatr & dados$PATRIMONIO < TmaxPatr)]


dat <- ggplot2::mpg
summary(dat$hwy)

hist(dat$hwy,
     xlab = "hwy",
     main = "Histogram of hwy",
     breaks = sqrt(nrow(dat))
) # set number of bins


boxplot(dat$hwy,
        ylab = "hwy"
)

boxplot.stats(dat$hwy)$out


cl <- select(cluster, -c(...1, SM_30, SM_60, SM_90, SM_180, SM_360))

database <- cl %>%
  filter(dados.k3.cluster %in% 2)
