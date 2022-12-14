# ID


library(cluster)
library(factoextra)
library(gridExtra)

setwd("C:\\Users\\Douglas\\Documents\\TRABALHO\\credit-risk\\script")

library(readxl)
base <- read_excel("default of credit card clients.xls")

max(base$AGE)
min(base$AGE)

base$perc1 = base$BILL_AMT1/base$LIMIT_BAL
base$perc2 = base$BILL_AMT2/base$LIMIT_BAL
base$perc3 = base$BILL_AMT3/base$LIMIT_BAL
base$perc4 = base$BILL_AMT4/base$LIMIT_BAL
base$perc5 = base$BILL_AMT5/base$LIMIT_BAL
base$perc6 = base$BILL_AMT6/base$LIMIT_BAL
base$perc1[base$perc1 < 0] <- 0

base2 <- base[, c("LIMIT_BAL", "AGE", "perc1")]

dados.padronizado <- scale(base2[, 1:ncol(base2)])
head(dados.padronizado)

set.seed(5)

dados.k2 <- kmeans(dados.padronizado, centers = 2, nstart = 25, iter.max = 100)
dados.k3 <- kmeans(dados.padronizado, centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(dados.padronizado, centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(dados.padronizado, centers = 5, nstart = 25, iter.max = 100)

G1 <- fviz_cluster(dados.k2, geom = "point", data = dados.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point", data = dados.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(dados.k4, geom = "point", data = dados.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(dados.k5, geom = "point", data = dados.padronizado) + ggtitle("k = 5")

grid.arrange(G1, G2, G3, G4, nrow = 2)

dadosFinal <- data.frame(base, dados.k3$cluster)

head(dadosFinal)

table(dados.k3$cluster)

par(mfrow = c(1,3))

boxplot(dadosFinal$LIMIT_BAL ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "Limite")
boxplot(dadosFinal$AGE ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "Idade")
boxplot(dadosFinal$perc1 ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "% uso de limite")

write.csv(dadosFinal, file = "cluster.csv")


##################################################################################
library(cluster)
library(factoextra)
library(gridExtra)

setwd("C:\\Users\\Douglas\\Documents\\TRABALHO")

library(readxl)
base <- read_excel("C:\\Users\\Douglas\\Documents\\TRABALHO\\Dataset.xlsx", sheet = 2)

dados <- select(base, c(IDADE, RENDA, PATRIMONIO, EMPRESTIMOS, CAPITAL, APLICACAO, LIMITE))

max(base$IDADE)
min(base$IDADE)

base$perc1 = base$BILL_AMT1/base$LIMIT_BAL
base$perc2 = base$BILL_AMT2/base$LIMIT_BAL
base$perc3 = base$BILL_AMT3/base$LIMIT_BAL
base$perc4 = base$BILL_AMT4/base$LIMIT_BAL
base$perc5 = base$BILL_AMT5/base$LIMIT_BAL
base$perc6 = base$BILL_AMT6/base$LIMIT_BAL
base$perc1[base$perc1 < 0] <- 0

base2 <- base[, c("LIMIT_BAL", "AGE", "perc1")]

dados.padr.cr <- scale(dados[, 1:ncol(dados)])
head(dados.padr.cr)

set.seed(1)

dados.k2 <- kmeans(dados.padr.cr, centers = 2, nstart = 25, iter.max = 100)
dados.k3 <- kmeans(dados.padr.cr, centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(dados.padr.cr, centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(dados.padr.cr, centers = 5, nstart = 25, iter.max = 100)

G1 <- fviz_cluster(dados.k2, geom = "point", data = dados.padr.cr) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point", data = dados.padr.cr) + ggtitle("k = 3")
G3 <- fviz_cluster(dados.k4, geom = "point", data = dados.padr.cr) + ggtitle("k = 4")
G4 <- fviz_cluster(dados.k5, geom = "point", data = dados.padr.cr) + ggtitle("k = 5")

grid.arrange(G1, G2, G3, G4, nrow = 2)

dadosFinal <- data.frame(base, dados.k3$cluster)

head(dadosFinal)

table(dados.k3$cluster)

par(mfrow = c(1,3))

boxplot(dadosFinal$LIMITE ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "Limite")
boxplot(dadosFinal$IDADE ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "Idade")
boxplot(dadosFinal$PATRIMONIO ~ as.factor(dadosFinal$dados.k3.cluster), col = "blue", main = "% uso de limite")

write.csv(dadosFinal, file = "cluster.xlsx")
