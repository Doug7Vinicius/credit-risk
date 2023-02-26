library(tidyverse)
library(rJava)
library(xlsx)
library(readxl)
library(dlookr)

Dados <- read_xlsx("C:/Users/Douglas/Documents/Trabalho/Dataset.xlsx", sheet = 2)

### Pré-processamento dos dados ###

# Verificando as variáveis
str(cr)
glimpse(cr)
  # Variáveis
# Sexo: 0 = Masculino, 1 = Feminino, 2 = PJ;
# Estado Civil: 0 = Solteiro, 1 = Casado, 2 = Divorciado, 3 = Viuvo, 4 = União Estável, 5 = PJ;
# Instrução: 0 = Não Informado, 1 = Sem instrução, 2 = 1º Grau, 3 = 2º Grau, 4 = Superior Incompleto, 
# 5 = Superior completo, 6 = Pós-Graduação, PJ = 7;
# Inadimplência: 0 = Não, 1 = Sim;


cr <- mutate_at(cr, vars(SEXO, ESTADO_CIVIL, ESCOLARIDADE, STATUS), as.factor)

cr <- select(Dados, -ID)

## Verificar se há NA nos dados
colSums(is.na(Dados))
colSums(is.na(Dados))

# Verificar se há valores vazios (espaço em branco)
colSums(Dados == '')
colSums(test == '')

# Remover missing values and valores vazios
test <- test[-which(is.na(Dados$PATRIMONIO)),]
train <- train[-which(train$Embarked == ""),]

# Imputar missing values - usando uma estratégia bem básica como exemplo
train$Age[is.na(train$Age)] <- median(train$Age, na.rm=T)
test$Age[is.na(test$Age)] <- median(test$Age, na.rm=T)

# Removendo as variáveis Cabin, passengerId, Ticket e Name por não serem importantes na modelagem 
train <- subset(train, select = -c(Cabin, PassengerId, Ticket, Name))
test <- subset(test, select = -c(Cabin, PassengerId, Ticket, Name))

# Verificando multicolinearidade
correlate(cr)
plot_correlate(cr)

# Verificou que as variáveis de Saldo médio de 30, 60, 90, 180 e 360 dias estão muito correlacionados, neste caso foi optado a retirar.
cr <- select(cr, -c("SM_30","SM_60","SM_90","SM_180","SM_360"))

# Variável resposta: STATUS
# Variáveis explicativas: as demais

# Correlação das variáveis numéricas
correlate(cr)
plot_correlate(cr)

# Vendo se a classe está balanceada - altamente desbalanceada
table(cr$STATUS)
prop.table(table(cr$STATUS))



train[, 4:10] <- scale(train[, 4:10])



#check table
table(cr$STATUS)

#check classes distribution
prop.table(table(cr$STATUS))

library(rpart)
treeimb <- rpart(STATUS ~ ., data = train)
pred.treeimb <- predict(treeimb, newdata = test)

accuracy.meas(test$STATUS, pred.treeimb[,2])

roc.curve(test$STATUS, pred.treeimb[,2], plotit = F)

#over sampling
data_balanced_over <- ovun.sample(STATUS ~ ., data = train, method = "over", N = 9596)$data
table(data_balanced_over$STATUS)

data_balanced_under <- ovun.sample(STATUS ~ ., data = train, method = "under", N = 1000, seed = 1)$data
table(data_balanced_under$STATUS)

data_balanced_both <- ovun.sample(STATUS ~ ., data = train, method = "both", p=0.5, N=5087, seed = 1)$data
table(data_balanced_both$STATUS)

data.rose <- ROSE(STATUS ~ ., data = train, seed = 1)$data
table(data.rose$STATUS)

#build decision tree models
tree.rose <- rpart(STATUS ~ ., data = data.rose)
tree.over <- rpart(STATUS ~ ., data = data_balanced_over)
tree.under <- rpart(STATUS ~ ., data = data_balanced_under)
tree.both <- rpart(STATUS ~ ., data = data_balanced_both)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)

roc.curve(test$STATUS, pred.tree.rose[,2])
roc.curve(test$STATUS, pred.tree.over[,2])
roc.curve(test$STATUS, pred.tree.under[,2])
roc.curve(test$STATUS, pred.tree.both[,2])


# Reparticionando o conjunto de dados em dataset de treinamento e test.
require(caTools)
set.seed(101) 
sample = sample.split(cr$ID, SplitRatio = .70)
train = subset(cr, sample == TRUE)
test  = subset(cr, sample == FALSE)




# Boxplot
e <- ggplot(train, aes(x = STATUS, y = RENDA_FATUR))
e2 <- e + geom_boxplot(aes(fill=STATUS)) + theme_light()  
e2

e1 <- ggplot(train, aes(x = SEXO, y = RENDA_FATUR)) 
e3 <- e1 + geom_boxplot(aes(fill = SEXO)) + theme_light()
e3







# Modelo 1
mod1 <- glm(STATUS ~ ., data = train, family = binomial(link = "logit"))
mod1
summary(mod1)

# Teste da Razão de Verossimilhança
anova(mod1, test="Chisq")# adiciona as variáveis sequencialmente (a variável adcional melhora o modelo?)
drop1(mod1, test="Chisq")# remove as variáveis sequencialmente (a variável adcional melhora o modelo?)

# Modelo 2
mod2 <- glm(STATUS ~ IDADE + ESTADO_CIVIL + EMPRESTIMOS + APLICACAO + LIMITE,
            data = train, family = binomial(link = "logit"))
summary(mod2)

# Comparando modelo menor com o maior
anova(mod2, mod1, test="LRT") # se valor p > niv.sig., as variáveis omitidas não são significativas 
# pode ser Chisq no lugar de LRT

# Intervalo de confiança
confint(mod2)

# Selecionando variáveis automaticamente
mod3 <- step(mod1, direction = "backward") # baseado no AIC
summary(mod3)

anova(mod3, mod2, test="LRT") # a variável Embarked pode ser excluída

# Razão de Chances
library(questionr)
odds.ratio(mod2)

library(sjPlot)
plot_model(mod2, vline.color = "red", sort.est = TRUE, 
           show.values = TRUE, value.offset = .3)

### Qualidado do ajuste ###

# Deviance

# Null deviance = 2 (LL(modelo saturado) - LL(modelo nulo)) 
# Residual deviance = 2 (LL (modelo saturado) - LL (modelo proposto)) 
# Modelo saturado: n parâmetros
# Modelo nulo: 1 parâmetro
# O Modelo proposto pressupõe que você possa explicar seus pontos de dados com 
# k parâmetros + um intercepto, para que você tenha k+ 1 parâmetros.

# Se a null deviance é realmente pequena, significa que o modelo nulo explica muito 
# bem os dados. Da mesma forma com a residual deviance. um valor mais baixo da deviance 
# residual indica que o modelo ficou melhor quando inclui variáveis independentes.

# D/n-k -> 790.68/(889-7) < 1 Modelo ADEQUADO!!!
summary(mod2)

# AIC - quanto menor, melhor
AIC(mod1)
AIC(mod2)
AIC(mod3)

### Diagnóstico do Modelo ###

# Uma maneira de investigar a diferença entre o valor observado e o ajustado é 
# o gráfico marginal do modelo. A variável resposta é plotada em relação à variável 
# explicativa. Os dados observados e a previsão do modelo são mostrados em linhas azuis
# e vermelhas, respectivamente. 
marginalModelPlots(mod2)


marginalModelPlots(mod1) 
marginalModelPlots(mod3) 
# Outliers
car::outlierTest(mod2) # não há outliers

# Pontos influentes
influenceIndexPlot(mod2)
influencePlot(mod2, col = "red", id = list(method = "noteworthy", 
                                           n = 4, cex = 1, col = carPalette()[1], 
                                           location = "lr"))
# Valores que ultrapassam -2 e 2: 262, 631, 298, ...

mod2_298 <- update(mod2, subset = c(-298))
car::compareCoefs(mod2, mod2_298) # não mudou quase nada - não é ponto influente

# Multicolinearidade
library(car)
vif(mod2) # valores abaixo de 5 - OK

# Gráfico dos efeitos
library(effects)
plot(allEffects(mod2))

### Predições ###
pred <- predict(mod2, test, type = "response") 
result <- as.factor(ifelse(pred > 0.5,1,0))

### Desempenho do modelo ###

# Matriz de confusão e medidas
library(caret)
confusionMatrix(result, test$STATUS, positive = "1")

# Curva ROC e AUC
library(pROC)
auc <- roc(test$STATUS, pred)
plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# Usando o novo ponto de corte
result2 <- as.factor(ifelse(pred > 0.551,1,0))
confusionMatrix(result2, test$Survived, positive = "1")

### Exportando a tabela do modelo ###

library(sjPlot)
tab_model(mod2)


















##############################################################################



gfg <- cr1$RENDA_FATUR
gfg <- gfg[!gfg %in% boxplot.stats(gfg)$out]   
boxplot(gfg)

gfg <- cr1$
gfg <- gfg[!gfg %in% boxplot.stats(gfg)$out]   
boxplot(gfg)

mtcars_com_outliers <- mtcars %>% 
  # aqui vamos incluir os pontos bizarros...
  dplyr::bind_rows(
    tibble::tibble(
      mpg = c(0.1, 1039, 481, 1402),
      wt = c(3.21, 1.8230, 2.6740, 3.6720)
    )
  ) 

mtcars_com_outliers %>% 
  dplyr::mutate(
    z_score_mpg = (mpg-mean(mpg))/sd(mpg),
    e_outlier_mpg = dplyr::if_else(abs(z_score_mpg) > 2, "É outlier", "Não é outlier")
  ) %>% 
  ggplot2::ggplot(ggplot2::aes(x = wt, y = mpg, color = e_outlier_mpg)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_minimal(15) +
  ggplot2::labs(x = "Peso do carro", y = "Consumo de combustível (milhas/galão)", color = "")



library(vegan)


env <- read.csv("https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv", header=TRUE, sep=",", dec=".", row.names=1)
spe <- read.csv("https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpe.csv", header=TRUE, sep=",", dec=".", row.names=1)


das <- env[ ,1]
env <- env[ ,-1]
pen2 <- rep("very_steep", nrow(env))
pen2[env$pen <= quantile(env$pen)[4]] = "steep"
pen2[env$pen <= quantile(env$pen)[3]] = "moderate"
pen2[env$pen <= quantile(env$pen)[2]] = "low"
pen2 <- factor(pen2, levels=c("low", "moderate", "steep", "very_steep"))
table(pen2)
env2 <- env
env2$pen <- pen2

str(env)
str(env2)

rda1 <- rda(spe ~ ., data=env)
vif.cca(rda1)

rda2 <- rda(spe ~ ., data=env2)
vif.cca(rda2)

library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)

outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
