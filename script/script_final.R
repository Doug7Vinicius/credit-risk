############################################################
####                 REGRESSÃO LOGISTICA                ####
############################################################
library(tidyverse)  #
library(rJava)      #
library(xlsx)       #
library(readxl)     #
library(dlookr)     #
library(questionr)
library(sjPlot)
library(car)
library(effects)
library(caret)
library(pROC)

# Importando os dados para o objeto "Dados".
Dados <- read_xlsx(".../Dataset.xlsx")

# Retirada da variável "ID" (atributo identificador).
cr <- select(Dados, -ID)

### Pré-processamento dos dados ###

# Verificando as variáveis
str(cr)
glimpse(cr)
  # Variáveis
# Sexo:  Masculino,  Feminino;
# Estado Civil: Solteiro, Casado, Divorciado, Viuvo e União Estável;
# Instrução: Não Informado, Sem instrução, 1º Grau, 2º Grau, Superior Incompleto, Superior completo e Pós-Graduação;
# STATUS (Inadimplência): 0 = Não, 1 = Sim.


# Converter "SEXO","ESTADO_CIVIL","ESCOLARIDADE" e "STATUS" para fatores.
cr <- mutate_at(cr, vars(SEXO, ESTADO_CIVIL, ESCOLARIDADE, STATUS), as.factor)

## Verificar se há NA nos dados
colSums(is.na(Dados))

# Verificar se há valores vazios (espaço em branco)
colSums(Dados == '')

# Verificando multicolinearidade
correlate(cr)
plot_correlate(cr)

# Vendo se a classe está balanceada - altamente desbalanceada
table(cr$STATUS)
prop.table(table(cr$STATUS))


