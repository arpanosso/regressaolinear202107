library(tidyverse)
library(broom)
visao <- read_csv2("https://raw.githubusercontent.com/curso-r/main-regressao-linear/master/misc/visao.csv")

ggplot(visao) + geom_point(aes(idade, distancia))

# regressao
modelo <- lm(distancia ~ idade, data = visao)

# sumários
summary(modelo)
glance(modelo)

# gráficos
plot(modelo)

# predicoes
predict(modelo)
augment(modelo)
