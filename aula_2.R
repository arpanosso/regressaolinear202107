# Valor p - quantfica o acaso para tomar uma decisão embasada sem
# levar olé do acaso.

# Vamos interpretar parãmetros
# intervalo de confiança
# e qualidade da nossa reta
# quais a métricas que vao dizer que o modelo está bom ou não??
# métricas são reumos de erros, de quanto a gente está errando.
# com amsi refinamento, podemos olhar os erros individualmente, pq não? olha?
# temos uma visãod e como as retas estão acomodadas no dado
# vamos ver detalhes, vamos ver outliers, nao lenaridade, informaçlão
# de interação, heterocedaticidade, multicolinearidade. alguns problemas
# e como tratar, vamso ver a medicina da reta, e vamos discutir a RLM, vamos
# para o mundo cheio de informação em conjuta, e vamos falar sobre um pouco
# como interpretar e ver
#' ver preditores categoricos e interação
#' e vamos brincar com codigos
#' Bora lá
#'
#' INTERPRETAÇÂO DOS PARAMETROS - MATEMATICO
#' A reta é interpretada como seu jeito geométrico ou estatistica
#' B0 - lugar que reta cruza o eixo Y
#' B1 - é a derivada de Y em relação
#'
#' INTERPRETAÇÃO DOS PARAMETROS - ESTATISTICO
#' B0 - é a distancia percorrida quando carro está parado, pois quando cruza,
#' quer dizer que o X é nulo, vale zero, temos um y = b0 + ZERO, y=B0
#' é o lugar onde a reta econtra o eixo Y, e na interpretação é a distancia
#' percorrida ESPERADA quando o carro está parado. chega a ser engraçado,
#' mas temos o erro aleatorio e o estrutural incorporados
#'
cars

mod<-lm(dist ~ speed, data=cars)
lm(dist ~ speed -1, data=cars) # menos 1 é fixar o zero para o modelo


#' se tá parado de freia, dá ré...fazmos uma previsão onde não temos
#' observação, pois temos valores somente entre carros com velocidade
#' de 5 a 2 até 25, não observamos o x = 0, estamso em EXTRAPOLAÇÃO, a estimativa
#' é feita fora do intervalo que vc observou, observei maior ou meno que 22.1, interpolação
#' extrapolação - mas podese ter previsão errada, mas devido ao ERRO ao acaso,
#' somente isso, no caso vc trunca, se for negativo, fica zero

#' B1 é o efeito médio na distância por varia 1 unidade na velocidade do carro.
#' B1 é o efeito da X no Y.
#  O Efeito esperado E() são sinônimos na estatistica ESPERADO = MEDIO
#' E(Y|X=x) = B0 + B1X - ideal para descrever
#' importante pois estamos um valor provável.
#' PREDIçÂO - usamos a conta para fazer a predição, trocamos a x e fazemos a
#' predição e faço predição pra diferentes valores dentro do intervalo temos
#' interpolação, se temos predição fora do intervalo, faço extrapolação.

#' INTERVALO DE CONFIANÇA PARA OS PARAMETROS
#' em qual região o efeito é plausível de ser
#' 95 - 1.96
#' 90 - 1.64

#' Medidas de erros, na saida
#' EQM - foi dado
#' EQR - erro padrao dos residuo, dá na saida, pareciso com o eqm, com GL (n-2)
#' pois leva em consideração os dois parãmetros a interpretação é mesma com
#' o eqm, quanto MAIOR maior estamos errando, qnto MENOR menos estamos errando.
#' a UNIDADE existe, mas está com os valores elevados ao quadrado, então, tiramos
#' a raiz para chegar ao valor

broom::augment(mod)
broom::augment(mod, interval="confidence") # faz o intervalode confiança para a previsão

# Intervalo de confiança para essa previsão, para a reta vermelha, é o que chamamos de envelop
# em cada ponto temos o para mais e para menos da previsção

# R2 - envolve, ainda os erros quadratico ele compara o quanto vc erra com o quanto
# vc erraria se fizesse uma reta so com a média, com o B0
# y = b0 + b1x VS y = B0
# o fato de colcoar um preditor for bom para reduzir o modelo, vai fazer o erro diminuir
# ou não

# R2 = 1 - SQR/SQT, and R2 for o mais proximo de 1, ele etá bem melhor que somente a média
# o R2 esta associado ao modelo de regressão, mas pode ser usado apra qq um que solte
# uma predição. R2 tem um problema, sempre aumenta conforme vc coloca informação nova
# a cada variavel adicionada no modelo
# O r2 ajustado é igual ao R2 mas vc coloca um fator (N-1)/(N-p), N é o tamamno amostra
# e o p é o numero de parãmetros considerados, assim qnto mais variavel, masi penalidade
# será dada ao R2. ele cobra um peso, um pedágio para colocar uma variável a mais.
# Se compnsar o preço no que vc acerta, o r2 cresce, se coloca uma var que não serve
# para muita coisa e não se paga, ele diminui, então, temos indicação que estamos
# exagerando na explicação do modelo, sem varia´veis supérfulas ou atrapalhando
# a previsão. Fica fácil para comparar dois modelos, que tem R2adj > tem maior comprimisso
# na complexidade do modelo e na previsibilidade. As vezes tem valo negativo inclusive
# pois vc esta comparando com uqe esta acertando com a MEDIA
# Compare modelos com R2 ajustado, e na interpretação fale sobre o r2

# Outliers egráfico de resíduos
# residuo = Yi - hY (observado menos esperado)
# Podemos vazer um grafico residuo vs VALORES PREDITOS
# nesse gráfico, a reta que ajustamos está no zero, que outrora estava inclinada
# pois o resíduo mora nas projeçoões, vemos como a reta ta erradndo conforme os
# preditos. é a núvem de pontos em volta da nossa curva, em volta da reta, ao seu decorrrer
# DIAGNOTICOS - OUTLIER - valores que estamos errando MUITO, FEIO, ele pode aocntecer em luga
# res diferentes, eles aparece em pontos que tinham valor comum, mas as vezes
# ele fica fora na PREDIção e no ERRO, ele é estranho em duas dimensões, chamado de
# ponto de ALAVANCA, pois tem efeito na reta muito maior, o MOMENTO dele,da fisica
# tem mais peso, da mais alavanca, ele tem mais influência nos dados, na alavancagem.
# puxa-se a reta para cima, pois é um ponto influente. PONTOS NO MEIO náo fazem diferença
# mas o ponto na ponta tem mais influencia

# AGORA TEMOS A DISTANCIA DE COOCK
plot(mod) # 4 grafico, Leverage - Alavanca
# Distancia de Coock para cada ponto vc faz a reta sem um ponto e ajusta
# os beta chapeu e vai tirando temos outra reta, com estimativas diferentes.
# para cada ponto temos a distancia de coock para cada ponto eliminado e criado
# a reta que é o erro quadratico médio da reta sem aquele ponto
# comparado EQM da reta toda, de todos os pontos. se o erro é muito diferente,
# a distancia de coock aumenta, discrepante do valor padral e seta destacado no grafico

cooks.distance(mod)

# Se no gráfico o valor ultrapassar os valores de fronteira, faz com que erros
# fiquem diferentes, então, se tem um ponto que tira, muda muito, temos um
# ponto influente, olhar o que está acontecendo com esse ponto, deixo de lado?
# algo que não queria medir mesmo? ou temos que mudar, investigar.
#  INTERPRETACÇAO DE LAVAREGE
# https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html
# so depende de X, é o erro cometido contra essa medida de inportância que
# so depende de X é o quando distante está o ponto da variavel explicartiva
# com a nuvem de pontos. Se olhamos somente o X, estmaos projetnado eles em x,
# somente isso, temos um ponto isolado. Eixo X é uma influencia potencia
# o residuo é zero, sem problemas, o problema qnd alavancagem é grande, distance
# dos outros x, e com um erro grande, pois o erro está zuando a reta.

# DIAGNOSTICO
# 1.1 - UNIVARIADA
# - HISTOGRAMA
# - Boxplot
hist(cars$speed)
boxplot(cars$speed)

# 1.2 - COmparação do desvio padrão, mais ou menos como o boxplot faz
# 1.5 x IQ + Q1 ou Q3
# COOK olha todas ao mesmo tempo, sem olhar marival por variavel, bom para RLM


# Como achar outrliers
# https://blog.curso-r.com/posts/2021-04-15-como-encontrar-outliers/

# TRATAMENTOS
# Nem sempre devemos tirar, pois outlier da inofrmaçõa, carro rápido,
# a curva deixou de ser reta, exemplo a umidade do solo
#

##############################
# REGRESSÂO LINEAR MULTIPLA  #
##############################
#' Associamos a Var resposta coma mais de uma coluna o memso tempo
#' RLMultipla (1 Y) é ideferenteda RLmultivariada (vairos Y)
#'  No R colocamos mais oclunas no céodigo

library(MASS)
modelo_boston <- lm(medv ~ lstat + age, data = Boston)
summary(modelo_boston)

# a variável é contínua - rlm
# se fosse sim ou não - logistica
# na prática, ganhmaos vários betas

mtcars |>
  dplyr::select(mpg, wt, disp) |>
  dplyr::mutate(plano_de_pred =  34.9 - 3.35*wt - 0.017*disp)

# trs ou mais variáveisl hiperplano, não da pra ver, a ideia pe a mesma
nrow(mtcars) - 3 # 29 DF B0, B1 e B2

# Preditores Categóricos
# ele devolve muuuuitos betas
# os preditores não são numeros
# com duas categorias, o saldo médio no cartao de crédito é diferentes em
# homes me mulheres

library(ISLR)
library(tidyverse)
glimpse(Credit)

summary(lm(Balance ~ Gender, data = Credit))

# saída da B0 e genderFemale = ele é o B1 do Female,
# 509 é deiferente de 0 mas os 19 dolares não é diferente de zero.
# noc aso de 19 ele olha assim
# x1 é 1 se Gender é female, e 0 para male

Credit |>
  mutate(
    GenderFemale = ifelse(Gender == "Female",1,0)
  )

# variaveis Dummy

Credit |>
  group_by(Gender) |>
  summarise(
    media_do_saldo = mean(Balance)
  ) |>
  data.frame()
# veja que a média é maior para o Feminino, a parametrização não é a média
# do homes ea media do female, é o incremento
509 + 19
# estamos testado se esse incremento é zero ou não, ou seja, a média do male
# é iaugal a média do femalle, não tem significancia, ou seja, 19 é igual a
# zero, não tem diferena ntre o saldo médio entre M e F, não existe
# diferencia entre genero.
# ele usa como refernecia o gender Male, no Intercepto e sempre se relacionado
# com a CASELA de referência

# veja agora

summary.lm(lm(Balance ~ Gender * Ethnicity, data = Credit))

Credit |>
  group_by(Gender, Ethnicity) |>
  summarise(
    media_do_saldo = mean(Balance)
  ) |>
  data.frame()

# Nesse caso não tem Africam Americam e male, ou seja, a CASELA é o Homem
# Afroamericano, ou Comparator, a figura que vamos comparar o tempo todo



# Se for para 3 ou mais categorias

# B0 + B1 x1i + B2 x2i


# x1 = Asian, 1, caso contrario 0
# x1 = Caucasion 1 caso 0
# zero nos dois efeito somende de B0 e é o African American
# no categorico, estou comparamdo medias, e muito usado,
# pois está comparando a media de certa resposta em função
# regressão em regressos categoria esta comparando
# a média da categorias diferneste.

model.matrix(Balance ~ Ethnicity, data = Credit)[1:6,]

model.matrix(Balance ~ Ethnicity + Gender, data = Credit)[1:6,]

model.matrix(Balance ~ Ethnicity*Gender, data = Credit)[1:6,]

model.matrix(Balance ~ Ethnicity*Gender+Income, data = Credit)[1:6,]


# N categorias geram N-1 Dummies
