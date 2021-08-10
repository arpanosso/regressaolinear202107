# regressão é sempre a soma de alguma coisa

# se ficarmos a vontade com o basico, para irmos a outro contexto
# será menos dolorido, mais natural
# fundamentação é essencial

# Naregressão temops duas culturas
# 01-Predição
# 02-Inferência

# a distin~ção. as duas usam a mesma ferramenta, mas o meu interess
# pode ser em aspectos diferentes, ela responde coisas distintas
# INFERENCIA, interessé e inteder aforma da relação entre duas
# quantidades, como elas se relacionam
# interesse não é prever a qualidade de vida em dose, e sim
# como a dose se relaciona na melhora do paciente.
# estamos interessados se a dose, qunt mais melhor? ou um remedio
# que sobe pouco e diminuiou muito, a forma com que as duas
# colunas do BD se associam é importante.
# será que o remédio impacta na cura?
# QUAIS A MAIS RELACIONADAS COM A RESPOTA???

# PREDIçÂO o objetivo é saber o que vai aconterce, eu quero fazer
# boas estimativas, eu quero advinhar em o que vai acontecer.
# preciso descobrir o que tenho pegar as informações que tenho
# e fazer estimativas, T do dia de amanhã, preço da Bolsa,
# a predição abre mão da interpretabilidade em troca de poder
# preditivo, sem mesmo saber ocmo funciona, para adivinhar o que vai
# aocntecer, vc terá que pagar o preço, mas explicar tudo,
#´c ocmplicado, vc não consegue explikcar tudo.
# ML é basicamente investimento em proder preditivo. haja vistoque a
# a modelagem retorna um previsto do futuro, X-men, e abrimos
# mão da interpretação

# Precisamos criar intuição.
# LER SBICK MENDONCA na 1.2 Predição versus inferencia

# Regressão Linear simples
# definimos uma reta
# y = b0 + b1.X
# dist_frenagem = b0 + b1.speed

# vamos associar essas duas quantidades com uma reta
# podemos usar um condicional. tipo broquem line
# então vamos associar quantidades utilizando retas
# a decisão de traçar uma reta é uma decisão minha, e isso me
# faz recorrer à regressão linear
# mas quel é o bom chute, não sei qual o melhor dos mundos

# Regressão vai nos ajudar a dar a melhor RETA nesses dados.
# dispensa o trabalho braçal de encontrar qual q melhor reta que
# passa ao meio dos pontos.
# b0 e b1 é quqe define a relação entre as duas quantidades

melhor_reta <- lm(dist ~ speed, data =cars)
# lm - linear model, modelo linear
# e vamos ajustar a realção usando os dados que tenho em mãos
summary(melhor_reta)

#paconte broom, glance ajuta tira informaçõ do modelo
# augment é muito bom pra extrair informaçõ da reta

# QUal a melhor reta? exite um critério por tras dessa melhor reta
# a ideia é que nos queremos uma reta que erre menos
# podemos errar muito, ou errar pouco
# qual o menor erro?
# a medida de erro que ela tenta minimizar é o erro quadrático
# médio

# EQM = 1/N SUM (Yi - hYi)^2 - erro ou o residuo, são sinônimos
# exemplo análise de resíduo, como estão os erros
# elevamos ao quadrado, pois não quero saber se estmaos errando para mais
# ou para menos, e dividimos por N para saber a média que estamos errando
# se o objetivo é a previsão.
# 1- No cenário de uma variável para associar, blx, mas as vezes serão mais
# de uma variavel para associar, e não temos como olha conjuntamente
# todas com as demais, ai cai por terra olhar no olhometrô.
# 2-Outra cenário, é não saber se esse efeito é devido ao acaso ou
# deviso a variável. A estatistica tras critérios objetivos
# uqe não rpecisa da subjitividade para tomar a decisão se existe ou
# não relação.

# so pode usar reta, nos diagnosticos der quie esta tudo ceetro
# media zero, erro indepoendente e normal.
# o que garante que todo mundo fizer e tiver a chance baixa de ter beta
# grande são as hipóteses. se o erro não tiver normal, as vezes quemos q trocar
# a funçaõq ue não seja uma reta. No contezxto de duas variaveis é a correlação
# entre elas

# todas as retas estão erradas, as forças são inacessiveis
# e a unica forma de entender o que aocntece é pelos dados
# então estamos vendo uma aproximação do real e simplificamos o modelo com
# um reta. TODAS RETAS SÃO ERRADOS MAS ALGUNS SÃO UTEIS...BOX

# ecolhemos uma reta, qual a melhor? mas poderia ser polinomio, poderi
# se outros, a melhor em relação a que?? o cri´terio é os mínimos
# quadrados

# REGRESSÂO LINEAR É A MELHOR RETA SEGUINDO O ERRO QUADRADICO MEDIO
# regrssão chega no par de numeros com o menor erro
# EQM é a função de custo da regressão LINEAR, fun~çaõ DE UTILIZA, FUNçÃO DEW
# CUSTO retorna o quanto estamos pagando pelos nosso erro.
# quanto menor melhor sempre, custo é ruim

# O fato de colocar uma reta é uma hipótese nossa, hipotese é que a
# ditancia é uma relação linar com a velocidade
# eqm ta embutido dentro e o critério fica indexado por b0 e b1

# com ajuda do cáluclo, podemos acahar oe melhores valores e b0 e b1 para e
# esse cenário, deriva e iguala a zero as contas minimizam o EQM
# estimadores de míminos quadrado emq
# processo de otimização

# depois de estima b0 e b1,
# chapeu pe estimativa

# Mudamos hipótese e função de custo
# rede neural, muda hipotese e não custo
# regressão linear é particula da RNA

# teste de hipóteses e valor p

# a taxa é uma reta, então b1 é diferente de zero
# H0: b1=0 e H1: b1 != zero
# se o zero for uma valor provavelm, vou assumir ele como zero
# e ponto final

# teoria assintotica
# distribuição t = (hB1-b1)/sdb1










































