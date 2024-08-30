library(tidyverse)
source("R/meu-tema.R")
dff_anual <- read.table("data/umidade_anual.txt", h=TRUE)
ko_xco2 <- readr::read_rds("data-raw/ko_final.rds")
ko_ch4 <- read.table("Data\\ko_ch4.txt", h=TRUE)
glimpse(dff_anual)
glimpse(ko_xco2)
glimpse(ko_ch4)

dff_anual |>
  filter(ano == 2015) |>
  ggplot(aes(x,y)) +
  geom_point()

ko_xco2 |>
  filter(ano == 2015) |>
  ggplot(aes(X,Y)) +
  geom_point()

ko_ch4 |>
  filter(Ano == 2015, Estacao == "wet") |>
  ggplot(aes(X,Y)) +
  geom_point()

# retirando os pontos mais próximos de ch4
# todos <- ko_xco2
# for(i in 2015:2020){
#   aux_xco2 <- ko_xco2 |>
#     filter(ano == i)
#   aux_ch4_wet <-   ko_ch4 |>
#     filter(Ano == i, Estacao == "wet")
#   aux_ch4_dry <-   ko_ch4 |>
#     filter(Ano == i, Estacao == "dry")
#   dff_anual_aux <- dff_anual |>
#     filter(ano == i)
#   vct_umidade <- vector(); dist_ch4_dry <- vector()
#   vct_ch4_wet <- vector(); dist_ch4_wet <- vector()
#   vct_ch4_dry <- vector(); dist_umid <- vector()
#   for( j in 1:nrow(aux_xco2)){ #
#     d <-  sqrt((aux_xco2$X[j]-dff_anual_aux$x)^2 +
#                  (aux_xco2$Y[j]-dff_anual_aux$y)^2)
#     indice_menor <- order(d)[1]
#     vct_umidade[j] <- dff_anual_aux$Umidade[indice_menor]
#     dist_umid[j] <- d[order(d)[1]]
#
#     d <-  sqrt((aux_xco2$X[j]-aux_ch4_wet$X)^2 +
#                  (aux_xco2$Y[j]-aux_ch4_wet$Y)^2)
#     indice_menor <- order(d)[1]
#     vct_ch4_wet[j] <- aux_ch4_wet$ch4[indice_menor]
#     dist_ch4_wet[j] <- d[order(d)[1]]
#
#
#     d <-  sqrt((aux_xco2$X[j]-aux_ch4_dry$X)^2 +
#                  (aux_xco2$Y[j]-aux_ch4_dry$Y)^2)
#     indice_menor <- order(d)[1]
#     vct_ch4_dry[j] <- aux_ch4_dry$ch4[indice_menor]
#     dist_ch4_dry[j] <- d[order(d)[1]]
#     # print(paste0(i,": ",j,"(",nrow(aux_xco2),")"))
#   }
#   aux_xco2$dist_umid <- dist_umid
#   aux_xco2$umidade <- vct_umidade
#   aux_xco2$dist_ch4_wet <- dist_ch4_wet
#   aux_xco2$ch4_wet <- vct_ch4_wet
#   aux_xco2$dist_ch4_dry <- dist_ch4_dry
#   aux_xco2$ch4_dry <- vct_ch4_dry
#   if(i == 2015){
#     todos <- aux_xco2
#   }else{
#     todos <- rbind(todos, aux_xco2)
#   }
# }
# glimpse(todos)
# todos <- todos |>
#   mutate(ch4 = (ch4_dry+ch4_wet)/2)
# readr::write_rds(todos,"data/data_set.rds")
data_set <- readr::read_rds("data/data_set.rds")

data_set |>
  sample_n(9000) |>
  filter(ano == 2015) |>
  ggplot(aes(x=X, y=Y)) +
  geom_point()

todos <- data_set |>
 mutate(ch4 = (ch4_dry+ch4_wet)/2)

names(todos)

tibble::as_tibble(todos) |>
  filter(ano == 2015, flag_norte) |>
  ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
  ggplot2::geom_tile(ggplot2::aes(fill =ch4)) +
  ggplot2::scale_fill_gradient(low = "yellow", high = "blue")+
  ggplot2::coord_equal()+
  ggplot2::labs(fill="βpixel") +
  ggplot2::theme_bw()


todos |>
  pivot_longer(cols = c(flag_norte,
                        flag_nordeste,
                        flag_centroeste,
                        flag_sudeste,
                        flag_sul),names_to = "reg") |>
  filter(value) |>
  filter(ano == 2015) |>
  ggplot(aes(x=Beta, y=umidade)) +
  geom_point() +
  facet_wrap(~reg)


todos %>%
  filter(ano == 2020) %>%
  ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
  ggplot2::geom_tile(ggplot2::aes(fill = Beta)) +
  ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
  ggplot2::coord_equal()+
  ggplot2::labs(fill="CH4") +
  ggspatial::annotation_scale(
    location="bl",
    plot_unit="km",
    height = ggplot2::unit(0.2,"cm"))

todos %>%
  filter(flag_nordeste) %>%
  group_by(ano) %>%
  summarise(
    ch4 = mean(ch4)
  )  %>%
  ggplot(aes(x=ano, y=ch4)) +
  geom_line()

aninhado <- todos %>%
       dplyr::select(X,Y,ano,Beta,ch4) %>%
       nest(data=ano:ch4)

## Criar o Betach4
beta_ch4 <- function(df) {
  x <- df %>% pull(ano)
  y <- df %>% pull(ch4)
  mod <- lm(y~x)
  mod$coefficients[[2]]
  # obj <- summary.lm(mod)
  # obj$coefficients[2,2]
}
beta_ch4(aninhado$data[[1]])

beta_xco2 <- function(df) {
  x <- df %>% pull(ano)
  y <- df %>% pull(Beta)
  mod <- lm(y~x)
  mod$coefficients[[2]]
  # obj <- summary.lm(mod)
  # obj$coefficients[2,2]
}
beta_xco2(aninhado$data[[1]])


aninhado <- aninhado %>%
  mutate(
    betach4 = purrr::map(data,beta_ch4),
    betaxco2 = purrr::map(data,beta_xco2)
  ) %>%
  select(X, Y, betaxco2, betach4) %>%
  ungroup()

aninhado %>%
  ungroup() %>%
  unnest(betach4) %>%
  ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
  ggplot2::geom_tile(ggplot2::aes(fill = betach4)) +
  #ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_equal() +
  ggplot2::labs(fill=expression(paste("Beta_x",CH[4]))) +
  ggspatial::annotation_scale(
    location="bl",
    plot_unit="km",
    height = ggplot2::unit(0.2,"cm")) +
  tema_mapa()

aninhado %>%
  ungroup() %>%
  unnest(betaxco2) %>%
  ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
  ggplot2::geom_tile(ggplot2::aes(fill = betaxco2)) +
  ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
  #ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_equal() +
  ggplot2::labs(fill=expression(paste("Beta_x",CO[2]))) +
  ggspatial::annotation_scale(
    location="bl",
    plot_unit="km",
    height = ggplot2::unit(0.2,"cm")) +
  tema_mapa()

aninhado <- aninhado %>% unnest(cols = c(betaxco2, betach4)) %>%
  mutate(
    flag_norte = def_pol(X, Y, pol_norte),
    flag_nordeste = def_pol(X, Y, pol_nordeste),
    flag_sul = def_pol(X, Y, pol_sul),
    flag_sudeste = def_pol(X, Y, pol_sudeste),
    flag_centroeste = def_pol(X, Y, pol_centroeste)
  )
aninhado %>%
  pivot_longer(cols=c(flag_norte,flag_sul,
                      flag_centroeste,
                      flag_nordeste, flag_sudeste),
               names_to = "regiao",
               values_to = "flag") %>%
  filter(flag) %>%
  mutate(
    regiao = str_remove(regiao,"flag_")
  ) %>%
  ggplot(aes(y=betaxco2,x=betach4,
             color=regiao)) +
  geom_point() +
  facet_wrap(~regiao)

