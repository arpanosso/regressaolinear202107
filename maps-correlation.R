library(tidyverse)
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
todos <- ko_xco2
for(i in 2015:2020){
  aux_xco2 <- ko_xco2 |>
    filter(ano == i)
  aux_ch4_wet <-   ko_ch4 |>
    filter(Ano == i, Estacao == "wet")
  aux_ch4_dry <-   ko_ch4 |>
    filter(Ano == i, Estacao == "dry")
  dff_anual_aux <- dff_anual |>
    filter(ano == i)
  vct_umidade <- vector(); dist_ch4_dry <- vector()
  vct_ch4_wet <- vector(); dist_ch4_wet <- vector()
  vct_ch4_dry <- vector(); dist_umid <- vector()
  for( j in 1:nrow(aux_xco2)){ #
    d <-  sqrt((aux_xco2$X[j]-dff_anual_aux$x)^2 +
                 (aux_xco2$Y[j]-dff_anual_aux$y)^2)
    indice_menor <- order(d)[1]
    vct_umidade[j] <- dff_anual_aux$Umidade[indice_menor]
    dist_umid[j] <- d[order(d)[1]]

    d <-  sqrt((aux_xco2$X[j]-aux_ch4_wet$X)^2 +
                 (aux_xco2$Y[j]-aux_ch4_wet$Y)^2)
    indice_menor <- order(d)[1]
    vct_ch4_wet[j] <- aux_ch4_wet$ch4[indice_menor]
    dist_ch4_wet[j] <- d[order(d)[1]]


    d <-  sqrt((aux_xco2$X[j]-aux_ch4_dry$X)^2 +
                 (aux_xco2$Y[j]-aux_ch4_dry$Y)^2)
    indice_menor <- order(d)[1]
    vct_ch4_dry[j] <- aux_ch4_dry$ch4[indice_menor]
    dist_ch4_dry[j] <- d[order(d)[1]]
    print(paste0(i,": ",j,"(",nrow(aux_xco2),")"))
  }
  aux_xco2$dist_umid <- dist_umid
  aux_xco2$umidade <- vct_umidade
  aux_xco2$dist_ch4_wet <- dist_ch4_wet
  aux_xco2$ch4_wet <- vct_ch4_wet
  aux_xco2$dist_ch4_dry <- dist_ch4_dry
  aux_xco2$ch4_dry <- vct_ch4_dry
  if(i == 2015){
    todos <- aux_xco2
  }else{
    todos <- rbind(todos, aux_xco2)
  }
}
glimpse(todos)
todos <- todos |>
  mutate(ch4 = (ch4_dry+ch4_wet)/2)
readr::write_rds(todos,"data/data_set.rds")
data_set <- readr::read_rds("data/data_set.rds")

# todos |>
#   sample_n(9000) |>
#   filter(ano == 2015) |>
#   ggplot(aes(x=X, y=Y)) +
#   geom_point()

todos <- todos |>
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

