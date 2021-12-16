library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(readr)
source("R/meu-tema.R")
source("R/minhas-funcoes.R")
source("R/polis.R")

# METANO (CH4) entrada e processamento ------------------------------------
ch4 <- readr::read_table("data-raw/GOSAT_CH4_biomas.txt") |>
  dplyr::mutate(
    longitude = round(as.numeric(
      stringr::str_replace_all(string = long,"\\.",""))/ 1e15,5),
    latitude = round(as.numeric(
      stringr::str_replace_all(string = lat,"\\.",""))/ 1e15,5),
    data = lubridate::make_date(year,month,day),
    dia = difftime(data,"2009-01-01", units = "days"),
    season = ifelse(month > 3 & month <= 9, "dry", "wet"),
    ch4 = ifelse(ch4 > 1850, 1775,ch4)
  )

ch4 <- ch4 |>
  dplyr::mutate(
    flag_norte = def_pol(longitude, latitude, pol_norte),
    flag_nordeste = def_pol(longitude, latitude, pol_nordeste),
    flag_sul = def_pol(longitude, latitude, pol_sul),
    flag_sudeste = def_pol(longitude, latitude, pol_sudeste),
    flag_centroeste = def_pol(longitude, latitude, pol_centroeste)
  )
readr::write_rds(ch4,"data/ch4.rds")
ch4 <- readr::read_rds("data/ch4.rds")
dplyr::glimpse(ch4)


ch4 |>
  pull(month) |>
  table()


# GRÁFICO PARA OS PONTOS em 2009-2019
br |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="#2D3E50", color="#FEBF57",
                   size=.15, show.legend = FALSE) +
  ggplot2::geom_point(data=ch4 |>
                        dplyr::filter(year %in% 2017,
                                      season == "wet") ,
                      ggplot2::aes(x=longitude, y=latitude),
                      shape=17,
                      col="red",
                      alpha=0.2)

ch4 |>
  dplyr::mutate(
    year = lubridate::year(data),
    month = lubridate::month(data),
    day = lubridate::day(data),
    dia = dplyr::case_when(
      year == 2009 ~ difftime(data,"2009-01-01", units = "days"),
      year == 2010 ~ difftime(data,"2010-01-01", units = "days"),
      year == 2011 ~ difftime(data,"2011-01-01", units = "days"),
      year == 2012 ~ difftime(data,"2012-01-01", units = "days"),
      year == 2013 ~ difftime(data,"2013-01-01", units = "days"),
      year == 2014 ~ difftime(data,"2014-01-01", units = "days"),
      year == 2015 ~ difftime(data,"2015-01-01", units = "days"),
      year == 2016 ~ difftime(data,"2016-01-01", units = "days"),
      year == 2017 ~ difftime(data,"2017-01-01", units = "days"),
      year == 2018 ~ difftime(data,"2018-01-01", units = "days"),
      year == 2019 ~ difftime(data,"2019-01-01", units = "days"),
      year == 2020 ~ difftime(data,"2020-01-01", units = "days"),
    ),
    day_week = lubridate::wday(data),
    month_year = lubridate::make_date(year, month, 1) ) |>
  dplyr::filter(year %in% 2009:2020) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=dia,y=ch4_mean,
                               fill=forcats::as_factor(year))) +
  ggplot2::geom_point(shape=21,color="black") +
  # ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~year,scales = "free")+
  # ggpubr::stat_regline_equation(ggplot2::aes(
  #    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill="")


ch4 |>
  dplyr::mutate(
    year = lubridate::year(data),
    month = lubridate::month(data),
    day = lubridate::day(data),
    dia = difftime(data,"2015-01-01", units = "days"),
    day_week = lubridate::wday(data),
    month_year = lubridate::make_date(year, month, 1) ) |>
  dplyr::filter(year %in% 2009:2020) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=dia,y=ch4_mean)) +
  ggplot2::geom_point(shape=21,color="black") +
  ggplot2::geom_line(color="red") +
  # ggplot2::geom_smooth(method = "lm") +
  # ggplot2::facet_wrap(~year,scales = "free")+
  # ggpubr::stat_regline_equation(ggplot2::aes(
  #    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill="")

ch4 |>
  dplyr::mutate(
    year = lubridate::year(data),
    month = lubridate::month(data),
    day = lubridate::day(data),
    dia = difftime(data,"2015-01-01", units = "days"),
    day_week = lubridate::wday(data),
    month_year = lubridate::make_date(year, month, 1) ) |>
  dplyr::filter(year %in% 2009:2020) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=dia,y=ch4_mean)) +
  ggplot2::geom_point(shape=21,color="black") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  #ggplot2::facet_wrap(~year,scales = "free")+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill="")


# Linear model por ano CH4 ----------------------------------------------------

dados <- ch4 |>
  dplyr::mutate(
    year = lubridate::year(data),
    month = lubridate::month(data),
    day = lubridate::day(data),
    dia = difftime(data,"2015-01-01", units = "days"),
    day_week = lubridate::wday(data),
    month_year = lubridate::make_date(year, month, 1))

x <- dados |>
  dplyr::filter(year == 2015) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm =TRUE)) |>
  dplyr::pull(dia)

y <- dados |>
  dplyr::filter(year == 2015) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm =TRUE)) |>
  dplyr::pull(ch4_mean)

lm(y~x)


ch4 |>
  dplyr::filter(year==2019) |>
  ggplot(aes(x=longitude, y=latitude, color=season)) +
  geom_point()

ch4_nest <- ch4 |>
  tibble::as_tibble() |>
  dplyr::mutate(quarter = lubridate::quarter(data),
                quarter_year = lubridate::make_date(year, quarter, 1)) |>
  tidyr::pivot_longer(
    starts_with("flag"),
    names_to = "region",
    values_to = "flag",
  ) |>
  dplyr::filter(flag) |>
  dplyr::mutate(region = stringr::str_remove(region,"flag_")) |>
  dplyr::group_by(region, longitude, latitude, year) |>
  dplyr::summarise(ch4_mean = mean(ch4, na.rm=TRUE)) |>
  dplyr::mutate(
    regi = region,
    id_time = year
  ) |>
  dplyr::group_by(region, latitude, longitude) |>
  tidyr::nest()

# ch4_nest <- ch4_nest |>
#   dplyr::mutate(
#     beta_line = purrr::map(data,linear_reg_ch4, output="beta1"),
#     p_value = purrr::map(data,linear_reg_ch4, output="p_value"),
#     n_obs = purrr::map(data,linear_reg_ch4, output="n")
#     #plot = purrr::map(data,linear_reg, output="plot"),
#     #hist = purrr::map(data,linear_reg, output="hist")
#   )

# ch4_nest |>
#   tidyr::unnest(cols = c(beta_line, p_value,n_obs)) |>
#   dplyr::select(region, longitude, latitude, n_obs, p_value, beta_line) |>
#   filter(n_obs >= 3)

ch4 <- ch4 |>
  dplyr::mutate(
    flag_br = def_pol(longitude, latitude, pol_br),
    flag_norte = def_pol(longitude, latitude, pol_norte),
    flag_nordeste = def_pol(longitude, latitude, pol_nordeste),
    flag_sul = def_pol(longitude, latitude, pol_sul),
    flag_sudeste = def_pol(longitude, latitude, pol_sudeste),
    flag_centroeste = def_pol(longitude, latitude, pol_centroeste),
    dia = case_when(
      year == 2015 ~ difftime(data,"2015-01-01", units = "days"),
      year == 2016 ~ difftime(data,"2016-01-01", units = "days"),
      year == 2017 ~ difftime(data,"2017-01-01", units = "days"),
      year == 2018 ~ difftime(data,"2018-01-01", units = "days"),
      year == 2019 ~ difftime(data,"2019-01-01", units = "days"),
      year == 2020 ~ difftime(data,"2020-01-01", units = "days"),
    )
  )

# Krigando para cada ano, e cada estação
# para isso vamos usar o grid já construído do CO2
plot(contorno)


  # Chutes iniciais para construção dos mapas
{
  par_list <- read_csv("data/tab.csv")
  par_list <- par_list[-1]
  par_list$cutoff<-30
  par_list$width<-2.2
  par_list[24,3] <- 5
  par_list[24,4] <- "Sph"
  par_list[24,5] <- 2
  par_list[24,6] <- 5
  par_list[24,7] <- 20

  # chutes para i=1
  par_list$cutoff[1] = 28;  par_list$width[1] = 1.2;   par_list$psill[1] = 200
  par_list$model[1] = "Gau";   par_list$range[1] = 5;   par_list$nugget[1] = 5

  # chutes para i=2
  par_list$cutoff[2] = 22;   par_list$width[2] = .5;  par_list$psill[2] = 200
  par_list$model[2] = "Sph";  par_list$range[2] = 5;  par_list$nugget[2] = 5

  # chutes para i=3
  par_list$cutoff[3] = 28;par_list$width[3] = 2.2

  # chutes para i=4
  par_list$cutoff[4] = 15;par_list$width[4] = 2;par_list$model[4] = "Sph"

  # chutes para i=5
  par_list$cutoff[5] = 25;par_list$width[5] = 2;par_list$model[5] = "Sph"

  # chutes para i=6
  par_list$cutoff[6] = 28;par_list$width[6] = 2

  # chutes para i=7
  par_list$cutoff[7] = 10;par_list$width[7] = .8

  # chutes para i=8
  par_list$cutoff[8] = 25;par_list$width[8] = 1.2

  # chutes para i=9
  par_list$cutoff[9] = 20;par_list$width[9] = 1.2;par_list$model[9] = "Sph"

  # chutes para i=10
  par_list$cutoff[10] = 20;par_list$width[10] = .2

  # chutes para i=11
  par_list$cutoff[11] = 24;par_list$width[11] = 1.2

  # chutes para i=12
  par_list$cutoff[12] = 20;par_list$width[12] = 2.2;par_list$model[12] = "Sph"

  # chutes para i=14
  par_list$cutoff[14] = 20

  # chutes para i=16
  par_list$cutoff[16] = 20

  # chutes para i=17
  par_list$model[17] = "Sph"

  # chutes para i=18
  par_list$model[18] = "Sph"

  # chutes para i=19
  par_list$model[19] = "Sph";par_list$cutoff[19] = 22

  # chutes para i=20
  par_list$model[20] = "Gau";par_list$cutoff[19] = 22

  # chutes para i=21
  par_list$model[21] = "Gau";par_list$cutoff[21] = 28

  # chutes para i=22
  par_list$model[22] = "Sph";par_list$cutoff[22] = 26

}

tab_par_ajust <- data.frame(
  Ano="",Estacao="",Modelo="",C0=0,Patamar=0,Alcance=0,SQR=0
)
# Definição das fórmulas para os semivariogramas
form_ch4<-ch4~1
for(i in 1:nrow(par_list)){
  legenda <- paste0(par_list$ano[i],"-",par_list$estacao[i])
  ch4_aux <- ch4 |>
    dplyr::filter(year == par_list$ano[i],
                  season == par_list$estacao[i])
  q1 <- quantile(ch4_aux$ch4,.25)
  q3 <- quantile(ch4_aux$ch4,.75)

  ch4_aux <- ch4_aux |>
    dplyr::filter(ch4> q1 & ch4 < q3)

  ch4_aux |>
    ggplot(aes(x=longitude, y=latitude, color=ch4)) +
    geom_point()

  ch4_aux <- ch4_aux |>
    group_by(longitude,latitude) |>
    summarise(ch4 = mean(ch4, na.rm=TRUE))

  # Definindo as coordenada para o objeto sp
  sp::coordinates(ch4_aux)=~longitude+latitude
  # Semivariograma para Beta
  vari_ch4 <- gstat::variogram(form_ch4, data=ch4_aux,
                               cutoff = par_list$cutoff[i],
                               width = par_list$width[i])

  m_ch4 <- gstat::fit.variogram(vari_ch4,fit.method = 7,
                                gstat::vgm(par_list$psill[i],
                                           model = par_list$model[i],
                                           par_list$range[i],
                                           par_list$nugget[i]))
  if(i==1){m_ch4$psill <- c(5,40); m_ch4$range[2] <- 10}
  if(i==3){m_ch4$model[2] <- "Gau"; m_ch4$psill <- c(5,20);m_ch4$range[2] <- 10}
  if(i==5){ m_ch4$psill <- c(10,20);m_ch4$range[2] <- 10;m_ch4$model[2] <- "Gau"}
  if(i==21){ m_ch4$psill <- c(9,6);m_ch4$range[2] <- 12}
  if(i==23){m_ch4$model[2] <- "Sph"; m_ch4$psill <- c(8,7);m_ch4$range[2] <- 16}

  print(plot(vari_ch4, model=m_ch4, col=1, pl=F, pch=16,
             main = legenda))

  png(paste0("imagens/variograma_ch4_",legenda,".png"),
      width = 1024, height = 768)
  print(plot(vari_ch4, model=m_ch4, col=1, pl=F, pch=16,
             main = legenda))
  dev.off()
  sqr<-attr(m_ch4, "SSErr")

   if(i==1) {
     tab_par_ajust$Ano <- par_list$ano[i]
     tab_par_ajust$Estacao <- par_list$estacao[i]
     tab_par_ajust$Modelo[i] <- as.character(m_ch4$model[2])
     tab_par_ajust$C0[i] <- m_ch4$psill[1]
     tab_par_ajust$Patamar[i] <- sum(m_ch4$psill)
     tab_par_ajust$Alcance[i] <- m_ch4$range[2]
     tab_par_ajust$SQR <- sqr
   }else{
     tab_par_aux <- data.frame(
       Ano=par_list$ano[i],
       Estacao=par_list$estacao[i],
       Modelo=m_ch4$model[2],
       C0=m_ch4$psill[1],
       Patamar=sum(m_ch4$psill),
       Alcance=m_ch4$range[2],
       SQR = sqr
     )
    tab_par_ajust <- rbind(tab_par_ajust,tab_par_aux)
  }
  # #Refinando o gradeado
  x<-ch4_aux$longitude
  y<-ch4_aux$latitude
  dis <- 0.25 #Distância entre pontos
  grid <- expand.grid(X=seq(min(x,contorno$X),max(x,contorno$X),dis),
                       Y=seq(min(y,contorno$Y),max(y,contorno$Y),dis))
  sp::gridded(grid) = ~ X + Y
  flag <- purrr::map_dfc(1:27, get_pol_in_pol, lista=mapa$geom,
                         gradeado = grid)
  flag_br <- apply(flag, 1, sum) != 0

  # Krigando metano
  ko_ch4<-gstat::krige(formula=form_ch4, ch4_aux, grid,
                       model=m_ch4,
                       block=c(0,0),
                       nsim=0,
                       na.action=na.pass,
                       debug.level=-1,
  )


  #######################################################
  writeGDAL(ko_ch4, paste0('tifs/krigagem_methane_',par_list$ano[i],'_',
                           par_list$estacao[i],'.tif'),drivername = "GTiff")
  ######################################################

  krigagem_ch4 <- tibble::as_tibble(ko_ch4) |>
    tibble::add_column(flag_br) |>
    dplyr::filter(flag_br) |>
    ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
    ggplot2::geom_tile(ggplot2::aes(fill = var1.pred)) +
    ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
    ggplot2::coord_equal()+
    ggplot2::labs(title=legenda,
                  fill="ch4") +
    ggplot2::theme_bw()

  png(paste0("imagens/krigagem_ch4_",legenda,".png"),
      width = 1024, height = 768)
  print(krigagem_ch4)
  dev.off()

  # vamos pegar os valores krigados de krigagem
  ko_ch4_aux <- tibble::as_tibble(ko_ch4) |>
    tibble::add_column(flag_br) |>
    dplyr::filter(flag_br)

  ko_aux <- ko_ch4_aux |>
    dplyr::select(X,Y)
  ko_aux$ch4 <- ko_ch4_aux$var1.pred
  ko_aux$Ano <- par_list$ano[i]
  ko_aux$Estacao <- par_list$estacao[i]


  ko_aux<-ko_aux |>
    mutate(
      flag_norte = def_pol(X, Y, pol_norte),
      flag_nordeste = def_pol(X, Y, pol_nordeste),
      flag_centroeste = def_pol(X, Y, pol_centroeste),
      flag_sudeste = def_pol(X, Y, pol_sudeste),
      flag_sul = def_pol(X, Y, pol_sul)
    )

  if(i == 1){
    ko_final <- ko_aux
  }else{
    ko_final <- rbind(ko_final,ko_aux)
  }

}

tab_par_ajust <- tab_par_ajust %>%
  mutate(
    Alcance = ifelse(C0 == Patamar, 0, ifelse(Modelo == "Sph",Alcance,
                     ifelse(Modelo=="Exp",Alcance*3,Alcance*3^.5))),
    Modelo = ifelse(C0 == Patamar, "EPP",Modelo),
    GDE = ifelse(Modelo=="EPP",0,100*C0/(C0+Patamar))
  )

write.table(tab_par_ajust,"Data/ajustes.txt",
            row.names = FALSE,quote = FALSE,sep="\t")

write.table(ko_final,"Data/ko_ch4.txt",
            row.names = FALSE,quote = FALSE,sep="\t")











