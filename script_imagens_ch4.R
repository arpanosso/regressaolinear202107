library(tidyverse)
library(rgdal)
library(sp)
library(raster)
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

par_list <- tibble::as_tibble(
  expand.grid(ano = 2009:2020,
              estacao = c("dry","wet"),psill = 200,
              model = "Gau",
              range = 15,
              nugget = 50))

# Definção das fórmulas para os semivariogramas
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
                                 cutoff = 30,
                                 width = 2.2)

    m_ch4 <- gstat::fit.variogram(vari_ch4,fit.method = 7,
                                   gstat::vgm(200,
                                              model = "Gau",
                                              15,
                                              50))
    if(m_ch4[[3]][[2]] < 0) m_ch4[[3]][[2]] <- 15
    if(m_ch4[[3]][[2]] > 30) m_ch4[[3]][[2]] <- 15
    if(m_ch4[[2]][[2]] > 600) m_ch4[[2]][[2]] <- 600
    print(plot(vari_ch4, model=m_ch4, col=1, pl=F, pch=16,
               main = legenda))

    png(paste0("imagens/variograma_ch4_",legenda,".png"),
        width = 1024, height = 768)
    print(plot(vari_ch4, model=m_ch4, col=1, pl=F, pch=16,
               main = legenda))
    dev.off()


    #Refinando o gradeado
    # x<-ch4_aux$longitude
    # y<-ch4_aux$latitude
    # dis <- 0.5 #Distância entre pontos
    # grid <- expand.grid(X=seq(min(x,contorno$X),max(x,contorno$X),dis),
    #                     Y=seq(min(y,contorno$Y),max(y,contorno$Y),dis))
    # sp::gridded(grid) = ~ X + Y
    # flag <- purrr::map_dfc(1:27, get_pol_in_pol, lista=mapa$geom,
    #                        gradeado = grid)
    # flag_br <- apply(flag, 1, sum) != 0

    # Krigando metano
    # ko_ch4<-gstat::krige(formula=form_ch4, ch4_aux, grid,
    #                      model=m_ch4,
    #                       block=c(0,0),
    #                       nsim=0,
    #                       na.action=na.pass,
    #                       debug.level=-1,
    # )
    #
    # krigagem_ch4 <- tibble::as_tibble(ko_ch4) |>
    #   tibble::add_column(flag_br) |>
    #   dplyr::filter(flag_br) |>
    #   ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
    #   ggplot2::geom_tile(ggplot2::aes(fill = var1.pred)) +
    #   ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
    #   ggplot2::coord_equal()+
    #   ggplot2::labs(title=legenda,
    #                 fill="ch4") +
    #   ggplot2::theme_bw()
    #
    # png(paste0("imagens/krigagem_ch4_",legenda,".png"),
    #     width = 1024, height = 768)
    # print(krigagem_ch4)
    # dev.off()
}


#
# # vamos pegar os valores krigados de krigagem
# ko_beta_aux <- tibble::as_tibble(ko_beta) |>
#   tibble::add_column(flag_br) |>
#   dplyr::filter(flag_br)
#
# ko_anom_aux <- tibble::as_tibble(ko_anom) |>
#   tibble::add_column(flag_br) |>
#   dplyr::filter(flag_br)
#
# ko_fogo_aux <- tibble::as_tibble(ko_fogo) |>
#   tibble::add_column(flag_br) |>
#   dplyr::filter(flag_br)
#
# ko_aux <- ko_beta_aux |>
#   dplyr::select(X,Y)
# ko_aux$Beta <- ko_beta_aux$var1.pred
# ko_aux$Anom <- ko_anom_aux$var1.pred
# ko_aux$Fogo <- ko_fogo_aux$var1.pred
# ko_aux$ano <- ano
#
#
# ko_aux<-ko_aux |>
#   mutate(
#     flag_norte = def_pol(X, Y, pol_norte),
#     flag_nordeste = def_pol(X, Y, pol_nordeste),
#     flag_centroeste = def_pol(X, Y, pol_centroeste),
#     flag_sudeste = def_pol(X, Y, pol_sudeste),
#     flag_sul = def_pol(X, Y, pol_sul)
#   )
#
# if(ano == 2009){
#   ko_final <- ko_aux
# }else{
#   ko_final <- rbind(ko_final,ko_aux)
# }













