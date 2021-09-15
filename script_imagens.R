# Adicionar o beta nacional de cada ano inserido nos mapas valor numéricos da
# derivada

# Keelin curve nacional de 2015 a 2015 e dentro de cada faixa de cada anos
# colocar o beta embaixo

#


# Gráfico Keelin ----------------------------------------------------------

"data/oco2.rds" |>
  readr::read_rds() |>
  dplyr::mutate(
    xco2 = xco2_moles_mole_1*1e06,
    data = lubridate::ymd_hms(time_yyyymmddhhmmss),
    year = lubridate::year(data),
    month = lubridate::month(data),
    day = lubridate::day(data),
    dia = difftime(data,"2014-01-09", units = "days"),
    day_week = lubridate::wday(data),
    month_year = lubridate::make_date(year, month, 1) ) |>
  dplyr::filter(year %in% 2015:2020) |>
  dplyr::group_by(year, dia) |>
  dplyr::summarise(xco2_mean = mean(xco2, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=dia,y=xco2_mean,
                               fill=forcats::as_factor(year))) +
  ggplot2::geom_point(shape=21,color="black") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~year,scales = "free")+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill="")


# Entrada dos dados -------------------------------------------------------
oco2_br_trend <- readr::read_rds("data/oco2_br_trend.rds")

mapa <- geobr::read_state(showProgress = FALSE)

get_contorno <- function(indice, lista){
  obj <- lista |> purrr::pluck(indice) |> as.matrix() |>
    as.data.frame()
  return(obj)
}
contorno <- purrr::map_dfr(1:27, get_contorno, lista=mapa$geom) |>
  dplyr::filter(V1 < -33) |>
  dplyr::filter(!(V1 < -38.5 & V1 > -39 & V2>-20 & V2 < -16))

names(contorno) <- c("X","Y")
plot(contorno)

# Definição das funções ---------------------------------------------------
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

get_pol_in_pol <- function(indice, lista, gradeado){
  poligono <- lista |> purrr::pluck(indice) |> as.matrix()
  flag <- def_pol(gradeado$X, gradeado$Y, poligono)
  return(flag)
}

linear_reg <- function(df, output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(xco2_mean ~ dia, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1)
  }

  # Salvando o valor P
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }

  # Criando gráfico
  if(output=="plot"){
    plot <- df |>
      ggplot2::ggplot(ggplot2::aes(x=dia,y=xco2_mean)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
    return(plot)
  }
  if(output=="hist"){
    hist <- df |>
      ggplot2::ggplot(ggplot2::aes(x=xco2_mean, y=..density..)) +
      ggplot2::geom_histogram(bins=10, color="black", fill="lightgray") +
      ggplot2::geom_density()+
      ggplot2::theme_bw()
    return(hist)
  }

  # Anomalia é o Xco2 do regional menos o Xco2 do pixel, melhor é o contrário.
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(xco2 = mean(xco2_mean), na.mr=TRUE) |>
      dplyr::pull(xco2)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }
}

# Definção das fórmulas para os semivariogramas
form_beta<-beta_line~1
form_anom<-anomaly~1
form_index<-beta_index~1

# Criando o banco de dados
for(ano in 2015:2020){
  oco2_nest <- oco2_br_trend |>
    dplyr::filter(year == ano) |>
    tibble::as_tibble() |>
    dplyr::mutate(quarter = lubridate::quarter(data),
                  quarter_year = lubridate::make_date(year, quarter, 1)) |>   tidyr::pivot_longer(
                    starts_with("flag"),
                    names_to = "region",
                    values_to = "flag",
                  ) |>
    dplyr::filter(flag) |>
    dplyr::mutate(region = stringr::str_remove(region,"flag_")) |>
    dplyr::group_by(region, longitude, latitude, dia) |>
    dplyr::summarise(xco2_mean = mean(xco2, na.rm=TRUE)) |>
    dplyr::mutate(
      regi = region,
      id_time = dia
    ) |>
    dplyr::group_by(region, latitude, longitude) |>
    tidyr::nest()

  oco2_nest <- oco2_nest |>
    dplyr::mutate(
      beta_line = purrr::map(data,linear_reg, output="beta1"),
      p_value = purrr::map(data,linear_reg, output="p_value"),
      partial = purrr::map(data,linear_reg, output="partial"),
      n_obs = purrr::map(data,linear_reg, output="n")
      #plot = purrr::map(data,linear_reg, output="plot"),
      #hist = purrr::map(data,linear_reg, output="hist")
    )

  oco2_aux <- oco2_nest |>
    dplyr::filter(n_obs > 7) |>
    tidyr::unnest(cols = c(beta_line, partial)) |>
    dplyr::ungroup() |>
    dplyr::select(longitude, latitude, beta_line, partial)

  q3_oco2 <- oco2_aux |> dplyr::pull(beta_line) |> quantile(.75)
  oco2_aux <- oco2_aux |>
    dplyr::mutate(
      anomaly =  partial - oco2_aux |>
        dplyr::pull(partial) |>
        mean(),
      Dbeta = beta_line - oco2_aux |>
        dplyr::pull(beta_line) |> mean(na.rm=TRUE)
    )
  q3_anom <- oco2_aux |> dplyr::pull(anomaly) |> quantile(.75)
  oco2_aux <- oco2_aux |>
    dplyr::mutate(
      beta_index =  ifelse(beta_line <=q3_oco2, 0, 1)
    )

  histograma_beta <- oco2_aux |>
    ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
    ggplot2::geom_histogram(bins=30,
                            fill="orange",
                            color="black") +
    ggplot2::labs(x="βpixel",y="Count") +
    ggplot2::geom_vline(xintercept = q3_oco2,
                        color = "red",
                        lty=2) +
    gghighlight::gghighlight(n=2, beta_line > q3_oco2,
                             unhighlighted_params = list(
                               color = "darkgray",
                               fill = "lightgray")) +
    ggplot2::theme_minimal()

  png(paste0("imagens/histograma_beta_",ano,".png"))
  print(histograma_beta)
  dev.off()

  histograma_anomaly <- oco2_aux |>
    ggplot2::ggplot(ggplot2::aes(x=anomaly)) +
    ggplot2::geom_histogram(bins=30,
                            fill="lightblue",
                            color="black") +
    ggplot2::labs(x="Anomaly",y="Count") +
    ggplot2::geom_vline(xintercept = q3_anom,
                        color = "red",
                        lty=2) +
    gghighlight::gghighlight(anomaly > q3_anom,
                             unhighlighted_params = list(
                               color = "darkgray",
                               fill = "lightgray")) +
    ggplot2::theme_minimal()

  png(paste0("imagens/histograma_anomaly_",ano,".png"))
  print(histograma_anomaly)
  dev.off()

  sp::coordinates(oco2_aux)=~ longitude+latitude

  # Semivariograma para Beta
  vari_beta <- gstat::variogram(form_beta, data=oco2_aux)
  m_beta <- gstat::fit.variogram(vari_beta,fit.method = 7,
                                 gstat::vgm(1, "Sph", 8, 1))

  png(paste0("imagens/variograma_beta_",ano,".png"),
      width = 1024, height = 768)
  print(plot(vari_beta, model=m_beta, col=1, pl=F, pch=16))
  dev.off()

  # Semivariograma para Anomalia
  vari_anom<-gstat::variogram(form_anom, data=oco2_aux)
  m_anom <- gstat::fit.variogram(vari_anom,fit.method = 7,
                                 gstat::vgm(.8,"Sph",9,.2))

  png(paste0("imagens/variograma_anomalia_",ano,".png"),
      width = 1024, height = 768)
  print(plot(vari_anom, model=m_anom, col=1, pl=F, pch=16))
  dev.off()

  # Refinando o gradeado
  x<-oco2_aux$longitude
  y<-oco2_aux$latitude
  dis <- .1 #Distância entre pontos
  grid <- expand.grid(X=seq(min(x,contorno$X),max(x,contorno$X),dis),
                      Y=seq(min(y,contorno$Y),max(y,contorno$Y),dis))
  sp::gridded(grid) = ~ X + Y


  flag <- purrr::map_dfc(1:27, get_pol_in_pol, lista=mapa$geom, gradeado = grid)
  flag_br <- apply(flag, 1, sum) != 0

  # Krigando
  ko_beta<-gstat::krige(formula=form_beta, oco2_aux, grid, model=m_beta,
                        block=c(0,0),
                        nsim=0,
                        na.action=na.pass,
                        debug.level=-1,
  )


  krigagem_beta <- tibble::as_tibble(ko_beta) |>
    tibble::add_column(flag_br) |>
    dplyr::filter(flag_br) |>
    ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
    ggplot2::geom_tile(ggplot2::aes(fill = var1.pred)) +
    ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
    ggplot2::coord_equal()+
    ggplot2::labs(fill="βpixel") +
    ggplot2::theme_bw()

  png(paste0("imagens/krigagem_beta_",ano,".png"),
      width = 1024, height = 768)
  print(krigagem_beta)
  dev.off()


  ko_anom<-gstat::krige(formula=form_anom, oco2_aux, grid, model=m_anom,
                        block=c(0,0),
                        nsim=0,
                        na.action=na.pass,
                        debug.level=-1,
  )

  krigagem_anomalia <- tibble::as_tibble(ko_anom) |>
    tibble::add_column(flag_br) |>
    dplyr::filter(flag_br) |>
    ggplot2::ggplot(ggplot2::aes(x=X, y=Y),color="black") +
    ggplot2::geom_tile(ggplot2::aes(fill = var1.pred)) +
    ggplot2::scale_fill_gradient(low = "yellow", high = "blue") +
    ggplot2::coord_equal()+
    ggplot2::labs(fill="Anomaly") +
    ggplot2::theme_bw()

  png(paste0("imagens/krigagem_anomalia_",ano,".png"),
      width = 1024, height = 768)
  print(krigagem_anomalia)
  dev.off()
}



# histogramas_ano ---------------------------------------------------------


beta_ano<-function(ano){
  oco2_nest <- oco2_br_trend |>
    dplyr::filter(year == ano) |>
    tibble::as_tibble() |>
    dplyr::mutate(quarter = lubridate::quarter(data),
                  quarter_year = lubridate::make_date(year, quarter, 1)) |>   tidyr::pivot_longer(
                    starts_with("flag"),
                    names_to = "region",
                    values_to = "flag",
                  ) |>
    dplyr::filter(flag) |>
    dplyr::mutate(region = stringr::str_remove(region,"flag_")) |>
    dplyr::group_by(region, longitude, latitude, dia) |>
    dplyr::summarise(xco2_mean = mean(xco2, na.rm=TRUE)) |>
    dplyr::mutate(
      regi = region,
      id_time = dia
    ) |>
    dplyr::group_by(region, latitude, longitude) |>
    tidyr::nest()

  return(oco2_nest |>
    dplyr::mutate(
      beta_line = purrr::map(data,linear_reg, output="beta1"),
      n_obs = purrr::map(data,linear_reg, output="n")
    ))
}

anos<-2015:2020
saidona <- purrr::map_dfr(anos, beta_ano, .id="anos")

saidona <- saidona |>
  dplyr::mutate(anos = forcats::as_factor(as.numeric(anos)+2014 ))

dplyr::glimpse(saidona)


saidona |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram()

media <- saidona |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_line)) |>
  dplyr::select(beta_line, anos) |>
  dplyr::ungroup() |>
  dplyr::pull(beta_line) |>
  mean()

saidona |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_line)) |>
  dplyr::select(beta_line, anos) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(
  ggplot2::aes(x = beta_line, y = anos, fill=anos)) +
  ggridges::geom_density_ridges(color="transparent", alpha=.6,
                                scale = 3, rel_min_height = 0.01) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(
    x = "βpixel",
    y = "Years"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = 'none',axis.text = ggplot2::element_text(size = 10)
  ) +
  ggplot2::geom_vline(xintercept = media)



# validação cruzada -------------------------------------------------------
validacao_cruzada <- function(variograma, form, dados, sill=.5, range=8, nugget=.1){
  m.f1 <- gstat::fit.variogram(variograma,fit.method = 7,
                               gstat::vgm(sill, "Sph", range, nugget))
  m.f2 <- gstat::fit.variogram(variograma,fit.method = 7,
                               gstat::vgm(sill, "Exp", range, nugget))
  m.f3 <- gstat::fit.variogram(variograma,fit.method = 7,
                               gstat::vgm(sill, "Gau", range, nugget))

  sqr.f1<-round(attr(m.f1, "SSErr"),4); c01<-round(m.f1$psill[[1]],4);
  c0_c11<-round(sum(m.f1$psill),4);a1<-round(m.f1$range[[2]],2)
  sqr.f2<-round(attr(m.f2, "SSErr"),4); c02<-round(m.f2$psill[[1]],4);
  c0_c12<-round(sum(m.f2$psill),4);a2<-round(3*m.f2$range[[2]],2)
  sqr.f3<-round(attr(m.f3, "SSErr"),4); c03<-round(m.f3$psill[[1]],4);
  c0_c13<-round(sum(m.f3$psill),4);a3<-round(m.f3$range[[2]]*3^.5,2)
  print(plot(variograma,model=m.f1, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Esf(C0= ",c01,"; C0+C1= ", c0_c11, "; a= ", a1,"; SQR = ", sqr.f1,")",sep="")))
  print(plot(variograma,model=m.f2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; SQR = ", sqr.f2,")",sep="")))
  print(plot(variograma,model=m.f3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; SQR = ", sqr.f3,")",sep="")))



}
validacao_cruzada(vari_beta, form_beta, oco2_aux)

modelos<-list(m.f1,m.f2,m.f3)
for(j in 1:3){
  est<-0
  vari<-as.character(form)[2]
  for(i in 1:length(dados[vari])){
    valid <- gstat::krige(formula=form, dados[vari][-i,], dados, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  # obs<-as.data.frame(dados[vari])[,1]
  # RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  # mod<-lm(obs~est)
  # b<-round(mod$coefficients[2],3)
  # se<-round(summary(mod)$coefficients[4],3)
  # r2<-round(summary(mod)$r.squared,3)
  # a<-round(mod$coefficients[1],3)
  # plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
  #      main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  # abline(lm(obs~est));
  # abline(0,1,lty=3)
}
# Validação Cruzada


########################################################
#	 ESCOLHA O MODELO MELHOR AJUSTADO                #
########################################################
m.nc <- fit.variogram(v.nc,vgm(patamar,"Sph",alcance,epepita)) # com valores iniciais de C0, C1 e a

