# Carregando os pacotes do geobr
mapa <- geobr::read_state(showProgress = FALSE)
states <- geobr::read_state(showProgress = FALSE)
regiao <- geobr::read_region(showProgress = FALSE)
br <- geobr::read_country(showProgress = FALSE)

# definindo os polígonos para o mapeamento
pol_norte <- regiao$geom |> purrr::pluck(1) |> as.matrix()
pol_nordeste <- regiao$geom |> purrr::pluck(2) |> as.matrix()
pol_sudeste <- regiao$geom |> purrr::pluck(3) |> as.matrix()
pol_sul <- regiao$geom |> purrr::pluck(4) |> as.matrix()
pol_centroeste<- regiao$geom |> purrr::pluck(5) |> as.matrix()

norte <- regiao$geom[1]
nordeste <- regiao$geom[2]
sudeste <- regiao$geom[3]
sul <- regiao$geom[4]
centro_oeste <- regiao$geom[5]

# função para extrais o contorno do territorio nacional
get_contorno <- function(indice, lista){
  obj <- lista |> purrr::pluck(indice) |> as.matrix() |>
    as.data.frame()
  return(obj)
}

# função que busca a coordenada mais próxima de um ponto krigado no mapa do
# fogo
get_coord <- function(x, y, type= "Long"){
  df <- df_raster_aux |>
    mutate(distancia = sqrt((x-Long)^2 + (y-Lat)^2)) |>
    arrange(distancia) |>
    slice(1)
  if(type == "Long") return(as.numeric(df[,1]))
  if(type == "Lat") return(as.numeric(df[,2]))
  if(type == "Area") return(as.numeric(df[,3]))
  if(type == "distancia") return(as.numeric(df[,4]))
}



# get_coord_2 <- function(x, y, type= "Long"){
#   df <- ko_beta_aux |>
#     mutate(distancia = sqrt((x-X)^2 + (y-Y)^2)) |>
#     arrange(distancia) |>
#     slice(1)
#   if(type == "Long") return(as.numeric(df[,1]))
#   if(type == "Lat") return(as.numeric(df[,2]))
#   if(type == "Area") return(as.numeric(df[,3]))
#   if(type == "distancia") return(as.numeric(df[,4]))
# }

# testa se um ponto pertence a um polígono
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

# generalização da função para todos os estados do país
get_pol_in_pol <- function(indice, lista, gradeado){
  poligono <- lista |> purrr::pluck(indice) |> as.matrix()
  flag <- def_pol(gradeado$X, gradeado$Y, poligono)
  return(flag)
}

# definição da equação de regressão linear e seus diagnósticos
linear_reg <- function(df, output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(xco2_mean ~ dia, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1*365) # <-------------------- BETA LINE POR 365
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

# definição da equação de regressão linear e seus diagnósticos
linear_reg_ch4 <- function(df, output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(ch4_mean ~ year, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1*365) # <-------------------- BETA LINE POR 365
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

  if(output == "n"){
    return(nrow(df))
  }
}

get_tif <- function(file){
  df<-file |>
    raster() |>
    rasterToPoints() |>
    as.data.frame()
  names(df) <- c("x","y","Umidade")
  n_split <- lengths(stringr::str_split(file,"/"))
  nomes <- stringr::str_split(file,"/",simplify = TRUE)[,n_split] |>
    stringr::str_remove(".tif")
  df$ano <- nomes
  return(df)
}



# # Função para pegar os chutes iniciais do variograma
# get_psill <- function(vari){
#   Gamma <- vari$gamma
#   return(-min(Gamma)+median(Gamma))
# }
#
# get_nugget <- function(vari){
#   Gamma <- vari$gamma
#   return(min(Gamma))
# }
#
#
# get_range <- function(vari){
#   Gamma <- vari$gamma
#   Dist <- vari$dist
#   Dist2 <- Dist*Dist
#
#   reg <- lm(Gamma ~ Dist + Dist2)
#   reg_anova <- summary(reg)
#   c <- reg_anova$coefficients[1]
#   b <- reg_anova$coefficients[2]
#   a <- reg_anova$coefficients[3]
#
#   Xv <- - b/2/a
#   # plot(Gamma~Dist)
#   # curve(a*x^2 + b*x +c,add=TRUE)
#
#   if(Xv < 30 & Xv > 0 ){
#     return(Xv)
#   }else{
#     return(median(Dist))
#   }
# }

