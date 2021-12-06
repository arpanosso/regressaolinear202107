# METANO (CH4) entrada e processamento ------------------------------------




# gráfico CH4


# Plot de todos os pontos.

br |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="#2D3E50", color="#FEBF57",
                   size=.15, show.legend = FALSE) +
  ggplot2::geom_point(data=ch4 |>
                        dplyr::filter(year == 2015) ,
                      ggplot2::aes(x=longitude, y=latitude),
                      shape=17,
                      col="red",
                      alpha=0.2)


# criando o arquivo nest para ch4



# Criando o banco de dados
ch4_nest_total <- data.frame()
for(ano in 2015:2020){
  # Criando banco de dados aninhado por ano
  ch4_nest <- ch4 |>
    dplyr::filter(year == ano) |>
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
    dplyr::group_by(region, longitude, latitude, ) |>
    dplyr::summarise(ch4_mean = mean(ch4, na.rm=TRUE)) |>
    dplyr::mutate(
      regi = region,
      id_time = dia
    ) |>
    dplyr::group_by(region, latitude, longitude) |>
    tidyr::nest()

  # Adicionando as colunas da regressão linear
  ch4_nest <- ch4_nest |>
    dplyr::mutate(
      beta_line = purrr::map(data,linear_reg_ch4, output="beta1"),
      p_value = purrr::map(data,linear_reg_ch4, output="p_value"),
      n_obs = purrr::map(data,linear_reg_ch4, output="n")
      #plot = purrr::map(data,linear_reg, output="plot"),
      #hist = purrr::map(data,linear_reg, output="hist")
    )
  ch4_nest$Ano <- ano
  ch4_nest <- ch4_nest |> relocate(Ano)

  if( ano == 2015){
    ch4_nest_total <-  ch4_nest
  } else {
    ch4_nest_total <- rbind(ch4_nest_total, ch4_nest)
  }
}
ch4_nest$data[1]


# readr::write_rds(ch4_nest_total,"data-raw/ch4_beta.rds")
ch4_beta <- readr::read_rds("data-raw/ch4_beta.rds")

for( ano in 2015:2020){
  # Filtrando os pontos com n > 7
  ch4_aux <- ch4_beta |>
    dplyr::filter(Ano == ano) |>
    # dplyr::filter(n_obs > 7) |>
    tidyr::unnest(cols = c(beta_line)) |>
    dplyr::ungroup() |>
    dplyr::select(region, longitude, latitude, beta_line)

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

  # Craindo os gráficos
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
}




