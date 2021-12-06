regiao <- geobr::read_region(showProgress = FALSE)
br <- geobr::read_country(showProgress = FALSE)
### Polígono Brasil
pol_br <- br$geom |> purrr::pluck(1) |> as.matrix()
pol_norte <- regiao$geom |> purrr::pluck(1) |> as.matrix()
pol_nordeste <- regiao$geom |> purrr::pluck(2) |> as.matrix()
pol_sudeste <- regiao$geom |> purrr::pluck(3) |> as.matrix()
pol_sul <- regiao$geom |> purrr::pluck(4) |> as.matrix()
pol_centroeste<- regiao$geom |> purrr::pluck(5) |> as.matrix()
pol_br <- pol_br[pol_br[,1]<=-34,]
pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
                     (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]
# Arrumando alguns pontos
pol_nordeste <- pol_nordeste[pol_nordeste[,1]<=-34,]
pol_nordeste <- pol_nordeste[!((pol_nordeste[,1]>=-38.7 & pol_nordeste[,1]<=-38.6) & pol_nordeste[,2]<= -15),]
# retirando pontos do sudeste
pol_sudeste <- pol_sudeste[pol_sudeste[,1]<=-30,]

contorno <- purrr::map_dfr(1:27, get_contorno, lista=mapa$geom) |>
  dplyr::filter(V1 < -33) |>
  dplyr::filter(!(V1 < -38.5 & V1 > -39 & V2>-20 & V2 < -16))
names(contorno) <- c("X","Y")
