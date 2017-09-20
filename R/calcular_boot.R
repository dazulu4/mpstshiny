library(boot)

calcular.boot <- function(datos, replicas = 1000) {
  resultado <- boot(data = datos, statistic = calcular.media, R = replicas)
  list(media = resultado$t0,
       desest = sd(resultado$t[,1]))
}

calcular.media <- function(x, d) {
  return(mean(x[d]))
}
