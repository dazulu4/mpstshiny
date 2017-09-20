library(testthat)

archivo <- paste(getwd(), '/tests/leer_archivo', sep = '')

test_that("Prueba cargar_archivo: carga los datos, totales y estadisticos",{
  datos <- data.frame(muestra = c(1.1, 2.2, 3.3, 4.4, 5.5))
  expected <- list(datos = datos,
                   total = 10,
                   totalNM = 5,
                   totalNN = 3,
                   totalNA = 2,
                   media = 3.30,
                   desest = round(1.739253, 2),
                   minimo = 1.10,
                   maximo = 5.50)
  actual <- cargar.archivo(archivo)
  expect_equal(expected$media, actual$media)
})

