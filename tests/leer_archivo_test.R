library(testthat)

archivo <- paste(getwd(), '/R/test/leer_archivo', sep = '')

test_that("Prueba leer_csv: carga los números",{
  expected <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  actual <- leer.csv(archivo)
  expect_equal(expected, actual$V1)
})

test_that("Prueba leer_delim: carga los números",{
  expected <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  actual <- leer.csv(archivo)
  expect_equal(expected, actual$V1)
})
