# Análisis muestra

df <- analisis_muestra(base1 = LifeCycleSavings,
                       id = sr,
                       var1 = pop15,
                       var2 = dpi)

test_that("Primer eliminado", {
  expect_equal(df$sr_eliminado[2], 10.78)
})


test_that("Tamaño de la base de salida", {
  expect_equal(dim(df), c(49, 10))
})

