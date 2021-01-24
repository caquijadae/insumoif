# Análisis marco

df <- analisis_marco(base1 = LifeCycleSavings,
                     id = sr,
                     var1 = pop15)

test_that("Marco: primer eliminado", {
  expect_equal(df$sr_eliminado[2], 10.78)
})


test_that("Marco: tamaño de la base de salida", {
  expect_equal(dim(df), c(49, 8))
})
