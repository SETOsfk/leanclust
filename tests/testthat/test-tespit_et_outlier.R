## Happy path ---------------------------------------------------------

test_that("IQR sinirlari ve aykiri degerler dogru hesaplanir", {
  veri <- c(-100, 10, 20, 30, 200)
  sonuc <- tespit_et_outlier(veri)

  # -100 ve 200 acikca aykiridir
  expect_setequal(sonuc$aykiri_degerler, c(-100, 200))
  expect_true(all(sonuc$indisler %in% c(1, 5)))
})

test_that("k arttiginda daha az aykiri deger isaretlenir", {
  set.seed(1)
  veri <- c(rnorm(100, 50, 5), 90, 95, -10)

  az_k  <- tespit_et_outlier(veri, k = 1.5)
  cok_k <- tespit_et_outlier(veri, k = 3.0)

  expect_gte(length(az_k$aykiri_degerler), length(cok_k$aykiri_degerler))
})

## Type & structure tests ---------------------------------------------

test_that("cikti yapisi bekleneni verir", {
  sonuc <- tespit_et_outlier(1:10)

  expect_type(sonuc, "list")
  expect_named(sonuc,
               c("alt_sinir", "ust_sinir", "aykiri_degerler", "indisler"))
  expect_type(sonuc$aykiri_degerler, "integer")
  expect_type(sonuc$indisler, "integer")
})

## Edge cases ---------------------------------------------------------

test_that("aykiri deger yoksa bos vektor donulur", {
  # normal veriden aykiri cikmamalidir
  set.seed(42)
  veri <- rnorm(1000)
  sonuc <- tespit_et_outlier(veri, k = 10) # cok yuksek esik

  expect_length(sonuc$aykiri_degerler, 0)
  expect_length(sonuc$indisler, 0)
})

test_that("NA degerler hesaplamayi bozmaz", {
  veri <- c(1, 2, 3, NA, 100)
  sonuc <- tespit_et_outlier(veri)

  # NA, quantile(na.rm = TRUE) ile gormezden gelinir
  expect_true(is.finite(sonuc$alt_sinir))
  expect_true(is.finite(sonuc$ust_sinir))
})
