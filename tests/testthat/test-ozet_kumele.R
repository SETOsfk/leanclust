test_that("ozet_kumele gt_tbl nesnesi doner", {
  veri <- data.frame(
    vize  = c(30, 35, 70, 72, 90, 92),
    final = c(32, 36, 68, 74, 88, 94)
  )
  sonuc <- ogrenci_performans_kumele(
    veri,
    degiskenler = c("vize", "final"),
    k           = 3,
    seed        = 1
  )
  tablo <- ozet_kumele(sonuc)

  expect_s3_class(tablo, "gt_tbl")
})

test_that("yanlis girdi turu aciklayici hata verir", {
  expect_error(ozet_kumele(list(veri = 1)),
               "ogrenci_performans_kumele")
})
