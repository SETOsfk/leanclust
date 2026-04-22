test_that("kumele_grafik ggplot nesnesi doner", {
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

  p <- kumele_grafik(sonuc, x = "vize", y = "final")
  expect_s3_class(p, "ggplot")
})

test_that("olmayan eksen degiskeni hata verir", {
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

  expect_error(kumele_grafik(sonuc, x = "bogusvar", y = "final"),
               "bulunamadi")
})

test_that("yanlis renk vektoru uzunlugu hata verir", {
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

  expect_error(
    kumele_grafik(sonuc, x = "vize", y = "final",
                  renkler = c("red", "blue")),
    "renkler"
  )
})
