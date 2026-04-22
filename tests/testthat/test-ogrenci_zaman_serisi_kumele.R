## Tum testler dtw paketine baglidir
skip_if_not_installed("dtw")

## Happy path --------------------------------------------------------

test_that("dort noktali trajektor ile temel alanlar donduruluyor", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    etiketler          = c("Dusuk", "Orta", "Yuksek")
  )

  expect_named(sonuc,
               c("veri", "hclust", "mesafe", "merkezler", "argumanlar"))
  expect_s3_class(sonuc, "leanclust_zaman_serisi")
  expect_s3_class(sonuc$hclust, "hclust")
  expect_s3_class(sonuc$mesafe, "dist")
  expect_s3_class(sonuc$veri$kume_etiketi, "factor")
  expect_equal(nlevels(sonuc$veri$kume_etiketi), 3L)
  expect_equal(nrow(sonuc$veri), nrow(ogrenci_notlari))
  expect_true(all(c("Dusuk", "Orta", "Yuksek") %in%
                    levels(sonuc$veri$kume_etiketi)))
})

test_that("hclust metodu Ward.D2 ile calisir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  expect_equal(sonuc$hclust$method, "ward.D2")
})

## Etiket sirasi ----------------------------------------------------

test_that("etiketler kume ortalamasina gore sirali atanir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    etiketler          = c("Dusuk", "Orta", "Yuksek")
  )

  df <- sonuc$veri
  zaman_cols <- c("quiz1", "quiz2", "vize", "final")
  dusuk_ort  <- mean(rowMeans(df[df$kume_etiketi == "Dusuk",  zaman_cols]))
  yuksek_ort <- mean(rowMeans(df[df$kume_etiketi == "Yuksek", zaman_cols]))
  expect_lt(dusuk_ort, yuksek_ort)
})

## Edge case: iki nokta uyarisi ------------------------------------

test_that("yalnizca iki zaman noktasi icin uyari verilir", {
  data(ogrenci_notlari)
  expect_warning(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("vize", "final"),
      k                  = 2L
    ),
    "iki zaman noktasi"
  )
})

## Type checks / hatalar -------------------------------------------

test_that("veri data.frame degilse hata verir", {
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = matrix(1:40, nrow = 10),
      zaman_degiskenleri = c("a", "b", "c", "d"),
      k                  = 2L
    ),
    "data\\.frame"
  )
})

test_that("eksik zaman degiskenleri icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "olmayan_sutun", "vize", "final")
    ),
    "bulunmayan"
  )
})

test_that("k < 2 icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 1
    ),
    "en az 2"
  )
})

test_that("yanlis uzunlukta etiketler icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L,
      etiketler          = c("A", "B")  # 3 yerine 2
    ),
    "etiketler"
  )
})

test_that("tek zaman degiskeni icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = "final"
    ),
    "en az iki"
  )
})

## Merkezler orijinal olcekte mi? ----------------------------------

test_that("merkezler orijinal puan olceginde (0-100 civari)", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  sayisal_merkezler <- sonuc$merkezler[,
    c("quiz1", "quiz2", "vize", "final")]
  expect_true(all(sayisal_merkezler >= 0 & sayisal_merkezler <= 100))
})
