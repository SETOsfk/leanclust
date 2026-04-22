## tests/testthat/test-ogrenci_zaman_serisi_kumele.R
## ---------------------------------------------------------------------------
## DTW + Ward.D2 zaman serisi kumeleme fonksiyonunun testleri.
##
## Tum DTW cagrilari `dtw` paketine baglidir; paket kurulu degilse
## testler atlanir (CRAN uyumu icin gereklidir).
## Ayrica testler, `ogrenci_notlari` icinde quiz1, quiz2, vize ve final
## sutunlarinin bulunmasini gerektirir (yeni veri sema surumu).
## ---------------------------------------------------------------------------

skip_if_not_installed("dtw")

## Yardimci: Test verilerini hazirla (dahili kullanim) --------------

.veri_hazir_mi <- function() {
  data(ogrenci_notlari, package = "leanclust",
       envir = environment())
  required <- c("quiz1", "quiz2", "vize", "final")
  all(required %in% names(ogrenci_notlari))
}

skip_if_not(.veri_hazir_mi(),
            message = paste(
              "ogrenci_notlari yeni sema degil; once",
              "`source(\"data-raw/ogrenci_notlari.R\")` calistirin."))


## ---- Happy path ---------------------------------------------------

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
  expect_setequal(levels(sonuc$veri$kume_etiketi),
                  c("Dusuk", "Orta", "Yuksek"))
})

test_that("hclust metodu Ward.D2 olarak ayarlanir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  expect_equal(sonuc$hclust$method, "ward.D2")
})

test_that("varsayilan etiketler 'Kume_1..k' formatinda olusturulur", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 4L
  )

  expect_equal(levels(sonuc$veri$kume_etiketi),
               c("Kume_1", "Kume_2", "Kume_3", "Kume_4"))
})


## ---- Etiket sirasi (determinizm) ----------------------------------

test_that("etiketler kume ortalamasina gore sirali atanir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    etiketler          = c("Dusuk", "Orta", "Yuksek")
  )

  zaman_cols <- c("quiz1", "quiz2", "vize", "final")
  df <- sonuc$veri

  dusuk_ort <- mean(rowMeans(
    df[df$kume_etiketi == "Dusuk",  zaman_cols]))
  orta_ort  <- mean(rowMeans(
    df[df$kume_etiketi == "Orta",   zaman_cols]))
  yuksek_ort <- mean(rowMeans(
    df[df$kume_etiketi == "Yuksek", zaman_cols]))

  expect_lt(dusuk_ort, orta_ort)
  expect_lt(orta_ort,  yuksek_ort)
})

test_that("ayni veri + ayni parametre = ayni ciktı (deterministik)", {
  data(ogrenci_notlari)

  sonuc1 <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )
  sonuc2 <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  expect_identical(sonuc1$veri$kume, sonuc2$veri$kume)
})


## ---- Ek parametre secenekleri ------------------------------------

test_that("Sakoe-Chiba penceresi calisir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    pencere            = "sakoechiba",
    pencere_genisligi  = 2L
  )

  expect_s3_class(sonuc, "leanclust_zaman_serisi")
  expect_equal(sonuc$argumanlar$pencere, "sakoechiba")
  expect_equal(sonuc$argumanlar$pencere_genisligi, 2L)
})

test_that("standartlastir = FALSE ham olcek uzerinden calisir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    standartlastir     = FALSE
  )

  expect_s3_class(sonuc, "leanclust_zaman_serisi")
  expect_false(sonuc$argumanlar$standartlastir)
})


## ---- Edge case: iki nokta uyarisi --------------------------------

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


## ---- Type checks / hatalar ---------------------------------------

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

test_that("tekrar eden zaman degiskeni icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "quiz1", "vize", "final")
    ),
    "tekrar eden"
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

test_that("k >= n gozlem icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari[1:3, ],
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L
    ),
    "gozlem sayisindan"
  )
})

test_that("yanlis uzunlukta etiketler icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L,
      etiketler          = c("A", "B")
    ),
    "etiketler"
  )
})

test_that("tekrar eden etiketler icin hata verir", {
  data(ogrenci_notlari)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = ogrenci_notlari,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L,
      etiketler          = c("A", "A", "B")
    ),
    "tekrar eden"
  )
})

test_that("NA icerigi icin hata verir", {
  data(ogrenci_notlari)
  bozuk <- ogrenci_notlari
  bozuk$vize[1L] <- NA_real_
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = bozuk,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L
    ),
    "NA"
  )
})

test_that("sayisal olmayan sutun icin hata verir", {
  data(ogrenci_notlari)
  bozuk <- ogrenci_notlari
  bozuk$quiz1 <- as.character(bozuk$quiz1)
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = bozuk,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L
    ),
    "sayisal"
  )
})

test_that("sabit (varyans = 0) sutun icin hata verir", {
  data(ogrenci_notlari)
  bozuk <- ogrenci_notlari
  bozuk$quiz1 <- 50  # Sabit deger -> sd = 0
  expect_error(
    ogrenci_zaman_serisi_kumele(
      veri               = bozuk,
      zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
      k                  = 3L
    ),
    "varyans"
  )
})


## ---- Yapisal dogrulama -------------------------------------------

test_that("merkezler orijinal puan olceginde (0-100 araligi)", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  sayisal <- sonuc$merkezler[, c("quiz1", "quiz2", "vize", "final")]
  expect_true(all(sayisal >= 0 & sayisal <= 100))
  expect_equal(nrow(sonuc$merkezler), 3L)
  expect_true("etiket" %in% names(sonuc$merkezler))
})

test_that("kume boyutlari toplami n gozleme esittir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L
  )

  expect_equal(sum(table(sonuc$veri$kume)), nrow(ogrenci_notlari))
  expect_true(all(table(sonuc$veri$kume) > 0))
})


## ---- Print metodu -------------------------------------------------

test_that("print metodu beklenen bilesenleri gosterir", {
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    etiketler          = c("Dusuk", "Orta", "Yuksek")
  )

  out <- capture.output(print(sonuc))
  birlesik <- paste(out, collapse = "\n")

  expect_match(birlesik, "leanclust_zaman_serisi")
  expect_match(birlesik, "Ward\\.D2")
  expect_match(birlesik, "DTW")
  expect_match(birlesik, "Dusuk")
  expect_match(birlesik, "quiz1")
})
