## Yardimci - yapay veri ---------------------------------------------

ornek_veri <- function(n = 60, seed = 7) {
  set.seed(seed)
  data.frame(
    id    = paste0("S", seq_len(n)),
    vize  = c(rnorm(n / 3, 40, 5), rnorm(n / 3, 65, 5), rnorm(n / 3, 90, 5)),
    final = c(rnorm(n / 3, 45, 5), rnorm(n / 3, 70, 5), rnorm(n / 3, 92, 5))
  )
}

## Happy path ---------------------------------------------------------

test_that("k-means 3 acik kume icin dogru yapida sonuc uretir", {
  veri <- ornek_veri()
  sonuc <- ogrenci_performans_kumele(
    veri,
    degiskenler = c("vize", "final"),
    k           = 3,
    seed        = 1
  )

  expect_type(sonuc, "list")
  expect_named(sonuc,
               c("veri", "kmeans", "merkezler", "ozet", "argumanlar"))

  # Kume sutunlari eklenmis olmali
  expect_true(all(c("kume", "kume_etiketi") %in% names(sonuc$veri)))
  expect_equal(nrow(sonuc$veri), nrow(veri))

  # Etiketler siralidir (dusuk < orta < yuksek)
  expect_s3_class(sonuc$veri$kume_etiketi, "ordered")
  expect_equal(levels(sonuc$veri$kume_etiketi),
               c("Dusuk", "Orta", "Yuksek"))
})

test_that("kumeler puan ortalamalarina gore monoton siralanmistir", {
  veri <- ornek_veri()
  sonuc <- ogrenci_performans_kumele(
    veri,
    degiskenler = c("vize", "final"),
    k           = 3,
    seed        = 1
  )
  ozet <- sonuc$ozet

  # vize ve final ortalamalari kume 1 -> kume 3 arasinda artmali
  expect_true(all(diff(ozet$vize_ort) >= 0))
  expect_true(all(diff(ozet$final_ort) >= 0))
})

test_that("seed tekrarlanabilirlik saglar", {
  veri  <- ornek_veri()
  a <- ogrenci_performans_kumele(veri,
                                 degiskenler = c("vize", "final"),
                                 seed = 42)
  b <- ogrenci_performans_kumele(veri,
                                 degiskenler = c("vize", "final"),
                                 seed = 42)
  expect_equal(a$veri$kume, b$veri$kume)
})

## Type / structure ---------------------------------------------------

test_that("merkezler orijinal olcege cevrilir", {
  veri <- ornek_veri()
  sonuc <- ogrenci_performans_kumele(
    veri,
    degiskenler    = c("vize", "final"),
    k              = 3,
    standartlastir = TRUE,
    seed           = 1
  )

  # Orijinal olcekteki merkezler, veri araliginda olmali
  expect_true(all(sonuc$merkezler$vize  >= min(veri$vize)  - 5))
  expect_true(all(sonuc$merkezler$vize  <= max(veri$vize)  + 5))
  expect_true(all(sonuc$merkezler$final >= min(veri$final) - 5))
})

## Edge cases ---------------------------------------------------------

test_that("gecersiz girdiler uygun hatalari verir", {
  veri <- ornek_veri()

  expect_error(
    ogrenci_performans_kumele("merhaba"),
    "data.frame"
  )
  expect_error(
    ogrenci_performans_kumele(veri, k = 1),
    ">= 2"
  )
  expect_error(
    ogrenci_performans_kumele(veri, degiskenler = c("mevcut_degil")),
    "yok"
  )
  expect_error(
    ogrenci_performans_kumele(veri, degiskenler = c("id")),
    "sayisal degil"
  )
})

test_that("NA iceren kumeleme degiskenleri hata verir", {
  veri <- ornek_veri()
  veri$vize[3] <- NA
  expect_error(
    ogrenci_performans_kumele(veri, degiskenler = c("vize", "final")),
    "NA"
  )
})

test_that("etiket uzunlugu k ile tutarsizsa hata verir", {
  veri <- ornek_veri()
  expect_error(
    ogrenci_performans_kumele(veri,
                              degiskenler = c("vize", "final"),
                              k = 3,
                              etiketler = c("az", "cok")),
    "etiketler"
  )
})
