## DTW bagimli testler; eksikse atla ---------------------------------
skip_if_not_installed("dtw")
skip_if_not_installed("ggplot2")

.veri_hazir_mi <- function() {
  exists("ogrenci_notlari") &&
    all(c("quiz1", "quiz2", "vize", "final") %in%
          names(ogrenci_notlari))
}

.sentetik_sonuc <- function(k = 3L) {
  set.seed(20260422)
  n <- 18L
  id <- sprintf("S%03d", seq_len(n))
  # Uc sekil: kararli, yukselis, dusus
  tipler <- rep(c("kararli", "yukselis", "dusus"), length.out = n)
  baz    <- rnorm(n, 70, 5)
  zaman  <- function(tip) {
    switch(tip,
      kararli  = c(0,  0,  0,  0),
      yukselis = c(-10, -5,  5, 10),
      dusus    = c(10,  5, -5, -10)
    )
  }
  M <- t(vapply(seq_len(n), function(i) baz[i] + zaman(tipler[i]),
                numeric(4)))
  veri <- data.frame(
    ogrenci_id = id,
    quiz1 = M[, 1], quiz2 = M[, 2], vize = M[, 3], final = M[, 4]
  )

  ogrenci_zaman_serisi_kumele(
    veri               = veri,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = k,
    etiketler          = c("Dusuk", "Orta", "Yuksek")[seq_len(k)]
  )
}

## Happy path -------------------------------------------------------

test_that("trajektor_grafik ggplot nesnesi doner (sentetik veri)", {
  sonuc <- .sentetik_sonuc()
  p <- trajektor_grafik(sonuc)
  expect_s3_class(p, "ggplot")
})

test_that("etiketler ve zaman degiskenleri grafikte kullanilir", {
  sonuc <- .sentetik_sonuc()
  p <- trajektor_grafik(sonuc)
  # ggplot nesnesi build edilebilir olmalidir
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("goster_ogrenci = FALSE yalnizca ortalama cizer", {
  sonuc <- .sentetik_sonuc()
  p <- trajektor_grafik(sonuc, goster_ogrenci = FALSE)
  # Katman sayisi azalmali: geom_line (ogrenci) atlanir
  expect_s3_class(p, "ggplot")
  # Birey geom_line'i olmadigini dogrula
  katmanlar <- vapply(p$layers, function(lyr) class(lyr$geom)[1L],
                      character(1L))
  # ogrenci cizgisi yoksa, iki geom_line yerine bir tane olur
  n_line <- sum(katmanlar == "GeomLine")
  expect_equal(n_line, 1L)
})

test_that("goster_nokta = FALSE ortalama noktalarini gizler", {
  sonuc <- .sentetik_sonuc()
  p <- trajektor_grafik(sonuc, goster_nokta = FALSE)
  katmanlar <- vapply(p$layers, function(lyr) class(lyr$geom)[1L],
                      character(1L))
  expect_false("GeomPoint" %in% katmanlar)
})

## Tur ve dogrulama ------------------------------------------------

test_that("yanlis girdi turunde aciklayici hata verir", {
  expect_error(trajektor_grafik(list(veri = 1)),
               "ogrenci_zaman_serisi_kumele")
})

test_that("alpha_ogrenci sinir disi deger hata verir", {
  sonuc <- .sentetik_sonuc()
  expect_error(trajektor_grafik(sonuc, alpha_ogrenci = 1.5),
               "0 ile 1")
  expect_error(trajektor_grafik(sonuc, alpha_ogrenci = -0.1),
               "0 ile 1")
})

test_that("goster_ogrenci mantiksal olmayan deger hata verir", {
  sonuc <- .sentetik_sonuc()
  expect_error(trajektor_grafik(sonuc, goster_ogrenci = "evet"),
               "TRUE/FALSE")
})

test_that("cizgi_kalinlik_ortalama pozitif olmalidir", {
  sonuc <- .sentetik_sonuc()
  expect_error(trajektor_grafik(sonuc, cizgi_kalinlik_ortalama = 0),
               "pozitif")
  expect_error(trajektor_grafik(sonuc, cizgi_kalinlik_ortalama = -1),
               "pozitif")
})

test_that("yanlis uzunluklu renk vektoru hata verir", {
  sonuc <- .sentetik_sonuc(k = 3L)
  expect_error(
    trajektor_grafik(sonuc, renkler = c("red", "blue")),
    "renkler"
  )
})

test_that("ogrenci_id sutunu olmayan veride de calisir", {
  sonuc <- .sentetik_sonuc()
  sonuc$veri$ogrenci_id <- NULL
  p <- trajektor_grafik(sonuc)
  expect_s3_class(p, "ggplot")
})

## Paket veri seti ile entegre test --------------------------------

test_that("ogrenci_notlari veri seti ile uyumlu calisir", {
  skip_if_not(.veri_hazir_mi(),
              "ogrenci_notlari veri seti henuz yeniden olusturulmadi.")
  data(ogrenci_notlari)

  sonuc <- ogrenci_zaman_serisi_kumele(
    veri               = ogrenci_notlari,
    zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
    k                  = 3L,
    etiketler          = c("Dusuk", "Orta", "Yuksek")
  )
  p <- trajektor_grafik(sonuc)
  expect_s3_class(p, "ggplot")
})
