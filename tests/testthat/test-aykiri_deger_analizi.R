## Happy path ---------------------------------------------------------

test_that("raporda temel alanlar beklenen uzunlukta doner", {
  set.seed(123)
  veri <- c(rnorm(50, 100, 10), 150, 200, 45, 30)

  out <- capture.output({
    sonuc <- aykiri_deger_analizi(veri, grafik = FALSE)
  })

  expect_named(sonuc, c("temiz_veri", "aykiri_degerler", "sinirlar"))
  expect_named(sonuc$sinirlar,
               c("Q1", "Q3", "IQR", "alt_sinir", "ust_sinir"))
  expect_equal(
    length(sonuc$temiz_veri) + length(sonuc$aykiri_degerler),
    length(veri)
  )
})

## Edge cases ---------------------------------------------------------

test_that("aykiri deger yokken temiz_veri asli ile ayni uzunluktadir", {
  # Normal veri - aykiri yok
  set.seed(7)
  veri <- rnorm(200, 0, 1)

  out <- capture.output({
    sonuc <- aykiri_deger_analizi(veri, grafik = FALSE)
  })

  # Bug kontrolu: veri[-integer(0)] bos vektor donerdi.
  # Duzeltilmis versiyonda tum veri temiz_veri'ye dusmeli.
  expect_equal(
    length(sonuc$temiz_veri) + length(sonuc$aykiri_degerler),
    length(veri)
  )
})

test_that("NA degerler uyariyla temizlenir", {
  veri <- c(1, 2, 3, NA, 100)

  expect_warning(
    capture.output({
      sonuc <- aykiri_deger_analizi(veri, grafik = FALSE)
    }),
    "eksik degerler"
  )
})

## Type check ---------------------------------------------------------

test_that("sayisal olmayan girdilerde hata verir", {
  expect_error(
    aykiri_deger_analizi(c("a", "b", "c")),
    "sayisal"
  )
})
