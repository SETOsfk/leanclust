## Happy path ---------------------------------------------------------

test_that("kural tabanli 6 sinif dogru ve siralidir", {
  puan <- c(45, 62, 77, 88, 95, 51)
  sonuc <- performans_sinifi_ata(puan)

  expect_s3_class(sonuc, "ordered")
  expect_s3_class(sonuc, "factor")
  expect_equal(levels(sonuc),
               c("FF", "DD", "CC", "BB", "BA", "AA"))
  # 45 -> FF, 62 -> CC, 77 -> BB, 88 -> BA, 95 -> AA, 51 -> DD
  expect_equal(as.character(sonuc),
               c("FF", "CC", "BB", "BA", "AA", "DD"))
})

test_that("ozel kesim noktalari ve etiketler calisir", {
  notlar <- c(30, 65, 85)
  sonuc <- performans_sinifi_ata(
    notlar,
    kesim_noktalari = c(60, 80),
    etiketler       = c("Basarisiz", "Orta", "Basarili")
  )

  expect_equal(as.character(sonuc),
               c("Basarisiz", "Orta", "Basarili"))
})

test_that("kantil tabanli siniflandirma n_sinif kadar seviye uretir", {
  set.seed(1)
  puanlar <- rnorm(100, 70, 10)
  sonuc <- performans_sinifi_ata(
    puanlar,
    yontem  = "kantil",
    n_sinif = 4
  )

  expect_s3_class(sonuc, "factor")
  expect_equal(nlevels(sonuc), 4L)
  # Kantil gruplari yaklasik esit buyukluktedir (+-5)
  expect_true(all(abs(table(sonuc) - 25) <= 5))
})

## Edge cases ---------------------------------------------------------

test_that("kantil tekrarli kesim noktalarinda uyarir ve seviyeleri azaltir", {
  # Cok yogun bir nokta - kantiller tekrarlar
  puan <- c(rep(50, 40), 100)
  expect_warning(
    sonuc <- performans_sinifi_ata(puan, yontem = "kantil", n_sinif = 5),
    "tekrarliyor"
  )
  expect_true(nlevels(sonuc) < 5)
})

test_that("NA iceren puanlar NA olarak doner", {
  sonuc <- performans_sinifi_ata(c(45, NA, 95))
  expect_true(is.na(sonuc[2]))
  expect_equal(as.character(sonuc[c(1, 3)]), c("FF", "AA"))
})

## Type & defensive -------------------------------------------------

test_that("hatali girdi turu hata verir", {
  expect_error(performans_sinifi_ata(c("a", "b")), "sayisal")
  expect_error(
    performans_sinifi_ata(1:10, kesim_noktalari = c(5, 10),
                          etiketler = c("az", "orta")),
    "etiketler"
  )
  expect_error(
    performans_sinifi_ata(1:10, yontem = "kantil", n_sinif = 1),
    ">= 2"
  )
})
