## data-raw/ogrenci_notlari.R
##
## `ogrenci_notlari` veri setini uretir ve `data/ogrenci_notlari.rda`
## dosyasina yazar. Yalnizca veri tanimi guncellendiginde calistirilir.
##
## Sinavlar kronolojik sirayla: quiz1 -> quiz2 -> vize -> final.
## Bu sira, zaman serisi tabanli kumeleme (DTW + Ward.D2) icin
## dort noktali bir performans trajektoru saglar.

set.seed(20260408)

n <- 30L

## Bazi ogrenciler icin "geri yukselis" egilimi ekliyoruz:
## donem basinda dusuk, sonlarina dogru artan puanlar.
egilim <- sample(c("kararli", "yukselis", "dusus"),
                 size = n,
                 replace = TRUE,
                 prob = c(0.5, 0.3, 0.2))

# Baz (kararli) puan - ortalama 65, sd 15
baz <- round(stats::rnorm(n, mean = 65, sd = 15))
baz <- pmin(pmax(baz, 40L), 100L)

puanlari_uret <- function(baz_puan, tip) {
  # Dort sinav: quiz1, quiz2, vize, final
  gurultu <- round(stats::rnorm(4L, mean = 0, sd = 5))
  trend <- switch(tip,
                  kararli  = c(0L, 0L, 0L, 0L),
                  yukselis = c(-10L, -5L, 5L, 10L),
                  dusus    = c(10L, 5L, -5L, -10L))
  puanlar <- baz_puan + trend + gurultu
  pmin(pmax(puanlar, 0L), 100L)
}

puan_mat <- t(mapply(puanlari_uret, baz, egilim))
colnames(puan_mat) <- c("quiz1", "quiz2", "vize", "final")

ogrenci_notlari <- data.frame(
  ogrenci_id = paste0("STU", sprintf("%03d", seq_len(n))),
  quiz1      = as.numeric(puan_mat[, "quiz1"]),
  quiz2      = as.numeric(puan_mat[, "quiz2"]),
  vize       = as.numeric(puan_mat[, "vize"]),
  final      = as.numeric(puan_mat[, "final"]),
  grup       = sample(c("A", "B", "C"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

## Agirlikli ortalama: quiz'ler %10+10, vize %30, final %50
ogrenci_notlari$ortalama <- with(
  ogrenci_notlari,
  0.10 * quiz1 + 0.10 * quiz2 + 0.30 * vize + 0.50 * final
)

ogrenci_notlari$harf_notu <- cut(
  ogrenci_notlari$ortalama,
  breaks = c(-Inf, 50, 60, 70, 80, 90, Inf),
  labels = c("FF", "DD", "CC", "BB", "BA", "AA"),
  right  = TRUE
)

usethis::use_data(ogrenci_notlari, overwrite = TRUE)
