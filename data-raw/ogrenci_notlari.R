## data-raw/ogrenci_notlari.R
## ===========================================================================
## `ogrenci_notlari` veri setini uretir ve `data/ogrenci_notlari.rda`
## dosyasina yazar. Yalnizca veri tanimi guncellendiginde calistirilir.
##
## Cikti:
##   30 ogrenci x 8 sutun; sinavlar kronolojik sirada:
##   quiz1 -> quiz2 -> vize -> final
##
## Bu sira, zaman serisi tabanli kumeleme (DTW + Ward.D2) icin
## dort noktali bir performans trajektoru saglar.
## ---------------------------------------------------------------------------

set.seed(20260408)

n <- 30L

## Egilim profilleri:
##   - kararli: donem boyunca puan dalgalanmadan sabit kalir.
##   - yukselis: baslangicta dusuk, zamanla yukselen trajektor
##                (nam-i diger "late bloomer").
##   - dusus:    baslangicta yuksek, zamanla dusen trajektor.
egilim <- sample(
  x       = c("kararli", "yukselis", "dusus"),
  size    = n,
  replace = TRUE,
  prob    = c(0.50, 0.30, 0.20)
)

## Baz puan: ortalama 65, sd 15; [40, 100] araliginda kisitli.
baz <- round(stats::rnorm(n, mean = 65, sd = 15))
baz <- pmin(pmax(baz, 40L), 100L)

## Tek bir ogrenci icin dort noktali trajektor uret.
##   trend: kronolojik egilim vektoru
##   gurultu: her sinavda bagimsiz Gaussian gurultu
puanlari_uret <- function(baz_puan, tip) {
  gurultu <- round(stats::rnorm(4L, mean = 0, sd = 5))
  trend <- switch(
    tip,
    kararli  = c(  0L,   0L,   0L,   0L),
    yukselis = c(-10L,  -5L,   5L,  10L),
    dusus    = c( 10L,   5L,  -5L, -10L)
  )
  puanlar <- baz_puan + trend + gurultu
  ## Puanlari [0, 100] araligina kisitla.
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

## Agirlikli ortalama: quiz'ler %10+10, vize %30, final %50 = %100
## Matematiksel olarak: 0.10*q1 + 0.10*q2 + 0.30*v + 0.50*f
ogrenci_notlari$ortalama <- with(
  ogrenci_notlari,
  0.10 * quiz1 + 0.10 * quiz2 + 0.30 * vize + 0.50 * final
)

## Harf notu: Turk yuksekogrenim sistemindeki klasik esikler.
## -Inf / Inf sinirlari guvenlik icin; ortalama zaten [0, 100]'da.
ogrenci_notlari$harf_notu <- cut(
  ogrenci_notlari$ortalama,
  breaks         = c(-Inf, 50, 60, 70, 80, 90, Inf),
  labels         = c("FF", "DD", "CC", "BB", "BA", "AA"),
  right          = TRUE,
  ordered_result = TRUE
)

## Son kontrol: beklenen sutun setini ve boyutu dogrula.
stopifnot(
  nrow(ogrenci_notlari) == n,
  identical(
    names(ogrenci_notlari),
    c("ogrenci_id", "quiz1", "quiz2", "vize", "final",
      "grup", "ortalama", "harf_notu")
  ),
  all(vapply(ogrenci_notlari[, c("quiz1", "quiz2", "vize", "final")],
             function(z) all(z >= 0 & z <= 100),
             logical(1L)))
)

usethis::use_data(ogrenci_notlari, overwrite = TRUE)

message("ogrenci_notlari guncellendi: ",
        nrow(ogrenci_notlari), " satir, ",
        ncol(ogrenci_notlari), " sutun.")
