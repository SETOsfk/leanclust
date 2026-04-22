#' Küme Bazlı Özet Tablosu
#'
#' [ogrenci_performans_kumele()] çıktısından `gt` kütüphanesi ile
#' yayın kalitesinde bir özet tablosu üretir.
#'
#' @param sonuc [ogrenci_performans_kumele()] tarafından döndürülen
#'   liste.
#' @param baslik Tablo başlığı. Varsayılan
#'   "Küme Bazlı Performans Özeti".
#' @param altbaslik Tablo alt başlığı. `NULL` verilirse küme sayısı ve
#'   toplam gözlem sayısından otomatik üretilir.
#' @param ondalik Sayısal değerler için ondalık basamak sayısı.
#'   Varsayılan 2.
#'
#' @return Bir `gt_tbl` nesnesi. `print()` ile görüntülenebilir,
#'   `gt::gtsave()` ile dışa aktarılabilir.
#'
#' @export
#'
#' @importFrom gt gt tab_header fmt_number cols_label
#'
#' @examples
#' data(ogrenci_notlari)
#' sonuc <- ogrenci_performans_kumele(
#'   veri        = ogrenci_notlari,
#'   degiskenler = c("vize", "final"),
#'   seed        = 42
#' )
#' ozet_kumele(sonuc)
ozet_kumele <- function(
    sonuc,
    baslik    = "Kume Bazli Performans Ozeti",
    altbaslik = NULL,
    ondalik   = 2L
) {

  if (!is.list(sonuc) ||
      !all(c("ozet", "argumanlar") %in% names(sonuc))) {
    stop(
      "`sonuc`, `ogrenci_performans_kumele()` ciktisi olmalidir.",
      call. = FALSE
    )
  }

  ozet  <- sonuc$ozet
  args  <- sonuc$argumanlar

  if (is.null(altbaslik)) {
    altbaslik <- sprintf(
      "%d kume, toplam %d gozlem",
      args$k, sum(ozet$n)
    )
  }

  sayisal_sutunlar <- setdiff(names(ozet), c("kume", "kume_etiketi", "n"))

  tbl <- gt::gt(ozet) |>
    gt::tab_header(title = baslik, subtitle = altbaslik) |>
    gt::cols_label(
      kume         = "Kume",
      kume_etiketi = "Etiket",
      n            = "n"
    )

  if (length(sayisal_sutunlar) > 0L) {
    tbl <- gt::fmt_number(tbl,
                          columns  = sayisal_sutunlar,
                          decimals = ondalik)
  }

  tbl
}
