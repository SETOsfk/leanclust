#' Küme Dağılım Grafiği
#'
#' [ogrenci_performans_kumele()] sonucundaki iki değişkeni küme
#' etiketleriyle renklendirerek dağılım grafiği çizer. Küme merkezleri
#' "X" işaretleri ile vurgulanır.
#'
#' @param sonuc [ogrenci_performans_kumele()] tarafından döndürülen
#'   liste.
#' @param x Yatay eksen değişkeni (karakter). `sonuc$veri` içindeki bir
#'   sütun adı olmalıdır. `NULL` verilirse kümelemede kullanılan ilk
#'   değişken seçilir.
#' @param y Dikey eksen değişkeni (karakter). `NULL` verilirse ikinci
#'   değişken seçilir.
#' @param baslik Grafik başlığı. Varsayılan
#'   "Öğrenci Performans Kümeleri".
#' @param renkler Karakter vektörü. Uzunluğu küme sayısına eşit
#'   olmalıdır. `NULL` verilirse varsayılan bir palet kullanılır.
#' @param merkez_goster Mantıksal. `TRUE` (varsayılan) ise küme
#'   merkezleri X işaretleriyle çizilir.
#'
#' @return Bir `ggplot` nesnesi.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text labs
#'   theme_minimal scale_color_manual
#' @importFrom rlang .data
#'
#' @examples
#' data(ogrenci_notlari)
#' sonuc <- ogrenci_performans_kumele(
#'   veri        = ogrenci_notlari,
#'   degiskenler = c("vize", "final"),
#'   seed        = 42
#' )
#' kumele_grafik(sonuc, x = "vize", y = "final")
kumele_grafik <- function(
    sonuc,
    x             = NULL,
    y             = NULL,
    baslik        = "Ogrenci Performans Kumeleri",
    renkler       = NULL,
    merkez_goster = TRUE
) {

  if (!is.list(sonuc) ||
      !all(c("veri", "merkezler", "argumanlar") %in% names(sonuc))) {
    stop(
      "`sonuc`, `ogrenci_performans_kumele()` ciktisi olmalidir.",
      call. = FALSE
    )
  }

  degiskenler <- sonuc$argumanlar$degiskenler
  if (is.null(x)) x <- degiskenler[1L]
  if (is.null(y)) {
    if (length(degiskenler) < 2L) {
      stop("En az iki kumeleme degiskeni gerekir ya da `y` elle verin.",
           call. = FALSE)
    }
    y <- degiskenler[2L]
  }

  for (v in c(x, y)) {
    if (!v %in% names(sonuc$veri)) {
      stop(sprintf("`%s` sutunu `sonuc$veri` icinde bulunamadi.", v),
           call. = FALSE)
    }
  }

  etiketler <- levels(sonuc$veri$kume_etiketi)
  if (is.null(renkler)) {
    palet <- c("#d7191c", "#fdae61", "#2c7bb6", "#1a9641",
               "#756bb1", "#e7298a", "#66a61e", "#e6ab02")
    renkler <- palet[seq_along(etiketler)]
  }
  if (length(renkler) != length(etiketler)) {
    stop(
      sprintf("`renkler` uzunlugu (%d), kume sayisi (%d) ile esit olmalidir.",
              length(renkler), length(etiketler)),
      call. = FALSE
    )
  }
  names(renkler) <- etiketler

  p <- ggplot2::ggplot(
    data    = sonuc$veri,
    mapping = ggplot2::aes(
      x     = .data[[x]],
      y     = .data[[y]],
      color = .data[["kume_etiketi"]]
    )
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.75) +
    ggplot2::scale_color_manual(values = renkler, drop = FALSE) +
    ggplot2::labs(
      title = baslik,
      x     = x,
      y     = y,
      color = "Kume"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  if (isTRUE(merkez_goster) &&
      all(c(x, y) %in% names(sonuc$merkezler))) {

    p <- p +
      ggplot2::geom_point(
        data = sonuc$merkezler,
        mapping = ggplot2::aes(
          x     = .data[[x]],
          y     = .data[[y]]
        ),
        shape = 4, size = 6, stroke = 1.5,
        color = "black",
        inherit.aes = FALSE
      )
  }

  p
}
