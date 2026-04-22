#' Öğrenci Trajektör Grafiği (Zaman Serisi Kümeleri)
#'
#' [ogrenci_zaman_serisi_kumele()] sonucundaki zaman serisi kümelerini
#' görselleştirir. Her öğrenci için ince bir çizgi (küme rengiyle)
#' çizilir ve üzerine her kümenin ortalama trajektörü kalın çizgi
#' olarak bindirilir. Bu görsel, DTW'nin şekle dayalı gruplama
#' niteliğini anlaşılır kılar.
#'
#' @param sonuc [ogrenci_zaman_serisi_kumele()] tarafından döndürülen,
#'   `"leanclust_zaman_serisi"` sınıflı nesne.
#' @param goster_ogrenci Mantıksal. `TRUE` (varsayılan) ise bireysel
#'   öğrenci çizgileri çizilir. `FALSE` ise yalnızca küme ortalamaları
#'   gösterilir.
#' @param alpha_ogrenci Bireysel öğrenci çizgilerinin saydamlığı
#'   (`[0, 1]`). Varsayılan `0.35`.
#' @param cizgi_kalinlik_ortalama Küme ortalama çizgisinin kalınlığı.
#'   Varsayılan `1.4`.
#' @param goster_nokta Mantıksal. `TRUE` (varsayılan) ise küme
#'   ortalama trajektörünün her zaman noktasına nokta eklenir.
#' @param baslik Grafik başlığı. Varsayılan
#'   `"Öğrenci Performans Trajektörleri (DTW + Ward.D2)"`.
#' @param y_etiket Y ekseni etiketi. Varsayılan `"Puan"`.
#' @param renkler Karakter vektörü. Uzunluğu küme sayısına eşit
#'   olmalıdır. `NULL` (varsayılan) verilirse dahili bir palet
#'   kullanılır.
#'
#' @return Bir `ggplot` nesnesi. `+` operatörüyle ek katmanlar
#'   eklenebilir.
#'
#' @seealso [ogrenci_zaman_serisi_kumele()] kümeleme hesabı;
#'   [kumele_grafik()] k-means tabanlı saçılma grafiği.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("dtw", quietly = TRUE) &&
#'     exists("ogrenci_notlari") &&
#'     all(c("quiz1", "quiz2", "vize", "final") %in%
#'         names(ogrenci_notlari))) {
#'   data(ogrenci_notlari)
#'   sonuc <- ogrenci_zaman_serisi_kumele(
#'     veri               = ogrenci_notlari,
#'     zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
#'     k                  = 3L,
#'     etiketler          = c("Dusuk", "Orta", "Yuksek")
#'   )
#'   trajektor_grafik(sonuc)
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs
#'   theme_minimal scale_color_manual
#' @importFrom rlang .data
#' @export
trajektor_grafik <- function(
    sonuc,
    goster_ogrenci          = TRUE,
    alpha_ogrenci           = 0.35,
    cizgi_kalinlik_ortalama = 1.4,
    goster_nokta            = TRUE,
    baslik                  = "Ogrenci Performans Trajektorleri (DTW + Ward.D2)",
    y_etiket                = "Puan",
    renkler                 = NULL
) {

  # ---- 1. Girdi dogrulama --------------------------------------------
  if (!inherits(sonuc, "leanclust_zaman_serisi")) {
    stop("`sonuc`, `ogrenci_zaman_serisi_kumele()` ciktisi olmalidir.",
         call. = FALSE)
  }

  if (!is.logical(goster_ogrenci) || length(goster_ogrenci) != 1L ||
      is.na(goster_ogrenci)) {
    stop("`goster_ogrenci` tek bir TRUE/FALSE degeri olmalidir.",
         call. = FALSE)
  }

  if (!is.numeric(alpha_ogrenci) || length(alpha_ogrenci) != 1L ||
      is.na(alpha_ogrenci) || alpha_ogrenci < 0 || alpha_ogrenci > 1) {
    stop("`alpha_ogrenci` 0 ile 1 arasinda tek bir sayi olmalidir.",
         call. = FALSE)
  }

  if (!is.numeric(cizgi_kalinlik_ortalama) ||
      length(cizgi_kalinlik_ortalama) != 1L ||
      is.na(cizgi_kalinlik_ortalama) || cizgi_kalinlik_ortalama <= 0) {
    stop("`cizgi_kalinlik_ortalama` pozitif bir sayi olmalidir.",
         call. = FALSE)
  }

  zaman_degiskenleri <- sonuc$argumanlar$zaman_degiskenleri
  etiket_seviyeleri  <- levels(sonuc$veri$kume_etiketi)
  k_sayi             <- length(etiket_seviyeleri)

  if (is.null(renkler)) {
    palet <- c("#d7191c", "#fdae61", "#2c7bb6", "#1a9641",
               "#756bb1", "#e7298a", "#66a61e", "#e6ab02")
    renkler <- palet[seq_len(k_sayi)]
  }
  if (length(renkler) != k_sayi) {
    stop(sprintf(
      "`renkler` uzunlugu (%d), kume sayisi (%d) ile esit olmalidir.",
      length(renkler), k_sayi), call. = FALSE)
  }
  names(renkler) <- etiket_seviyeleri

  # ---- 2. Veriyi uzun forma cevir ------------------------------------
  veri <- sonuc$veri
  # Satir kimligi; ogrenci_id yoksa satir numarasina dus.
  if ("ogrenci_id" %in% names(veri)) {
    satir_id <- as.character(veri$ogrenci_id)
  } else {
    satir_id <- as.character(seq_len(nrow(veri)))
  }

  uzun_ogr <- do.call(rbind, lapply(zaman_degiskenleri, function(z) {
    data.frame(
      id           = satir_id,
      zaman        = factor(z, levels = zaman_degiskenleri),
      puan         = as.numeric(veri[[z]]),
      kume_etiketi = veri$kume_etiketi,
      stringsAsFactors = FALSE
    )
  }))

  merkezler <- sonuc$merkezler
  uzun_mrk  <- do.call(rbind, lapply(zaman_degiskenleri, function(z) {
    data.frame(
      zaman        = factor(z, levels = zaman_degiskenleri),
      puan         = as.numeric(merkezler[[z]]),
      kume_etiketi = factor(merkezler$etiket, levels = etiket_seviyeleri),
      stringsAsFactors = FALSE
    )
  }))

  # ---- 3. Grafigi kur -------------------------------------------------
  p <- ggplot2::ggplot()

  if (isTRUE(goster_ogrenci)) {
    p <- p +
      ggplot2::geom_line(
        data = uzun_ogr,
        mapping = ggplot2::aes(
          x     = .data[["zaman"]],
          y     = .data[["puan"]],
          group = .data[["id"]],
          color = .data[["kume_etiketi"]]
        ),
        alpha = alpha_ogrenci,
        linewidth = 0.5
      )
  }

  p <- p +
    ggplot2::geom_line(
      data = uzun_mrk,
      mapping = ggplot2::aes(
        x     = .data[["zaman"]],
        y     = .data[["puan"]],
        group = .data[["kume_etiketi"]],
        color = .data[["kume_etiketi"]]
      ),
      linewidth = cizgi_kalinlik_ortalama
    )

  if (isTRUE(goster_nokta)) {
    p <- p +
      ggplot2::geom_point(
        data = uzun_mrk,
        mapping = ggplot2::aes(
          x     = .data[["zaman"]],
          y     = .data[["puan"]],
          color = .data[["kume_etiketi"]]
        ),
        size = 2.8
      )
  }

  p <- p +
    ggplot2::scale_color_manual(values = renkler, drop = FALSE) +
    ggplot2::labs(
      title = baslik,
      x     = "Sinav (kronolojik)",
      y     = y_etiket,
      color = "Kume"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  p
}
