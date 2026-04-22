#' @title Aykiri Deger Analizi (IQR Yontemi)
#'
#' @description
#' Sayisal bir vektordeki aykiri degerleri Tukey'nin IQR yontemiyle tespit eder,
#' ozet istatistikleri gt tablosu olarak raporlar ve istege bagli boxplot cizer.
#'
#' @param veri Sayisal bir vektor (\code{numeric}).
#' @param grafik Mantiksal (\code{logical}, varsayilan: \code{FALSE}).
#'   \code{TRUE} verilirse ham ve temizlenmis verinin boxplot'i cizilir.
#'
#' @return Asagidaki elemanlari iceren bir \code{list}:
#'   \itemize{
#'     \item \code{temiz_veri}: Aykiri degerler cikarilmis vektor.
#'     \item \code{aykiri_degerler}: Tespit edilen aykiri degerler.
#'     \item \code{sinirlar}: Q1, Q3, IQR, alt ve ust sinir degerleri.
#'   }
#'
#' @export
#'
#' @importFrom stats quantile
#' @importFrom graphics par boxplot
#' @importFrom gt gt tab_header cols_label
#'
#' @examples
#' set.seed(42)
#' veri <- c(rnorm(50, mean = 100, sd = 10), 150, 200, 45, 30)
#' sonuc <- aykiri_deger_analizi(veri, grafik = FALSE)
aykiri_deger_analizi <- function(veri, grafik = FALSE) {

  # 1. VERI KONTROLU
  if (!is.numeric(veri)) {
    stop("Hata: Lutfen sayisal bir veri giriniz!")
  }

  if (any(is.na(veri))) {
    warning("Uyari: Veride eksik degerler bulundu, bunlar analiz disi birakildi.")
    veri <- veri[!is.na(veri)]
  }

  # 2. AYKIRI DEGER HESAPLAMASI
  Q1        <- stats::quantile(veri, 0.25)
  Q3        <- stats::quantile(veri, 0.75)
  IQR       <- Q3 - Q1
  alt_sinir <- Q1 - 1.5 * IQR
  ust_sinir <- Q3 + 1.5 * IQR

  # 3. AYKIRI DEGERLERI TESPIT ET
  aykiri_indeks   <- which(veri < alt_sinir | veri > ust_sinir)
  aykiri_degerler <- veri[aykiri_indeks]
  # Not: length(aykiri_indeks) == 0 iken veri[-integer(0)] bos vektor
  # doner; o yuzden acik kontrol yapiyoruz.
  temiz_veri      <- if (length(aykiri_indeks) > 0L) {
    veri[-aykiri_indeks]
  } else {
    veri
  }

  # 4. OZET TABLO
  ozet_df <- data.frame(
    Metrik = c(
      "Toplam Gozlem", "Temiz Gozlem", "Aykiri Deger Sayisi",
      "Ortalama (Ham)", "Ortalama (Temiz)",
      "Q1", "Q3", "IQR", "Alt Sinir", "Ust Sinir"
    ),
    Deger = c(
      length(veri), length(temiz_veri), length(aykiri_degerler),
      round(mean(veri), 4), round(mean(temiz_veri), 4),
      round(Q1, 4), round(Q3, 4), round(IQR, 4),
      round(alt_sinir, 4), round(ust_sinir, 4)
    )
  )

  print(
    ozet_df |>
      gt::gt() |>
      gt::tab_header(title = "Aykiri Deger Analiz Raporu") |>
      gt::cols_label(Metrik = "Metrik", Deger = "Deger")
  )

  # 5. AYKIRI DEGERLER TABLOSU
  if (length(aykiri_degerler) > 0) {
    aykiri_df <- data.frame(
      Sira  = seq_along(aykiri_degerler),
      Deger = round(aykiri_degerler, 4),
      Konum = ifelse(aykiri_degerler < alt_sinir, "Alt Sinir Alti", "Ust Sinir Ustu"),
      Sapma = round(ifelse(aykiri_degerler < alt_sinir,
                           alt_sinir - aykiri_degerler,
                           aykiri_degerler - ust_sinir), 4)
    )

    print(
      aykiri_df |>
        gt::gt() |>
        gt::tab_header(title = "Tespit Edilen Aykiri Degerler") |>
        gt::cols_label(
          Sira  = "Sira",
          Deger = "Deger",
          Konum = "Konum",
          Sapma = "Sinirdan Sapma"
        )
    )
  }

  # 6. BOXPLOT
  if (grafik == TRUE) {
    graphics::par(mfrow = c(1, 2))
    graphics::boxplot(veri, main = "Ham Veri", ylab = "Deger", col = "lightblue")
    graphics::boxplot(temiz_veri, main = "Temizlenmis Veri", ylab = "Deger", col = "lightgreen")
  }

  # 7. DONDUR
  return(list(
    temiz_veri      = temiz_veri,
    aykiri_degerler = aykiri_degerler,
    sinirlar        = list(Q1 = Q1, Q3 = Q3, IQR = IQR,
                           alt_sinir = alt_sinir, ust_sinir = ust_sinir)
  ))
}
