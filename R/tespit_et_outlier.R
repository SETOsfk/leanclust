#' @title Aykiri Deger Tespiti (Tukey IQR Yontemi)
#'
#' @description
#' Bu fonksiyon, Tukey'nin ceyrekler arasi aciklik (IQR) yontemini kullanarak
#' sayisal bir vektordeki aykiri degerleri tespit eder. Alt ve ust sinirlar,
#' kullanici tarafindan belirlenebilen bir esik katsayisi (k) ile hesaplanir.
#'
#' @param x Sayisal bir vektor (\code{numeric}). Aykiri deger analizi
#'   uygulanacak veriyi icermelidir.
#' @param k Esik katsayisi (\code{numeric}, varsayilan: \code{1.5}).
#'   IQR ile carpilarak alt ve ust sinirlari belirler.
#'   Tukey'nin standart onerisine gore varsayilan deger 1.5'tir.
#'
#' @return Asagidaki elemanlari iceren bir \code{list}:
#'   \itemize{
#'     \item \code{alt_sinir}: Hesaplanan alt sinir degeri.
#'     \item \code{ust_sinir}: Hesaplanan ust sinir degeri.
#'     \item \code{aykiri_degerler}: Vektordeki aykiri degerlerin kendisi.
#'     \item \code{indisler}: Aykiri degerlerin vektordeki indeks konumlari.
#'   }
#'
#' @export
#'
#' @seealso \code{\link[stats]{quantile}}, \code{\link[graphics]{boxplot}}
#'
#' @importFrom stats quantile
#'
#' @examples
#' # 1. Normal dagilimdan uretilmis temiz veri
#' set.seed(123)
#' temiz_veri <- rnorm(100, mean = 50, sd = 5)
#' tespit_et_outlier(temiz_veri)
#'
#' # 2. Kesinlikle aykiri deger iceren veri
#' kirli_veri <- c(rnorm(50, mean = 50, sd = 5), 150, 200, -80)
#' tespit_et_outlier(kirli_veri, k = 1.5)
tespit_et_outlier <- function(x, k = 1.5) {
  Q1        <- stats::quantile(x, 0.25, na.rm = TRUE)
  Q3        <- stats::quantile(x, 0.75, na.rm = TRUE)
  IQR_val   <- Q3 - Q1
  alt_sinir <- Q1 - k * IQR_val
  ust_sinir <- Q3 + k * IQR_val

  indisler        <- which(x < alt_sinir | x > ust_sinir)
  aykiri_degerler <- x[indisler]

  return(list(
    alt_sinir       = alt_sinir,
    ust_sinir       = ust_sinir,
    aykiri_degerler = aykiri_degerler,
    indisler        = indisler
  ))
}
