#' Temizlenmis Palmer Penguenleri Verisi
#'
#' @description
#' palmerpenguins paketindeki orijinal \code{penguins} verisinden eksik
#' degerler (NA) temizlenerek elde edilmis alt kumedir. Egitim amacli
#' veri madenciligi ve kumeleme analizleri icin hazirlanmistir.
#'
#' @format 333 satir ve 8 degiskenden olusan bir \code{data.frame}:
#' \describe{
#'   \item{species}{Penguen turu (\code{factor}): Adelie, Chinstrap, Gentoo.}
#'   \item{island}{Yasadigi ada (\code{factor}): Biscoe, Dream, Torgersen.}
#'   \item{bill_length_mm}{Gaga uzunlugu, milimetre cinsinden (\code{numeric}).}
#'   \item{bill_depth_mm}{Gaga derinligi, milimetre cinsinden (\code{numeric}).}
#'   \item{flipper_length_mm}{Yuzgec uzunlugu, milimetre cinsinden (\code{integer}).}
#'   \item{body_mass_g}{Vucut kutlesi, gram cinsinden (\code{integer}).}
#'   \item{sex}{Cinsiyet (\code{factor}): female, male.}
#'   \item{year}{Gozlem yili (\code{integer}).}
#' }
#'
#' @source Gorman KB, Williams TD, Fraser WR (2014).
#'   \emph{PLOS ONE}, 9(3): e90081.
#'
#' @examples
#' data(temiz_penguinler)
#' head(temiz_penguinler)
#' summary(temiz_penguinler)
"temiz_penguinler"
