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


#' Ogrenci Sinav Notlari Veri Seti
#'
#' @description
#' Bir istatistik dersindeki 30 ogrenciye ait sentetik sinav notlari.
#' Dort noktali bir performans trajektoru saglamak amaciyla kronolojik
#' sirada quiz1, quiz2, vize ve final puanlarini icerir. Betimsel
#' istatistik, not dagilimi, k-means kumeleme ve DTW tabanli zaman
#' serisi kumeleme analizleri icin ornek veri olarak tasarlanmistir.
#'
#' @format 30 satir ve 8 sutundan olusan bir \code{data.frame}:
#' \describe{
#'   \item{ogrenci_id}{\code{character}. Ogrenci kimlik kodu (orn. "STU001").}
#'   \item{quiz1}{\code{numeric}. Donem icindeki ilk quiz puani (0-100).}
#'   \item{quiz2}{\code{numeric}. Donem icindeki ikinci quiz puani (0-100).}
#'   \item{vize}{\code{numeric}. Vize sinavi puani (0-100).}
#'   \item{final}{\code{numeric}. Final sinavi puani (0-100).}
#'   \item{grup}{\code{character}. Sube bilgisi ("A", "B" veya "C").}
#'   \item{ortalama}{\code{numeric}. Agirlikli ortalama:
#'     \eqn{0.10 \cdot quiz1 + 0.10 \cdot quiz2 + 0.30 \cdot vize +
#'     0.50 \cdot final}.}
#'   \item{harf_notu}{\code{factor}. Harf notu (FF, DD, CC, BB, BA, AA).}
#' }
#'
#' @details
#' Veri \code{set.seed(20260408)} ile uretilmistir. Ogrencilerin bir
#' kismi donem boyunca yukselis (\emph{late bloomer}) ya da dusus
#' egilimi gosterir; bu trajektorler, DTW tabanli zaman serisi
#' kumelemesinin sekil hizalamasini anlamli kilar. Gercek ogrenci
#' bilgisi icermez. Uretim scripti:
#' \code{data-raw/ogrenci_notlari.R}.
#'
#' @source Sentetik veri - IST 5560 Istatistiksel Programlama ders
#'   materyali.
#'
#' @examples
#' data(ogrenci_notlari)
#' head(ogrenci_notlari, 10)
#' table(ogrenci_notlari$harf_notu)
#' # Zaman serisi goruntusu
#' matplot(t(ogrenci_notlari[, c("quiz1", "quiz2", "vize", "final")]),
#'         type = "l", lty = 1, xlab = "Sinav", ylab = "Puan")
"ogrenci_notlari"
