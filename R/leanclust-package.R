#' leanclust: Student Performance Clustering and Classification
#'
#' `leanclust`, eğitim veri madenciliği (educational data mining)
#' için hazırlanmış, ince (*lean*) bir R paketidir.
#' Öğrenci performansını kümeleme, kural tabanlı sınıflandırma,
#' aykırı değer tespiti ve özetleme için birbirini tamamlayan
#' fonksiyonlar sunar.
#'
#' @section Ana fonksiyonlar:
#' * [ogrenci_performans_kumele()]: k-means tabanlı öğrenci kümeleme
#' * [ogrenci_zaman_serisi_kumele()]: DTW + Ward.D2 zaman serisi kümeleme
#' * [performans_sinifi_ata()]: Kural tabanlı performans sınıflandırması
#' * [ozet_kumele()]: Küme bazlı özet tablosu (`gt`)
#' * [kumele_grafik()]: Küme görselleştirmesi (`ggplot2`)
#' * [tespit_et_outlier()]: Tukey IQR aykırı değer tespiti
#' * [aykiri_deger_analizi()]: Aykırı değer özet raporu
#'
#' @section Veri setleri:
#' * [ogrenci_notlari]: Sentetik öğrenci notları (30 gözlem)
#' * [temiz_penguinler]: Eksik değeri temizlenmiş palmerpenguins alt kümesi
#'
#' @keywords internal
"_PACKAGE"

## Tidy evaluation — `R CMD check` "no visible binding" uyarilarini engellemek
## icin kullanilan global degiskenler.
utils::globalVariables(c(
  "species", "bill_length_mm",
  "kume", "kume_etiketi", "ortalama", "grup"
))
