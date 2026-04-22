# leanclust

<!-- badges: start -->
<!-- badges: end -->

Student Performance Clustering and Classification — IST 5560 ders
paketi.

`leanclust`, eğitim veri madenciliği için hazırlanmış *lean* bir R
paketidir. Öğrenci performansını kümeleme, kural tabanlı
sınıflandırma, aykırı değer tespiti ve raporlama için birbirini
tamamlayan fonksiyonlar sunar.

## Kurulum

Geliştirme sürümünü GitHub üzerinden kurmak için:

```r
# install.packages("devtools")
devtools::install_github("SETOsfk/leanclust", build_vignettes = TRUE)
```

## Hızlı Başlangıç

```r
library(leanclust)

data(ogrenci_notlari)

# 1. Aykiri deger kontrolu
tespit_et_outlier(ogrenci_notlari$ortalama)

# 2. Kural tabanli harf notu
performans_sinifi_ata(ogrenci_notlari$ortalama)

# 3. k-means ile kumeleme
sonuc <- ogrenci_performans_kumele(
  ogrenci_notlari,
  degiskenler = c("vize", "final"),
  k           = 3,
  etiketler   = c("Dusuk", "Orta", "Yuksek"),
  seed        = 42
)

# 4. Yayin kaliteli gt tablosu
ozet_kumele(sonuc)

# 5. Gorsellestirme
kumele_grafik(sonuc, x = "vize", y = "final")
```

## Ana Fonksiyonlar

| Fonksiyon | Amaç |
|---|---|
| `ogrenci_performans_kumele()` | k-means tabanlı öğrenci kümeleme |
| `performans_sinifi_ata()` | Kural veya kantil tabanlı sınıflandırma |
| `ozet_kumele()` | Küme bazlı `gt` özet tablosu |
| `kumele_grafik()` | `ggplot2` küme dağılım grafiği |
| `tespit_et_outlier()` | Tukey IQR aykırı değer tespiti |
| `aykiri_deger_analizi()` | Aykırı değer detaylı rapor |
| `penguen_grafik()` | `palmerpenguins` için demo grafik |

## Veri Setleri

- `ogrenci_notlari` — Sentetik öğrenci notları (30 gözlem, 6 değişken)
- `temiz_penguinler` — `palmerpenguins::penguins` alt kümesi (NA
  temizlenmiş)

## Vignette

Tam kullanım rehberi için:

```r
vignette("leanclust-rehber", package = "leanclust")
```

## Geliştirici Notları

İlk klonladıktan sonra:

```r
# Roxygen etiketlerinden NAMESPACE ve Rd dosyalarini yenile
devtools::document()

# Sentetik veri setini uret (data-raw betigi)
source("data-raw/ogrenci_notlari.R")

# Test, kontrol ve vignette olusturma
devtools::test()
devtools::check()
devtools::build_vignettes()
```

## Lisans

MIT © Sertan Safak
