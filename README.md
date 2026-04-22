# leanclust

<!-- badges: start -->
<!-- badges: end -->

Student Performance Clustering and Classification — IST 5560 ders
paketi.

`leanclust`, eğitim veri madenciliği için hazırlanmış *lean* bir R
paketidir. Öğrenci performansını iki farklı motorla kümeler
(statik puan için k-means; kronolojik sınav dizisi için DTW + Ward.D2
zaman serisi kümelemesi), kural tabanlı harf notu sınıflandırması
yapar, aykırı değerleri işaretler ve sonuçları yayın kalitesinde
tablolar ile ggplot2 grafikleriyle raporlar.

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

# 5. Gorsellestirme (k-means)
kumele_grafik(sonuc, x = "vize", y = "final")

# 6. Zaman serisi kumelemesi (DTW + Ward.D2)
sonuc_dtw <- ogrenci_zaman_serisi_kumele(
  veri               = ogrenci_notlari,
  zaman_degiskenleri = c("quiz1", "quiz2", "vize", "final"),
  k                  = 3L,
  etiketler          = c("Dusuk", "Orta", "Yuksek")
)
trajektor_grafik(sonuc_dtw)
```

## Ana Fonksiyonlar

| Fonksiyon | Amaç |
|---|---|
| `ogrenci_performans_kumele()` | k-means tabanlı öğrenci kümeleme |
| `ogrenci_zaman_serisi_kumele()` | DTW + Ward.D2 zaman serisi kümelemesi |
| `performans_sinifi_ata()` | Kural veya kantil tabanlı sınıflandırma |
| `ozet_kumele()` | Küme bazlı `gt` özet tablosu |
| `kumele_grafik()` | K-means kümeleri için (x, y) saçılım grafiği |
| `trajektor_grafik()` | Zaman serisi kümeleri için trajektör grafiği |
| `tespit_et_outlier()` | Tukey IQR aykırı değer tespiti |
| `aykiri_deger_analizi()` | Aykırı değer detaylı rapor |
| `penguen_grafik()` | `palmerpenguins` için demo grafik |

## Veri Setleri

- `ogrenci_notlari` — Sentetik öğrenci notları (30 gözlem, 8 değişken:
  `ogrenci_id`, `quiz1`, `quiz2`, `vize`, `final`, `grup`, `ortalama`,
  `harf_notu`)
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
