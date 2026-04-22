#' Performans Sınıfı Ata (Kural veya Kantil Tabanlı)
#'
#' Sayısal bir puan vektörünü, iki farklı yöntemden birini kullanarak
#' performans sınıflarına (faktör) atar.
#'
#' * `yontem = "kural"`: Kullanıcı tarafından verilen sabit kesim
#'   noktalarına (`kesim_noktalari`) göre sınıflandırır. Geleneksel
#'   harf notu sistemi için uygundur.
#' * `yontem = "kantil"`: Verinin kendi dağılımına dayalı, eşit
#'   yoğunluklu gruplara böler (örn. çeyrekler, beşli dilimler).
#'   Göreceli performans değerlendirmesi için uygundur.
#'
#' @param puan Sayısal vektör (`numeric`). Sınıflandırılacak puanlar.
#' @param yontem Sınıflandırma yöntemi. `"kural"` (varsayılan) veya
#'   `"kantil"`.
#' @param kesim_noktalari Sayısal vektör. `yontem = "kural"` için
#'   kullanılan iç kesim noktaları. Varsayılan `c(50, 60, 70, 80, 90)`
#'   beş kesim noktası oluşturur; sonuçta 6 sınıf üretilir.
#' @param etiketler Karakter vektör. Her bir sınıfın etiketi.
#'   Uzunluğu, oluşturulan sınıf sayısına eşit olmalıdır.
#'   `yontem = "kural"` için varsayılan
#'   `c("FF","DD","CC","BB","BA","AA")`; `yontem = "kantil"` için
#'   `n_sinif` uzunluğunda otomatik üretilir.
#' @param n_sinif `yontem = "kantil"` için sınıf sayısı. Varsayılan 4
#'   (çeyrekler).
#' @param na.rm Mantıksal. `TRUE` ise `puan` içindeki `NA`'lar
#'   sınıflandırılmadan `NA` olarak döner.
#'
#' @return `puan` ile aynı uzunlukta, sıralı bir `factor` nesnesi.
#'   Seviyeler `etiketler` argümanıyla aynı sırada olur.
#'
#' @export
#'
#' @importFrom stats quantile
#'
#' @examples
#' # 1) Kural tabanlı harf notu ataması
#' notlar <- c(45, 62, 77, 88, 95, 51)
#' performans_sinifi_ata(notlar)
#'
#' # 2) Kendi kesim noktalarınız
#' performans_sinifi_ata(
#'   notlar,
#'   kesim_noktalari = c(60, 80),
#'   etiketler       = c("Başarısız", "Orta", "Başarılı")
#' )
#'
#' # 3) Kantil tabanlı (göreceli) sınıflandırma
#' set.seed(1)
#' puanlar <- rnorm(100, mean = 70, sd = 10)
#' tablo   <- performans_sinifi_ata(
#'   puanlar,
#'   yontem  = "kantil",
#'   n_sinif = 4
#' )
#' table(tablo)
performans_sinifi_ata <- function(
    puan,
    yontem          = c("kural", "kantil"),
    kesim_noktalari = c(50, 60, 70, 80, 90),
    etiketler       = NULL,
    n_sinif         = 4,
    na.rm           = TRUE
) {

  # --- Girdi kontrolu ---------------------------------------------------
  if (!is.numeric(puan)) {
    stop("`puan` sayisal bir vektor olmalidir.", call. = FALSE)
  }

  yontem <- match.arg(yontem)

  # --- Yonteme gore kesim noktalari ve etiketler ------------------------
  if (yontem == "kural") {

    if (!is.numeric(kesim_noktalari) || length(kesim_noktalari) < 1L) {
      stop("`kesim_noktalari` en az bir sayisal deger icermelidir.",
           call. = FALSE)
    }
    kesim_noktalari <- sort(unique(kesim_noktalari))

    # 5 ic kesim noktasi -> 6 sinif (FF/DD/CC/BB/BA/AA)
    n <- length(kesim_noktalari) + 1L

    if (is.null(etiketler)) {
      if (n == 6L) {
        etiketler <- c("FF", "DD", "CC", "BB", "BA", "AA")
      } else {
        etiketler <- paste0("Sinif_", seq_len(n))
      }
    }

    if (length(etiketler) != n) {
      stop(
        sprintf(
          "`etiketler` uzunlugu (%d), olusturulan sinif sayisina (%d) esit olmalidir.",
          length(etiketler), n
        ),
        call. = FALSE
      )
    }

    breaks <- c(-Inf, kesim_noktalari, Inf)
    out <- cut(
      puan,
      breaks         = breaks,
      labels         = etiketler,
      right          = FALSE,
      include.lowest = TRUE,
      ordered_result = TRUE
    )

  } else { # yontem == "kantil"

    if (!is.numeric(n_sinif) || length(n_sinif) != 1L || n_sinif < 2L) {
      stop("`n_sinif` >= 2 olan tek bir tamsayi olmalidir.", call. = FALSE)
    }
    n_sinif <- as.integer(n_sinif)

    if (is.null(etiketler)) {
      etiketler <- paste0("Q", seq_len(n_sinif))
    }
    if (length(etiketler) != n_sinif) {
      stop(
        sprintf(
          "`etiketler` uzunlugu (%d), `n_sinif` (%d) ile esit olmalidir.",
          length(etiketler), n_sinif
        ),
        call. = FALSE
      )
    }

    probs <- seq(0, 1, length.out = n_sinif + 1L)
    breaks <- stats::quantile(puan, probs = probs, na.rm = na.rm,
                              names = FALSE)

    if (anyDuplicated(breaks)) {
      warning(
        "Kantil kesim noktalari tekrarliyor (yogun atanmis puanlar). ",
        "Sinif sayisini (`n_sinif`) azaltmayi deneyin.",
        call. = FALSE
      )
      breaks <- unique(breaks)
      etiketler <- etiketler[seq_len(length(breaks) - 1L)]
    }

    out <- cut(
      puan,
      breaks         = breaks,
      labels         = etiketler,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  }

  if (!na.rm && anyNA(puan)) {
    # NA puanlar otomatik NA faktore doner - davranis degismez
  }

  out
}
