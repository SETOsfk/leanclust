#' Öğrenci Performansını K-Means ile Kümele
#'
#' Bir öğrenci veri çerçevesindeki sayısal performans değişkenlerini
#' kullanarak k-means kümeleme yapar. Standartlaştırma, etiketleme ve
#' çoklu başlangıç adımlarını otomatik yönetir.
#'
#' İçeride [stats::kmeans()] çağrılır. Değişkenler varsayılan olarak
#' z-standartlaştırılır (`scale()`); bu, farklı ölçekteki puanların
#' kümelemede adil temsilini sağlar.
#'
#' Küme etiketleri, kümelerin tüm kümeleme değişkenlerinin ortalaması
#' üzerinden sıralanmasıyla atanır: en düşük ortalamalı küme ilk
#' etikete, en yüksek ortalamalı küme son etikete gider. Böylece
#' `etiketler = c("Düşük","Orta","Yüksek")` verildiğinde atamalar
#' istatistiksel olarak anlamlıdır.
#'
#' @param veri `data.frame`. Öğrenci verisi.
#' @param degiskenler Karakter vektörü. Kümelemede kullanılacak
#'   **sayısal** sütun adları. `NULL` (varsayılan) verilirse `veri`
#'   içindeki tüm sayısal sütunlar seçilir.
#' @param k Küme sayısı. Varsayılan 3.
#' @param standartlastir Mantıksal. `TRUE` (varsayılan) ise
#'   kümelemeden önce `scale()` uygulanır.
#' @param etiketler Karakter vektörü. Uzunluğu `k`'ye eşit olmalıdır.
#'   `NULL` verilirse `c("Düşük","Orta","Yüksek")` (k = 3 için) veya
#'   `Kume_1 ... Kume_k` kullanılır.
#' @param nstart [stats::kmeans()]'e iletilir. Birden fazla rastgele
#'   başlangıç için. Varsayılan 25.
#' @param seed Tekrarlanabilirlik için `set.seed()` tohumu.
#'   `NULL` verilirse tohum ayarlanmaz.
#'
#' @return Aşağıdaki öğeleri içeren bir `list`:
#' \describe{
#'   \item{`veri`}{Girdi `data.frame`'ine `kume` (`integer`) ve
#'     `kume_etiketi` (`factor`) sütunları eklenmiş hali.}
#'   \item{`kmeans`}{[stats::kmeans()] çıktısı.}
#'   \item{`merkezler`}{Orijinal ölçekte küme merkezleri
#'     (`data.frame`).}
#'   \item{`ozet`}{Küme bazlı gözlem sayısı ve her değişken için
#'     ortalama içeren `data.frame`.}
#'   \item{`argumanlar`}{Çağrıda kullanılan argümanların bir listesi.}
#' }
#'
#' @export
#'
#' @importFrom stats kmeans sd setNames
#'
#' @examples
#' data(ogrenci_notlari)
#'
#' # En basit kullanim - tum sayisal sutunlarla k = 3
#' sonuc <- ogrenci_performans_kumele(
#'   veri = ogrenci_notlari,
#'   degiskenler = c("vize", "final"),
#'   seed = 42
#' )
#' head(sonuc$veri)
#' sonuc$merkezler
#'
#' # Farkli k ve ozel etiketler
#' sonuc2 <- ogrenci_performans_kumele(
#'   veri        = ogrenci_notlari,
#'   degiskenler = c("vize", "final", "ortalama"),
#'   k           = 4,
#'   etiketler   = c("Risk", "Standart", "Iyi", "Ust"),
#'   seed        = 1
#' )
#' table(sonuc2$veri$kume_etiketi)
ogrenci_performans_kumele <- function(
    veri,
    degiskenler    = NULL,
    k              = 3L,
    standartlastir = TRUE,
    etiketler      = NULL,
    nstart         = 25L,
    seed           = NULL
) {

  # --- Girdi dogrulamalari --------------------------------------------
  if (!is.data.frame(veri)) {
    stop("`veri` bir data.frame olmalidir.", call. = FALSE)
  }
  if (nrow(veri) < 2L) {
    stop("`veri` en az 2 satir icermelidir.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || k < 2L || k != as.integer(k)) {
    stop("`k` 2 veya daha buyuk bir tamsayi olmalidir.", call. = FALSE)
  }
  k <- as.integer(k)

  if (k >= nrow(veri)) {
    stop(
      sprintf("`k` (%d), gozlem sayisindan (%d) kucuk olmalidir.",
              k, nrow(veri)),
      call. = FALSE
    )
  }

  # --- Degisken secimi ------------------------------------------------
  sayisal_sutunlar <- names(veri)[vapply(veri, is.numeric, logical(1))]

  if (is.null(degiskenler)) {
    degiskenler <- sayisal_sutunlar
    if (length(degiskenler) == 0L) {
      stop("`veri` icinde sayisal sutun bulunamadi.", call. = FALSE)
    }
  } else {
    eksik <- setdiff(degiskenler, names(veri))
    if (length(eksik) > 0L) {
      stop(
        sprintf("Su sutun(lar) `veri` icinde yok: %s",
                paste(eksik, collapse = ", ")),
        call. = FALSE
      )
    }
    sayisal_olmayan <- setdiff(degiskenler, sayisal_sutunlar)
    if (length(sayisal_olmayan) > 0L) {
      stop(
        sprintf("Su sutun(lar) sayisal degil: %s",
                paste(sayisal_olmayan, collapse = ", ")),
        call. = FALSE
      )
    }
  }

  X <- veri[, degiskenler, drop = FALSE]

  if (anyNA(X)) {
    stop(
      "Kumeleme matrisinde NA var. Once eksik deger temizlemesi yapin.",
      call. = FALSE
    )
  }

  # --- Etiketler ------------------------------------------------------
  if (is.null(etiketler)) {
    etiketler <- if (k == 3L) {
      c("Dusuk", "Orta", "Yuksek")
    } else {
      paste0("Kume_", seq_len(k))
    }
  }
  if (length(etiketler) != k) {
    stop(
      sprintf("`etiketler` uzunlugu (%d) `k` (%d) ile esit olmalidir.",
              length(etiketler), k),
      call. = FALSE
    )
  }

  # --- Standartlastirma ----------------------------------------------
  if (isTRUE(standartlastir)) {
    ortalamalar <- vapply(X, mean, numeric(1))
    sapmalar    <- vapply(X, stats::sd, numeric(1))
    if (any(sapmalar == 0)) {
      stop("Bir veya daha fazla kumeleme degiskeninin sd'si 0 (sabit sutun).",
           call. = FALSE)
    }
    X_scaled <- sweep(X, 2L, ortalamalar, FUN = "-")
    X_scaled <- sweep(X_scaled, 2L, sapmalar, FUN = "/")
  } else {
    X_scaled <- X
  }

  # --- k-means --------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  km <- stats::kmeans(as.matrix(X_scaled), centers = k, nstart = nstart)

  # Kumeleri ortalama puana gore sirala
  kume_puanlari <- vapply(seq_len(k), function(j) mean(km$centers[j, ]),
                          numeric(1))
  siralama <- order(kume_puanlari)  # kuçukten buyuge
  yeni_etiket <- stats::setNames(seq_along(siralama), siralama)
  yeni_kume_ids <- unname(yeni_etiket[as.character(km$cluster)])

  veri_cikis <- veri
  veri_cikis$kume         <- as.integer(yeni_kume_ids)
  veri_cikis$kume_etiketi <- factor(etiketler[yeni_kume_ids],
                                    levels = etiketler,
                                    ordered = TRUE)

  # --- Orijinal olcekte merkezler ------------------------------------
  if (isTRUE(standartlastir)) {
    orig_merkezler <- sweep(km$centers, 2L, sapmalar, FUN = "*")
    orig_merkezler <- sweep(orig_merkezler, 2L, ortalamalar, FUN = "+")
  } else {
    orig_merkezler <- km$centers
  }
  orig_merkezler <- orig_merkezler[siralama, , drop = FALSE]
  merkezler_df <- data.frame(
    kume         = seq_len(k),
    kume_etiketi = factor(etiketler, levels = etiketler, ordered = TRUE),
    orig_merkezler,
    check.names  = FALSE,
    stringsAsFactors = FALSE
  )

  # --- Ozet -----------------------------------------------------------
  ozet_df <- data.frame(
    kume         = seq_len(k),
    kume_etiketi = factor(etiketler, levels = etiketler, ordered = TRUE),
    n            = as.integer(table(factor(veri_cikis$kume,
                                           levels = seq_len(k)))),
    stringsAsFactors = FALSE
  )
  for (v in degiskenler) {
    ozet_df[[paste0(v, "_ort")]] <- vapply(
      seq_len(k),
      function(j) mean(veri[[v]][veri_cikis$kume == j]),
      numeric(1)
    )
  }

  list(
    veri       = veri_cikis,
    kmeans     = km,
    merkezler  = merkezler_df,
    ozet       = ozet_df,
    argumanlar = list(
      degiskenler    = degiskenler,
      k              = k,
      standartlastir = standartlastir,
      etiketler      = etiketler,
      nstart         = nstart,
      seed           = seed
    )
  )
}
