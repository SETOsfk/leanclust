#' @title Penguen Turlerine Gore Gaga Uzunlugu Dagilimi
#'
#' @description
#' palmerpenguins paketindeki penguins verisini kullanarak tur bazli
#' gaga uzunlugu (bill_length_mm) dagilimini ggplot2 ile gorsellestirir.
#'
#' @return Bir \code{ggplot} nesnesi.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme_minimal
#'
#' @examples
#' \dontrun{
#'   penguen_grafik()
#' }
penguen_grafik <- function() {

  species <- bill_length_mm <- NULL  # R CMD check icin

  if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
    stop(
      "Hata: 'palmerpenguins' paketi gereklidir.",
      " Yuklemek icin: install.packages('palmerpenguins')"
    )
  }

  penguins <- palmerpenguins::penguins

  ggplot2::ggplot(
    data = penguins,
    ggplot2::aes(x = species, y = bill_length_mm, fill = species)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::labs(
      title = "Ture Gore Gaga Uzunlugu Dagilimi",
      x     = "Tur",
      y     = "Gaga Uzunlugu (mm)",
      fill  = "Tur"
    ) +
    ggplot2::theme_minimal()
}
