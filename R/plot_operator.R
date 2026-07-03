#' Plot Tren Konsumsi Energi Harian
#'
#' @description Menghasilkan grafik tren runtun waktu untuk kebutuhan Dashboard Monitoring Operator.
#' @param data Data.frame log transaksi EV.
#' @return Objek grafik ggplot2.
#' @import ggplot2
#' @importFrom stats aggregate
#' @export
plot_energy_trend <- function(data) {
  tren_harian <- stats::aggregate(konsumsi_kwh ~ tanggal + stasiun, data = data, sum)
  colnames(tren_harian) <- c("tanggal", "stasiun", "energi_harian")

  ggplot2::ggplot(tren_harian, ggplot2::aes(x = tanggal, y = energi_harian, color = stasiun)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = "Tren Konsumsi Energi Harian per Stasiun",
                  x = "Tanggal", y = "Total Konsumsi Energi (kWh)") +
    ggplot2::theme_minimal()
}
