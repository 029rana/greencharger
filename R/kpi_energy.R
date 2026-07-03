#' Hitung Kinerja Operasional Per Stasiun
#'
#' @description Mengklasifikasikan data transaksi untuk memantau total energi, sesi transaksi, dan finansial.
#' @param data Data.frame log transaksi hasil dari generate_ev_data().
#' @return Sebuah data.frame ringkasan statistik per stasiun.
#' @importFrom magrittr %>%
#' @export
calculate_stasiun_kpi <- function(data) {
  if (nrow(data) == 0) return(data.frame())

  ringkasan <- data %>%
    dplyr::group_by(stasiun) %>%
    dplyr::summarise(
      Total_Transaksi = dplyr::n(),
      Total_Konsumsi_kWh = round(sum(konsumsi_kwh), 2),
      Total_Pendapatan_IDR = round(sum(pendapatan), 0),
      Rata_Rata_kWh = round(mean(konsumsi_kwh), 2),
      .groups = "drop"
    )
  return(ringkasan)
}
