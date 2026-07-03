#' Generate Data Simulasi Transaksi EV
#'
#' @description Fungsi untuk menghasilkan data tiruan transaksi pengisian daya listrik sesuai dengan skenario stasiun RANA.
#' @param bulan_tahun String format "YYYY-MM" (default = "2026-06").
#' @param seed Angka acuan untuk generator bilangan acak (default = 2026).
#' @return Sebuah data.frame berisi log transaksi pengisian EV.
#' @importFrom stats rbeta rnorm
#' @importFrom lubridate ceiling_date days
#' @export
generate_ev_data <- function(bulan_tahun = "2026-06", seed = 2026) {
  set.seed(seed)
  tarif_per_kwh <- 2466
  biaya_admin <- 5000

  daftar_stasiun <- c("Stasiun Sudirman", "Stasiun Tuanku Tambusai", "Stasiun Panam", "Stasiun Duri", "Stasiun Dumai")
  tanggal_mulai <- as.Date(paste0(bulan_tahun, "-01"))
  tanggal_selesai <- lubridate::ceiling_date(tanggal_mulai, "month") - lubridate::days(1)
  deret_tanggal <- seq(tanggal_mulai, tanggal_selesai, by = "day")
  total_hari <- length(deret_tanggal)
  n_transaksi <- length(daftar_stasiun) * total_hari * 12

  data <- data.frame(
    id_transaksi = paste0("TX-", 10000 + 1:n_transaksi),
    stasiun = sample(daftar_stasiun, n_transaksi, replace = TRUE),
    tanggal = sample(deret_tanggal, n_transaksi, replace = TRUE),
    jam = round(stats::rbeta(n_transaksi, 5, 3) * 23)
  )

  data$konsumsi_kwh <- pmax(stats::rnorm(n_transaksi, 35, 8), 5)
  data$pendapatan <- round(data$konsumsi_kwh * tarif_per_kwh + biaya_admin)

  return(data)
}
