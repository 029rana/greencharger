#' Prediksi Kebutuhan Energi Bulan Berikutnya
#'
#' @description Membangun model regresi linear untuk memproyeksikan akumulasi beban daya 30 hari ke depan.
#' @param data Data.frame transaksi EV.
#' @return Sebuah list berisi objek model lm, ringkasan prediksi, dan batas interval kepercayaan.
#' @importFrom stats lm predict aggregate
#' @export
predict_next_month_energy <- function(data) {
  if (nrow(data) == 0) return(list(total_prediksi_kWh = 0))

  harian <- stats::aggregate(konsumsi_kwh ~ tanggal, data = data, sum)
  colnames(harian) <- c("tanggal", "total_kwh")
  harian$indeks_hari <- 1:nrow(harian)

  model_linier <- stats::lm(total_kwh ~ indeks_hari, data = harian)

  total_hari <- nrow(harian)
  hari_depan <- data.frame(indeks_hari = (total_hari + 1):(total_hari + 30))

  prediksi_interval <- stats::predict(model_linier, newdata = hari_depan, interval = "confidence")

  return(list(
    model = model_linier,
    total_prediksi_kWh = sum(prediksi_interval[, "fit"]),
    batas_bawah = sum(prediksi_interval[, "lwr"]),
    batas_atas = sum(prediksi_interval[, "upr"])
  ))
}
