#' Mengelola Tarif Pengisian Stasiun
#'
#' Fungsi ini digunakan oleh Operator Stasiun untuk menetapkan atau
#' memperbarui tarif pengisian energi (Rp per kWh) pada satu atau
#' beberapa stasiun pengisian kendaraan listrik. Fungsi ini merepresentasikan
#' use case "Mengelola Tarif Pengisian" pada diagram Sistem Pengisian
#' Energi Kendaraan Listrik (EV Charging System).
#'
#' @param nama_stasiun Character vector. Nama satu atau lebih stasiun yang
#'   akan diperbarui tarifnya, misalnya "Dumai" atau c("Dumai", "Duri").
#' @param tarif_baru Numeric vector. Nilai tarif baru (Rp/kWh) untuk setiap
#'   stasiun pada \code{nama_stasiun}. Panjang harus sama dengan
#'   \code{nama_stasiun}, atau satu nilai tunggal untuk diterapkan ke semua
#'   stasiun yang disebutkan.
#' @param tarif_lama Data frame opsional berisi kolom \code{stasiun} dan
#'   \code{tarif} yang merepresentasikan tarif saat ini. Jika NULL, fungsi
#'   akan membuat tabel tarif baru dari awal.
#'
#' @return Data frame berisi kolom \code{stasiun}, \code{tarif_kwh}, dan
#'   \code{tanggal_update} yang mencerminkan tarif terbaru setiap stasiun.
#'
#' @examples
#' # Menetapkan tarif baru untuk dua stasiun
#' set_tarif_stasiun(
#'   nama_stasiun = c("Dumai", "Duri"),
#'   tarif_baru   = c(2500, 2350)
#' )
#'
#' # Memperbarui tarif dari tabel yang sudah ada
#' tarif_awal <- data.frame(
#'   stasiun = c("Dumai", "Duri", "Panam"),
#'   tarif   = c(2400, 2300, 2450)
#' )
#' set_tarif_stasiun("Duri", 2500, tarif_lama = tarif_awal)
#'
#' @export
set_tarif_stasiun <- function(nama_stasiun, tarif_baru, tarif_lama = NULL) {

  # Validasi input dasar
  if (missing(nama_stasiun) || missing(tarif_baru)) {
    stop("Argumen 'nama_stasiun' dan 'tarif_baru' wajib diisi.")
  }

  if (!is.numeric(tarif_baru) || any(tarif_baru <= 0)) {
    stop("Tarif harus berupa angka positif (Rp per kWh).")
  }

  # Jika tarif_baru diberikan sebagai satu nilai, terapkan ke semua stasiun
  if (length(tarif_baru) == 1 && length(nama_stasiun) > 1) {
    tarif_baru <- rep(tarif_baru, length(nama_stasiun))
  }

  if (length(nama_stasiun) != length(tarif_baru)) {
    stop("Panjang 'nama_stasiun' dan 'tarif_baru' harus sama.")
  }

  update_df <- data.frame(
    stasiun        = nama_stasiun,
    tarif_kwh      = tarif_baru,
    tanggal_update = Sys.Date(),
    stringsAsFactors = FALSE
  )

  # Jika belum ada tabel tarif lama, langsung kembalikan tabel baru
  if (is.null(tarif_lama)) {
    return(update_df)
  }

  # Validasi struktur tarif_lama
  if (!all(c("stasiun", "tarif") %in% names(tarif_lama))) {
    stop("'tarif_lama' harus memiliki kolom 'stasiun' dan 'tarif'.")
  }

  # Gabungkan: perbarui stasiun yang ada, tambahkan stasiun baru jika perlu
  hasil <- tarif_lama
  names(hasil)[names(hasil) == "tarif"] <- "tarif_kwh"
  hasil$tanggal_update <- as.Date(NA)

  for (i in seq_len(nrow(update_df))) {
    idx <- which(hasil$stasiun == update_df$stasiun[i])
    if (length(idx) > 0) {
      hasil$tarif_kwh[idx]      <- update_df$tarif_kwh[i]
      hasil$tanggal_update[idx] <- update_df$tanggal_update[i]
    } else {
      hasil <- rbind(hasil, update_df[i, ])
    }
  }

  rownames(hasil) <- NULL
  hasil
}
