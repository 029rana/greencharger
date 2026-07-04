test_that("set_tarif_stasiun berhasil membuat tabel tarif baru", {
  hasil <- set_tarif_stasiun(
    nama_stasiun = c("Dumai", "Duri"),
    tarif_baru   = c(2500, 2350)
  )

  expect_s3_class(hasil, "data.frame")
  expect_equal(nrow(hasil), 2)
  expect_true(all(c("stasiun", "tarif_kwh", "tanggal_update") %in% names(hasil)))
  expect_equal(hasil$tarif_kwh, c(2500, 2350))
})

test_that("set_tarif_stasiun menerapkan satu nilai tarif ke banyak stasiun", {
  hasil <- set_tarif_stasiun(
    nama_stasiun = c("Dumai", "Duri", "Panam"),
    tarif_baru   = 2400
  )
  expect_equal(unique(hasil$tarif_kwh), 2400)
  expect_equal(nrow(hasil), 3)
})

test_that("set_tarif_stasiun memperbarui tarif dari tabel lama", {
  tarif_awal <- data.frame(
    stasiun = c("Dumai", "Duri", "Panam"),
    tarif   = c(2400, 2300, 2450)
  )

  hasil <- set_tarif_stasiun("Duri", 2500, tarif_lama = tarif_awal)

  expect_equal(nrow(hasil), 3)
  expect_equal(hasil$tarif_kwh[hasil$stasiun == "Duri"], 2500)
  expect_equal(hasil$tarif_kwh[hasil$stasiun == "Dumai"], 2400)
})

test_that("set_tarif_stasiun menambahkan stasiun baru jika belum ada di tabel lama", {
  tarif_awal <- data.frame(
    stasiun = c("Dumai", "Duri"),
    tarif   = c(2400, 2300)
  )

  hasil <- set_tarif_stasiun("Sudirman", 2600, tarif_lama = tarif_awal)

  expect_equal(nrow(hasil), 3)
  expect_true("Sudirman" %in% hasil$stasiun)
})

test_that("set_tarif_stasiun menolak tarif negatif atau nol", {
  expect_error(set_tarif_stasiun("Dumai", -100))
  expect_error(set_tarif_stasiun("Dumai", 0))
})

test_that("set_tarif_stasiun menolak panjang argumen yang tidak sesuai", {
  expect_error(set_tarif_stasiun(c("Dumai", "Duri"), c(2400, 2500, 2600)))
})
