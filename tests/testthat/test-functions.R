test_that("Fungsi-fungsi RPackage Greencharger berjalan dengan valid", {
  sample_data <- generate_ev_data(bulan_tahun = "2026-06")
  expect_s3_class(sample_data, "data.frame")
  expect_true(nrow(sample_data) > 0)

  kpi <- calculate_stasiun_kpi(sample_data)
  expect_s3_class(kpi, "data.frame")
  expect_true("Total_Pendapatan_IDR" %in% colnames(kpi))

  prediksi <- predict_next_month_energy(sample_data)
  expect_s3_class(prediksi$model, "lm")
  expect_true(prediksi$total_prediksi_kWh > 0)
})

