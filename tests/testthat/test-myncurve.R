test_that("mu is stored correctly", {
  result <- myncurve(10,4,12)
  expect_equal(result$mu, 10)
})

test_that("sigma is stored correctly", {
  result <- myncurve(10,4,12)
  expect_equal(result$sigma, 4)
})

test_that("probability is correct", {
  result <- myncurve(0,1,0)
  expect_equal(result$probability, 0.5)
})
