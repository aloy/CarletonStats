## boot() ---------------------------------------------------------------

test_that("boot() single variable returns carlboot with correct attributes", {
  result <- boot(ToothGrowth$len, B = 500, seed = 1, plot.hist = FALSE)
  expect_s3_class(result, "carlboot")
  expect_length(result, 500)
  expect_equal(attr(result, "statistic"), "mean")
  expect_null(attr(result, "groups"))
})

test_that("boot() two-group returns correct observed difference", {
  result <- boot(
    ToothGrowth$len,
    ToothGrowth$supp,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  obs <- mean(ToothGrowth$len[ToothGrowth$supp == "OJ"]) -
    mean(ToothGrowth$len[ToothGrowth$supp == "VC"])
  expect_equal(attr(result, "observed"), obs)
})

test_that("boot() formula interface matches default interface", {
  r1 <- boot(
    len ~ supp,
    data = ToothGrowth,
    B = 200,
    seed = 42,
    plot.hist = FALSE
  )
  r2 <- boot(
    ToothGrowth$len,
    ToothGrowth$supp,
    B = 200,
    seed = 42,
    plot.hist = FALSE
  )
  expect_equal(as.numeric(r1), as.numeric(r2))
})

test_that("boot() print output is stable", {
  result <- boot(ToothGrowth$len, B = 500, seed = 1, plot.hist = FALSE)
  expect_snapshot(print(result))
})

## bootPaired() ----------------------------------------------------------

test_that("bootPaired() statistic attribute is 'mean'", {
  result <- bootPaired(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "mean")
})

test_that("bootPaired() formula and default interfaces match", {
  r1 <- bootPaired(
    VanillaCalories ~ ChocCalories,
    data = Icecream,
    B = 200,
    seed = 7,
    plot.hist = FALSE
  )
  r2 <- bootPaired(
    Icecream$ChocCalories,
    Icecream$VanillaCalories,
    B = 200,
    seed = 7,
    plot.hist = FALSE
  )
  expect_equal(as.numeric(r1), as.numeric(r2))
})

test_that("bootPaired() print output is stable", {
  result <- bootPaired(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## bootCor() -------------------------------------------------------------

test_that("bootCor() statistic attribute is 'correlation'", {
  result <- bootCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "correlation")
})

test_that("bootCor() observed matches cor()", {
  result <- bootCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(
    attr(result, "observed"),
    cor(Icecream$VanillaCalories, Icecream$ChocCalories)
  )
})

test_that("bootCor() formula and default interfaces match", {
  r1 <- bootCor(
    VanillaCalories ~ ChocCalories,
    data = Icecream,
    B = 200,
    seed = 3,
    plot.hist = FALSE
  )
  r2 <- bootCor(
    Icecream$ChocCalories,
    Icecream$VanillaCalories,
    B = 200,
    seed = 3,
    plot.hist = FALSE
  )
  expect_equal(as.numeric(r1), as.numeric(r2))
})

test_that("bootCor() print output is stable", {
  result <- bootCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## bootSlope() -----------------------------------------------------------

test_that("bootSlope() statistic attribute is 'slope'", {
  result <- bootSlope(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "slope")
})

test_that("bootSlope() observed matches manual calculation", {
  x <- Icecream$VanillaCalories
  y <- Icecream$ChocCalories
  expected <- round(cor(x, y) * sd(y) / sd(x), 4)
  result <- bootSlope(x, y, B = 500, seed = 1, plot.hist = FALSE)
  expect_equal(attr(result, "observed"), expected)
})

test_that("bootSlope() print output is stable", {
  result <- bootSlope(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 500,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## permTest() ------------------------------------------------------------

test_that("permTest() returns carlperm with p-value in [0, 1]", {
  result <- permTest(
    states03$ViolentCrime,
    states03$DeathPenalty,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_s3_class(result, "carlperm")
  expect_true(attr(result, "pval") >= 0 && attr(result, "pval") <= 1)
})

test_that("permTest() formula and default interfaces match", {
  r1 <- permTest(
    ViolentCrime ~ DeathPenalty,
    data = states03,
    B = 500,
    seed = 5,
    plot.hist = FALSE
  )
  r2 <- permTest(
    states03$ViolentCrime,
    states03$DeathPenalty,
    B = 500,
    seed = 5,
    plot.hist = FALSE
  )
  expect_equal(as.numeric(r1), as.numeric(r2))
})

test_that("permTest() print output is stable", {
  result <- permTest(
    states03$ViolentCrime,
    states03$DeathPenalty,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## permTestPaired() ------------------------------------------------------

test_that("permTestPaired() statistic attribute is 'paired difference'", {
  result <- permTestPaired(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "paired difference")
})

test_that("permTestPaired() print output is stable", {
  result <- permTestPaired(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## permTestAnova() -------------------------------------------------------

test_that("permTestAnova() statistic attribute is 'F'", {
  result <- permTestAnova(
    chickwts$weight,
    chickwts$feed,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "F")
})

test_that("permTestAnova() observed matches aov()", {
  result <- permTestAnova(
    chickwts$weight,
    chickwts$feed,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expected_f <- summary(aov(weight ~ feed, data = chickwts))[[1]][["F value"]][
    1
  ]
  expect_equal(attr(result, "observed"), expected_f)
})

test_that("permTestAnova() print output is stable", {
  result <- permTestAnova(
    chickwts$weight,
    chickwts$feed,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## permTestCor() ---------------------------------------------------------

test_that("permTestCor() statistic attribute is 'correlation'", {
  result <- permTestCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "correlation")
})

test_that("permTestCor() observed matches cor()", {
  result <- permTestCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(
    attr(result, "observed"),
    round(cor(Icecream$VanillaCalories, Icecream$ChocCalories), 4)
  )
})

test_that("permTestCor() print output is stable", {
  result <- permTestCor(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## permTestSlope() -------------------------------------------------------

test_that("permTestSlope() statistic attribute is 'slope'", {
  result <- permTestSlope(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_equal(attr(result, "statistic"), "slope")
})

test_that("permTestSlope() print output is stable", {
  result <- permTestSlope(
    Icecream$VanillaCalories,
    Icecream$ChocCalories,
    B = 999,
    seed = 1,
    plot.hist = FALSE
  )
  expect_snapshot(print(result))
})

## anovaSummarized() -----------------------------------------------------

test_that("anovaSummarized() output is stable", {
  N <- table(chickwts$feed)
  mn <- tapply(chickwts$weight, chickwts$feed, mean)
  stdev <- tapply(chickwts$weight, chickwts$feed, sd)
  expect_snapshot(anovaSummarized(N, mn, stdev))
})

test_that("anovaSummarized() errors on mismatched vector lengths", {
  expect_error(anovaSummarized(c(10, 20), c(1, 2, 3), c(1, 2, 3)))
})

## groupedBar() ----------------------------------------------------------

test_that("groupedBar() single variable returns invisibly", {
  expect_invisible(groupedBar(factor(mtcars$cyl)))
})

test_that("groupedBar() two-variable returns invisibly", {
  expect_invisible(groupedBar(factor(mtcars$am), factor(mtcars$cyl)))
})
