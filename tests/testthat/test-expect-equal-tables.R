context("expect_equal_tables")

test_that("expect_equal_tables accepts equivalent native and legacy encoded columns", {
  table <- list(
    list(effect = "JaspColumn_18_Encoded", p = 0.1),
    list(effect = "JaspColumn_5_Encoded<unicode><unicode><unicode>JaspColumn_26_Encoded", p = 0.2)
  )

  ref <- list(
    "jaspColumn4", 0.1,
    "jaspColumn2<unicode><unicode><unicode>jaspColumn3", 0.2
  )

  expect_silent(jaspTools::expect_equal_tables(table, ref))
})

test_that("legacy jaspColumn placeholders are not arbitrary text wildcards", {
  table <- list(
    list(effect = "Sleep", p = 0.1),
    list(effect = "Chronotype", p = 0.2)
  )

  ref <- list(
    "jaspColumn4", 0.1,
    "jaspColumn2", 0.2
  )

  expect_error(jaspTools::expect_equal_tables(table, ref), "not equal")
  expect_false(jaspTools:::tableValuesMatch("<jaspColumn1>", "Sleep"))
  expect_false(jaspTools:::tableValuesMatch("<jaspColumn1>", ""))
  expect_true(jaspTools:::tableValuesMatch("<jaspColumn1>", "<jaspColumn1>"))
})

test_that("native and legacy column tokens canonicalize without touching ordinary text", {
  x <- c("JaspColumn_18_Encoded", "jaspColumn4", "Sleep")

  expect_equal(
    jaspTools:::canonicalizeJaspColumnTokens(x),
    c("<jaspColumn1>", "<jaspColumn2>", "Sleep")
  )
})
