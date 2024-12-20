test_that("basic styling orig hash works", {
    fn <- testthat::test_path("test.ipynb")
    print(fn)

    expect_true(file.exists(fn))

    out <- style_nb(fn, verbose = TRUE, overwrite = FALSE, save = FALSE)
    expect_equal(out$original_hash, "5c187b7526c9a81448acff739cbba8e1")
    expect_equal(out$styled_hash, "3df1c6c148c850adc64ef9202ae80db1")
})
