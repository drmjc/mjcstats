context("testing permute")

test_that("permute works", {
	expect_equal(length(permute(1:10)), 10)
	expect_equal(length(permute(letters)), 26)

	set.seed(12315)
	expected.res <- c("v", "m", "o", "e", "n", "a", "x", "f", "b", "p", "u", "r", 
	"c", "t", "z", "l", "j", "y", "d", "w", "i", "h", "s", "q", "k", 
	"g")
	expect_equal(permute(letters), expected.res)

	expected.res <- c(6L, 3L, 2L, 1L, 10L, 9L, 5L, 4L, 8L, 7L)
	expect_equal(permute(1:10), expected.res)
})
