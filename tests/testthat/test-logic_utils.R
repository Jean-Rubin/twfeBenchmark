describe("insert_name()", {
  it("inserts the name of the sublist of a list inside it", {
    list_1 <- list(a = list())
    list_2 <- list(a = list(aa = 1, ab = 2))
    expect_identical(insert_name(list_1), list(a = list(name = "a")))
    expect_identical(insert_name(list_2), list(a = list(name = "a", aa = 1, ab = 2)))
  })

  it("inserts the name of the sublist of a list for each sublist", {
    list_ex <- list(a = list(), b = list(bb = 1))
    expect_identical(
      insert_name(list_ex),
      list(a = list(name = "a"), b = list(name = "b", bb = 1))
    )
  })
})
