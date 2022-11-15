test_that("Testing months to save down deposit function", {
  expect_equal(m_to_save_down_deposit(salary=120000, house_cost =  1000000, prop_saved = 0.10, ret_rate = 0.04), 183)
  expect_error(m_to_save_down_deposit(salary=TRUE, house_cost =  1000000, prop_saved = 0.10, ret_rate = 0.04))
  expect_error(m_to_save_down_deposit(salary=120000, house_cost =  1000000, prop_saved = -0.10, ret_rate = 0.04))
})
