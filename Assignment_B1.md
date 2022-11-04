fun_calculate_months_to_save
================
Karanvir Singh
2022-11-03

## Function to calculate the months needed to save down on a deposit

The following code chunk contains function `m_to_save_down_deposit`, a
function that calculates the months to save on a down deposit given a
specific annual salary, the cost of the house to buy, the proportion of
monthly income invested every month and the annual return rate on
investments. Assumes that the initial amount of savings is 0.

Note: I adapted this function from my answer for assignment 1A of the
MOOC 6.0001 from MIT OpenCourseware. The original assignment asked to
code for a similar function in python.

``` r
#' Calculate months to save down deposit
#' 
#' 
#' Calculates the number of months needed to save for a 25% down-payment on a house, based on current
#' income, the cost of the house, the proportion of monthly income invested every month, and the annual
#' investment return rate. Assumes that the initial amount of savings is 0.
#'
#' @keywords internal
#'
#' @param salary Annual salary, in dollars (named just salary instead of ann_salary for simplicity)
#' @param house_cost The cost of the house to be purchased (named this way to make it easier to read     
#' instead of housecost)
#' @param prop_saved The proportion of monthly salary set aside for investment every month (I abbreviated 
#' proportion to prop, and added an underscore for clarity)
#' @param ret_rate The annual return rate on investments (I abbreviated return to ret for simplicity)
#'
#' @return Returns the numbers of months needed to save for a 25% downpayment on a house

m_to_save_down_deposit <- function(salary, house_cost, prop_saved, ret_rate){
  stopifnot(is.numeric(salary), is.numeric(house_cost), is.numeric(prop_saved), is.numeric(ret_rate))
  stopifnot(salary > 0 , house_cost > 0, prop_saved > 0 , ret_rate > 0)
  
  portion_down_payment = 0.25*house_cost
  current_savings = 0
  months = 0
  
  while (current_savings < portion_down_payment) {
    months = months + 1
    current_savings = current_savings + (ret_rate/12)*current_savings + prop_saved*(salary/12)
  }
  
return (months)
  }
```

## Examples

Here I am using my function with two different sets of inputs. I am
printing the output of the function to the console as well as a sentence
to explain the output.

Example 1: My income is 80,000 and my dream home costs 2,000,000. I plan
on saving 0.05 of my monthly salary, and my investment return rate is
0.05.

``` r
example1 <- m_to_save_down_deposit(salary=80000, house_cost=2000000, prop_saved=0.05, ret_rate=0.05)
print(example1)
```

    ## [1] 477

    ## [1] "It will take you 477 months to save for a downpayment on this house"

Example 2: My income is 150,000 and my dream home costs 1,500,000. I
plan on saving 0.25 of my monthly salary, and my investment return rate
is 0.02.

``` r
example2 <- m_to_save_down_deposit(salary=150000, house_cost=1500000, prop_saved=0.25, ret_rate=0.02)
print(example2)
```

    ## [1] 110

    ## [1] "It will take you 110 months to save for a downpayment on this house"

## Tests for the functions.

Here I am testing three scenarios:

1)  The inputs are all positive numbers, and the function returns the
    correct number of months (183)
2)  One of the inputs is a boolean. The function returns an error.
3)  One of the inputs is a negative number instead of a positive number.

``` r
test_that("Testing months to save down deposit function", {
  expect_equal(m_to_save_down_deposit(salary=120000, house_cost =  1000000, prop_saved = 0.10, ret_rate = 0.04), 183)
  expect_error(m_to_save_down_deposit(salary=TRUE, house_cost =  1000000, prop_saved = 0.10, ret_rate = 0.04))
  expect_error(m_to_save_down_deposit(salary=120000, house_cost =  1000000, prop_saved = -0.10, ret_rate = 0.04))
})
```

    ## Test passed 😀
