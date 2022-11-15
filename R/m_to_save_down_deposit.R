#' Calculate months to save down deposit
#'
#'
#' Calculates the number of months needed to save for a 25% down-payment on a house, based on current
#' income, the cost of the house, the proportion of monthly income invested every month, and the annual
#' investment return rate. Assumes that the initial amount of savings is 0.
#'
#' @param salary Annual salary, in dollars
#' @param house_cost The cost of the house to be purchased
#' @param prop_saved The proportion of monthly salary set aside for investment every month
#' @param ret_rate The annual return rate on investments
#'
#' @return Returns the numbers of months needed to save for a 25% downpayment on a house
#' @export
#'
#' @examples
#' m_to_save_down_deposit(salary=80000, house_cost=2000000, prop_saved=0.05, ret_rate=0.05)
#' m_to_save_down_deposit(salary=150000, house_cost=1500000, prop_saved=0.25, ret_rate=0.02)


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
