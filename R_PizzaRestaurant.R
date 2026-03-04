## 2. Pizza Restaurant
## Create menu
id <- 1:5
menu_list <- c("cheese pizza", "truffle pizza", "spaghetti carbonara", "coke", "water")
price <- c(250, 350, 200, 25, 10)

menus <- data.frame(id, menu_list, price)

## Prepare order table (to collect order from customers)
order_details <- data.frame(
  items = character(),
  amount = numeric(),
  unit_prices = numeric(),
  total_prices = numeric(),
  stringsAsFactors = FALSE
)

total_bill <- 0

## while loop (to start receive order from customers)
taking_order <- TRUE
order_pizza <- function() {
  print("---Welcome to My Pizza Restaurant!---")
  print("Here is our menu!")
  print(menus)
  while (taking_order == TRUE) {
    cust_order <- readline("What would you like to eat? or 'done' to finish: ")
    
    ## [1] if condition to stop the loop (order/not order)
    
    if (tolower(cust_order) == "done") {
      
      ## [1.1] condition: not order
      if (nrow(order_details) == 0) {
      print("You did not order anything. Thank you for visiting")
      } 
      ## [1.2] condition: finish order 
        else {
        total_bill <- sum(order_details$total_prices)
        print("-----Summary Total-----")
        print(order_details)
        print(paste0("Your total: ", total_bill, " THB"))
      }
      taking_order <- FALSE
      break
    }
    
    ## [2] if condition to continue the loop
    
    ## [2.1] check item is TRUE
    found_item <- menus[tolower(menus$menu_list) == tolower(cust_order), ]
    
    ## [2.2] condition: item amount is TRUE
    if (nrow(found_item) > 0) {
      item_amount <- as.numeric(readline("How many items do you want?(enter number): "))
      
      ## [2.2.1] condition: item amount is FALSE
      if (is.na(item_amount) | item_amount <= 0) {
        print("Invalid number, Please enter again!")
        item_amount <- as.numeric(readline("please enter amount again: "))
        next
      } 
      ## for check bill
      unit_price <- found_item$price  ## price per item
      total_price <- unit_price * item_amount  ## cal total price per item
      new_order_row <- data.frame(  ## create cust order to put in df
        items = cust_order,
        amount = item_amount,
        unit_prices = unit_price,
        total_prices = total_price,
        stringsAsFactors = FALSE
      )
      ## put cust order in prepared data.frame
      order_details <- rbind(order_details, new_order_row)
      print(paste0(item_amount, " x ", unit_price, " :" ,cust_order, " your order total: ", total_price, " THB"))
    } 
    ## [2.3] condition: item is FALSE (nrow = 0 menu not found)
    else if (nrow(found_item) == 0) {
      print("Sorry, that item is not on our menu. Please try again.")
    } 
  }
}