best_sales_models_tbl <- read_rds("artifacts/best_models_sales_tbl.rds")
best_tickets_models_tbl <- read_rds("artifacts/best_models_tickets_tbl.rds")
indicators_sales <- best_sales_models_tbl$film_code
cinema_data_full <- read_csv('data/cinema_data.csv')

cinema_code_selected_vector <- unique(cinema_data_full$cinema_code)[1:10]

cinema_data <- read_csv('data/cinema_data.csv') %>%
   filter(film_code %in% best_sales_models_tbl$film_code) %>%
   filter(cinema_code %in% cinema_code_selected_vector) %>%
   group_by(film_code,cinema_code, date) %>%
   summarise(
      total_sales  = sum(total_sales),
      tickets_sold = sum(tickets_sold),
      tickets_out  = sum(tickets_out),
      show_time    = sum(show_time),
      occu_perc    = mean(occu_perc),
      ticket_price = sum(ticket_price),
      ticket_use   = sum(ticket_use),
      capacity     = sum(capacity)
   ) %>% ungroup()


get_min_data <- function(film_code_selected) {
   tbl <- cinema_data %>%
      filter(film_code == film_code_selected) %>%
      select(date) %>%
      pull() %>%
      min()

   return(tbl)
}


create_custom_output_table <- function(film_code_selected, date_selecred){
   tbl <- cinema_data %>%
            filter(film_code == film_code_selected) %>%
            filter(date == date_selecred) %>%
            select(-c(film_code, date)) %>%
            pivot_longer(cols = !cinema_code, names_to = "indicator", values_to = "value") %>%
            mutate(indicator = str_replace(indicator, pattern = '_', replacement = ' ') %>% str_to_title())

   return(tbl)
}

get_full_data_cinema <- function(film_code_selected) {
   tbl <- cinema_data_full %>%
      filter(film_code %in% best_sales_models_tbl$film_code) %>%
      filter(cinema_code %in% cinema_code_selected_vector) %>%
      filter(film_code == film_code_selected) %>%
      group_by(cinema_code) %>%
      summarise(
         total_sales  = list(total_sales),
         tickets_sold = list(tickets_sold),
         tickets_out  = list(tickets_out),
         show_time    = list(show_time),
         occu_perc    = list(occu_perc),
         ticket_price = list(ticket_price),
         ticket_use   = list(ticket_use),
         capacity     = list(capacity)
      ) %>%
      pivot_longer(cols = !cinema_code, names_to = "indicator", values_to = "value")

   return(tbl)
}


