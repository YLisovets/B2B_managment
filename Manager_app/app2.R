# LIBRARIES ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggalt)
library(here)
library(tableHTML)
library(DT)


# DATA SETUP ----

predict_month <- readxl::read_xlsx(here(paste0("Планирование/Прогноз_продаж_Январь.xlsx")))

predict_week <- readxl::read_xlsx(here(paste0("Планирование/План_на_неделю_",
                                         week(today()),
                                         ".xlsx")))

rfm_results <- readxl::read_xlsx(here(paste0("RFM/RFM-анализ_за_Декабрь.xlsx"))) %>% 
    mutate(user_name = str_trim(customer_name, side = "right"))

sale_df_previos <- read_rds(here("Data/sale_df_finish_дек.xlsx"))

segment_assort <- readxl::read_xlsx(here(paste0("Ассортимент/Чемпионы_группы_",
                                    month(floor_date(today(), unit = "quarter"),
                                          label = TRUE),
                                     ".xlsx")))

segments_basket <- readxl::read_xlsx(here("Data/Корзины_ТОР_сегменты.xlsx")) %>% 
    select(segment_form:RHS)

managers <- predict_month %>% 
    filter(`Подразделение` == "Хмельницький см сервіс") %>% 
    distinct(`Менеджер`) %>% 
    filter(!is.na(`Менеджер`)) %>% 
    pull()

sale_data <- read_rds(here("Data/sale_data.rds"))

returns_data <- read_rds(here("Data/returns_data.rds"))

ref_customers <- read_rds(here("Data/ref_customers.rds"))

ref_users <- read_rds(here("Data/ref_users.rds"))

ref_partner_manager <- read_rds(here("Data/ref_partner_manager.rds"))

retail_customers <- data.frame(customer_id = unique(sale_data$customer_id)) %>%
    left_join(ref_customers) %>% 
    filter(is.na(customer_type) & edrp_code == "") %>% 
    pull(customer_id)


sale_df <- sale_data %>% 
    
    # Добавляем возвраты
    left_join(returns_data) %>% 
    replace_na(list(sum_refund = 0)) %>% 
    mutate(doc_sum = doc_sum - sum_refund) %>%
    
    # Удаляем документы, по которым возвраты на ту же сумму
    filter(doc_sum > 0)


sale_df_clr <- sale_df %>% 

    # Объединяем документы одного дня по подразделению  
    group_by(customer_id, subdiv_name, date_sale_doc) %>% 
    summarise(sum_customer = sum(doc_sum)) %>%
    ungroup() %>% 
    
    # Удаляем документы ниже 1грн    
    filter(sum_customer > 1) %>% 
    
    # Удаляем конечных покупателей ("PROM", "Интернет-магазин" и др.)
    filter(!customer_id %in% retail_customers)

ref_partner_manager_clr <- ref_partner_manager %>%
    filter(partner_id %in% unique(sale_df_clr$customer_id)) %>% 
    mutate(date_entry = as.Date(date_entry) - years(2000)) %>% 
    
    ## Если есть менеджер, но нет подразделения - определяем к какому подразделению
    ## прикреплен менеджер 
    left_join(ref_users) %>% 
    mutate(subdiv_name_new = ifelse(!is.na(subdiv_name),
                                    subdiv_name,
                                    user_subdiv)) %>% 
    ## Удаляем дубликаты записей (оставляем с последней датой)
    arrange(desc(date_entry)) %>% 
    distinct(partner_id, subdiv_name_new, user_name, .keep_all = TRUE) %>%
    
    ## Добавляем основного менеджера и его подразделение 
    left_join(select(ref_customers, customer_id, main_manager),
              by = c("partner_id" = "customer_id")) %>%
    drop_na(subdiv_name_new) %>% 
    mutate(user_name = str_trim(user_name, side = "right"),
           main_manager = str_trim(main_manager, side = "right")) %>%
    left_join(select(ref_users, user_name, main_manager_subdiv_assigned = user_subdiv),
              by = c("main_manager" = "user_name")) %>%
    ## Если по контрагенту по подразделению определено 2 разных менеджера - основной
    arrange(partner_id, subdiv_name_new) %>%
    group_by(partner_id, subdiv_name_new) %>% 
    mutate(customer_manager = ifelse(
        !is.na(main_manager_subdiv_assigned) &
             main_manager_subdiv_assigned == subdiv_name_new,
        main_manager, first(user_name))) %>%
    ungroup() %>% 
    distinct(partner_id, subdiv_name_new, customer_manager, .keep_all = TRUE) %>%
    
    ## Если по контрагенту менеджер указан по нескольким подразделениям - выбираем
    ## подразделение по последней дате_CRM
    group_by(partner_id, customer_manager) %>%
    mutate(subdiv_name_new = subdiv_name_new[date_entry == max(date_entry)]) %>% 
    ungroup() %>% 
    distinct(partner_id, customer_manager, .keep_all = TRUE) %>%
    
    select(customer_id = partner_id, subdiv_name = subdiv_name_new, customer_manager)

customers_with_one_managers <- ref_partner_manager_clr %>% 
    group_by(customer_id) %>% 
    filter(n() == 1) %>% 
    ungroup() %>%
    #mutate(user_name = str_trim(user_name, side = "right")) %>% 
    select(customer_id, only_one_manager = customer_manager,
           only_one_man_subdiv = subdiv_name)

previos_customers_data <- sale_df_previos %>% 
    select(customer_id, customer_rfm_id, subdiv_name, subdiv_new, customer_manager_new,
           registr_date_rfm, main_flag, subdiv_change_flag) %>% 
    distinct(customer_id, subdiv_name, .keep_all = TRUE)

previos_customers_data_change_subdiv <- sale_df_previos %>% 
    select(customer_id, customer_rfm_id_2 = customer_rfm_id, subdiv_new,
           customer_manager_new_2 = customer_manager_new) %>% 
    distinct(customer_id, subdiv_new, .keep_all = TRUE)

previos_customers_data_main_cust <- sale_df_previos %>% 
    select(customer_rfm_id, subdiv_new,
           customer_manager_new_3 = customer_manager_new,
           registr_date_rfm_3 = registr_date_rfm) %>% 
    distinct(customer_rfm_id, subdiv_new, .keep_all = TRUE)

sale_df_new <- sale_df_clr %>% 
    left_join(previos_customers_data) %>% 
    left_join(ref_partner_manager_clr) %>%
    left_join(select(ref_customers, customer_id, main_cust_id, customer_type,
                     registr_date, main_manager)) %>%
    mutate(registr_date = ifelse(year(registr_date - years(2000)) <= 2016 |
                                     is.na(registr_date),
                                 2016,
                                 year(registr_date - years(2000)))) %>% 
    mutate(main_manager = str_trim(main_manager, side = "right")) %>%
    left_join(select(ref_partner_manager_clr, customer_id, customer_manager,
                     main_manag_subdiv_tbl = subdiv_name),
              by = c("customer_id" = "customer_id",
                     "main_manager" = "customer_manager")) %>% 
    left_join(customers_with_one_managers) %>%
    left_join(previos_customers_data_change_subdiv,
              by = c("customer_id",
                     "subdiv_name" = "subdiv_new")) %>%
    left_join(previos_customers_data_main_cust,
              by = c("main_cust_id" = "customer_rfm_id",
                     "subdiv_name" = "subdiv_new")) %>%
    
    mutate(customer_rfm_id = ifelse(!is.na(customer_rfm_id),
                                    customer_rfm_id,
                                    ifelse(!is.na(customer_rfm_id_2),
                                           customer_rfm_id_2,
                                           ifelse(!is.na(customer_manager_new_3),
                                                  main_cust_id,
                                                  customer_id))),
           subdiv_new = ifelse(!is.na(subdiv_new),
                               subdiv_new,
                               ifelse(!is.na(only_one_man_subdiv),
                                      only_one_man_subdiv,
                                      subdiv_name)),
           customer_manager_new = ifelse(!is.na(customer_manager_new),
                                         customer_manager_new,
                                         ifelse(!is.na(customer_manager_new_2),
                                                customer_manager_new_2,
                                                ifelse(!is.na(customer_manager_new_3),
                                                       customer_manager_new_3,
                                                       ifelse(!is.na(only_one_manager),
                                                              only_one_manager,
                                                              customer_manager)))),
           registr_date_rfm = ifelse(!is.na(registr_date_rfm),
                                     registr_date_rfm,
                                     ifelse(!is.na(registr_date_rfm_3) &
                                                registr_date_rfm_3 < registr_date,
                                            registr_date_rfm_3,
                                            registr_date)),
           main_flag = ifelse(!is.na(main_flag),
                              main_flag,
                              ifelse(customer_rfm_id == customer_id,
                                     0,
                                     1)),
           subdiv_change_flag = ifelse(!is.na(subdiv_change_flag),
                                       subdiv_change_flag,
                                       ifelse(subdiv_name == subdiv_new,
                                              0,
                                              1))
    )

sale_month_data <- sale_df_new %>% 
    group_by(customer_rfm_id, subdiv_new) %>% 
    summarise(customer_type      = last(customer_type),
              customer_manager   = last(customer_manager_new),
              registr_date       = last(registr_date_rfm),
              main_flag          = last(main_flag),
              subdiv_change_flag = last(subdiv_change_flag),
              actual_trans       = n(),
              actual_sum         = sum(sum_customer)) %>% 
    left_join(select(ref_customers, customer_id, customer_name),
              by = c("customer_rfm_id" = "customer_id")) %>% 
    relocate(customer_name, .after = customer_rfm_id) %>% 
    left_join(select(rfm_results, customer_id, subdiv_name, segment),
              by = c("customer_rfm_id" = "customer_id",
                     "subdiv_new" = "subdiv_name")) %>% 
    relocate(segment, customer_type, .after = customer_name) %>% 
    select(-c(registr_date, subdiv_change_flag))

names(sale_month_data) <- c("Код_покупателя", "Наименование_покупателя",
                        "RFM-статус", "Тип", "Подразделение", "Менеджер",
                         "Флаг_измен_Головной", "Факт_транзакций", "Факт_Сумма")

names(predict_month) <- c('Код_покупателя','Наименование_покупателя',
                          'Подразделение', 'Флаг_измен_Головной', 'Тип', 'Менеджер',
                          'RFM-статус', 'Частота','Давность',
                          'Прогноз сред.сумма', 'Кол-во_транзакций',
                          'Прогноз_Сумма')

month_data <- predict_month %>% 
    relocate('RFM-статус', 'Тип', .before = 'Подразделение') %>% 
    relocate('Флаг_измен_Головной', .after = "Менеджер") %>% 
    full_join(sale_month_data) %>% 
    replace_na(list(`Факт_транзакций` = 0,
                    `Факт_Сумма` = 0,
                    `Кол-во_транзакций` = 0,
                    `Прогноз_Сумма` = 0)) %>% 
    select(-c('Частота':'Прогноз сред.сумма')) %>% 
    mutate(`Факт_Сумма` = round(`Факт_Сумма`)) %>% 
    rename('Флаг_Головн' = 'Флаг_измен_Головной',
           'Прогноз_транзакций' = 'Кол-во_транзакций')
    
sale_week_data <- sale_df_new %>% 
    filter(date_sale_doc > floor_date(today(), unit = "week")) %>% 
    group_by(customer_rfm_id, subdiv_new) %>% 
    summarise(customer_type      = last(customer_type),
              customer_manager   = last(customer_manager_new),
              registr_date       = last(registr_date_rfm),
              main_flag          = last(main_flag),
              subdiv_change_flag = last(subdiv_change_flag),
              actual_trans       = n(),
              actual_sum         = sum(sum_customer)) %>% 
    left_join(select(ref_customers, customer_id, customer_name),
              by = c("customer_rfm_id" = "customer_id")) %>% 
    relocate(customer_name, .after = customer_rfm_id) %>% 
    left_join(select(rfm_results, customer_id, subdiv_name, segment),
              by = c("customer_rfm_id" = "customer_id",
                     "subdiv_new" = "subdiv_name")) %>% 
    relocate(segment, customer_type, .after = customer_name) %>% 
    select(-c(registr_date, subdiv_change_flag))

names(sale_week_data) <- c("Код_покупателя", "Наименование_покупателя",
                           "RFM-статус", "Тип", "Подразделение", "Менеджер",
                           "Флаг_измен_Головной", "Факт_транзакций", "Факт_Сумма")

week_data <- predict_week %>%
    select(-c("Год регистрации", "Флаг_измен_Подразд",
              "Кол-во_транзакций")) %>% 
    full_join(sale_week_data) %>% 
    replace_na(list(`Факт_транзакций` = 0,
                    `Факт_Сумма` = 0,
                    `Условно-ожидаемые_транзакции` = 0,
                    `Прогноз_Сумма` = 0)) %>% 
    mutate(`Прогноз_Сумма` = round(`Прогноз_Сумма`)) %>% 
    rename('Флаг_Головн' = 'Флаг_измен_Головной')


supplier_order_df <- read_rds(here("Data/supplier_order_df.rds"))

ref_items <- read_rds(here("Data/ref_items.rds"))

customer_orders_df <- read_rds(here("Data/customer_orders_df.rds"))


customer_orders_current_month <- customer_orders_df %>% 
    filter(date_cust_order_doc >= floor_date(today() - months(1),
                                             unit = "month")) %>% 
    group_by(nmbr_cust_order_doc, date_cust_order_doc) %>% 
    summarise(subdiv_name = first(subdiv_name),
              customer_id = first(customer_id),
              cust_order_doc_sum = first(cust_order_doc_sum),
              responsible_name = first(responsible_name),
              group_qty = n()) %>% 
    ungroup() %>% 
    left_join(select(sale_df, nmbr_sale_doc:date_cust_order_doc)) %>% 
    left_join(supplier_order_df) %>% 
    left_join(ref_partner_manager_clr) %>% 
    mutate(customer_manager = ifelse(!is.na(customer_manager),
                                     customer_manager,
                                     responsible_name))


ref_item_group <- read_rds(here("Data/ref_item_group.rds"))

ref_item_group_df <- ref_item_group %>%
    left_join(select(ref_item_group, group_id, parent_name = group_name),
              by = c("parent_group" = "group_id")) %>% 
    filter(group_marked == FALSE,
           parent_name != "- УДАЛЕНИЕ -")

groups_3_lev <- ref_item_group_df %>% 
    select(group_id, group_name, parent_group, parent_name) %>%
    filter(group_id %in% unique(customer_orders_df$group_id)) %>% 
    left_join(select(ref_item_group, group_id, parent_group_2lev = parent_group),
              by = c("parent_group" = "group_id")) %>% 
    left_join(select(ref_item_group, group_id, parent_2lev_name = group_name,
                     parent_group_3lev = parent_group),
              by = c("parent_group_2lev" = "group_id")) %>% 
    left_join(select(ref_item_group, group_id, parent_3lev_name = group_name),
              by = c("parent_group_3lev" = "group_id")) %>% 
    mutate(group_name_3lev = case_when(
        str_detect(parent_2lev_name,"^\\d+\\s") &
            str_detect(group_name, "^\\d+\\.\\d+\\.\\d+\\s") ~ group_name,
        
        str_detect(parent_2lev_name,"^\\d+\\s") &
            !str_detect(group_name, "^\\d+\\.\\d+\\.\\d+\\s") ~ parent_name,
        
        str_detect(parent_2lev_name, "^\\d+\\.\\d+\\s") ~ parent_name,
        
        str_detect(parent_2lev_name, "^\\d+\\.\\d+\\.\\d+\\s") ~ parent_2lev_name,
        
        str_detect(parent_2lev_name, "^\\d+\\.\\d+\\.\\d+\\.\\d+") ~
            parent_3lev_name,
        TRUE ~ parent_2lev_name))

ref_customer_managers <- ref_partner_manager %>%
    filter(partner_id %in% unique(customer_orders_df$customer_id)) %>% 
    mutate(date_entry = as.Date(date_entry) - years(2000)) %>% 
    
    ## Если есть менеджер, но нет подразделения - определяем к какому подразделению
    ## прикреплен менеджер 
    left_join(ref_users) %>% 
    mutate(subdiv_name_new = ifelse(!is.na(subdiv_name),
                                    subdiv_name,
                                    user_subdiv)) %>% 
    ## Удаляем дубликаты записей (оставляем с последней датой)
    arrange(desc(date_entry)) %>% 
    distinct(partner_id, subdiv_name_new, user_name, .keep_all = TRUE) %>%
    
    ## Добавляем основного менеджера и его подразделение 
    left_join(select(ref_customers, customer_id, main_manager),
              by = c("partner_id" = "customer_id")) %>%
    drop_na(subdiv_name_new) %>% 
    mutate(user_name = str_trim(user_name, side = "right"),
           main_manager = str_trim(main_manager, side = "right")) %>%
    left_join(select(ref_users, user_name, main_manager_subdiv_assigned = user_subdiv),
              by = c("main_manager" = "user_name")) %>%
    ## Если по контрагенту по подразделению определено 2 разных менеджера - основной
    arrange(partner_id, subdiv_name_new) %>%
    group_by(partner_id, subdiv_name_new) %>% 
    mutate(customer_manager = ifelse(!is.na(main_manager_subdiv_assigned) &
                                         main_manager_subdiv_assigned == subdiv_name_new,
                                     main_manager, first(user_name))) %>%
    ungroup() %>% 
    distinct(partner_id, subdiv_name_new, customer_manager, .keep_all = TRUE) %>%
    
    ## Если по контрагенту менеджер указан по нескольким подразделениям - выбираем
    ## подразделение по последней дате_CRM
    group_by(partner_id, customer_manager) %>%
    mutate(subdiv_name_new = subdiv_name_new[date_entry == max(date_entry)]) %>% 
    ungroup() %>% 
    distinct(partner_id, customer_manager, .keep_all = TRUE) %>%
    
    select(customer_id = partner_id, subdiv_name = subdiv_name_new, customer_manager)

customers_with_one_managers <- ref_customer_managers %>% 
    group_by(customer_id) %>% 
    filter(n() == 1) %>% 
    ungroup() %>%
    #mutate(user_name = str_trim(user_name, side = "right")) %>% 
    select(customer_id, only_one_manager = customer_manager,
           only_one_man_subdiv = subdiv_name)

customer_orders_3month_qty <- customer_orders_df %>% 
    distinct(nmbr_cust_order_doc, date_cust_order_doc, .keep_all = TRUE) %>% 
    group_by(customer_id, subdiv_name) %>% 
    summarise(last_order = max(date_cust_order_doc),
              orders_qty = length(unique(nmbr_cust_order_doc)),
              orders_sum = sum(cust_order_doc_sum),
              responsible_name = first(responsible_name)) %>% 
    ungroup() %>% 
    left_join(ref_customer_managers) %>% 
    mutate(customer_manager = ifelse(!is.na(customer_manager),
                                     customer_manager,
                                     responsible_name)) %>% 
    select(-responsible_name)

customer_orders_assort <- customer_orders_df %>% 
    left_join(select(groups_3_lev, group_id, group_name = group_name_3lev)) %>% 
    group_by(customer_id, subdiv_name, group_name) %>% 
    summarise(group_sum = sum(group_sum),
              responsible_name = first(responsible_name)) %>% 
    ungroup() %>% 
    left_join(ref_customer_managers) %>% 
    mutate(customer_manager = ifelse(!is.na(customer_manager),
                                     customer_manager,
                                     responsible_name)) %>% 
    select(-responsible_name)

order_selections <- customer_orders_df %>%
    filter(subdiv_name == "Хмельницький см сервіс") %>% 
    left_join(select(ref_customers, customer_id, segment_form)) %>% 
    filter(segment_form %in% unique(segments_basket$segment_form)) %>%
    distinct(nmbr_cust_order_doc) %>% 
    slice(1:10) %>%
    pull()


debt_customer_data <- read_rds(here("Data/debt_customer_data.rds"))


receivable_cust_df <- debt_customer_data %>% 
    left_join(select(ref_customers, customer_id, customer_folder),
              by = c("partner_id" = "customer_id")) %>% 
    filter(!customer_folder %in% c("КІНЦЕВИЙ ПОКУПЕЦЬ", "РЕГИОНАЛЬНЫЕ МАГАЗИНЫ",
                                  "СПЕЦИАЛИСТЫ"),
           debt_sum > 10)

receivable_cust_contract <- receivable_cust_df %>% 
    pull(contract_id)


debt_cust_mov_data <- read_rds(here("Data/debt_cust_mov_data.rds"))

ref_contract <- read_rds(here("Data/ref_contract.rds"))


debt_cust_moving_df <- debt_cust_mov_data %>% 
    filter(contract_id %in% receivable_cust_contract,
           !is.na(nmbr_sale_doc)) %>% 
    mutate(date_movement = as.Date(date_movement) - years(2000),
           date_sale_doc = as.Date(date_sale_doc) - years(2000),
           moving_value = ifelse(type_moving == 1,
                                        moving_sum * (-1),
                                        moving_sum)) %>% 
    group_by(contract_id, nmbr_sale_doc, date_sale_doc) %>% 
    summarise(sale_doc_sum = sum(moving_value),
              subdiv_name = first(subdiv_name),
              responsible_name = first(responsible_name)) %>% 
    ungroup() %>% 
    filter(sale_doc_sum > 0.01) %>% 
    arrange(contract_id, desc(date_sale_doc)) %>% 
    left_join(select(receivable_cust_df, contract_id, debt_sum)) %>% 
    group_by(contract_id) %>% 
    mutate(sum_docs = sum(sale_doc_sum))  %>% 
    #ungroup() %>% 
    mutate(sum_differ = debt_sum - sum_docs) %>% 
    #filter(sum_differ < -1)
    mutate(cum_debt_sum = cumsum(sale_doc_sum),
           diff_cum = debt_sum - cum_debt_sum,
           qty_negativ = sum(diff_cum < 0),
           doc_rank = min_rank(diff_cum)) %>% 
    filter(!(sum_differ < -1 & diff_cum < 0 & doc_rank < qty_negativ)) %>% 
    ungroup() %>% 
    select(-c(sum_differ:doc_rank)) %>% 
    left_join(select(ref_contract, contract_id, contract_arrears_day)) %>% 
    mutate(doc_recency = as.integer(as.Date(today()) - date_sale_doc),
           debt_period = case_when(
               doc_recency <= contract_arrears_day       ~ "Непросроченная",
               doc_recency <= contract_arrears_day + 7   ~ "Проср. до 7дн",
               doc_recency <= contract_arrears_day + 30  ~ "Проср. 7-30дн",
               TRUE                                      ~ "Проср. >30дн"
           ),
           customer_id = partner_id)

    
debt_customers <- debt_cust_moving_df %>% 
    pull(customer_id)

debt_customer_managers <- ref_partner_manager %>%
    filter(partner_id %in% debt_customers ) %>% 
    mutate(date_entry = as.Date(date_entry) - years(2000)) %>% 
    
    ## Если есть менеджер, но нет подразделения - определяем к какому подразделению
    ## прикреплен менеджер 
    left_join(ref_users) %>% 
    mutate(subdiv_name_new = ifelse(!is.na(subdiv_name),
                                    subdiv_name,
                                    user_subdiv)) %>% 
    ## Удаляем дубликаты записей (оставляем с последней датой)
    arrange(desc(date_entry)) %>% 
    distinct(partner_id, subdiv_name_new, user_name, .keep_all = TRUE) %>%
    
    ## Добавляем основного менеджера и его подразделение 
    left_join(select(ref_customers, customer_id, main_manager),
              by = c("partner_id" = "customer_id")) %>%
    drop_na(subdiv_name_new) %>% 
    mutate(user_name = str_trim(user_name, side = "right"),
           main_manager = str_trim(main_manager, side = "right")) %>%
    left_join(select(ref_users, user_name, main_manager_subdiv_assigned = user_subdiv),
              by = c("main_manager" = "user_name")) %>%
    ## Если по контрагенту по подразделению определено 2 разных менеджера - основной
    arrange(partner_id, subdiv_name_new) %>%
    group_by(partner_id, subdiv_name_new) %>% 
    mutate(customer_manager = ifelse(!is.na(main_manager_subdiv_assigned) &
                                         main_manager_subdiv_assigned == subdiv_name_new,
                                     main_manager, first(user_name))) %>%
    ungroup() %>% 
    distinct(partner_id, subdiv_name_new, customer_manager, .keep_all = TRUE) %>%
    
    ## Если по контрагенту менеджер указан по нескольким подразделениям - выбираем
    ## подразделение по последней дате_CRM
    group_by(partner_id, customer_manager) %>%
    mutate(subdiv_name_new = subdiv_name_new[date_entry == max(date_entry)]) %>% 
    ungroup() %>% 
    distinct(partner_id, customer_manager, .keep_all = TRUE) %>%
    
    select(customer_id = partner_id, subdiv_name = subdiv_name_new, customer_manager)

customers_with_one_managers <- debt_customer_managers %>% 
    group_by(customer_id) %>% 
    filter(n() == 1) %>% 
    ungroup() %>%
    #mutate(user_name = str_trim(user_name, side = "right")) %>% 
    select(customer_id, only_one_manager = customer_manager,
           only_one_man_subdiv = subdiv_name)


debt_customer_df <- debt_cust_moving_df %>% 
    filter(subdiv_name != "Администрация") %>% 
    left_join(debt_customer_managers) %>% 
    left_join(select(previos_customers_data, customer_id, subdiv_name,
                     customer_manager_new)) %>% 
    left_join(customers_with_one_managers) %>% 
    mutate(customer_manager = ifelse(
        !is.na(customer_manager),
        customer_manager,
        ifelse(!is.na(only_one_manager),
                only_one_manager,
                ifelse(!is.na(customer_manager_new),
                       customer_manager_new,
                       responsible_name)))) %>% 
    select(customer_id, contract_id, contract_arrears_day, subdiv_name,
           customer_manager, debt_sum, nmbr_sale_doc, date_sale_doc,
           sale_doc_sum, debt_period)


# INFO CARD ----
info_card <- function(title, value, prop_value, extra_value, extra_prop_value,
                      main_icon = "chart-line", 
                      bg_color = "default", text_color = "black", 
                      sub_text_color = "success") {
    
    div(
        class = "panel panel-default",
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa-4x", main_icon)),
            h4(title),
            h5(value),
            h6(str_glue(prop_value, "% выполнения")),
            p(
                class = str_glue("text-{sub_text_color}"),
                tags$small(str_glue("Дополнительные - ", extra_value, " (",
                                    extra_prop_value, "% от плана)"))
            )
        )
    )
    
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"),
    
    # Application title
    titlePanel("Рабочая панель Менеджера по продажам"),
    
    # Sidebar with a slider input for select manager
    sidebarLayout(
        sidebarPanel(
            width = 3,
            shiny::selectInput(inputId  = "manager",
                               label = h4("Выберите менеджера"), 
                               choices = managers,
                               selected = managers[1])
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
               tabPanel(
                        title = "План на месяц",
                        fluidPage(
                            fluidRow(
                                uiOutput("value_boxes_month")
                                ),
                            fluidRow(
                                column(6,
                                       plotOutput("orders", height = "300px")
                                       ),
                                column(6,
                                       plotOutput("rfm", height = "300px")
                                       )
                                ),
                            fluidRow(
                                column(
                                    width = 12,
                                    dataTableOutput("month_table", width="100%")
                                )
                            )
                        )), 
               tabPanel(
                        title = "План на неделю",
                        uiOutput("value_boxes_week"),
                        column(width = 12,
                               dataTableOutput("weekly_table", width = "100%")
                        )
                       ),
                        
                tabPanel(
                         title = "Дебиторcкая задолженность",
                         fluidRow(
                             column(width = 8,
                                    plotOutput("debt", height = "300px")),
                             column(width = 12,
                                    dataTableOutput("debt_table", width="100%")
                                   )
                             )
                         ),
                tabPanel(
                         title = "План на день",
                         column(width = 12,
                                DT::dataTableOutput("day_plan_table", width="100%")
                                ),
                         uiOutput("customer_info"),
                         div(
                             class = "row",
                             tabsetPanel(
                                 tabPanel(
                                     title = "Заказанные группы",
                                     dataTableOutput("basket")
                                    ),
                                  tabPanel(
                                     title = "Рекомендуемые группы",
                                     dataTableOutput("recommendation")
                                    )
                                 )
                             )
                         ),
                tabPanel(
                    title = "Анализ заказов",
                    column(
                        width = 12,
                        shiny::selectInput(
                            inputId  = "order_selection", 
                            label    = "Выберите заказ:",
                            choices  = order_selections,
                            selected = order_selections[1]
                        
                    )),
                    div(
                        class = "row",
                        tabsetPanel(
                            tabPanel(
                                title = "Информация о заказе",
                                dataTableOutput("order_basket")
                            ),
                            tabPanel(
                                title = "Рекомендуемые группы",
                                dataTableOutput("order_recommend")
                            )
                        )
                    )
                )
                
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # DATA PREPARAION ----
    weekly_tbl <- eventReactive(input$manager, {
        week_data %>% 
            filter(`Менеджер` %in% input$manager)
    })
    
    month_tbl <- eventReactive(input$manager, {
        month_data %>% 
            filter(`Менеджер` %in% input$manager)
    })
    
    orders_current_tbl <- eventReactive(input$manager, {
        customer_orders_current_month %>% 
            filter(customer_manager %in% input$manager)
    })
    
    debt_tbl <- eventReactive(input$manager, {
        debt_customer_df %>% 
            filter(customer_manager %in% input$manager)
    })
    
    orders_3month_qty_tbl <- eventReactive(input$manager, {
        customer_orders_3month_qty %>% 
            filter(customer_manager %in% input$manager)
    })
    
    orders_3month_assort_tbl <- eventReactive(input$manager, {
        customer_orders_assort %>% 
            filter(customer_manager %in% input$manager)
    })

    
    # VALUEBOX MONTH OUTPUT ----
    output$value_boxes_month <- renderUI({
        
        total_predict_sum <- month_tbl() %>%
            pull(`Прогноз_Сумма`) %>% 
            sum()
        total_actual_sum <- month_tbl() %>%
            filter(`Прогноз_Сумма` > 0) %>% 
            pull(`Факт_Сумма`) %>% 
            sum()
        act_sum_box <- format(round(total_actual_sum), big.mark = "'")
        sum_completion_percent <- round(total_actual_sum / total_predict_sum * 100)
        total_actual_extra_sum <- month_tbl() %>%
            filter(`Прогноз_Сумма` == 0) %>% 
            pull(`Факт_Сумма`) %>% 
            sum()
        total_predict_customers <- month_tbl() %>% 
            filter(`Прогноз_Сумма` > 0) %>% 
            nrow()
        total_actual_customers <- month_tbl() %>% 
            filter(`Прогноз_Сумма` > 0,
                   `Факт_Сумма`    > 0) %>% 
            nrow()
        total_actual_extra_customers <- month_tbl() %>% 
            filter(`Прогноз_Сумма` == 0,
                   `Факт_Сумма`    > 0) %>% 
            nrow()
        qty_completion_percent <- round(total_actual_customers / total_predict_customers * 100)
        
        tagList(
            column(
                width = 6,
                info_card(
                    title = HTML("<span style='color:white;'>Общая сумма</span>"), 
                    value = HTML(str_glue("<span class='label label-info'>{act_sum_box}</span>")), 
                    prop_value = sum_completion_percent,
                    extra_value = format(round(total_actual_extra_sum), big.mark = "'"),
                    extra_prop_value = round(total_actual_extra_sum / total_predict_sum * 100),
                    bg_color  = case_when(
                        sum_completion_percent < 50  ~ "danger",
                        sum_completion_percent < 80  ~ "warning",
                        sum_completion_percent >= 80 ~ "success"
                    ), 
                    sub_text_color = "default", 
                    main_icon = "money-bill-wave"
                )
            ),
            
            column(
                width = 6,
                info_card(
                    title = HTML("<span style='color:white;'>Количество покупателей</span>"), 
                    value = HTML(str_glue("<span class='label label-info'>{total_actual_customers}</span>")),
                    prop_value = qty_completion_percent,
                    extra_value = total_actual_extra_customers,
                    extra_prop_value = round(total_actual_extra_customers / total_predict_customers * 100),
                    bg_color = case_when(
                        qty_completion_percent < 50  ~ "danger",
                        qty_completion_percent < 80  ~ "warning",
                        qty_completion_percent >= 80 ~ "success"
                    ), 
                    sub_text_color = "default",
                    main_icon = "users"
                )
            )
        )
    }) 
    
    # HISTOGRAM ORDERS
    output$orders <- renderPlot({
       
        avr_order_sum <- orders_current_tbl() %>% 
            distinct(nmbr_cust_order_doc, cust_order_doc_sum) %>% 
            pull(cust_order_doc_sum) %>% 
            median()
        
        qty_supplier_orders <- orders_current_tbl() %>% 
            filter(!is.na(nmbr_supplier_order)) %>% 
            distinct(nmbr_cust_order_doc) %>% 
            nrow()
        
        total_cust_orders <- orders_current_tbl() %>% 
            distinct(nmbr_cust_order_doc) %>% 
            nrow()
        
        orders_current_tbl() %>% 
            mutate(flag_sale_doc = ifelse(is.na(nmbr_sale_doc),
                                          0,
                                          1)) %>%
            ggplot(aes(x = date_cust_order_doc,
                       fill = factor(flag_sale_doc,
                                     levels = c("0", "1"),
                                     labels = c("Нет", "Да")))) +
            geom_bar(stat = "count", position = "stack") +
            geom_text(aes(label=..count..), stat="count", position=position_stack(0.5)) +
            scale_x_date(date_minor_breaks = "1 day") +
            labs(x = "Дата", y = "Кол-во заказов", fill = "Создание\nнакладных",
                 title = "Количество созданных заказов по дням",
                 subtitle = paste0("Общее количество закзов - ",
                                   total_cust_orders,
                                   "\n Средняя сумма заказа - ",
                                   round(avr_order_sum),
                    "грн \n Доля заказов, потребовавших заказа у поставщика - ",
                            round(qty_supplier_orders /total_cust_orders*100, 1),
                            "%")) +
            theme_bw()
    })
    
    # RFM-STATUS FIGURE
    
    output$rfm <- renderPlot({
        
        month_tbl() %>%  
            filter(`Прогноз_Сумма` > 0) %>% 
            group_by(`RFM-статус`) %>% 
            summarise(actual  = sum(`Факт_Сумма` > 0),
                      predict = n()) %>% 
            arrange(desc(`predict`)) %>% 
            ggplot(aes(y = reorder(`RFM-статус`, predict),
                       x = actual,
                       xend = predict)) +  
            geom_dumbbell(size = 1.2,
                          size_x = 3, 
                          size_xend = 3,
                          colour = "grey", 
                          colour_x = "blue", 
                          colour_xend = "red") +
            geom_text(aes(label = (predict - actual), x = (predict + actual) / 2), vjust = -0.5 , size = 4) +
            theme_minimal() + 
            labs(title = "План-факт по RFM-сегментам",
                 subtitle = "Указана разница между прогнозом (красный) и фактом (синий)",
                 x = "Кол-во покупателей",
                 y = "")
        
    })
    
    # DATA TABLE MONTH PLAN ----
    output$month_table <- renderDataTable({
        
        month_tbl() %>%
            select(-c(`Менеджер`, `Подразделение`, `Факт_транзакций`,
                      `Код_покупателя`)) %>%
            DT::datatable(
                rownames = FALSE,
                caption = "Выполнение по покупателям",
                extensions = c('Scroller',
                               'ColReorder'),
                options = list(
                    dom = 'Brtip',
                    colReorder = TRUE,
                    columnDefs = list(list(
                        targets = 0,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 20 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                            "}"))),
                    scrollX      = TRUE,
                    deferRender  = TRUE,
                    scrollY      = 300,
                    scroller     = TRUE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                        "}"),
                    autowidth = TRUE
                ),
                filter = list(position = 'top',
                              plain    = TRUE)
            )
    })
    
    # VALUEBOX WEEK OUTPUT ----
    output$value_boxes_week <- renderUI({
        
        total_predict_sum <- weekly_tbl() %>%
            pull(`Прогноз_Сумма`) %>% 
            sum()
        total_actual_sum <- weekly_tbl() %>%
            filter(`Прогноз_Сумма` > 0) %>% 
            pull(`Факт_Сумма`) %>% 
            sum()
        act_sum_box <- format(round(total_actual_sum), big.mark = "'")
        sum_completion_percent <- round(total_actual_sum / total_predict_sum * 100)
        total_actual_extra_sum <- weekly_tbl() %>%
            filter(`Прогноз_Сумма` == 0) %>% 
            pull(`Факт_Сумма`) %>% 
            sum()
        total_predict_customers <- weekly_tbl() %>% 
            filter(`Прогноз_Сумма` > 0) %>% 
            nrow()
        total_actual_customers <- weekly_tbl() %>% 
            filter(`Прогноз_Сумма` > 0,
                   `Факт_Сумма`    > 0) %>% 
            nrow()
        total_actual_extra_customers <- weekly_tbl() %>% 
            filter(`Прогноз_Сумма` == 0,
                   `Факт_Сумма`    > 0) %>% 
            nrow()
        qty_completion_percent <- round(total_actual_customers / total_predict_customers * 100)
        
        tagList(
            column(
                width = 5,
                info_card(
                    title = HTML("<span style='color:white;'>Общая сумма</span>"), 
                    value = HTML(str_glue("<span class='label label-info'>{act_sum_box}</span>")), 
                    prop_value = sum_completion_percent,
                    extra_value = format(round(total_actual_extra_sum), big.mark = "'"),
                    extra_prop_value = round(total_actual_extra_sum / total_predict_sum * 100),
                    bg_color  = case_when(
                        sum_completion_percent < 50  ~ "danger",
                        sum_completion_percent < 80  ~ "warning",
                        sum_completion_percent >= 80 ~ "success"
                        ), 
                    sub_text_color = "default", 
                    main_icon = "money-bill-wave"
                )
            ),

            column(
                width = 5,
                info_card(
                    title = HTML("<span style='color:white;'>Количество покупателей</span>"), 
                    value = HTML(str_glue("<span class='label label-info'>{total_actual_customers}</span>")),
                    prop_value = qty_completion_percent,
                    extra_value = total_actual_extra_customers,
                    extra_prop_value = round(total_actual_extra_customers / total_predict_customers * 100),
                    bg_color = case_when(
                        qty_completion_percent < 50  ~ "danger",
                        qty_completion_percent < 80  ~ "warning",
                        qty_completion_percent >= 80 ~ "success"
                    ), 
                    sub_text_color = "default",
                    main_icon = "users"
                )
            )
        )
    }) 
    
    # DATA TABLE WEEK PLAN ----
    output$weekly_table <- renderDataTable({
        weekly_tbl() %>%
            select(-c(`Менеджер`, `Подразделение`, `Условно-ожидаемые_транзакции`,
                      `Факт_транзакций`, `Код_покупателя`)) %>%
            relocate(`RFM-статус`, `Тип`, .after = `Факт_Сумма`) %>% 
            DT::datatable(
                rownames = FALSE,
                caption = "Выполнение по покупателям",
                extensions = c('Scroller',
                               'ColReorder'),
                options = list(
                    dom = 'Brtip',
                    colReorder = TRUE,
                    columnDefs = list(list(
                        targets = 0,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 20 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                            "}"))),
                    scrollX      = TRUE,
                    deferRender  = TRUE,
                    scrollY      = 300,
                    scroller     = TRUE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                        "}"),
                    autowidth = TRUE
                ),
                filter = list(position = 'top',
                              plain    = TRUE)
            )
    })
    
    
    # DEBT PIE CHART
    output$debt <- renderPlot({

        plot_tbl <- debt_tbl() %>%
            group_by(debt_period) %>%
            summarise(value = round(sum(sale_doc_sum)/1000, 1))
        total_debt <- sum(plot_tbl$value)
        plot_tbl %>%
            ggplot(aes(x = "", y = value, fill = debt_period)) + 
            geom_bar(width = 1, stat = "identity") +
            coord_polar(theta = "y", start=0) +
            scale_fill_brewer(palette="Blues") +
            labs(fill="", 
                 x=NULL, 
                 y=NULL, 
                 title="Структура дебиторской задолженности, тыс.грн",
                 subtitle = paste0("Общая сумма - ", total_debt, "тыс.грн")) + 
            ggrepel::geom_text_repel(aes(label = value), size = 4,
                                     position = position_stack(vjust = 0.5)) +
            theme_void()
    })
    
    
    # DATA TABLE DEBT ----
    
    output$debt_table <- renderDataTable({
        
        debt_tbl() %>%
            left_join(select(ref_customers, customer_id, customer_name)) %>%
            group_by(customer_name, debt_period) %>% 
            summarise(`Общая` = first(debt_sum),
                      value = round(sum(sale_doc_sum), 2)) %>% 
            pivot_wider(names_from = debt_period, values_from = value) %>% 
            mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
            rename(`Покупатель` = customer_name) %>% 
            DT::datatable(
                rownames = FALSE,
                caption = "Дебиторская задолженность по покупателям",
                extensions = c('Scroller',
                               'ColReorder'),
                options = list(
                    dom = 'Brtip',
                    colReorder = TRUE,
                    columnDefs = list(list(
                        targets = 0,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 20 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                            "}"))),
                    scrollX      = TRUE,
                    deferRender  = TRUE,
                    scrollY      = 400,
                    scroller     = TRUE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                        "}"),
                    autowidth = TRUE
                ),
                filter = list(position = 'top',
                              plain    = TRUE)
            )
    })
    
    
    # DAY PLAN TABLE ----
    
    output$day_plan_table <- DT::renderDataTable({
        
        customers_for_plan <- weekly_tbl() %>%
            mutate(diff_trans = `Условно-ожидаемые_транзакции` - `Факт_транзакций`) %>% 
            arrange(desc(diff_trans), desc(`Прогноз_Сумма`)) %>% 
            slice_head(n = 15) %>% 
            select(-c(`Подразделение`, `Менеджер`, `Флаг_Головн`, diff_trans))
        
        customers_for_plan %>%
            select(-`Код_покупателя`) %>% 
            DT::datatable(
                selection = list(mode = 'single', selected = 1),
                caption = "Наиболее вероятные покупатели",
                extensions = c('Scroller'),
                options = list(
                    dom = 'Brtip',
                    columnDefs = list(list(
                        targets = 1,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 20 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                            "}"))),
                    scrollX      = TRUE,
                    deferRender  = TRUE,
                    scrollY      = 300,
                    scroller     = TRUE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                        "}"),
                    autowidth = TRUE
                )
            )
    })
    
    selected_customer <- eventReactive(input$day_plan_table_rows_selected, {
        
        weekly_tbl() %>%
            mutate(diff_trans = `Условно-ожидаемые_транзакции` -
                       `Факт_транзакций`) %>% 
            arrange(desc(diff_trans), desc(`Прогноз_Сумма`)) %>% 
            .[input$day_plan_table_rows_selected, ] %>% 
            select(`Код_покупателя`) %>% 
            pull()
    })
    
   
    # SELECTED CUSTOMERS INFORMATION
    
    output$customer_info <- renderUI({

       customer_orders_info <- orders_3month_qty_tbl() %>%
            filter(customer_id == selected_customer())

        customer_orders_qty <- ifelse(nrow(customer_orders_info) == 0,
                                      0,
                                      customer_orders_info$orders_qty)
        last_customer_orders <- ifelse(
            nrow(customer_orders_info) == 0,
            "-",
            as.integer(today() - customer_orders_info$last_order))
        
        cust_avr_order_sum <- ifelse(
            nrow(customer_orders_info) == 0,
            0,
            round(customer_orders_info$orders_sum / customer_orders_qty))
        
        manager_avr_order_sum <- round(
            sum(orders_3month_qty_tbl()$orders_sum) /
                sum(orders_3month_qty_tbl()$orders_qty))
        
        customer_debt_info <- debt_tbl() %>%
            filter(customer_id == selected_customer())

        customer_arrears_sum <- ifelse(
            nrow(customer_debt_info) == 0,
                 0,
                 round(customer_debt_info$debt_sum[1] -
                     sum(customer_debt_info$sale_doc_sum[
                         customer_debt_info$debt_period == "Непросроченная"]),2))

        customer_arrears_day <- ifelse(
            nrow(customer_debt_info) == 0,
                 "-",
                 customer_debt_info$contract_arrears_day[1])

        arrears_info <- customer_debt_info %>%
            filter(debt_period != "Непросроченная") %>%
            select(nmbr_sale_doc:sale_doc_sum) %>%
            rename(`Номер` = nmbr_sale_doc,
                   `Дата` = date_sale_doc,
                   `Сумма` = sale_doc_sum)

        tagList(
            hr(),
            h5("Дополнительная информация о выбранном клиенте"),

            p(h6(str_glue(
                "Количество заказов за последние 3 месяца: ",
                customer_orders_qty,
                ",  количество дней от последнего заказа: ",
                last_customer_orders))),
            p(h6(str_glue("Средняя сумма заказа: ", cust_avr_order_sum,
                          "грн (по менеджеру - ", manager_avr_order_sum, ")"))),
            p(h6(str_glue(
               "Количество дней от последнего контакта: ",
               ",  причина: "))),
            p(h6(str_glue(
                "Просроченная заложенность: ", customer_arrears_sum,
                ",  отсрочка: ", customer_arrears_day))),
            if(customer_arrears_sum != 0){
                  p(tableHTML(arrears_info))
            }
        )
    })
    
    rv_basket <-eventReactive(selected_customer(), {
        orders_3month_assort_tbl() %>%
            filter(customer_id == selected_customer(),
                   group_sum > 10) %>% 
            left_join(select(ref_item_group, group_id, group_name)) %>% 
            select(group_name, group_sum)
    })
    
    
    # BASKET ITEM GROUPS ----
    output$basket <- renderDataTable({
        
      rv_basket() %>%
          DT::datatable(
                caption = "Заказы за последних 3 месяца",
                colnames = c("Номенклатурная группа", "Сумма"),
                options = list(
                    dom = 't',
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                        "}"))
            )
         
    })
    
    
    # RECOMMENDATION ITEM GROUPS ----
    output$recommendation <- renderDataTable({

        selected_customer_segment <-data.frame(
                  customer_id = selected_customer()) %>%
            left_join(select(ref_customers, customer_id, segment_form)) %>% 
            pull(segment_form)

        if(is.na(selected_customer_segment)){
            message("У выбранного покупателя не указан сегмент")
            
        } else {
            segment_assort %>% 
                filter(segment_form == selected_customer_segment) %>% 
                select(-segment_form) %>%
                anti_join(rv_basket(),
                          by = c("group_name_3lev" = "group_name")) %>% 
                mutate(cust_qty_share = cust_qty_share / 100) %>% 
                rename(`Номенклатурная группа` = group_name_3lev,
                       `Доля у постоянных` = cust_qty_share) %>% 
                DT::datatable(
                    caption = "Наиболее популярные группы у постоянных сегмента",
                    options = list(
                        dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                            "}"))
                ) %>% 
                formatPercentage('Доля у постоянных', 0)
        }
  
    })
    
    
    selected_order_tbl <- eventReactive(input$order_selection, {
        customer_orders_df %>% 
            filter(nmbr_cust_order_doc %in% input$order_selection)
    })
    
    # DATA TABLE FULL ORDER ----
    output$order_basket <- renderDataTable({
        
        selected_order_customer <- selected_order_tbl() %>%
            distinct(customer_id) %>% 
            left_join(select(ref_customers, customer_id, customer_name,
                             segment_form))
        
        cust_name <- selected_order_customer %>% 
            pull(customer_name)
        
        cust_segment <- selected_order_customer %>% 
            pull(segment_form)
        
        selected_order_tbl() %>%
            left_join(select(ref_item_group, group_id, group_name)) %>% 
            select(group_name, group_qty, group_sum) %>%
            datatable(options = list(
                                dom = "t",
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                                    "}")),
                      caption = paste0("Содержание заказа по покупателю ",
                                       cust_name, ",
                                       сегмент - ", cust_segment),
                      colnames = c("Номенклатурная группа", "Кол-во", "Сумма"))
    })
    
    
    # DATA TABLE ORDER RECOMMENDATION ----
    
    output$order_recommend <- renderDataTable({
        
        order_groups <- selected_order_tbl() %>%
            left_join(select(ref_item_group, group_id, group_name)) %>% 
            pull(group_name)
        
        permut_2set <- if(length(order_groups) >= 2){
            order_groups %>% gtools::permutations(n = length(order_groups),
                                                  r = 2) %>%
                apply(1, paste, collapse = ',')
        } else NULL
        
        permut_3set <- if(length(order_groups) >= 3){
            order_groups %>% gtools::permutations(n = length(order_groups),
                                                  r = 3) %>%
                apply(1,paste, collapse = ',')
        } else NULL
        
        order_permut <- data.frame(set = c(order_groups,
                                           permut_2set,
                                           permut_3set))
        
        
        selected_order_cust_segment <- selected_order_tbl() %>%
            distinct(customer_id) %>% 
            left_join(select(ref_customers, customer_id, customer_name,
                             segment_form)) %>% 
            pull(segment_form)
        
        segment_basket <- segments_basket %>% 
            filter(segment_form == selected_order_cust_segment) %>% 
            select(-segment_form)
        
        recommend_tbl <- segment_basket %>% 
            semi_join(order_permut, by = c("LHS" = "set")) %>% 
            distinct(RHS, .keep_all = TRUE) %>% 
            datatable(options = list(
                                dom = "t",
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'black'});",
                                    "}")),
                      caption = paste0(
                          "Список, что покупают с выбранными группами"),
                      colnames = c("Товары в заказе",
                                   "Часто покупаются с выбранными"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
