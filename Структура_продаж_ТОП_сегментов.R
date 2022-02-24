library(tidyverse)
library(lubridate)
library(dtplyr)

source(here::here("global.R"))

rm(list = ls(pattern = "tbl", all.names = TRUE))

date_finish <- as_datetime(floor_date(today(), unit = "day"))
date_finish_for_base <- date_finish + years(2000)

top_segments <- readxl::read_xlsx("data/top_segments_2021.xlsx") %>% 
    filter(cum_share < 80,
           !is.na(segment_form)) %>% 
    pull(segment_form)

top_customers <- readxl::read_xlsx(paste0("RFM/RFM-анализ_за_",
                                   month(date_finish - months(1), label = TRUE, abbr = FALSE),
                                   ".xlsx")) %>% 
    filter(segment %in% c("Чемпионы", "Лояльные"),
           segment_form %in% top_segments) %>% 
    distinct(customer_id) %>% 
    pull()

b2b_projects <- ref_projects %>% 
    filter(parent_project == "Сервис итого") %>% 
    pull(project_name)

full_sale_data <- sale_data_full(date_finish_for_base - years(1), date_finish_for_base) %>%
    filter(project %in% b2b_projects,
           !subdiv_name %in% c("Бухгалтерия",
                               "Відділ складської логістики",
                               "Администрация")
    ) %>% 
    mutate(date_sale_doc = as.Date(date_sale_doc) - years(2000))

full_returns_df <- refund_data(date_finish_for_base - years(1), date_finish_for_base) %>% 
    mutate(date_refund_doc = as.Date(date_refund_doc) - years(2000),
           date_sale_doc = as.Date(date_sale_doc) - years(2000)) %>% 
    filter(date_sale_doc >= date_finish - years(1)) %>% 
    group_by(nmbr_sale_doc, date_sale_doc, item_id) %>% 
    summarise(return_item_qty = sum(item_qty),
              return_item_sum = sum(item_sum))

full_sale_df <- lazy_dt(full_sale_data) %>%

    group_by(nmbr_sale_doc, date_sale_doc, item_id) %>% 
    summarise(customer_id = first(customer_id),
              item_qty = sum(item_qty),
              item_sum = sum(item_sum)) %>%
    ungroup() %>%
    
    # Добавляем возвраты    
    left_join(full_returns_df) %>% 
    replace_na(list(return_item_qty = 0, return_item_sum = 0)) %>% 
    mutate(item_qty = item_qty - return_item_qty,
           item_sum = item_sum - return_item_sum) %>%
    
    # Удаляем документы, по которым возвраты на ту же сумму    
    filter(item_qty > 0) %>% 
    
    select(-c(return_item_qty, return_item_sum)) %>% 
    
    left_join(select(ref_items, item_id, group_id)) %>% 
    left_join(select(ref_item_group, group_id, group_name)) %>% 
    left_join(select(ref_customers, customer_id, segment_form, main_cust_id)) %>% 

    filter(segment_form %in% top_segments,
           customer_id %in% top_customers |
             (!is.na(main_cust_id) & main_cust_id %in% top_customers)
    ) %>% 
    collect()

segment_cust <- full_sale_df %>%
    group_by(segment_form) %>% 
    summarise(segment_cust_qty = length(unique(customer_id))) %>% 
    ungroup()

ref_item_group_df <- ref_item_group %>%
    left_join(select(ref_item_group, group_id, parent_name = group_name),
              by = c("parent_group" = "group_id")) %>% 
    filter(group_marked == FALSE,
           parent_name != "- УДАЛЕНИЕ -")

groups_3_lev <- ref_item_group_df %>% 
    select(group_id, group_name, parent_group, parent_name) %>%
    filter(group_id %in% unique(full_sale_df$group_id)) %>% 
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

segment_df <- full_sale_df %>% 
    drop_na(group_id) %>% 
    left_join(select(groups_3_lev, group_id, group_name_3lev)) %>%
    group_by(nmbr_sale_doc, date_sale_doc, group_name_3lev) %>% 
    summarise(customer_id = first(customer_id),
              segment_form = first(segment_form),
              group_qty = sum(item_qty),
              group_sum = sum(item_sum)) %>%
    ungroup() %>%
    drop_na(group_name_3lev) %>% 
    group_by(segment_form, group_name_3lev) %>% 
    summarise(doc_qty = n(),
              cust_qty = length(unique(customer_id)),
              sale_sum = sum(group_sum)) %>% 
    ungroup() %>% 
    left_join(segment_cust) %>% 
    group_by(segment_form) %>% 
    mutate(segment_sale_sum = sum(sale_sum)) %>% 
    ungroup() %>% 
    mutate(cust_qty_share = round(cust_qty / segment_cust_qty *100), 
           group_sum_share = sale_sum / segment_sale_sum) %>% 
    arrange(segment_form, desc(cust_qty_share))

threshold <- 45 # %

final_df <- segment_df %>% 
    filter(cust_qty_share > threshold) %>% 
    select(segment_form, group_name_3lev, cust_qty_share)

writexl::write_xlsx(final_df, paste0("Ассортимент/Чемпионы_группы_",
                                     month(today(), label = TRUE),
                                     ".xlsx"))


count_other_fun <- function(df) {
    df %>% 
    count(group_name_3lev = fct_collapse(group_name_3lev, 
                                         `Інші` = group_name_3lev[cust_qty_share < threshold]),
          wt = group_sum_share)
}

final_df <- segment_df %>% 
    group_by(segment_form) %>% 
    nest() %>% 
    transmute(top_group = map(data, count_other_fun)) %>% 
    unnest()
