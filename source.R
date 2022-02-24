new_customers <- setdiff(unique(sale_df$customer_id), unique(sale_df_previos$customer_id))

change_customers <- setdiff(unique(select(sale_df, customer_id, subdiv_name)),
                            unique(select(sale_df_previos, customer_id = customer_rfm_id,
                                          subdiv_name))) %>%
    pull(customer_id) %>% 
    setdiff(new_customers)

change_customers_data <- sale_df_previos %>% 
    filter(customer_id %in% change_customers,
           main_flag > 0) %>% 
    distinct(customer_id, .keep_all = TRUE) %>% 
    select(customer_id, customer_rfm_id, subdiv_new, customer_manager_new, registr_date_rfm)

unique(b$customer_id)

sum(b$subdiv_change_flag)

sale_df_new <- sale_df %>% 
    left_join(change_customers_data)

change_subdiv_previos <- sale_df_previos %>%
    filter(subdiv_change_flag > 0) %>% 
    distinct(customer_id, subdiv_name, subdiv_new)

change_subdiv <- sale_df %>% 
    filter(!customer_id %in% new_customers,
           !customer_id %in% change_customers) %>% 
    distinct(customer_id, subdiv_name) %>% 
    setdiff(unique(select(sale_df_previos, customer_id = customer_rfm_id,
                          subdiv_name = subdiv_new))) %>% 
    intersect(select(change_subdiv_previos, customer_id, subdiv_name,))

change_subdiv <- sale_df %>% 
    filter(!customer_id %in% new_customers,
           !customer_id %in% change_customers) %>% 
    distinct(customer_id, subdiv_name) %>% 
    setdiff(unique(select(sale_df_previos, customer_id = customer_rfm_id,
                          subdiv_name = subdiv_new))) %>% 
    setdiff(select(change_subdiv_previos, customer_id, subdiv_name,))

a <- sale_df_previos %>% 
    filter(customer_id == "000011714")

previos_customers_data <- sale_df_previos %>% 
    select(customer_id, customer_rfm_id, subdiv_name, subdiv_new, customer_manager_new,
           registr_date_rfm, main_flag, subdiv_change_flag) %>% 
    distinct(customer_id, subdiv_name, .keep_all = TRUE)

sale_df_new <- sale_df %>% 
    left_join(previos_customers_data)

without_change <- sale_df_new %>% 
    filter(customer_id == customer_rfm_id & subdiv_name == subdiv_new)
sum(without_change$main_flag)
sum(without_change$subdiv_change_flag)

new_customers <- sale_df_new %>% 
    filter(is.na(customer_rfm_id))
length(unique(new_customers$customer_id))

change_customers <- sale_df_new %>%
    filter(customer_id != customer_rfm_id)
length(unique(change_customers$customer_id))

change_subdiv <- sale_df_new %>%
    filter(subdiv_name != subdiv_new)
length(unique(change_subdiv$subdiv_name))

a <- previos_customers_data %>% 
    filter(customer_id == "001032614")
sum(is.na(sale_df_new$customer_manager_new))
b <- sale_df_new %>% 
    filter(is.na(subdiv_change_flag),
           #customer_manager != only_one_manager
           )

filter(
    # subdiv_name == subdiv_new,
    #customer_id != customer_rfm_id)
    is.na(customer_rfm_id),
    # is.na(customer_rfm_id_2),
    # is.na(customer_manager_new_3),
    # !is.na(main_cust_id)
    # is.na(only_one_manager),
    # is.na(customer_manager) | is.na(main_manager) | customer_manager != main_manager
)

date_finish - week(1)

summary(clv_df)
summary(estimation)
pred_week_act <- transaction_log %>% 
    filter(date > date_finish - weeks(2),
           date <= date_finish - weeks(1)) %>% 
    group_by(cust) %>% 
    summarise(actual_trans = n(),
              actual_spending = sum(sales))

pred_df <- prediction %>% 
    select(Id, predicted.mean.spending, CET) %>% 
    mutate(predict_spending = round(predicted.mean.spending * round(CET))) %>% 
    left_join(pred_week_act,
              by = c("Id" = "cust")) %>% 
    replace_na(list(actual_trans = 0, actual_spending = 0))

library(pROC)
library(cvms)
ROC_threshold <- function(truth, prediction) {
    ROC <- roc(truth, prediction)
    ROC_table <- cbind(ROC$thresholds,
                       ROC$specificities,
                       ROC$sensitivities)
    ROC_table[which.max(ROC_table[,2] + ROC_table[,3]),]
}
roc_tbl_raw <- prediction %>% 
    transmute(pred = ifelse(CET < 1, CET, 1),
              truth = ifelse(actual.x == 0, 0, 1))
ROC_threshold(roc_tbl_raw$truth, roc_tbl_raw$pred)
conf_tbl <- prediction %>% 
    transmute(pred = ifelse(CET < 0.4, 0, 1),
              truth = ifelse(actual.x == 0, 0, 1))

eval <- evaluate(
    data = conf_tbl,
    target_col = "truth",
    prediction_cols = "pred",
    type = "binomial"
)
plot_confusion_matrix(eval) +
    ggplot2::labs(x = "Факт", y = "Прогноз")

a <- pred_df %>%  
    filter(CET < 0.2,
           actual_trans > 0)
sum(pred_df$actual_spending)
a <- pred_df %>% 
    mutate(predict_trans = case_when(
                                     CET < .2           ~ 0,
                                     CET < .5           ~ 1,
                                     TRUE               ~ round(CET)
                                     ),
           predict_spending = predicted.mean.spending * predict_trans * 1.1,
           CET = round(CET, 3)) %>% 
     summarise(sum(predict_spending))

