library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dbplyr)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "SERVER2",
                 Database = "work",
                 UID = "sa",
                 PWD = rstudioapi::askForPassword("Database password")
)

tbl_item <- tbl(con, "_Reference120") %>% 
    select("_IDRRef", item_id = "_Code", item_name = "_Description",
           pack_qty = "_Fld20858", "_Fld2048RRef", "_Fld2045RRef",
           "_Fld2060RRef", is_tva = "_Fld20858", item_marked = "_Marked") %>% 
    mutate(is_tva = as.logical(is_tva),
           item_marked = as.logical(item_marked))

tbl_group <- tbl(con, "_Reference122") %>% 
    select("_IDRRef", group_id = "_Code", group_name = "_Description",
           "_Fld19599RRef", "_Fld18403RRef", "_ParentIDRRef",
           group_marked = "_Marked") %>% 
    mutate(group_marked = as.logical(group_marked))

tbl_category <- tbl(con, "_Reference19598") %>% 
    select("_IDRRef", category_id = "_Code", category_name = "_Description")

tbl_price_group <- tbl(con, "_Reference235") %>% 
    select("_IDRRef", price_group = "_Description")

tbl_partner <- tbl(con, "_Reference102") %>% 
    select("_IDRRef", marked = "_Marked",
           partner_id = "_Code", partner_name = "_Description",
           "_ParentIDRRef", is_customer = "_Fld1741", is_supplier = "_Fld1742",
           edrp_code = "_Fld1754", link_main_cust = "_Fld21315RRef",
           link_main_manager = "_Fld1747RRef", registr_date = "_Fld1762",
           link_type_relationship = "_Fld1763RRef", link_main_act_form = "_Fld1753RRef",
           link_segment_form = "_Fld22000RRef"
           ) %>% 
    mutate(is_customer = as.logical(is_customer),
           is_supplier = as.logical(is_supplier),
           marked = as.logical(marked))

tbl_partner_tbl <- tbl(con, "_Reference102_VT1772") %>%
    select("_Reference102_IDRRef", link_subdiv = "_Fld1775RRef",
           link_manager = "_Fld1774RRef", date_entry = "_Fld1776")

tbl_type_relationship <- tbl(con, "_Reference232") %>% 
    select("_IDRRef", type_relationship = "_Description")

tbl_segment_form <- tbl(con, "_Reference21996") %>% 
    select("_IDRRef", segment_form = "_Description")

tbl_main_act_form <- tbl(con, "_Reference51") %>% 
    select("_IDRRef", main_act_form = "_Description")

tbl_character_types <- tbl(con, "_Chrc806") %>% 
    select("_IDRRef", code = "_Code")

tbl_char_types_users_set <- tbl(con, "_Chrc804") %>% 
    select("_IDRRef", code = "_Code")

tbl_object_property_value_ref <- tbl(con, "_Reference85") %>% 
    select("_IDRRef", name = "_Description")

tbl_user_set_rg <- tbl(con, "_InfoRg14842") %>% 
    select("_Fld14843RRef", link_setting = "_Fld14844RRef",
           link_user_subdiv = "_Fld14845_RRRef")

tbl_object_property_value_rg <- tbl(con, "_InfoRg14478") %>% 
    select("_Fld14479_RRRef", property = "_Fld14480RRef",
           value = "_Fld14481_RRRef")
 
tbl_users <- tbl(con, "_Reference138") %>% 
    select("_IDRRef", marked = "_Marked", user_id = "_Code",
           user_name = "_Description") %>% 
    mutate(marked = as.logical(marked))

tbl_store <- tbl(con, "_Reference156") %>%
    select("_IDRRef", store_name = "_Description", "_Fld2296RRef",
           "_ParentIDRRef")

tbl_subdiv <- tbl(con, "_Reference135") %>% 
    select("_IDRRef", subdiv_name = "_Description", subdiv_marked = "_Marked") %>% 
    mutate(subdiv_marked = as.logical(subdiv_marked))

tbl_individuals <- tbl(con,"_Reference191") %>% 
    select("_IDRRef", user_id = "_Code", user_name = "_Description")


ref_individuals <- tbl_individuals %>% 
    select(user_id, user_name) %>% 
    collect()

ref_items <- tbl_item %>% 
    left_join(tbl_price_group, by = c("_Fld2060RRef" = "_IDRRef")) %>%
    left_join(tbl_group, by = c("_Fld2045RRef" = "_IDRRef")) %>%
    left_join(tbl_partner, by = c("_Fld2048RRef" = "_IDRRef")) %>% 
    select(item_id, item_name, price_group, group_id, partner_id, pack_qty,
           is_tva, item_marked) %>% 
    collect()

ref_item_group <- tbl_group %>%
    left_join(select(tbl_group, "_IDRRef", parent_group = group_id),
              by = c("_ParentIDRRef" = "_IDRRef")) %>%
    left_join(tbl_category, by = c("_Fld19599RRef" = "_IDRRef")) %>% 
    left_join(tbl_users, by = c("_Fld18403RRef" = "_IDRRef")) %>% 
    select(group_marked, group_id, group_name, parent_group, category_name,
           category_manager = user_name) %>% 
    collect()

ref_store <- tbl_store %>% 
    left_join(select(tbl_store, "_IDRRef", parent_store = store_name),
              by = c("_ParentIDRRef" = "_IDRRef")) %>% 
    left_join(tbl_subdiv, 
              by = c("_Fld2296RRef" = "_IDRRef")) %>% 
    select(subdiv_name, subdiv_marked, store_name, parent_store) %>% 
    collect()

ref_users <- tbl_users %>%
    left_join(tbl_user_set_rg, by = c("_IDRRef" = "_Fld14843RRef")) %>% 
    inner_join(tbl_char_types_users_set, by = c(link_setting = "_IDRRef")) %>% 
    filter(code == "00005") %>%
    left_join(select(tbl_subdiv, "_IDRRef", subdiv_name),
              by = c(link_user_subdiv = "_IDRRef")) %>% 
    select(user_name, user_subdiv = subdiv_name, marked) %>% 
    collect()

tbl_type_customer <- select(tbl_partner, "_IDRRef", partner_id) %>% 
    left_join(tbl_object_property_value_rg, by = c("_IDRRef" = "_Fld14479_RRRef")) %>%
    inner_join(tbl_character_types, by = c(property = "_IDRRef")) %>%
    filter(code == "000000002") %>%
    left_join(tbl_object_property_value_ref, by = c(value = "_IDRRef")) %>% 
    select(partner_id, name) 

ref_customers <- tbl_partner %>% 
    filter(is_customer == "TRUE") %>%
    left_join(select(tbl_partner, "_IDRRef", main_cust_id = partner_id),
              by = c(link_main_cust = "_IDRRef")) %>%
    left_join(select(tbl_users, "_IDRRef", main_manager = user_id),
              by = c(link_main_manager = "_IDRRef")) %>% 
    left_join(tbl_type_relationship, by = c(link_type_relationship = "_IDRRef")) %>%
    left_join(tbl_segment_form, by = c(link_segment_form = "_IDRRef")) %>% 
    left_join(tbl_type_customer) %>%
    left_join(tbl_main_act_form, by = c(link_main_act_form = "_IDRRef")) %>% 
    left_join(select(tbl_partner, "_IDRRef", customer_folder = partner_name),
              by = c("_ParentIDRRef" = "_IDRRef")) %>%
    select(customer_id = partner_id, customer_name = partner_name,
           edrp_code, main_cust_id, main_manager, type_relationship,
           segment_form, customer_type = name, main_act_form,
           customer_folder, registr_date, marked
           ) %>%
    collect() 

 
ref_suppliers <- tbl_partner %>%
    filter(is_supplier == "TRUE") %>% 
    left_join(tbl_type_relationship, by = c(link_type_relationship = "_IDRRef")) %>%
    select(supplier_id = partner_id, supplier_name = partner_name,
           type_relationship, edrp_code, marked) %>% 
    collect()

tbl_contract <- tbl(con, "_Reference78") %>% 
    select("_IDRRef", contract_id = "_Code", link_partner = "_OwnerIDRRef",
           contract_name = "_Description", link_contract_type = "_Fld1600RRef",
           contract_arrears_day = "_Fld1589", contract_arrears_sum = "_Fld1588")
tbl_type_contract <- tbl(con, "_Enum484") %>% 
    select("_IDRRef", type_order = "_EnumOrder")

ref_contract <- tbl_contract %>% 
    left_join(select(tbl_partner, "_IDRRef", partner_id),
                                 by = c(link_partner = "_IDRRef")) %>% 
    left_join(tbl_type_contract, by = c(link_contract_type = "_IDRRef")) %>% 
    select(contract_id, contract_name, owner_partner = partner_id, type_order,
           contract_arrears_day, contract_arrears_sum) %>%
    filter(type_order %in% c(0,1)) %>% 
    collect()

ref_partner_manager <- tbl_partner_tbl %>% 
    left_join(select(tbl_partner, "_IDRRef", partner_id, partner_name),
              by = c("_Reference102_IDRRef" = "_IDRRef")) %>% 
    left_join(select(tbl_subdiv, "_IDRRef", subdiv_name),
              by = c(link_subdiv = "_IDRRef")) %>% 
    left_join(select(tbl_users, "_IDRRef", user_name),
              by = c(link_manager = "_IDRRef")) %>%
    select(partner_id, partner_name, subdiv_name, user_name, date_entry) %>% 
    collect()


tbl_delivery_addresses <- tbl(con, "_Reference194") %>% 
    select("_IDRRef", link_customer = "_OwnerID_RRRef", marked = "_Marked",
           address_name = "_Description", link_locality = "_Fld18128RRef",
           link_route = "_Fld19663RRef", link_region = "_Fld2738RRef") %>% 
    mutate(marked = as.logical(marked))
tbl_route <- tbl(con, "_Reference19642") %>% 
    select("_IDRRef", route = "_Description")
tbl_region <- tbl(con, "_Reference149") %>% 
    select("_IDRRef", region_name = "_Description", "_ParentIDRRef")
tbl_locality <- tbl(con, "_Reference199") %>% 
    select("_IDRRef", locality_name = "_Description")
ref_delivery_addresses <- tbl_delivery_addresses %>% 
    left_join(tbl_route,    by = c(link_route = "_IDRRef")) %>% 
    left_join(tbl_region,   by = c(link_region = "_IDRRef")) %>%
    left_join(select(tbl_region, "_IDRRef", parent_region = region_name),  
                     by = c("_ParentIDRRef" = "_IDRRef")) %>%
    left_join(tbl_locality, by = c(link_locality = "_IDRRef")) %>%
    left_join(select(tbl_partner, "_IDRRef", partner_id, partner_name),
              by = c(link_customer = "_IDRRef")) %>%
    select(partner_id, partner_name, address_name, locality_name, region_name,
           parent_region, route, marked) %>% 
    collect()

tbl_projects <- tbl(con, "_Reference143") %>% 
    select("_IDRRef", project_name = "_Description", "_ParentIDRRef")
ref_projects <- tbl_projects %>% 
    left_join(select(tbl_projects, "_IDRRef", parent_project = project_name),  
              by = c("_ParentIDRRef" = "_IDRRef")) %>%
    select(project_name, parent_project) %>% 
    collect()
    

dbDisconnect(con)


## Фейсы магазинов
get_face <- function() {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    tbl_face <- tbl(con,"_InfoRg16359") %>% 
        select(link_item = "_Fld16361RRef", link_store = "_Fld16360RRef",
               date = "_Period", face_qty = "_Fld16362", active = "_Active") %>% 
        mutate(active = as.logical(active)) %>% 
        filter(active == "TRUE")
    
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    
    tbl_store <- tbl(con,"_Reference156") %>%
        select("_IDRRef", store_name = "_Description")

    df_face <- tbl_face %>%
        group_by(link_item, link_store) %>% 
        slice_max(date) %>%
        ungroup() %>% 
        left_join(tbl_item, by = c(link_item = "_IDRRef")) %>% 
        left_join(tbl_store, by = c(link_store = "_IDRRef")) %>% 
        select(date, item_id, store_name, face_qty) %>%
        filter(face_qty > 0) %>% 
        collect()
    
    return(df_face)
    
    dbDisconnect(con)
}


## Нормы магазинов
get_norm <- function() {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    tbl_norm <- tbl(con,"_InfoRg18379") %>% 
        select(link_item = "_Fld18381RRef", link_store = "_Fld18380RRef",
               date = "_Period", norm_qty = "_Fld18382", active = "_Active") %>% 
        mutate(active = as.logical(active)) %>% 
        filter(active == "TRUE")
    
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    
    tbl_store <- tbl(con,"_Reference156") %>%
        select("_IDRRef", store_name = "_Description")
    
    df_norm <- tbl_norm %>%
        group_by(link_item, link_store) %>% 
        slice_max(date) %>%
        ungroup() %>% 
        left_join(tbl_item, by = c(link_item = "_IDRRef")) %>% 
        left_join(tbl_store, by = c(link_store = "_IDRRef")) %>% 
        select(date, item_id, store_name, norm_qty) %>%
        filter(norm_qty > 0) %>% 
        collect()
    
    return(df_norm)

    dbDisconnect(con)

}


## Продажи по розничным чекам
checks_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_checks_tbl <- tbl(con, "_Document329_VT7992") %>% 
        select("_Fld8019", "_Fld8020", "_Fld7994RRef", "_Fld7998", "_Fld7999",
               "_Document329_IDRRef")
    tbl_checks <- tbl(con, "_Document329") %>% 
        select("_IDRRef", posted = "_Posted", date = "_Date_Time",
               "_Fld7966RRef", "_Fld7960RRef") %>%
        mutate(posted = as.logical(posted)) %>%
        filter(date >= date_start,
               date < date_finish,
               posted == "TRUE")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")
    tbl_cashbox <- tbl(con, "_Reference89") %>% 
        select("_IDRRef", cashbox = "_Description")

    df_checks <- left_join(tbl_checks, tbl_checks_tbl,
                           by = c("_IDRRef" = "_Document329_IDRRef")) %>%
        left_join(tbl_item, by = c("_Fld7994RRef" = "_IDRRef")) %>%
        left_join(tbl_subdiv, by = c("_Fld7966RRef" = "_IDRRef")) %>%
        left_join(tbl_cashbox, by = c("_Fld7960RRef" = "_IDRRef")) %>%
        select(date, subdiv_name, cashbox,
               check_nmbr = "_Fld8019", check_time = "_Fld8020", item_id,
               qty = "_Fld7998", sum = "_Fld7999") %>% 
        collect()
    
    return(df_checks)
    
    dbDisconnect(con)
}


## Поступления товаров
receipt_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )

    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_receipt_tbl <- tbl(con,"_Document360_VT9812") %>% 
        select("_Document360_IDRRef", link_item = "_Fld9814RRef",
               item_qty = "_Fld9819", sum = "_Fld9821" , vat = "_Fld9823") %>% 
        mutate(sum = as.numeric(sum),
               vat = as.numeric(vat),
               item_sum = sum + vat)
    tbl_receipt <- tbl(con,"_Document360") %>% 
        select("_IDRRef", posted_receipt = "_Posted", nmbr_receipt = "_Number",
               date_receipt = "_Date_Time", is_managerial = "_Fld9766",
               doc_sum_currency = "_Fld9772", doc_rate = "_Fld9760",
               link_supplier = "_Fld9758RRef", link_suppl_contr = "_Fld9755RRef",
               link_subdiv = "_Fld9767RRef", link_store = "_Fld9770_RRRef",
               link_organization = "_Fld9762RRef",
               date_ready_for_receipt = "_Fld18354",
               date_processed = "_Fld18386",) %>%
         mutate(doc_sum_currency = as.numeric(doc_sum_currency),
                doc_rate = as.numeric(doc_rate),
                doc_sum = doc_sum_currency * doc_rate,
                posted_receipt = as.logical(posted_receipt),
                is_managerial = as.logical(is_managerial)
               ) %>% 
        filter(date_receipt >= date_start,
               date_receipt < date_finish,
               posted_receipt == "TRUE",
               is_managerial == "TRUE"
               )
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", supplier_name = "_Description")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", supplier_contract = "_Description")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organiz_name = "_Description")
    
    df_receipt <- tbl_receipt %>%
        left_join(tbl_receipt_tbl, by = c("_IDRRef" = "_Document360_IDRRef")) %>%
        left_join(tbl_item   ,     by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_partner,     by = c(link_supplier = "_IDRRef")) %>% 
        left_join(tbl_contract,    by = c(link_suppl_contr = "_IDRRef")) %>% 
        left_join(tbl_subdiv,      by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,       by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_organization, by = c(link_organization = "_IDRRef")) %>%
        select(nmbr_receipt, date_receipt, doc_sum,
               subdiv_name, store_name,
               date_ready_for_receipt, date_processed,
               supplier_name, supplier_contract, organiz_name,
               item_id, item_qty, item_sum
               ) %>% 
        collect()

    return(df_receipt)
    
    dbDisconnect(con)
}


## Заказ поставщику
supplier_order_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_supplier_order_tbl <- tbl(con,"_Document263_VT4682") %>% 
        select("_Document263_IDRRef", link_item = "_Fld4690RRef",
               item_qty = "_Fld4687", item_price = "_Fld4696",
               item_sum = "_Fld4693",
               item_pack_qty = "_Fld4698", link_unit = "_Fld4684RRef",
               link_order = "_Fld4686_RRRef") 
    tbl_supplier_order <- tbl(con,"_Document263") %>% 
        select("_IDRRef", posted = "_Posted", nmbr_supplier_order = "_Number",
               date_supplier_order = "_Date_Time",
               doc_sum_currency = "_Fld4672", doc_rate = "_Fld4665",
               link_supplier = "_Fld4663RRef",
               link_suppl_contr = "_Fld4658RRef",
               link_subdiv = "_Fld4669RRef", link_store = "_Fld4670RRef",
               link_organization = "_Fld4667RRef",
               date_creation = "_Fld4679"         # дата создания) 
               ) %>%  
        mutate(doc_sum_currency = as.numeric(doc_sum_currency),
               doc_rate = as.numeric(doc_rate),
               supplier_order_doc_sum = doc_sum_currency * doc_rate,
               posted = as.logical(posted)
        ) %>% 
        filter(date_supplier_order >= date_start,
               date_supplier_order < date_finish,
               posted == "TRUE"
        )
    
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", supplier_name = "_Description")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", supplier_contract = "_Description")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organiz_name = "_Description")
    tbl_unit <- tbl(con, "_Reference84") %>% 
        select("_IDRRef", item_unit = "_Description")
    tbl_customer_order <- tbl(con,"_Document262") %>% 
        select("_IDRRef", nmbr_cust_order_doc = "_Number",
               date_cust_order_doc = "_Date_Time")
    tbl_internal_order <- tbl(con,"_Document249") %>% 
        select("_IDRRef", nmbr_internal_order = "_Number",
               date_internal_order = "_Date_Time")
    
    df_supplier_order <- tbl_supplier_order %>%
        left_join(tbl_partner,        by = c(link_supplier = "_IDRRef")) %>% 
        left_join(tbl_contract,       by = c(link_suppl_contr = "_IDRRef")) %>% 
        left_join(tbl_subdiv,         by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,          by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_organization,   by = c(link_organization = "_IDRRef")) %>%
        left_join(tbl_supplier_order_tbl,
                                  by = c("_IDRRef" = "_Document263_IDRRef")) %>%
        left_join(tbl_item,           by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_unit,           by = c(link_unit = "_IDRRef")) %>%
        left_join(tbl_customer_order, by = c(link_order = "_IDRRef")) %>%
        left_join(tbl_internal_order, by = c(link_order = "_IDRRef")) %>%
        select(nmbr_supplier_order, date_supplier_order, supplier_order_doc_sum,
               subdiv_name, store_name, supplier_name, supplier_contract,
               organiz_name, date_creation,
               item_id, item_unit, item_qty, item_price, item_sum,item_pack_qty,
               nmbr_cust_order_doc, date_cust_order_doc,
               nmbr_internal_order, date_internal_order
        ) %>% 
        collect()
    
    return(df_supplier_order)
    
    dbDisconnect(con)
}


## Возврат товаров поставщикам
return_supplier_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_return_supplier <- tbl(con,"_Document252") %>% 
        select("_IDRRef", posted_return = "_Posted", nmbr_return = "_Number",
               date_return = "_Date_Time", is_managerial = "_Fld4048",
               link_supplier = "_Fld4056RRef", link_suppl_contr = "_Fld4055RRef",
               link_subdiv = "_Fld4051RRef", link_store = "_Fld4054RRef",
               link_organization = "_Fld4045RRef",
               doc_sum_currency = "_Fld4066", doc_rate = "_Fld4061") %>%
        mutate(doc_sum_currency = as.numeric(doc_sum_currency),
               doc_rate = as.numeric(doc_rate),
               doc_sum = doc_sum_currency * doc_rate,
               posted_return = as.logical(posted_return),
               is_managerial = as.logical(is_managerial)) %>% 
        filter(date_return >= date_start,
               date_return < date_finish,
               posted_return == "TRUE",
               is_managerial == "TRUE")
        
    tbl_return_supplier_tbl <- tbl(con,"_Document252_VT4098") %>% 
        select("_Document252_IDRRef", link_item = "_Fld4100RRef",
               item_qty = "_Fld4101", item_sum = "_Fld4106",
               link_receipt = "_Fld4111_RRRef")
    tbl_receipt <- tbl(con,"_Document360") %>% 
        select("_IDRRef", nmbr_receipt = "_Number", date_receipt = "_Date_Time")
        
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", supplier_name = "_Description")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", supplier_contract = "_Description")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organiz_name = "_Description")
    
    df_return <- tbl_return_supplier %>%
        left_join(tbl_return_supplier_tbl,
                                    by = c("_IDRRef" = "_Document252_IDRRef")) %>%
        left_join(tbl_receipt,      by = c(link_receipt = "_IDRRef")) %>% 
        left_join(tbl_item,         by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_partner,      by = c(link_supplier = "_IDRRef")) %>% 
        left_join(tbl_contract,     by = c(link_suppl_contr = "_IDRRef")) %>% 
        left_join(tbl_subdiv,       by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,        by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_organization, by = c(link_organization = "_IDRRef")) %>%
        select(nmbr_return, date_return, doc_sum,
               subdiv_name, store_name,
               supplier_name, supplier_contract, organiz_name,
               item_id, item_qty, item_sum, nmbr_receipt, date_receipt
        ) %>% 
        collect()
    
    return(df_return)
    
    dbDisconnect(con)
}


## Поступления товаров (приемка)
inspection_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    date_start_2 <- date_start - days(45)

    tbl_inspection_tbl <- tbl(con,"_Document21634_VT21637") %>% 
        select("_Document21634_IDRRef", link_item = "_Fld21639RRef",
               item_qty = "_Fld21640") 
    tbl_inspection <- tbl(con,"_Document21634") %>% 
        select("_IDRRef", posted_inspection = "_Posted", nmbr_inspection = "_Number",
               date_inspection = "_Date_Time", link_picker = "_Fld21648RRef",
               time_start = "_Fld21649", time_finish = "_Fld21650",
               link_document_base = "_Fld21635_RRRef") %>%
        mutate(posted_inspection = as.logical(posted_inspection)) %>% 
        filter(date_inspection >= date_start,
               date_inspection < date_finish,
               posted_inspection == "TRUE")
    tbl_individuals <- tbl(con,"_Reference191") %>% 
        select("_IDRRef", user_id = "_Code")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    
    tbl_receipt <- tbl(con,"_Document360") %>% 
        select("_IDRRef", nmbr_receipt = "_Number",  date_receipt = "_Date_Time",
               link_store = "_Fld9770_RRRef", link_supplier = "_Fld9758RRef") %>% 
        filter(date_receipt >= date_start_2,
               date_receipt < date_finish)
    tbl_moving_out <- tbl(con,"_Document411") %>% 
        select("_IDRRef", nmbr_moving_out = "_Number",  date_moving_out = "_Date_Time",
               link_store_sender = "_Fld12656RRef",
               link_store_recipient = "_Fld12657RRef") %>% 
        filter(date_moving_out >= date_start_2,
               date_moving_out < date_finish)
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", supplier_id = "_Code")

    
    df_inspection <- tbl_inspection %>%
        left_join(tbl_individuals,    by = c(link_picker = "_IDRRef")) %>%
        left_join(tbl_inspection_tbl, by = c("_IDRRef" = "_Document21634_IDRRef")) %>%
        left_join(tbl_item,           by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_receipt,        by = c(link_document_base = "_IDRRef")) %>%
        left_join(tbl_partner,        by = c(link_supplier = "_IDRRef")) %>% 
        left_join(tbl_store,          by = c(link_store = "_IDRRef")) %>%
        mutate(store_receipt = store_name) %>% 
        select(-store_name) %>% 
        left_join(tbl_moving_out,     by = c(link_document_base = "_IDRRef")) %>%
        left_join(tbl_store,          by = c(link_store_sender = "_IDRRef")) %>%
        mutate(store_sender = store_name) %>% 
        select(-store_name) %>%
        left_join(tbl_store,          by = c(link_store_recipient = "_IDRRef")) %>%
        mutate(store_recipient = store_name) %>%
        select(nmbr_inspection, date_inspection, user_id, time_start, time_finish, 
               item_id, item_qty,
               nmbr_receipt, date_receipt, store_receipt, supplier_id,
               nmbr_moving_out, date_moving_out, store_sender, store_recipient
        ) %>% 
        collect()
    
    return(df_inspection)
    
    dbDisconnect(con)
}


## Сборка товаров
assembly_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    date_start_2 <- date_start - days(30)
    
    tbl_assembly_tbl_1 <- tbl(con,"_Document21718_VT21727") %>% 
        select("_Document21718_IDRRef", link_item = "_Fld21729RRef",
               item_qty = "_Fld21730")
    tbl_assembly_tbl_2 <- tbl(con,"_Document21718_VT21734") %>% 
        select("_Document21718_IDRRef", link_box = "_Fld21736RRef",
               box_qty = "_Fld21737")
    tbl_assembly <- tbl(con,"_Document21718") %>% 
        select("_IDRRef", posted_assembly = "_Posted", nmbr_assembly = "_Number",
               date_assembly = "_Date_Time", link_picker = "_Fld21722RRef",
               time_start = "_Fld21725", time_finish = "_Fld21726",
               lieb_qty = "_Fld21755",
               link_document_base = "_Fld21719RRef") %>%
        mutate(posted_assembly = as.logical(posted_assembly)) %>% 
        filter(date_assembly >= date_start,
               date_assembly < date_finish,
               posted_assembly == "TRUE")
    tbl_individuals <- tbl(con,"_Reference191") %>% 
        select("_IDRRef", user_id = "_Code")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_box <- tbl(con,"_Reference192") %>% 
        select("_IDRRef", box_id = "_Code")
    tbl_deliv_order <- tbl(con,"_Document409") %>% 
        select("_IDRRef", nmbr_deliv_order = "_Number",
               date_deliv_order = "_Date_Time",
               link_store = "_Fld12588_RRRef") %>% 
        filter(date_deliv_order >= date_start_2,
               date_deliv_order < date_finish)
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")

    df_assembly <- tbl_assembly %>%
        left_join(tbl_individuals,    by = c(link_picker = "_IDRRef")) %>%
        left_join(tbl_assembly_tbl_2, by = c("_IDRRef" = "_Document21718_IDRRef")) %>%
        left_join(tbl_box,            by = c(link_box = "_IDRRef")) %>%
        left_join(tbl_assembly_tbl_1, by = c("_IDRRef" = "_Document21718_IDRRef")) %>%
        left_join(tbl_item,           by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_deliv_order,    by = c(link_document_base = "_IDRRef")) %>%
        left_join(tbl_store,          by = c(link_store = "_IDRRef")) %>%
        select(nmbr_assembly, date_assembly, user_id,
               time_start, time_finish, lieb_qty, box_id, box_qty,
               item_id, item_qty,
               nmbr_deliv_order, date_deliv_order, store_name
        ) %>% 
        collect()
    
    return(df_assembly)
    
    dbDisconnect(con)
}


## Заказ на доставку
delivery_order_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    date_start_2 <- date_start - days(30)
    
    tbl_deliv_order <- tbl(con,"_Document409") %>% 
        select("_IDRRef", posted_deliv_order = "_Posted", nmbr_deliv_order = "_Number",
               date_deliv_order = "_Date_Time", link_picker = "_Fld12599RRef",
               link_store = "_Fld12588_RRRef", 
               time_start = "_Fld12618", time_finish = "_Fld12619",
               pack_duration = "_Fld12613", box_qty = "_Fld12612",
               order_weight = "_Fld12605", date_delivery = "_Fld12603",
               link_direction = "_Fld12592RRef", link_region = "_Fld12591RRef",
               link_route = "_Fld19669RRef", link_delivery_type = "_Fld19733RRef",
               link_address = "_Fld12593RRef",
               link_document_base = "_Fld12587_RRRef") %>%
        mutate(posted_deliv_order = as.logical(posted_deliv_order)) %>% 
        filter(date_deliv_order >= date_start,
               date_deliv_order < date_finish,
               posted_deliv_order == "TRUE")
    tbl_individuals <- tbl(con,"_Reference191") %>% 
        select("_IDRRef", user_id = "_Code")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_direction <- tbl(con, "_Reference193") %>% 
        select("_IDRRef", direction = "_Description")
    tbl_region <- tbl(con, "_Reference149") %>% 
        select("_IDRRef", region = "_Description")
    tbl_route <- tbl(con, "_Reference19642") %>% 
        select("_IDRRef", route = "_Description")
    tbl_deliv_type <- tbl(con, "_Reference19643") %>% 
        select("_IDRRef", deliv_type = "_Description")
    tbl_address <- tbl(con, "_Reference194") %>% 
        select("_IDRRef", deliv_address = "_Description")
    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", nmbr_sale_doc = "_Number", date_sale_doc = "_Date_Time") %>% 
        filter(date_sale_doc >= date_start_2,
               date_sale_doc < date_finish)
    tbl_moving_out <- tbl(con,"_Document411") %>% 
        select("_IDRRef", nmbr_moving_out = "_Number",
               date_moving_out = "_Date_Time") %>% 
        filter(date_moving_out >= date_start_2,
               date_moving_out < date_finish)
    tbl_event <- tbl(con,"_Document382") %>% 
        select("_IDRRef", nmbr_event = "_Number",
               date_event = "_Date_Time") %>% 
        filter(date_event >= date_start_2,
               date_event < date_finish)
    
    df_deliv_order <- tbl_deliv_order %>%
        left_join(tbl_individuals, by = c(link_picker = "_IDRRef")) %>%
        left_join(tbl_store,       by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_direction,   by = c(link_direction = "_IDRRef")) %>%
        left_join(tbl_region,      by = c(link_region = "_IDRRef")) %>%
        left_join(tbl_route,       by = c(link_route = "_IDRRef")) %>%
        left_join(tbl_deliv_type,  by = c(link_delivery_type = "_IDRRef")) %>%
        left_join(tbl_address,     by = c(link_address = "_IDRRef")) %>%
        left_join(tbl_sale_doc,    by = c(link_document_base = "_IDRRef")) %>%
        left_join(tbl_moving_out,  by = c(link_document_base = "_IDRRef")) %>%
        left_join(tbl_event     ,  by = c(link_document_base = "_IDRRef")) %>%
        select(nmbr_deliv_order, date_deliv_order,
               direction, region, route, deliv_type, deliv_address,
               store_name, user_id, time_start, time_finish, pack_duration,
               box_qty, order_weight,
               nmbr_sale_doc, date_sale_doc, date_delivery,
               nmbr_moving_out, date_moving_out,
               nmbr_event, date_event
        ) %>% 
        collect()
    
    return(df_deliv_order)
    
    dbDisconnect(con)
}


## Заказы покупателей
customer_order_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_order_doc <- tbl(con,"_Document262") %>% 
        select("_IDRRef", posted_doc="_Posted", nmbr_cust_order_doc = "_Number",
               date_cust_order_doc = "_Date_Time", link_organiz ="_Fld4552RRef",
               link_subdiv = "_Fld4554RRef", link_customer = "_Fld4548RRef",
               link_cust_contract = "_Fld4543RRef",
               link_responsible = "_Fld4553RRef",
               cust_order_doc_sum = "_Fld4558") %>% 
        mutate(posted_doc = as.logical(posted_doc)) %>% 
        filter(date_cust_order_doc >= date_start,
               date_cust_order_doc < date_finish,
               posted_doc == "TRUE")
    tbl_order_doc_tbl <- tbl(con, "_Document262_VT4577") %>% 
        select(link_item = "_Fld4584RRef", item_qty = "_Fld4581",
               item_sum = "_Fld4589", item_auto_discount = "_Fld4593",
               "_Document262_IDRRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_id = "_Code")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", cust_contract_id = "_Code")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organization = "_Description")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_users <- tbl(con, "_Reference138") %>% 
        select("_IDRRef", responsible_name = "_Description")
    
    df_order<- tbl_order_doc %>%
        left_join(tbl_organization, by = c(link_organiz = "_IDRRef")) %>% 
        left_join(tbl_subdiv,       by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_partner,      by = c(link_customer = "_IDRRef")) %>% 
        left_join(tbl_contract,     by = c(link_cust_contract = "_IDRRef")) %>% 
        left_join(tbl_users,        by = c(link_responsible = "_IDRRef")) %>% 
        left_join(tbl_order_doc_tbl,by = c("_IDRRef"="_Document262_IDRRef")) %>%
        left_join(tbl_item,         by = c(link_item = "_IDRRef")) %>%
        select(nmbr_cust_order_doc, date_cust_order_doc, cust_order_doc_sum,
               organization, subdiv_name, customer_id, cust_contract_id,
               responsible_name,
               item_id, item_qty, item_sum, item_auto_discount
        ) %>% 
        collect()
    
    return(df_order)
    
    dbDisconnect(con)
}


## Реализация товаров и услуг детальная
sale_data_full <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)

    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", posted_sale_doc = "_Posted", nmbr_sale_doc = "_Number",
               is_managerial = "_Fld10901", date_sale_doc = "_Date_Time",
               link_subdiv = "_Fld10904RRef", link_store = "_Fld10907RRef", 
               link_customer = "_Fld10909RRef", link_project = "_Fld10927_RRRef") %>% 
        mutate(posted_sale_doc = as.logical(posted_sale_doc),
               is_managerial = as.logical(is_managerial)) %>% 
        filter(date_sale_doc >= date_start,
               date_sale_doc < date_finish,
               posted_sale_doc == "TRUE",
               is_managerial == "TRUE")
    tbl_sale_doc_tbl <- tbl(con, "_Document375_VT10956") %>% 
        select(link_item = "_Fld10964RRef", item_qty = "_Fld10961",
               item_sum = "_Fld10969", item_auto_discount = "_Fld10974",
               "_Document375_IDRRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_id = "_Code")
    tbl_project <- tbl(con, "_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")

    df_sale <- tbl_sale_doc %>%
        left_join(tbl_sale_doc_tbl, by = c("_IDRRef" = "_Document375_IDRRef")) %>%
        left_join(tbl_item,         by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_partner,      by = c(link_customer = "_IDRRef")) %>% 
        left_join(tbl_subdiv,       by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,        by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_project,      by = c(link_project = "_IDRRef")) %>%
        select(nmbr_sale_doc, date_sale_doc,
               subdiv_name, store_name, customer_id, project,
               item_id, item_qty, item_sum, item_auto_discount
        ) %>% 
        collect()
    
    return(df_sale)
    
    dbDisconnect(con)
}


## Реализация товаров и услуг общая
sale_data_global <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)

    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", posted_sale_doc = "_Posted", nmbr_sale_doc = "_Number",
               is_managerial = "_Fld10901", date_sale_doc = "_Date_Time", 
               link_subdiv = "_Fld10904RRef", link_store = "_Fld10907RRef", 
               link_customer = "_Fld10909RRef", link_project = "_Fld10927_RRRef",
               link_order = "_Fld10906_RRRef", doc_sum = "_Fld10920") %>% 
        mutate(posted_sale_doc = as.logical(posted_sale_doc),
               is_managerial = as.logical(is_managerial)) %>% 
        filter(date_sale_doc >= date_start,
               date_sale_doc < date_finish,
               posted_sale_doc == "TRUE",
               is_managerial == "TRUE")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_id = "_Code", customer_name = "_Description")
    tbl_project <- tbl(con, "_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_cust_order <- tbl(con, "_Document262") %>% 
        select("_IDRRef", nmbr_cust_order_doc = "_Number",
               date_cust_order_doc = "_Date_Time", cust_order_sum = "_Fld4558")

    df_sale <- tbl_sale_doc %>%
        left_join(tbl_partner,    by = c(link_customer = "_IDRRef")) %>% 
        left_join(tbl_subdiv,     by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,      by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_project,    by = c(link_project = "_IDRRef")) %>%
        left_join(tbl_cust_order, by = c(link_order = "_IDRRef")) %>%
        select(nmbr_sale_doc, date_sale_doc, doc_sum,
               nmbr_cust_order_doc, date_cust_order_doc, cust_order_sum,
               subdiv_name, store_name, customer_id, customer_name, project
        ) %>% 
        collect()
    
    return(df_sale)
    
    dbDisconnect(con)
}


## Возврат товаров от покупателей
refund_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)

    tbl_refund_doc <- tbl(con,"_Document251") %>% 
        select("_IDRRef", posted_refund_doc = "_Posted", nmbr_refund_doc = "_Number",
               is_managerial = "_Fld3903", date_refund_doc = "_Date_Time",
               link_subdiv = "_Fld3917RRef", link_store = "_Fld3907_RRRef", 
               link_customer = "_Fld3913RRef", link_project = "_Fld3922_RRRef") %>% 
        mutate(posted_refund_doc = as.logical(posted_refund_doc),
               is_managerial = as.logical(is_managerial)) %>% 
        filter(date_refund_doc >= date_start,
               date_refund_doc < date_finish,
               posted_refund_doc == "TRUE",
               is_managerial == "TRUE")
    tbl_refund_doc_tbl <- tbl(con, "_Document251_VT3939") %>% 
        select(link_item = "_Fld3941RRef", item_qty = "_Fld3942",
               item_sum = "_Fld3948", link_sale_doc = "_Fld3953_RRRef",
               "_Document251_IDRRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_id = "_Code")
    tbl_project <- tbl(con, "_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", nmbr_sale_doc = "_Number", date_sale_doc = "_Date_Time")
    
    df_refund <- tbl_refund_doc %>%
        left_join(tbl_refund_doc_tbl, by = c("_IDRRef" = "_Document251_IDRRef")) %>%
        left_join(tbl_item,           by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_partner,        by = c(link_customer = "_IDRRef")) %>% 
        left_join(tbl_subdiv,         by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_store,          by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_project,        by = c(link_project = "_IDRRef")) %>%
        left_join(tbl_sale_doc,       by = c(link_sale_doc = "_IDRRef")) %>% 
        select(nmbr_refund_doc, date_refund_doc,
               subdiv_name, store_name, customer_id, project,
               item_id, item_qty, item_sum,
               nmbr_sale_doc, date_sale_doc
        ) %>% 
        collect()
    
    return(df_refund)
    
    dbDisconnect(con)
}


## План продаж
sale_plan_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_saleplan_doc <- tbl(con,"_Document350") %>% 
        select("_IDRRef", posted_saleplan_doc = "_Posted",
               date_saleplan_doc = "_Fld8946",
               link_subdiv = "_Fld8951RRef", link_project = "_Fld8952RRef", 
               link_script = "_Fld8953RRef", saleplan_doc_sum = "_Fld8954") %>% 
        mutate(posted_saleplan_doc = as.logical(posted_saleplan_doc)) %>% 
        filter(date_saleplan_doc >= date_start,
               date_saleplan_doc < date_finish,
               posted_saleplan_doc == "TRUE")
    tbl_saleplan_doc_tbl <- tbl(con, "_Document350_VT8960") %>% 
        select(link_partner = "_Fld8973RRef", link_item = "_Fld8962_RRRef",
               sum = "_Fld8968", "_Document350_IDRRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_name = "_Description")
    tbl_project <- tbl(con, "_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_script <- tbl(con,"_Reference178") %>% 
        select("_IDRRef", script_name = "_Description")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")

    df_saleplan <- tbl_saleplan_doc %>%
        left_join(tbl_subdiv,         by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_project,        by = c(link_project = "_IDRRef")) %>%
        left_join(tbl_script,         by = c(link_script = "_IDRRef")) %>%
        left_join(tbl_saleplan_doc_tbl, by = c("_IDRRef" = "_Document350_IDRRef")) %>%
        left_join(tbl_partner,        by = c(link_partner = "_IDRRef")) %>%
        left_join(tbl_item,           by = c(link_item = "_IDRRef")) %>%
        select(date_saleplan_doc,  subdiv_name, project, script_name, saleplan_doc_sum,
               customer_name, item_id, sum
        ) %>% 
        collect()
    
    return(df_saleplan)
    
    dbDisconnect(con)
}


## Себестоимость проданых товаров
cost_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_cost <- tbl(con,"_AccumRg17487") %>% 
        select(date = "_Period", link_doc = "_RecorderRRef",
               link_item = "_Fld17488RRef",
               link_subdiv = "_Fld17492RRef", link_project = "_Fld17493RRef",
               item_qty = "_Fld17494", item_sum = "_Fld17495", item_vat = "_Fld17496") %>% 
        filter(date >= date_start,
               date < date_finish)
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_project <- tbl(con, "_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", nmbr_sale_doc = "_Number")
    tbl_refund_doc <- tbl(con,"_Document251") %>% 
        select("_IDRRef", nmbr_refund_doc = "_Number") 
    
    df_cost <- tbl_cost %>%
        left_join(tbl_item,        by = c(link_item = "_IDRRef")) %>%
        left_join(tbl_subdiv,      by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_project,     by = c(link_project = "_IDRRef")) %>%
        left_join(tbl_sale_doc,    by = c(link_doc = "_IDRRef")) %>%
        left_join(tbl_refund_doc,  by = c(link_doc = "_IDRRef")) %>%
        select(date, nmbr_sale_doc, nmbr_refund_doc,
               subdiv_name, project,
               item_id, item_qty, item_sum, item_vat
        ) %>% 
        collect()
    
    return(df_cost)
    
    dbDisconnect(con)
}


## Развозка
delivery_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    date_start_2 <- date_start - weeks(2)

    tbl_delivery <- tbl(con,"_Document416") %>% 
        select("_IDRRef", posted_delivery = "_Posted", nmbr_delivery = "_Number",
               date_delivery = "_Date_Time", 
               link_store = "_Fld12841RRef", 
               time_depart = "_Fld12837", time_return = "_Fld12838",
               link_direction = "_Fld12831RRef", link_route = "_Fld19689RRef",
               link_vehicle = "_Fld12834RRef",
               speedo_start = "_Fld12835", speedo_finish = "_Fld12836") %>%
        mutate(posted_delivery = as.logical(posted_delivery)) %>% 
        filter(date_delivery >= date_start,
               date_delivery < date_finish,
               posted_delivery == "TRUE")
    tbl_delivery_tbl_forwarder <- tbl(con, "_Document416_VT12850") %>% 
        select(link_forwarder = "_Fld12852RRef", "_Document416_IDRRef")
    tbl_delivery_tbl_driver <- tbl(con, "_Document416_VT12853") %>% 
        select(link_driver = "_Fld12855RRef", "_Document416_IDRRef")
    tbl_delivery_tbl_fuel <- tbl(con, "_Document416_VT12867") %>% 
        select(link_fuel = "_Fld12869RRef", fuel_consumption = "_Fld21170",
               "_Document416_IDRRef")
    tbl_delivery_tbl_order_deliv <- tbl(con, "_Document416_VT12842") %>% 
        select(link_order_deliv = "_Fld12844_RRRef", "_Document416_IDRRef",
               "_Fld19722")
    tbl_individuals <- tbl(con,"_Reference191") %>% 
        select("_IDRRef", user_id = "_Code")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    tbl_direction <- tbl(con, "_Reference193") %>% 
        select("_IDRRef", direction = "_Description")
    tbl_route <- tbl(con, "_Reference19642") %>% 
        select("_IDRRef", route = "_Description")
    tbl_vehicle <- tbl(con, "_Reference195") %>% 
        select("_IDRRef", vehicle_name = "_Description")
    # tbl_fuel <- tbl(con, "_Reference197") %>% 
    #     select("_IDRRef", fuel_item_link = "_Fld2760RRef")
    # tbl_item <- tbl(con,"_Reference120") %>% 
    #     select("_IDRRef", item_id = "_Code")
    tbl_deliv_order <- tbl(con,"_Document409") %>% 
        select("_IDRRef", posted_deliv_order = "_Posted", nmbr_deliv_order = "_Number",
               date_deliv_order = "_Date_Time") %>% 
        mutate(posted_deliv_order = as.logical(posted_deliv_order)) %>% 
        filter(date_deliv_order >= date_start_2,
               date_deliv_order < date_finish,
               posted_deliv_order == "TRUE")
    

    df_delivery <- tbl_delivery %>%
        # left_join(tbl_delivery_tbl_fuel, 
        #           by = c("_IDRRef" = "_Document416_IDRRef")) %>%
        # left_join(tbl_fuel,        by = c(link_fuel = "_IDRRef")) %>%
        # left_join(tbl_item,        by = c(fuel_item_link = "_IDRRef")) %>%
        left_join(tbl_delivery_tbl_forwarder, 
                                   by = c("_IDRRef" = "_Document416_IDRRef")) %>% 
        left_join(select(tbl_individuals, "_IDRRef", forwarder = user_id),
                                   by = c(link_forwarder = "_IDRRef")) %>%
        left_join(tbl_delivery_tbl_driver, 
                                   by = c("_IDRRef" = "_Document416_IDRRef")) %>% 
        left_join(select(tbl_individuals, "_IDRRef", driver = user_id),
                                   by = c(link_driver = "_IDRRef")) %>%
        left_join(tbl_store,       by = c(link_store = "_IDRRef")) %>%
        left_join(tbl_direction,   by = c(link_direction = "_IDRRef")) %>%
        left_join(tbl_route,       by = c(link_route = "_IDRRef")) %>%
        left_join(tbl_vehicle,     by = c(link_vehicle = "_IDRRef")) %>%
        left_join(tbl_delivery_tbl_order_deliv, 
                                   by = c("_IDRRef" = "_Document416_IDRRef")) %>%
        left_join(tbl_deliv_order, by = c(link_order_deliv = "_IDRRef")) %>%
        select(nmbr_delivery, date_delivery, forwarder, driver, vehicle_name,
               direction, route, store_name, 
               time_depart, time_return, speedo_start, speedo_finish,
               # item_id, fuel_consumption,
               nmbr_deliv_order, date_deliv_order
        ) %>% 
        collect()
    
    return(df_delivery)
    
    dbDisconnect(con)
}


## Текущая задолженность контрагентов
partner_current_debt_data <- function() {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(today() + years(2000))

    
    tbl_partner_settlements<- tbl(con,"_AccumRgT16915") %>% 
        select(date_settlements = "_Period", link_organization = "_Fld16910RRef",
               link_partner = "_Fld16911RRef", link_contract = "_Fld16906RRef",
               debt_sum = "_Fld16913") %>% 
        filter(date_settlements > date_start)

    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", partner_id = "_Code")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", contract_id = "_Code",
               link_contract_type = "_Fld1600RRef")
    tbl_type_contract <- tbl(con, "_Enum484") %>% 
        select("_IDRRef", type_contract = "_EnumOrder")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organiz_name = "_Description")
    
    df_partner_debt <- tbl_partner_settlements  %>%
        left_join(tbl_partner,       by = c(link_partner = "_IDRRef")) %>% 
        left_join(tbl_contract,      by = c(link_contract = "_IDRRef")) %>% 
        left_join(tbl_type_contract, by = c(link_contract_type = "_IDRRef")) %>% 
        left_join(tbl_organization,  by = c(link_organization = "_IDRRef")) %>%
        select(organiz_name, partner_id, type_contract, contract_id,
               debt_sum) %>% 
        group_by(organiz_name, partner_id, contract_id, type_contract) %>% 
        summarise(debt_sum = sum(debt_sum)) %>%
        filter(type_contract %in% c(0,1)) %>% 
        collect() %>% 
        mutate(type_contract = ifelse(type_contract == 0,
                                     "supplier",
                                     "customer"))
    
    return(df_partner_debt)
    
    dbDisconnect(con)
}


## Данные по взаиморасчетам с покупателями по документам расчета
customer_debt_moving_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_partner_debt_movement <- tbl(con,"_AccumRg16905") %>% 
        select(date_movement = "_Period", link_organization = "_Fld16910RRef",
               link_partner = "_Fld16911RRef", link_contract = "_Fld16906RRef",
               link_sale_doc = "_Fld16908_RRRef",
               type_moving = "_RecordKind", moving_sum = "_Fld16913") %>% 
        filter(date_movement >= date_start,
               date_movement < date_finish)
    
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", partner_id = "_Code")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", contract_id = "_Code",
               link_contract_type = "_Fld1600RRef")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organization_name = "_Description")
    tbl_type_contract <- tbl(con, "_Enum484") %>% 
        select("_IDRRef", type_contract = "_EnumOrder")
    tbl_sale_doc <- tbl(con,"_Document375") %>% 
        select("_IDRRef", nmbr_sale_doc = "_Number",
               date_sale_doc = "_Date_Time", link_subdiv = "_Fld10904RRef",
               link_responsible = "_Fld10922RRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")
    tbl_users <- tbl(con, "_Reference138") %>% 
        select("_IDRRef", responsible_name = "_Description")

    df_customer_motion <- tbl_partner_debt_movement %>%
        left_join(tbl_partner,       by = c(link_partner = "_IDRRef")) %>% 
        left_join(tbl_contract,      by = c(link_contract = "_IDRRef")) %>% 
        left_join(tbl_organization,  by = c(link_organization = "_IDRRef")) %>%
        left_join(tbl_type_contract, by = c(link_contract_type = "_IDRRef")) %>%
        left_join(tbl_sale_doc,      by = c(link_sale_doc = "_IDRRef")) %>%
        left_join(tbl_subdiv,        by = c(link_subdiv = "_IDRRef")) %>%
        left_join(tbl_users,         by = c(link_responsible = "_IDRRef")) %>%
        select(date_movement, organization_name, partner_id, contract_id,
               type_contract, nmbr_sale_doc, date_sale_doc, subdiv_name,
               responsible_name, type_moving, moving_sum) %>% 
        filter(type_contract == 1) %>% 
        collect()

        return(df_customer_motion)
    
    dbDisconnect(con)
}


## Данные по взаиморасчетам с поставщиками по документам расчета
supplier_debt_moving_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_partner_debt_movement <- tbl(con,"_AccumRg16905") %>% 
        select(date_movement = "_Period", link_organization = "_Fld16910RRef",
               link_partner = "_Fld16911RRef", link_contract = "_Fld16906RRef",
               link_receipt_doc = "_Fld16908_RRRef",
               type_moving = "_RecordKind", moving_sum = "_Fld16913") %>% 
        filter(date_movement >= date_start,
               date_movement < date_finish)
    
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", partner_id = "_Code")
    tbl_contract <- tbl(con, "_Reference78") %>% 
        select("_IDRRef", contract_id = "_Code",
               link_contract_type = "_Fld1600RRef")
    tbl_organization <- tbl(con, "_Reference128") %>% 
        select("_IDRRef", organization_name = "_Description")
    tbl_type_contract <- tbl(con, "_Enum484") %>% 
        select("_IDRRef", type_contract = "_EnumOrder")
    tbl_receipt_doc <- tbl(con,"_Document360") %>% 
        select("_IDRRef", nmbr_receipt_doc = "_Number",
               date_receipt_doc = "_Date_Time")
    
    df_supplier_motion <- tbl_partner_debt_movement %>%
        left_join(tbl_partner,       by = c(link_partner = "_IDRRef")) %>% 
        left_join(tbl_contract,      by = c(link_contract = "_IDRRef")) %>% 
        left_join(tbl_organization,  by = c(link_organization = "_IDRRef")) %>%
        left_join(tbl_type_contract, by = c(link_contract_type = "_IDRRef")) %>%
        left_join(tbl_receipt_doc,      by = c(link_receipt_doc = "_IDRRef")) %>%
        select(date_movement, organization_name, partner_id, contract_id,
               type_contract, nmbr_receipt_doc, date_receipt_doc,
               type_moving, moving_sum) %>% 
        filter(type_contract == 1) %>% 
        collect()
    
    return(df_supplier_motion)
    
    dbDisconnect(con)
}


## Документ телемаркетинг
telemarketing_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_telemarketing_doc <- tbl(con,"_Document437") %>% 
        select("_IDRRef", posted_doc = "_Posted", nmbr_telemark_doc = "_Number",
               date_telemark_doc = "_Date_Time", link_subdiv = "_Fld13498RRef",
               link_project = "_Fld13499RRef", link_status = "_Fld13500RRef",
               link_cause = "_Fld21890RRef") %>% 
        mutate(posted_doc = as.logical(posted_doc)) %>% 
        filter(date_telemark_doc >= date_start,
               date_telemark_doc < date_finish,
               posted_doc == "TRUE")
    tbl_telemarketing_tbl <- tbl(con, "_Document437_VT13505") %>% 
        select(link_customer = "_Fld13508RRef", link_event = "_Fld13512RRef",
               "_Document437_IDRRef")
    tbl_subdiv <- tbl(con, "_Reference135") %>% 
        select("_IDRRef", subdiv_name = "_Description")    
    tbl_partner <- tbl(con, "_Reference102") %>% 
        select("_IDRRef", customer_id = "_Code")
    tbl_project <- tbl(con,"_Reference143") %>% 
        select("_IDRRef", project = "_Description")
    tbl_cause <- tbl(con,"_Reference21889") %>% 
        select("_IDRRef", cause = "_Description")
    tbl_status <-tbl(con, "_Enum677") %>% 
        select("_IDRRef", type_status = "_EnumOrder")
    tbl_event <- tbl(con,"_Document382") %>% 
        select("_IDRRef", nmbr_event_doc = "_Number",
               date_event_doc = "_Date_Time")
    
    df_telemarketing <- tbl_telemarketing_doc %>%
        left_join(tbl_subdiv,         by = c(link_subdiv  = "_IDRRef")) %>%
        left_join(tbl_project,        by = c(link_project = "_IDRRef")) %>%
        #filter(project == "Сервис") %>% 
        left_join(tbl_status,         by = c(link_status = "_IDRRef")) %>%
        filter(type_status == 1) %>% 
        left_join(tbl_cause,          by = c(link_cause = "_IDRRef")) %>% 
        left_join(tbl_telemarketing_tbl, by = c("_IDRRef" = "_Document437_IDRRef")) %>%
        left_join(tbl_partner,        by = c(link_customer = "_IDRRef")) %>% 
        left_join(tbl_event,          by = c(link_event = "_IDRRef")) %>% 
        select(nmbr_telemark_doc, date_telemark_doc, subdiv_name, project,
               cause, type_status, customer_id, nmbr_event_doc, date_event_doc
        ) %>% 
        collect()
    
    return(df_telemarketing)
    
    dbDisconnect(con)
}


## Текущие остатки на складах
current_stock_balance <- function() {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )
    
    date_start <- as_datetime(today() + years(2000))
    
    tbl_stock_balance <- tbl(con,"_AccumRgT17710") %>% 
        select(date_balance = "_Period", link_store = "_Fld17703RRef",
               link_item = "_Fld17704RRef", item_store_qty ="_Fld17708") %>% 
        filter(date_balance > date_start)

    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    
    df_stock_balance <- tbl_stock_balance  %>%
        left_join(tbl_store, by = c(link_store = "_IDRRef")) %>% 
        left_join(tbl_item,  by = c(link_item = "_IDRRef")) %>% 
        select(store_name, item_id, item_store_qty) %>% 
        collect()

    return(df_stock_balance)
    
    dbDisconnect(con)
}


## Данные для расчета остатков на складах на определенную дату
stock_motion_data <- function(date_start, date_finish) {
    
    library(DBI)
    library(odbc)
    library(tidyverse)
    library(lubridate)
    library(dbplyr)
    
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "SERVER2",
                     Database = "work",
                     UID = "sa",
                     PWD = rstudioapi::askForPassword("Database password")
    )

    date_start <- as_datetime(date_start)
    date_finish <- as_datetime(date_finish)
    
    tbl_stock_movement <- tbl(con,"_AccumRg17702") %>% 
        select(date_movement = "_Period",
               link_store = "_Fld17703RRef", link_item = "_Fld17704RRef",
               type_motion = "_RecordKind", item_motion_qty = "_Fld17708") %>% 
        filter(date_movement >= date_start,
               date_movement < date_finish)
    
    tbl_item <- tbl(con,"_Reference120") %>% 
        select("_IDRRef", item_id = "_Code")
    tbl_store <- tbl(con, "_Reference156") %>% 
        select("_IDRRef", store_name = "_Description")
    
    df_stock_motion <- tbl_stock_movement %>%
        left_join(tbl_store, by = c(link_store = "_IDRRef")) %>% 
        left_join(tbl_item,  by = c(link_item = "_IDRRef")) %>% 
        select(date_movement, store_name, item_id, type_motion, item_motion_qty) %>% 
        collect()
    
    return(df_stock_motion)
    
    dbDisconnect(con)
}
