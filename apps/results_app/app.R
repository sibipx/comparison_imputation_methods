# Results app

library(shiny)
library(cowplot)
library(tidyverse)
library(DT)

# makes friendly percent
make_percent <- function(x) paste0(round(x, 4) * 100, "%")

# silent min and max
min_quiet <- function(x) suppressWarnings(min(x))
max_quiet <- function(x) suppressWarnings(max(x))

# aggregate results for app

# source("R/aggregate_results.R")
# results_dir <- "results/"
# results_all_datasets <- aggregrate_results(results_dir)
#
# # prettify
# results_all_datasets <- results_all_datasets %>%
#   # make shorter names
#   mutate(method = case_when(#method == "mean/mode" ~ "mean",
#     method == "bagging_tidy" ~ "bagging",
#     method == "knn_tidy" ~ "knn",
#     method == "linear - bin" ~ "linear",
#     method == "IterativeImputer - bin" ~ "python_II",
#     method == "IterativeImputer" ~ "python_II",
#     method == "missForestPredict" ~ "missFP",
#     method == "missForestPredict_md10" ~ "missFP_md10",
#     TRUE ~ method)) %>%
#   mutate(method = factor(method, levels = c("mean/mode", "linear", "mice_default",
#                                             "knn", "bagging",
#                                             "python_II", "mice_rf", "miceRanger",
#                                             "missFP", "missFP_md10",
#                                             "original"))) %>%
#   mutate(Type = case_when(method == "original" ~ "original",
#                           method == "mean/mode" ~ "mean/mode",
#                           method %in% c("linear", "mice_default") ~ "linear",
#                           method == "knn" ~ "knn",
#                           TRUE ~ "tree based")) %>%
#   mutate(Noise = if_else(str_detect(dataset, "noise"), "YES", "NO"),
#          Noise = factor(Noise, levels = c("YES", "NO"))) %>%
#   mutate(model = if_else(model == "cubic_spline", "Cubic spline", model),
#          model = factor(model, levels = c("Ridge", "Cubic spline", "RF", "XGB"))) %>%
#   mutate(metric = if_else(metric == "mean_calibration", "E:O ratio", metric)) %>%
#   filter(!metric %in% c("n_all_missing_train", "n_all_missing_test", "n_train", "n_test",
#                         "n_complete_cases_train", "n_complete_cases_test", "n_iter_converge")) %>%
#   mutate(method = factor(method, levels = c("mean/mode", "linear", "mice_default",
#                                             "knn", "bagging",
#                                             "python_II", "mice_rf", "miceRanger",
#                                             "missFP", "missFP_md10",
#                                             "original")))
#
# # var importance
# results_var_imp <- results_all_datasets %>%
#   filter(metric == "Variable importance",
#          variable != "(Intercept)") %>%
#   select(dataset, value, variable, method, model)
#
# save(results_var_imp, file = "apps/results_app/results_var_imp.Rdata")
#
# results_all_datasets <- results_all_datasets %>%
#   filter(metric != "Variable importance" | is.na(metric))
#
# # timing
# results_timing <- results_all_datasets %>%
#   filter(!is.na(time_sec))
#
# # object size
# results_obj_size <- results_all_datasets %>%
#   filter(method != "python_II") %>%
#   filter(!is.na(obj_size_bytes)) %>%
#   select(iteration_i, iteration_j, dataset, method, obj_size_bytes, Type, Noise)
#
# results_timing_table <- results_timing %>%
#   mutate(dataset = factor(dataset)) %>%
#   pivot_wider(names_from = "type", values_from = "time_sec") %>%
#   mutate(train_fit_predict = train_fit + train_predict) %>%
#   pivot_longer(cols = c(train_fit, train_predict, train_fit_predict, test_predict),
#                names_to = "type", values_to = "time_sec") %>%
#   group_by(dataset, method, type) %>%
#   mutate(median = round(median(time_sec), 1),
#          Q1 = round(quantile(time_sec, probs = 0.25), 1),
#          Q3 = round(quantile(time_sec, probs = 0.75), 1),
#          `median (IQR)` = sprintf("%s (%s - %s)", median, Q1, Q3)
#   ) %>%
#   ungroup() %>%
#   distinct(dataset, method, type, `median (IQR)`, median) %>%
#   group_by(dataset, type) %>%
#   mutate(max_median = max_quiet(median),
#          min_median = min_quiet(median)) %>%
#   ungroup() %>%
#   group_by(dataset, type) %>%
#   mutate(`median (IQR)` = if_else(median == min_median,
#                                   sprintf("<strong>%s</strong>", `median (IQR)`),
#                                   `median (IQR)`),
#          `median (IQR)` = if_else(median == max_median,
#                                   sprintf("<em>%s</em>", `median (IQR)`),
#                                   `median (IQR)`)) %>%
#   ungroup() %>%
#   dplyr::select(-c(median, max_median, min_median)) %>%
#   pivot_wider(names_from = "method", values_from = `median (IQR)`) %>%
#   mutate(id = case_when(type == "train_fit" ~ 1,
#                         type == "train_predict" ~ 2,
#                         TRUE ~ 3)) %>%
#   arrange(id, dataset) %>%
#   dplyr::select(-c(id)) %>%
#   dplyr::select(dataset, type, linear, knn, mice_default, bagging,
#                 python_II, mice_rf, miceRanger, missFP, missFP_md10) %>%
#   mutate(type = case_when(type == "train_fit" ~ "Train (fit)",
#                           type == "train_predict" ~ "Train (impute)",
#                           type == "train_fit_predict" ~ "Train (fit + impute)",
#                           type == "test_predict" ~ "Test (impute)",
#                           TRUE ~ NA_character_
#   ),
#   type = factor(type, levels = c("Train (fit)", "Train (impute)",
#                                  "Train (fit + impute)", "Test (impute)"))) %>%
#   arrange(dataset, type)
#
# save(results_timing_table, file = "apps/results_app/results_timing_table.Rdata")
#
# results_obj_size_table <- results_obj_size %>%
#   mutate(obj_size_MB = obj_size_bytes/1024/1024) %>%
#   group_by(dataset, method) %>%
#   mutate(median = round(median(obj_size_MB), 3),
#          Q1 = round(quantile(obj_size_MB, probs = 0.25), 3),
#          Q3 = round(quantile(obj_size_MB, probs = 0.75), 3),
#          #`median (IQR)` = sprintf("%s (%s - %s)", median, Q1, Q3),
#          `median (IQR)` = as.character(median)) %>%
#   ungroup() %>%
#   distinct(dataset, method, `median (IQR)`, median) %>%
#   group_by(dataset) %>%
#   mutate(max_median = max_quiet(median),
#          min_median = min_quiet(median)) %>%
#   ungroup() %>%
#   group_by(dataset) %>%
#   mutate(`median (IQR)` = if_else(median == min_median,
#                                   sprintf("<strong>%s</strong>", `median (IQR)`),
#                                   `median (IQR)`),
#          `median (IQR)` = if_else(median == max_median,
#                                   sprintf("<em>%s</em>", `median (IQR)`),
#                                   `median (IQR)`)) %>%
#   ungroup() %>%
#   dplyr::select(-c(median, max_median, min_median)) %>%
#   pivot_wider(names_from = "method", values_from = `median (IQR)`) %>%
#   dplyr::select(dataset, `mean/mode`, linear, mice_default, knn, bagging,
#                 mice_rf, miceRanger, missFP, missFP_md10) %>%
#   arrange(dataset)
#
# save(results_obj_size_table, file = "apps/results_app/results_obj_size_table.Rdata")
#
# # tables
# cols_min_best <- c("BS", "RMSE", "MAE", "MAPE", "SMAPE")
# cols_max_best <- c("AUROC", "AUPRC", "BSS", "Rsquared")
# cols_0_best <- c("intercept")
# cols_1_best <- c("slope", "E:O ratio")
#
# data_tables <- results_all_datasets %>%
#   filter(!is.na(model)) %>%
#   filter(type == "test") %>%
#   group_by(method, dataset, model, metric) %>%
#   mutate(median = round(median(value), 3),
#          Q1 = round(quantile(value, probs = 0.25), 3),
#          Q3 = round(quantile(value, probs = 0.75), 3) ,
#          `median (IQR), %` = sprintf("%s (%s - %s)", median, Q1, Q3)) %>%
#   ungroup() %>%
#   distinct(dataset, method, `median (IQR), %`, median, model, metric) %>%
#   arrange(method, model) %>%
#   group_by(dataset, model, metric) %>%
#   mutate(max_median = max_quiet(median[method != "original"])[[1]],
#          min_median = min_quiet(median[method != "original"])[[1]],
#          one_median = median[abs(1 - median[method != "original"]) ==
#                                min_quiet(abs(1 - median[method != "original"]))][[1]],
#          zero_median = min_quiet(abs(median[method != "original"]))[[1]]) %>%
#   ungroup() %>%
#   group_by(dataset, model, metric) %>%
#   mutate(`median (IQR), %` = if_else(median == min_median & metric %in% cols_min_best,
#                                      sprintf("<strong>%s</strong>", `median (IQR), %`),
#                                      `median (IQR), %`),
#          `median (IQR), %` = if_else(median == max_median & metric %in% cols_max_best,
#                                      sprintf("<strong>%s</strong>", `median (IQR), %`),
#                                      `median (IQR), %`),
#          `median (IQR), %` = if_else(abs(median) == zero_median & metric %in% cols_0_best,
#                                      sprintf("<strong>%s</strong>**", `median (IQR), %`),
#                                      `median (IQR), %`),
#          `median (IQR), %` = if_else(abs(median) == one_median & metric %in% cols_1_best,
#                                      sprintf("<strong>%s</strong>", `median (IQR), %`),
#                                      `median (IQR), %`)
#   ) %>%
#   ungroup() %>%
#   dplyr::select(-c(median, max_median, min_median, one_median, zero_median)) %>%
#   pivot_wider(names_from = "method", values_from = `median (IQR), %`) %>%
#   pivot_longer(cols = c(-c(dataset, model, metric)), names_to = "Method") %>%
#   pivot_wider(names_from = c(dataset)) %>%
#   rename(Metric = metric,
#          Model = model) %>%
#   mutate(Method = factor(Method, levels = c("mean/mode", "linear", "mice_default",
#                                             "knn", "bagging",
#                                             "python_II", "mice_rf", "miceRanger",
#                                             "missFP", "missFP_md10",
#                                             "original"))) %>%
#   arrange(Metric, Model, Method)
#
# save(data_tables, file = "apps/results_app/data_tables.Rdata")
#
# # # keep failures apart
# # results_failures <- results_all_datasets %>%
# #   filter(fail)
# # # 0 failures
#
# # NOT NEEDED if 0 failures
# # results_all_datasets <- results_all_datasets %>%
# #   filter(is.na(fail))
# #
# # # discard failing iterations
# # results_all_datasets <- results_all_datasets %>%
# #   group_by(across(-c(iteration_j, value, time_sec))) %>%
# #   arrange(iteration_j) %>%
# #   dplyr::slice(n()) %>%
# #   ungroup()
#
# # variable-wise results
# results <- results_all_datasets %>%
#   filter(!is.na(variable),
#          metric != "Variable importance",
#          !metric %in% c("MSE", "F1_score", "macro_F1")) %>% # filter out some metrics for memory usage on shiny io
#   dplyr::select(dataset, metric, value, variable, method, Type, Noise, type)
# save(results, file = "apps/results_app/results.Rdata")
#
# # prediction model
# results_pred_model <- results_all_datasets %>%
#   filter(!is.na(model)) %>%
#   filter(type == "test") %>%
#   dplyr::select(dataset, metric, value, method, model, Type, Noise)
# save(results_pred_model, file = "apps/results_app/results_pred_model.Rdata")


#############################################################################

#load("apps/results_app/results_var_imp.Rdata")
load("results_var_imp.Rdata")
#load("apps/results_app/results_timing_table.Rdata")
load("results_timing_table.Rdata")
#load("apps/results_app/results_obj_size_table.Rdata")
load("results_obj_size_table.Rdata")
# load("apps/results_app/data_tables.Rdata")
load("data_tables.Rdata")
# load("apps/results_app/results.Rdata")
load("results.Rdata")
# load("apps/results_app/results_pred_model.Rdata")
load("results_pred_model.Rdata")

# filter options
all_models <- results_pred_model %>% distinct(model) %>% pull(model) %>% as.character()
all_methods <- results_pred_model %>% distinct(method) %>% pull(method) %>% as.character()
all_methods_OOB <- c("missFP", "missFP_md10", "miceRanger")
metrics_binary <- c("AUROC", "AUPRC", "slope", "intercept", "E:O ratio", "BS", "BSS")
metrics_cont <- c("Rsquared", "RMSE", "MAE", "MAPE", "SMAPE")

all_datasets <- unique(results_pred_model$dataset)
all_datasets <- c(all_datasets[!str_detect(all_datasets, "sim_")],
                  all_datasets[str_detect(all_datasets, "sim_")])

datasets_group <- c("Real data with MV", "Real data with simulated MV",
                    "Sim data corr 0.1 AUROC 0.75",
                    "Sim data corr 0.1 AUROC 0.90",
                    "Sim data corr 0.7 AUROC 0.75",
                    "Sim data corr 0.7 AUROC 0.90"
)

datasets_MV <- results_pred_model %>%
  distinct(dataset) %>%
  filter(str_detect(dataset, "- MV")) %>%
  pull(dataset)

datasets_no_MV <- results_pred_model %>%
  distinct(dataset) %>%
  filter(!str_detect(dataset, "- MV"),
         !str_starts(dataset, "sim_")) %>%
  pull(dataset)

datasets_sim <- results_pred_model %>%
  distinct(dataset) %>%
  filter(str_starts(dataset, "sim_")) %>%
  pull(dataset)

# var imp
all_datasets <- unique(results_var_imp$dataset)
all_datasets <- c(all_datasets[!str_detect(all_datasets, "sim_")],
                  all_datasets[str_detect(all_datasets, "sim_")])

all_models <- unique(results_var_imp$model)

#rm(results_all_datasets)

# UI
ui <- fluidPage(

  titlePanel("Comparison of imputation methods in prediction settings"),

  tabsetPanel(type = "tabs",
              tabPanel("Prediction performance (plots)",
                       fluidRow(
                         column(2,
                                selectInput("plot_type", "Plot type",
                                            choices = c("IQR bars", "Boxplots"),
                                            selected = "IQR bars"),
                                selectInput("datasets", "Datasets group",
                                            choices = datasets_group,
                                            selected = datasets_group[1]),
                                selectInput("metric_binary", "Metric binary outcome",
                                            choices = metrics_binary,
                                            selected = "BSS"),
                                selectInput("metric_cont", "Metric cont. outcome",
                                            choices = metrics_cont,
                                            selected = "Rsquared"),
                                checkboxGroupInput("methods_compare", "Imputation methods",
                                                   choices = all_methods,
                                                   selected = all_methods),
                                checkboxGroupInput("models_compare", "Models",
                                                   choices = all_models,
                                                   selected = all_models)
                         ),
                         column(10,
                                plotOutput("pred_performance",
                                           height = "600px")
                         )
                       )
              ),
              tabPanel("Prediction performance (table)",
                       fluidRow(
                         column(2,
                                selectInput("datasets_tab", "Datasets group",
                                            choices = datasets_group,
                                            selected = datasets_group[1]),
                                selectInput("metric_binary_tab", "Metric binary outcome",
                                            choices = metrics_binary,
                                            selected = "BSS"),
                                selectInput("metric_cont_tab", "Metric cont. outcome",
                                            choices = metrics_cont,
                                            selected = "Rsquared")
                         ),
                         column(10,
                                htmlOutput("text_table"),
                                DTOutput("table_performance"))
                       )
              ),
              tabPanel("Errors (deviations from true values)",
                       fluidRow(
                         column(2,
                                selectInput("plot_type_err", "Plot type",
                                            choices = c("IQR bars", "Boxplots"),
                                            selected = "IQR bars"),
                                selectInput("datasets_err", "Datasets group",
                                            choices = datasets_group[2:length(datasets_group)],
                                            selected = datasets_group[2]),
                                selectInput("train_test_type_err", "Train / test",
                                            choices = c("test", "train"),
                                            selected = "test"),
                                selectInput("metric_cat_err", "Metric categorical variables",
                                            choices = c("MER"),
                                            selected = "MER"),
                                selectInput("metric_cont_err", "Metric continuous variables",
                                            choices = c("NMSE"),
                                            selected = "NMSE"),
                                checkboxGroupInput("methods_compare_err", "Imputation methods",
                                                   choices = all_methods,
                                                   selected = all_methods)
                         ),
                         column(10,
                                htmlOutput("text_err_true"),
                                plotOutput("plot_err_true_value",
                                           height = "600px", width = "125%")
                         )
                       )
              ),
              tabPanel("OOB errors",
                       fluidRow(
                         column(2,
                                selectInput("plot_type_OOB", "Plot type",
                                            choices = c("IQR bars", "Boxplots"),
                                            selected = "IQR bars"),
                                selectInput("datasets_OOB", "Datasets group",
                                            choices = datasets_group,
                                            selected = datasets_group[1]),
                                selectInput("metric_cat_OOB", "Metric categorical variables",
                                            choices = c("MER", "macro_F1", "F1_score"),
                                            selected = "MER"),
                                selectInput("metric_cont_OOB", "Metric continuous variables",
                                            choices = c("NMSE", "MSE"),
                                            selected = "NMSE"),
                                checkboxGroupInput("methods_compare_OOB", "Imputation methods",
                                                   choices = all_methods_OOB,
                                                   selected = all_methods_OOB)
                         ),
                         column(10,
                                htmlOutput("text_err_OOB"),
                                plotOutput("plot_OOB",
                                           height = "900px", width = "125%")
                         )
                       )
              ),
              tabPanel("Runtimes (table)",
                       fluidRow(
                         column(2,
                                selectInput("datasets_timing", "Datasets group",
                                            choices = datasets_group,
                                            selected = datasets_group[1])
                         ),
                         column(10,
                                htmlOutput("text_timings"),
                                DTOutput("table_timings"))
                       )
              ),
              tabPanel("Object size (table)",
                       fluidRow(
                         column(2,
                                selectInput("datasets_obj_size", "Datasets group",
                                            choices = datasets_group,
                                            selected = datasets_group[1])
                         ),
                         column(10,
                                htmlOutput("text_obj_size"),
                                DTOutput("table_obj_size"))
                       )
              ),
              tabPanel("Variable importance",
                       fluidRow(
                         column(2,
                                selectInput("plot_type_var_imp", "Plot type",
                                            choices = c("IQR bars", "Boxplots"),
                                            selected = "IQR bars"),
                                selectInput("datasets_var_imp", "Dataset",
                                            choices = all_datasets,
                                            selected = all_datasets[1]),
                                selectInput("model_var_imp", "Model",
                                            choices = all_models,
                                            selected = all_models[1])
                         ),
                         column(10,
                                htmlOutput("text_var_imp"),
                                plotOutput("plot_var_imp",
                                           height = "600px")
                         )
                       )
              )
  )
)


# SERVER
server <- function(input, output, session) {

  rv <- reactiveValues()
  # prediction performance plots
  observeEvent(input$datasets,
               {
                 if (input$datasets == "Real data with MV"){
                   rv$datasets <- datasets_MV
                   rv$data_plot <- results_pred_model  %>%
                     filter(dataset %in% datasets_MV) %>%
                     mutate(dataset = str_replace(dataset, " - MV", ""))

                 } else if (input$datasets == "Real data with simulated MV"){
                   rv$datasets <- datasets_no_MV
                   rv$data_plot <- results_pred_model  %>%
                     filter(dataset %in% datasets_no_MV)

                 } else if (str_starts(input$datasets, "Sim data")){
                   rv$datasets <- datasets_sim
                   rv$data_plot <- results_pred_model  %>%
                     filter(dataset %in% datasets_sim) %>%
                     mutate(dataset = str_replace(dataset, "_noise", ""))

                   if (input$datasets == "Sim data corr 0.1 AUROC 0.75"){
                     rv$data_plot <- rv$data_plot %>%
                       filter(str_detect(dataset, "sim_75_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_1 - ", ""))
                   } else if (input$datasets == "Sim data corr 0.7 AUROC 0.75"){
                     rv$data_plot <- rv$data_plot %>%
                       filter(str_detect(dataset, "sim_75_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_7 - ", ""))
                   } else if (input$datasets == "Sim data corr 0.1 AUROC 0.90"){
                     rv$data_plot <- rv$data_plot %>%
                       filter(str_detect(dataset, "sim_90_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_1 - ", ""))
                   } else if (input$datasets == "Sim data corr 0.7 AUROC 0.90"){
                     rv$data_plot <- rv$data_plot %>%
                       filter(str_detect(dataset, "sim_90_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_7 - ", ""))
                   }

                   rv$data_plot <- rv$data_plot %>%
                     mutate(dataset = factor(dataset, levels = c("MCAR", "MAR_2", "MAR_circ", "MNAR",
                                                                 "MAR_2_out", "MAR_circ_out")))
                 }
               })
  # prediction performance tables
  observeEvent(input$datasets_tab,
               {
                 if (input$datasets_tab == "Real data with MV"){

                   rv$data_tables <- data_tables %>%
                     dplyr::select(all_of(c("Model", "Metric", "Method", datasets_MV))) %>%
                     rename_with(~str_replace(., " - MV", ""))

                 } else if (input$datasets_tab == "Real data with simulated MV"){

                   rv$data_tables <- data_tables %>%
                     dplyr::select(all_of(c("Model", "Metric", "Method", datasets_no_MV)))

                 } else if (str_starts(input$datasets_tab, "Sim data")){

                   rv$data_tables <- data_tables %>%
                     dplyr::select(all_of(c("Model", "Metric", "Method", datasets_sim)))

                   noise_at_end <- function(x){

                     noise_one <- function(s){
                       if (str_detect(s, "_noise")){
                         s <- str_replace(s, "_noise", "")
                         s <- paste0(s, "_noise")
                       }
                       return(s)
                     }

                     sapply(x, noise_one)
                   }

                   cols_data_tables <- colnames(rv$data_tables)

                   if (input$datasets_tab == "Sim data corr 0.1 AUROC 0.75"){

                     tables_select <- cols_data_tables[str_detect(cols_data_tables,
                                                                  "sim_75_1")]

                     rv$data_tables <- rv$data_tables %>%
                       dplyr::select(all_of(c("Model", "Metric", "Method", tables_select))) %>%
                       rename_with(~noise_at_end(.)) %>%
                       rename_with(~str_replace(., "sim_75_1 - ", ""))

                   } else if (input$datasets_tab == "Sim data corr 0.7 AUROC 0.75"){
                     tables_select <- cols_data_tables[str_detect(cols_data_tables,
                                                                  "sim_75_7")]

                     rv$data_tables <- rv$data_tables %>%
                       dplyr::select(all_of(c("Model", "Metric", "Method", tables_select))) %>%
                       rename_with(~noise_at_end(.)) %>%
                       rename_with(~str_replace(., "sim_75_7 - ", ""))

                   } else if (input$datasets_tab == "Sim data corr 0.1 AUROC 0.90"){
                     tables_select <- cols_data_tables[str_detect(cols_data_tables,
                                                                  "sim_90_1")]

                     rv$data_tables <- rv$data_tables %>%
                       dplyr::select(all_of(c("Model", "Metric", "Method", tables_select))) %>%
                       rename_with(~noise_at_end(.)) %>%
                       rename_with(~str_replace(., "sim_90_1 - ", ""))

                   } else if (input$datasets_tab == "Sim data corr 0.7 AUROC 0.90"){
                     tables_select <- cols_data_tables[str_detect(cols_data_tables,
                                                                  "sim_90_7")]

                     rv$data_tables <- rv$data_tables %>%
                       dplyr::select(all_of(c("Model", "Metric", "Method", tables_select))) %>%
                       rename_with(~noise_at_end(.)) %>%
                       rename_with(~str_replace(., "sim_90_7 - ", ""))
                   }

                   cols_order <- c("Model", "Metric", "Method",
                                   "MCAR", "MCAR_noise",
                                   "MAR_2", "MAR_2_noise",
                                   "MAR_circ", "MAR_circ_noise",
                                   "MNAR", "MNAR_noise",
                                   "MAR_2_out", "MAR_2_out_noise",
                                   "MAR_circ_out", "MAR_circ_out_noise")

                   rv$data_tables <- rv$data_tables %>%
                     dplyr::select(all_of(cols_order))
                 }
               })
  # err from true value
  observeEvent(c(input$datasets_err,
                 input$train_test_type_err),
               {
                 if (input$datasets_err == "Real data with simulated MV"){
                   rv$datasets_err <- datasets_no_MV
                   rv$data_plot_err <- results %>%
                     filter(dataset %in% datasets_no_MV) %>%
                     filter(type == input$train_test_type_err)

                 } else if (str_starts(input$datasets_err, "Sim data")){
                   rv$datasets_err <- datasets_sim
                   rv$data_plot_err <- results %>%
                     filter(dataset %in% datasets_sim) %>%
                     filter(type == input$train_test_type_err) %>%
                     mutate(dataset = str_replace(dataset, "_noise", ""))

                   if (input$datasets_err == "Sim data corr 0.1 AUROC 0.75"){
                     rv$data_plot_err <- rv$data_plot_err %>%
                       filter(str_detect(dataset, "sim_75_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_1 - ", ""))
                   } else if (input$datasets_err == "Sim data corr 0.7 AUROC 0.75"){
                     rv$data_plot_err <- rv$data_plot_err %>%
                       filter(str_detect(dataset, "sim_75_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_7 - ", ""))
                   } else if (input$datasets_err == "Sim data corr 0.1 AUROC 0.90"){
                     rv$data_plot_err <- rv$data_plot_err %>%
                       filter(str_detect(dataset, "sim_90_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_1 - ", ""))
                   } else if (input$datasets_err == "Sim data corr 0.7 AUROC 0.90"){
                     rv$data_plot_err <- rv$data_plot_err %>%
                       filter(str_detect(dataset, "sim_90_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_7 - ", ""))
                   }

                   rv$data_plot_err <- rv$data_plot_err %>%
                     mutate(dataset = factor(dataset, levels = c("MCAR", "MAR_2", "MAR_circ", "MNAR",
                                                                 "MAR_2_out", "MAR_circ_out")))
                 }
               })

  # OOB
  observeEvent(input$datasets_OOB,
               {
                 metrics_OOB <- c("MER", "macro_F1", "F1_score", "NMSE", "MSE")
                 if (input$datasets_OOB == "Real data with MV"){
                   rv$datasets_OOB <- datasets_MV
                   rv$data_plot_OOB <- results  %>%
                     filter(dataset %in% datasets_MV,
                            metric %in% metrics_OOB) %>%
                     mutate(dataset = str_replace(dataset, " - MV", ""))

                 } else if (input$datasets_OOB == "Real data with simulated MV"){
                   rv$datasets_OOB <- datasets_no_MV
                   rv$data_plot_OOB <- results %>%
                     filter(dataset %in% datasets_no_MV,
                            metric %in% metrics_OOB)

                 } else if (str_starts(input$datasets_OOB, "Sim data")){
                   rv$datasets_OOB <- datasets_sim
                   rv$data_plot_OOB <- results %>%
                     filter(dataset %in% datasets_sim,
                            metric %in% metrics_OOB) %>%
                     mutate(dataset = str_replace(dataset, "_noise", ""))

                   if (input$datasets_OOB == "Sim data corr 0.1 AUROC 0.75"){
                     rv$data_plot_OOB <- rv$data_plot_OOB %>%
                       filter(str_detect(dataset, "sim_75_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_1 - ", ""))
                   } else if (input$datasets_OOB == "Sim data corr 0.7 AUROC 0.75"){
                     rv$data_plot_OOB <- rv$data_plot_OOB %>%
                       filter(str_detect(dataset, "sim_75_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_75_7 - ", ""))
                   } else if (input$datasets_OOB == "Sim data corr 0.1 AUROC 0.90"){
                     rv$data_plot_OOB <- rv$data_plot_OOB %>%
                       filter(str_detect(dataset, "sim_90_1")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_1 - ", ""))
                   } else if (input$datasets_OOB == "Sim data corr 0.7 AUROC 0.90"){
                     rv$data_plot_OOB <- rv$data_plot_OOB %>%
                       filter(str_detect(dataset, "sim_90_7")) %>%
                       mutate(dataset = str_replace(dataset, "sim_90_7 - ", ""))
                   }

                   rv$data_plot_OOB <- rv$data_plot_OOB %>%
                     mutate(dataset = factor(dataset, levels = c("MCAR", "MAR_2", "MAR_circ", "MNAR",
                                                                 "MAR_2_out", "MAR_circ_out")))
                 }
               })

  # runtimes table
  observeEvent(input$datasets_timing,
               {
                 if (input$datasets_timing == "Real data with MV"){

                   rv$table_timing <- results_timing_table %>%
                     filter(dataset %in% datasets_MV) %>%
                     mutate(dataset = str_replace(dataset, " - MV", ""))

                 } else if (input$datasets_timing == "Real data with simulated MV"){

                   rv$table_timing <- results_timing_table %>%
                     filter(dataset %in% datasets_no_MV)

                 } else if (str_starts(input$datasets_timing, "Sim data")){

                   rv$table_timing <- results_timing_table %>%
                     filter(dataset %in% datasets_sim)

                   rv$table_timing <- rv$table_timing %>%
                     separate(dataset, into = c("dataset", "MV_mechanism"), sep = " - ") %>%
                     mutate(dataset = factor(dataset),
                            MV_mechanism = factor(MV_mechanism))
                 }
               })

  # object size table
  observeEvent(input$datasets_obj_size,
               {
                 if (input$datasets_obj_size == "Real data with MV"){

                   rv$table_obj_size <- results_obj_size_table %>%
                     filter(dataset %in% datasets_MV) %>%
                     mutate(dataset = str_replace(dataset, " - MV", ""))

                 } else if (input$datasets_obj_size == "Real data with simulated MV"){

                   rv$table_obj_size <- results_obj_size_table %>%
                     filter(dataset %in% datasets_no_MV)

                 } else if (str_starts(input$datasets_obj_size, "Sim data")){

                   rv$table_obj_size <- results_obj_size_table %>%
                     filter(dataset %in% datasets_sim)

                   rv$table_obj_size <- rv$table_obj_size %>%
                     separate(dataset, into = c("dataset", "MV_mechanism"), sep = " - ") %>%
                     mutate(dataset = factor(dataset),
                            MV_mechanism = factor(MV_mechanism))
                 }
               })

  output$pred_performance <- renderPlot({

    metric_plot_bin <- input$metric_binary
    metric_ensym <- ensym(metric_plot_bin)

    scales = "free_y"

    data_plot <- rv$data_plot %>%
      filter(metric == metric_plot_bin) %>%
      rename(!!metric_ensym := value) %>%
      filter(method %in% input$methods_compare,
             model %in% input$models_compare)

    if (input$plot_type == "IQR bars"){

      if (any(str_detect(rv$datasets, "sim_"))){ # alpha for noise
        p1 <- data_plot %>%
          ggplot(aes(method,!!metric_ensym, col = Type, alpha = Noise)) +
          scale_alpha_discrete(range = c(0.25, 1))
      } else {
        p1 <- data_plot %>%
          ggplot(aes(method,!!metric_ensym, col = Type))
      }

      p1 <- p1 +
        stat_summary(fun.min = function(z) { quantile(z,0.25) },
                     fun.max = function(z) { quantile(z,0.75) },
                     fun = median,
                     size = 0.25,
                     linewidth = 0.7)
    } else { # boxplots
      if (any(str_detect(rv$datasets, "sim_"))){ # alpha for noise
        p1 <- data_plot %>%
          ggplot(aes(method,!!metric_ensym, col = Type, alpha = Noise, fill = Type)) +
          geom_boxplot() +
          scale_alpha_discrete(range = c(0.25, 1))
      } else {
        p1 <- data_plot %>%
          ggplot(aes(method,!!metric_ensym, col = Type)) +
          geom_boxplot()
      }
    }
    p1 <- p1 +
      facet_grid(rows = vars(dataset), cols = vars(model), scales = scales) +
      theme_minimal(12) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.position="bottom") +
      xlab("Imputation method")

    metric_plot_cont <- input$metric_cont
    metric_ensym <- ensym(metric_plot_cont)

    data_plot_2 <- rv$data_plot %>%
      filter(metric == metric_plot_cont) %>%
      rename(!!metric_ensym := value) %>%
      filter(method %in% input$methods_compare,
             model %in% input$models_compare)

    if (nrow(data_plot_2) > 0){

      p1 <- p1 +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              panel.border = element_rect(colour = "black", fill=NA),
              legend.position="none")

      if (input$plot_type == "IQR bars"){
        p2 <- data_plot_2 %>%
          ggplot(aes(method,!!metric_ensym, col = Type)) +
          stat_summary(fun.min = function(z) { quantile(z,0.25) },
                       fun.max = function(z) { quantile(z,0.75) },
                       fun = median,
                       size = 0.25,
                       linewidth = 0.7)
      } else {
        p2 <- data_plot_2 %>%
          ggplot(aes(method,!!metric_ensym, col = Type)) +
          geom_boxplot()
      }
      p2 <- p2 +
        facet_grid(rows = vars(dataset), cols = vars(model), scales = scales) +
        theme_minimal(12) +
        theme(axis.text.x=element_text(angle=45, hjust=1),
              panel.border = element_rect(colour = "black", fill=NA),
              legend.position="bottom") +
        xlab("Imputation method")

      plot_grid(p1, p2, nrow = 2, rel_heights = c(1, 1.5))
    } else {
      if (nrow(p1$data) > 0) p1 else NULL
    }

  })

  output$table_performance = renderDT({

    metric_table_bin <- input$metric_binary_tab
    metric_table_cont <- input$metric_cont_tab

    metrics_tables <- c(metric_table_bin, metric_table_cont)

    scales = "free_y"

    data_tables <- rv$data_tables %>%
      filter(Metric %in% metrics_tables)

    cols_datasets <- colnames(data_tables)[!colnames(data_tables) %in%
                                             c("Model", "Metric", "Method")]

    data_tables <- data_tables %>%
      filter_at(vars(all_of(cols_datasets)), any_vars(!is.na(.)))

    n_col <- ncol(data_tables) - 1

    DT::datatable(data_tables,
                  escape = FALSE, rownames = FALSE,
                  #extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                  extensions = c("FixedColumns"),
                  options = list(dom = "t",
                                 iDisplayLength = 100,
                                 scrollX = TRUE, #scrollCollapse = TRUE,
                                 #scroller = TRUE,
                                 scrollY = "500px",
                                 scrollY = TRUE,
                                 fixedHeader = TRUE,
                                 autoWidth = TRUE,
                                 fixedColumns = list(leftColumns = 3),
                                 columnDefs = list(list(width = "130px",
                                                        targets = 3:n_col))),
                  filter = list(position = "top"))

  })

  output$plot_err_true_value <- renderPlot({

    metric_plot_cat <- input$metric_cat_err
    metric_plot_cont <- input$metric_cont_err

    scales = "fixed"

    data_plot <- rv$data_plot_err %>%
      filter(metric %in% c(metric_plot_cat, metric_plot_cont)) %>%
      filter(method %in% input$methods_compare_err)

    if (any(str_detect(rv$datasets_err, "sim_"))){ # alpha for noise

      data_plot <- data_plot %>%
        mutate(variable = factor(variable, levels = c(paste0("V", 1:9), paste0("V", 10: 16))))

      if (input$plot_type_err == "IQR bars"){

        p <- data_plot %>%
          ggplot(aes(method, value, col = method, alpha = Noise)) +
          scale_alpha_discrete(range = c(0.25, 1)) +
          stat_summary(fun.min = function(z) { quantile(z,0.25) },
                       fun.max = function(z) { quantile(z,0.75) },
                       fun = median,
                       size = 0.25,
                       linewidth = 0.7) +
          geom_hline(yintercept = 1, col = "darkgrey") +
          facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
          theme_minimal(12) +
          theme(axis.text.x=element_blank(),
                axis.title.x=element_blank(),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.position="bottom") +
          guides(colour=guide_legend(nrow=3))

      } else { # boxplots
        p <- data_plot %>%
          ggplot(aes(method, value, col = method, alpha = Noise, fill = method)) +
          geom_boxplot() +
          geom_hline(yintercept = 1, col = "darkgrey") +
          facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
          theme_minimal(12) +
          theme(axis.text.x=element_blank(),
                axis.title.x=element_blank(),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.position="bottom") +
          scale_alpha_discrete(range = c(0.25, 1)) +
          guides(colour=guide_legend(nrow=3))
      }

      p

    } else { # real datasets with MV

      datasets_plot <- unique(data_plot$dataset)

      list_p <- list()

      if (input$plot_type_err == "IQR bars"){
        for (d in 1:length(datasets_plot)){
          p <- data_plot %>%
            filter(dataset == datasets_plot[d]) %>%
            ggplot(aes(method, value, col = method)) +
            stat_summary(fun.min = function(z) { quantile(z,0.25) },
                         fun.max = function(z) { quantile(z,0.75) },
                         fun = median,
                         size = 0.25,
                         linewidth = 0.7) +
            geom_hline(yintercept = 1, col = "darkgrey") +
            facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
            theme_minimal(12) +
            theme(axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA),
                  legend.position="bottom") +
            guides(colour=guide_legend(nrow=3))

          if (d < length(datasets_plot)) p <- p + theme(legend.position = "none")

          list_p <- c(list_p, list(p))
        }

      } else { # box plots
        for (d in 1:length(datasets_plot)){
          p <- data_plot %>%
            filter(dataset == datasets_plot[d]) %>%
            ggplot(aes(method, value, col = method)) +
            geom_boxplot() +
            geom_hline(yintercept = 1, col = "darkgrey") +
            facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
            theme_minimal(12) +
            theme(axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA),
                  legend.position="bottom") +
            guides(colour=guide_legend(nrow=3))

          if (d < length(datasets_plot)) p <- p + theme(legend.position = "none")

          list_p <- c(list_p, list(p))
        }
      }

      plot_grid(plotlist = list_p, nrow = length(datasets_plot),
                rel_heights = c(1, 1, 1, 1.6))
    }

  })

  output$plot_OOB <- renderPlot({

    metric_plot_cat <- input$metric_cat_OOB
    metric_plot_cont <- input$metric_cont_OOB

    scales = "fixed"

    data_plot <- rv$data_plot_OOB %>%
      filter(metric %in% c(metric_plot_cat, metric_plot_cont)) %>%
      filter(method %in% input$methods_compare_OOB)

    if (any(str_detect(rv$datasets_OOB, "sim_"))){ # alpha for noise

      data_plot <- data_plot %>%
        mutate(variable = factor(variable, levels = c(paste0("V", 1:9), paste0("V", 10: 16))))

      if (input$plot_type_OOB == "IQR bars"){

        p <- data_plot %>%
          ggplot(aes(method, value, col = type, alpha = Noise)) +
          scale_alpha_discrete(range = c(0.25, 1)) +
          stat_summary(fun.min = function(z) { quantile(z,0.25) },
                       fun.max = function(z) { quantile(z,0.75) },
                       fun = median,
                       size = 0.25,
                       linewidth = 0.7) +
          geom_hline(yintercept = 1, col = "darkgrey") +
          facet_grid(rows = vars(dataset), cols = vars(variable), scales = scales) +
          theme_minimal(12) +
          theme(axis.text.x=element_text(angle=45, hjust=1),
                axis.title.x=element_blank(),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.position="bottom") +
          guides(colour=guide_legend(nrow=3))

      } else { # boxplots
        p <- data_plot %>%
          ggplot(aes(method, value, col = type, alpha = Noise, fill = type)) +
          geom_boxplot() +
          geom_hline(yintercept = 1, col = "darkgrey") +
          facet_grid(rows = vars(dataset), cols = vars(variable), scales = scales) +
          theme_minimal(12) +
          theme(axis.text.x=element_text(angle=45, hjust=1),
                axis.title.x=element_blank(),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.position="bottom") +
          scale_alpha_discrete(range = c(0.25, 1)) +
          guides(colour=guide_legend(nrow=3))
      }

      p

    } else { # real datasets with MV

      datasets_plot <- unique(data_plot$dataset)

      list_p <- list()

      if (input$plot_type_OOB == "IQR bars"){
        for (d in 1:length(datasets_plot)){
          p <- data_plot %>%
            filter(dataset == datasets_plot[d]) %>%
            ggplot(aes(method, value, col = type)) +
            stat_summary(fun.min = function(z) { quantile(z,0.25) },
                         fun.max = function(z) { quantile(z,0.75) },
                         fun = median,
                         size = 0.25,
                         linewidth = 0.7) +
            geom_hline(yintercept = 1, col = "darkgrey") +
            facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
            theme_minimal(12) +
            theme(axis.text.x=element_text(angle=45, hjust=1),
                  axis.title.x=element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA),
                  legend.position="bottom") +
            guides(colour=guide_legend(nrow=3))

          if (d < length(datasets_plot)) p <- p + theme(legend.position = "none")

          list_p <- c(list_p, list(p))
        }

      } else { # box plots
        for (d in 1:length(datasets_plot)){
          p <- data_plot %>%
            filter(dataset == datasets_plot[d]) %>%
            ggplot(aes(method, value, col = type)) +
            geom_boxplot() +
            geom_hline(yintercept = 1, col = "darkgrey") +
            facet_grid(rows = vars(dataset), cols = vars(variable), scales = "free") +
            theme_minimal(12) +
            theme(axis.text.x=element_text(angle=45, hjust=1),
                  axis.title.x=element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA),
                  legend.position="bottom") +
            guides(colour=guide_legend(nrow=3))

          if (d < length(datasets_plot)) p <- p + theme(legend.position = "none")

          list_p <- c(list_p, list(p))
        }
      }

      plot_grid(plotlist = list_p, nrow = length(datasets_plot),
                rel_heights = c(1, 1, 1, 1.6))
    }

  })

  output$text_table <- renderText({
    sprintf("<b>Best values</b> for each dataset and model (close to 0 or 1 for calibration metrics; minimum or maxmimum for other metrics) are shown in <b>bold</b>.
            The results on the original dataset (whenever applicable) are excluded from assessing the best results.")

  })

  output$text_err_true <- renderText({
    sprintf("The y axis value reflects the metrics chosen for continuous and categorical variables.")

  })

  output$text_err_OOB <- renderText({
    sprintf("The y axis value reflects the metrics chosen for continuous and categorical variables.")

  })

  output$text_timings <- renderText({
    sprintf("Median runtime in seconds (Q1 - Q3).
            <b>Best runtime</b> for each dataset and type are shown in <b>bold</b>.
            <i>Worst runtime</i> for each dataset and type are shown in <i>italic</i>.")

  })

  output$text_obj_size <- renderText({
    sprintf("Median object size in MB (Q1 - Q3).
            <b>Lowest object size</b> for each dataset and type are shown in <b>bold</b>.
            <i>Highest object size</i> for each dataset and type are shown in <i>italic</i>.")

  })

  output$table_timings = renderDT({

    DT::datatable(rv$table_timing,
                  escape = FALSE, rownames = FALSE,
                  #extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                  extensions = c("FixedColumns"),
                  options = list(dom = "t",
                                 iDisplayLength = 100,
                                 scrollX = TRUE, #scrollCollapse = TRUE,
                                 #scroller = TRUE,
                                 scrollY = "500px",
                                 scrollY = TRUE,
                                 fixedHeader = TRUE,
                                 autoWidth = TRUE#,
                                 # fixedColumns = list(leftColumns = 3),
                                 # columnDefs = list(list(width = "130px",
                                 #                        targets = 3:n_col))
                  ),
                  filter = list(position = "top"))

  })

  output$table_obj_size = renderDT({

    DT::datatable(rv$table_obj_size,
                  escape = FALSE, rownames = FALSE,
                  #extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                  extensions = c("FixedColumns"),
                  options = list(dom = "t",
                                 iDisplayLength = 100,
                                 scrollX = TRUE, #scrollCollapse = TRUE,
                                 #scroller = TRUE,
                                 scrollY = "500px",
                                 scrollY = TRUE,
                                 fixedHeader = TRUE,
                                 autoWidth = TRUE#,
                                 # fixedColumns = list(leftColumns = 3),
                                 # columnDefs = list(list(width = "130px",
                                 #                        targets = 3:n_col))
                  ),
                  filter = list(position = "top"))

  })


  output$text_var_imp <- renderText({
    sprintf("For linear models (Ridge and Cubic spline) the coefficients are kept for all imputation methods.
            For RF and XGB the variable importance represents the scaled decrease in perfromance error (MSE or Gini index) and is kept only for the models built on the missForestPredict imputed dataset and original dataset (to minimize the compuation time).")

  })

  output$plot_var_imp <- renderPlot({

    scales = "free_y"

    data_plot <- results_var_imp %>%
      filter(dataset %in% input$datasets_var_imp)  %>%
      mutate(variable = if_else(str_detect(variable, "rcs\\("),
                                str_replace_all(variable, "c\\(.+?\\)\\)", ""), variable))

    # put V5-V16 at the end
    if (all(c("V5", "V9", "V16") %in% unique(data_plot$variable))){
      all_vars <- unique(data_plot$variable)
      all_vars_V <- all_vars[str_starts(all_vars, "V")]
      all_vars_rcs <- all_vars[str_starts(all_vars, "rcs")]
      all_vars <- c(all_vars_V, all_vars_rcs)

      data_plot <- data_plot %>%
        mutate(variable = factor(variable, levels = all_vars))

    }

    data_plot <- data_plot %>%
      filter(model %in% input$model_var_imp)

    if (input$plot_type_var_imp == "IQR bars"){

      p <- data_plot %>%
        rename(`Variable importance` = value) %>%
        ggplot(aes(variable, `Variable importance`)) +
        stat_summary(fun.min = function(z) { quantile(z,0.25) },
                     fun.max = function(z) { quantile(z,0.75) },
                     fun = median,
                     size = 0.25,
                     linewidth = 0.7) +
        facet_wrap(~method, scales = scales) +
        theme_bw(12) +
        theme(axis.text.x=element_text(angle=45, hjust=1))

    } else { # boxplots
      p <- data_plot %>%
        rename(`Variable importance` = value) %>%
        ggplot(aes(variable, `Variable importance`)) +
        geom_boxplot() +
        facet_wrap(~method, scales = scales) +
        theme_bw(12) +
        theme(axis.text.x=element_text(angle=45, hjust=1))

    }

    p

  })


}

# Run the application
shinyApp(ui = ui, server = server)
