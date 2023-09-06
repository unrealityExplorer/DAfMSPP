# meta ------------

pacman::p_load(
  rio,          # data import/export
  tidyverse,    # data management
  shiny,        # framework for web application, interactive user interface
  shinyWidgets, # additional UI templates
  DT,           # interactive tables for data overview
  ggstatsplot,  # charts with statistical calculations results
  gtsummary,    # summary table with calculations results
  gt,           # for rendering summary table in ui
  waiter,       # loading placeholders
  shinythemes,  # ui colors templates
  rlang,        # workaround solution for reading inputs in the server code
  scales,       # for auto-gradient coloring feature for catergorical variables chart
  xtable,       # simplistic table output in ui
  ggside,       # side histograms for scatterplot
  esquisse,     # for plots download modules
  datamods,     # -//-
  officer,      # -//-
  rvg,          # -//-
  htmltools,    # -//-
  phosphoricons # -//-
)

options(shiny.maxRequestSize = 50 * 1024^2)

not_sel <- "Не выбрано"

rio_import_formats <- structure(list(
  `Расширение файла` = c(".csv", ".psv",
                         ".tsv", ".csvy",
                         ".xls", ".xlsx",
                         ".sas7bdat", ".sav",
                         ".zsav", ".dta",
                         ".xpt", ".por",
                         ".R", ".RData",
                         ".rds",
                         ".dif", ".fwf",
                         ".csv.gz", ".json", ".ods",
                         ".html", ".xml", ".yml"),
  Описание = c("Данные, разделённые запятой", "Данные, разделенные вертикальной чертой",
               "Данные, разделённые табуляцией", "(файл.csv с заголовком YAML)",
               "Формат данных программы Microsoft Excel", "Формат данных программы Microsoft Excel",
               "Формат данных программы SAS", "Формат данных программы SPSS",
               "Формат сжатых данных программы SPSS", "Формат данных программы Stata",
               "Формат данных программы SAS XPORT", "Формат данных программы SPSS Portable",
               "Формат кода языка R", "Формат сохранённых объектов R",
               "Формат сохранённых данных табличного вида на языке R",
               "Формат обмена данными отдельных электронных таблиц", "Формат данных фиксированной ширины",
               "(файл .csv, упакованный в zip)", "Формат файлов Java Script Object Notation", "Электронная таблица OpenDocument",
               "Таблица в HTML", "Таблица в XML", "Данные формата YAML")
), row.names = c(NA, -23L), class = c("tbl_df", "tbl", "data.frame"))


set_i18n(
  value = list(
    "Filename:" = "Название файла:",
    "Filename" = "Название файла",
    "Width:" = "Ширина:",
    "Height:" = "Высота:",
    "Update Preview" = "Обновить изображение",
    "Export format:" = "Нужный формат изображения:",
    "Close" = "Закрыть"
  )
)

# data upload and overview page --------------------

data_upload_filter_and_overview_page <- tabPanel(
  title = "Загрузка данных и их обзор",
  sidebarLayout(
    sidebarPanel(
      fileInput("uploaded_data", label = "Загрузить файл с таблицей данных", buttonLabel = "Выбрать файл...", placeholder = "Файл не выбран", accept = rio_import_formats$`Расширение файла`),
      tabsetPanel(
        id = "transform_trigger",
        type = "hidden",
        selected = "placeholder_panel_f_transform_trigger",
        tabPanel("placeholder_panel_f_transform_trigger"),
        tabPanel("transformation_trigger",
                 actionButton("transform_and_clean_data_trigger", label = "Трансформировать и очистить данные", icon = icon("broom")))
      ),
      shinyWidgets::materialSwitch("switch_info_about_extensions", label = "Показать допустимые расширения файлов", value = FALSE, status = "info"),
      tabsetPanel(
        id = "additional_info",
        type = "hidden",
        selected = "placeholder_panel_1st_page",
        tabPanel("placeholder_panel_1st_page"),
        tabPanel("extensions_info",
                 tableOutput("file_extensions_info"))
      ),
    ),
    mainPanel(
      waiter::autoWaiter(html = waiter::spin_solar()),
      DT::dataTableOutput("raw_data_table")
    )
  )
)




# groups comparison page --------------------

groups_comparison_charts_and_testing_page <- tabPanel(
  title = "Сравнение групп",
  h2("Сравнение групп, отличающихся по какому-то критерию, по какому-то интересующему показателю или характеристике"),
  h3("(cинонимы: проверка гипотезы, контролируемое исследование, А/Б-тест)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grouping_variable_for_comparison", label = "Выбрать группирующую характеристику", choices = c(not_sel)),
      selectInput("numerical_variable_for_comparison", label = "Выбрать интересующую числовую переменную", choices = c(not_sel)),
      selectInput("categorical_variable_for_comparison", label = "Выбрать интересующую категориальную переменную", choices = c(not_sel)),
      waiter::autoWaiter(html = waiter::spin_solar()),
      actionButton("run_comparison", "Проанализировать", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "График для числовых переменных",
          waiter::autoWaiter(),
          plotOutput(outputId = "plot_num_comp"),
          tabsetPanel(
            id = "save_comparison_num_variables_button_space",
            type = "hidden",
            selected = "placeholder_f_comparison_num_variables_chart",
            tabPanel("placeholder_f_comparison_num_variables_chart"),
            tabPanel("save_comparison_num_variables_chart_button",
                     actionButton("save_comparison_num_variables_chart", "Загрузить изображение в нужном формате и размере", icon = icon("download"))
            )
          )
        ),
        tabPanel(
          title = "График для категориальных переменных",
          waiter::autoWaiter(),
          plotOutput(outputId = "plot_cat_comp"),
          tabsetPanel(
            id = "save_comparison_cat_variables_button_space",
            type = "hidden",
            selected = "placeholder_f_comparison_cat_variables_chart",
            tabPanel("placeholder_f_comparison_cat_variables_chart"),
            tabPanel("save_comparison_cat_variables_chart_button",
                     actionButton("save_comparison_cat_variables_chart", "Загрузить изображение в нужном формате и размере", icon = icon("download"))
          )
        )
      )
    )
  )
)
)



# single distribution charts page ----------------------

single_distribution_charts_overview_page <- tabPanel(
  title = "Отдельные распределения",
  h2("Рассмотрение распределений значений отдельных переменных"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_data_type_switcher", "Выбрать тип рассматриваемой переменной", choices = c("Количественная", "Категориальная")),
      tabsetPanel(
        id = "data_type_params",
        type = "hidden",
        tabPanel(
          "Количественная",
          selectInput("numerical_variable_solo", label = "Выбрать интересующую числовую переменную", choices = c(not_sel)),
          sliderInput("num_var_solo_bins_number", "Выбрать число промежутков",
            value = 30, min = 1, max = 100
          ),
          sliderInput("num_var_solo_range", "Выбрать рассматриваемые границы значений",
            value = c(0, 100), min = 0, max = 100
          ),
          selectInput("cutting_variable_for_solo_numerical", label = "Выбрать срез значений по следующей категориальной переменной", choices = c(not_sel)),
          checkboxGroupInput("cutting_variable_values_for_solo_numerical", label = "Выбрать значения переменной, включённые в срез", choices = c(not_sel))
        ),
        tabPanel(
          "Категориальная",
          selectInput("categorical_variable_solo", label = "Выбрать интересующую категориальную переменную", choice = c(not_sel)),
          selectInput("coloring_grouping_variable_for_solo_categorical", label = "Выбрать закрашивающую группирующую переменную", choices = c(not_sel))
        )
      ),
    ),
    mainPanel(
      plotOutput("solo_var_chart"),
      tabsetPanel(
        id = "save_solo_chart_button_space",
        type = "hidden",
        selected = "placeholder_f_save_solo_chart_button",
        tabPanel("placeholder_f_save_solo_chart_button"),
        tabPanel("save_solo_chart_button",
                 actionButton("save_solo_chart_plot", "Загрузить изображение в нужном формате и размере", icon = icon("download")))
      )
    )
  )
)



# summary table page -------------------

summary_table_page <- tabPanel(
  title = "Сводная таблица по данным",
  h2("Сведение в одну таблицу всех указанных расчётных характеристик для всех указанных переменных из таблицы данных"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_columns_for_summary_table", label = "Выбрать колонки переменных для вычислений", choices = c(not_sel), multiple = TRUE),
      selectInput("grouping_variable_for_summary_table", label = "Выбрать группирующую характеристику", choices = c(not_sel)),
      actionButton("run_summary", "Проанализировать", icon = icon("play")),
    ),
    mainPanel(
      waiter::autoWaiter(),
      gt::gt_output("summ_table")
    )
  )
)





# correlations and regression models -------------------------

correlation_and_regression_page <- tabPanel(
  title = "Линейные зависимости и связи",
  h2("Рассмотрение соотношения изменений значений интересующей переменной при изменении значений другой переменной"),
  sidebarLayout(
    sidebarPanel(
      selectInput("choice_of_method_of_interest", label = "Выбрать интересующий метод", choices = c("Проверить наличие зависимости между изменениями значений переменных и её выраженность",
                                                                                                    "Выявить меру изменения одной зависимой переменной при изменении другой переменной")),
      tabsetPanel(
        id = "corr_regr_choice",
        type = "hidden",
        tabPanel(
          "Проверить наличие зависимости между изменениями значений переменных и её выраженность",
          selectInput("numerical_variable_predictor_f_corr", label = "Выбрать интересующую числовую переменную №1", choices = c(not_sel)),
          selectInput("numerical_variable_dependent_f_corr", label = "Выбрать интересующую числовую переменную №2", choices = c(not_sel)),
          actionButton("run_correlation", label = "Проанализировать")
        ),
        tabPanel(
          "Выявить меру изменения одной зависимой переменной при изменении другой переменной",
          h3("** В разработке **"))

      )
      ),
    mainPanel(
      plotOutput("corr_plot"),
      tabsetPanel(
        id = "save_corr_chart_button_space",
        type = "hidden",
        selected = "placeholder_f_corr_chart",
        tabPanel("placeholder_f_corr_chart"),
        tabPanel("save_corr_chart_button",
                 actionButton("save_corr_chart", "Загрузить изображение в нужном формате и размере", icon = icon("download"))
        )
      )
    )
  )
)


# logistic regression and survival analysis ----

log_reg_n_surv_analysis <- tabPanel(
  title = "Определение вероятности события или состояния",
  mainPanel(
  h1("** В разработке **")
  )
)


# final ui wrapper ----------------------
ui <-
  navbarPage(
    title = "Быстрый статистический анализ данных для исследования",
    theme = shinythemes::shinytheme("darkly"),
    data_upload_filter_and_overview_page,
    single_distribution_charts_overview_page,
    groups_comparison_charts_and_testing_page,
    summary_table_page,
    correlation_and_regression_page,
    log_reg_n_surv_analysis
  )



# server -----------------

server <- function(input, output, session) {

    # загрузка таблицы с данными -----------------

  correct_extension <- reactive({
    if (
      any(
        str_detect(
          input$uploaded_data$datapath,
          vapply(
            rio_import_formats$`Расширение файла`,
            function(x){
              str_glue("\\", x, "$")
              },
            FUN.VALUE = "smth")
          )
        ) == TRUE){
      return("value_true")
    } else {
      return("value_false")
    }
  })


  load_data <- function(){

    switcher <- correct_extension()

    if (switcher == "value_false"){
      return(NULL)
    } else {
      test_upload <- try(rio::import(input$uploaded_data$datapath, setclass = "tibble"), silent =  TRUE)
      if (inherits(test_upload, "try-error")){return(NULL)}
      return(test_upload)
    }
  }

  data_backup <- reactive({

    req(input$uploaded_data$datapath)

    load_data()

  })


  data <- reactiveVal(NULL)

  observe({data(data_backup())})



  transform_and_clean_data <- function(data){

    modified_df <- lapply(data, function(x){
      if (length(unique(x)) < 10){
        x <- as_factor(x)
      } else {x <- as.numeric(x)}
    }) |> as_tibble()

    indexation_of_prevalent_na <- vapply(modified_df, FUN.VALUE = TRUE, FUN = function(x){
      (sum(is.na(x)) / length(x)) < 0.9
    })

    filtered_df <- modified_df[, indexation_of_prevalent_na]

    indexation_of_homogenous_columns <- vapply(filtered_df, FUN.VALUE = TRUE , FUN = function(x){
      !(length(unique(x[!is.na(x)])) == 1)
    })

    return(filtered_df[indexation_of_homogenous_columns])
  }

  observeEvent(input$transform_and_clean_data_trigger, {
    data(transform_and_clean_data(data()))
  })

  output$raw_data_table <- DT::renderDT(
    datatable(
      data(),
      style = "auto",
      filter = "top",
      rownames = FALSE,
      extensions = c("KeyTable", "Scroller"),
      options = list(keys = TRUE, scrollY = 1000, scrollX = 200, scroller = TRUE, dom = "tp"),
      editable = TRUE
      )
    )

  output$file_extensions_info <- renderTable(rio_import_formats |>
                                                        xtable::xtable())

    # обновление элементов интерфейса ---------

  observeEvent(data(), {
    choices_all <- c(not_sel, names(data()))
    choices_numerical <- c(not_sel, names(data())[vapply(data(), is.numeric, FUN.VALUE = TRUE)])
    choices_categorical <- c(not_sel, names(data())[vapply(data(), function(x) {
      !is.numeric(x)
    }, FUN.VALUE = TRUE)])
    updateSelectInput(inputId = "grouping_variable_for_comparison", choices = choices_categorical)
    updateSelectInput(inputId = "numerical_variable_for_comparison", choices = choices_numerical)
    updateSelectInput(inputId = "categorical_variable_for_comparison", choices = choices_categorical)
    updateSelectInput(inputId = "numerical_variable_solo", choices = choices_numerical)
    updateSelectInput(inputId = "categorical_variable_solo", choices = choices_categorical)
    updateSelectInput(inputId = "grouping_variable_for_summary_table", choices = choices_categorical)
    updateSelectInput(inputId = "selected_columns_for_summary_table", choices = choices_all)
    updateSelectInput(inputId = "coloring_grouping_variable_for_solo_categorical", choices = choices_categorical)
    updateSelectInput(inputId = "cutting_variable_for_solo_numerical", choices = choices_categorical)
    updateSelectInput(inputId = "numerical_variable_predictor_f_corr", choices = choices_numerical)
    updateSelectInput(inputId = "numerical_variable_dependent_f_corr", choices = choices_numerical)
  })

  additional_info_switch <- reactive(input$switch_info_about_extensions)

  observeEvent(additional_info_switch(), {
    if (input$switch_info_about_extensions == TRUE){
     updateTabsetPanel(inputId = "additional_info", selected = "extensions_info")
    } else if (input$switch_info_about_extensions == FALSE){
     updateTabsetPanel(inputId = "additional_info", selected = "placeholder_panel_1st_page")
    }
  })

  observeEvent(data(), {

    updateTabsetPanel(inputId = "transform_trigger", selected = "transformation_trigger")

  })

  data_type_param <- reactive(input$variable_data_type_switcher)

  observeEvent(data_type_param(), {
    updateTabsetPanel(inputId = "data_type_params", selected = input$variable_data_type_switcher)
  })

  chosen_cutting_var_f_solo_num <- reactive(unique(data()[[input$cutting_variable_for_solo_numerical]]))

  observeEvent(chosen_cutting_var_f_solo_num(), {
    choices <- c(chosen_cutting_var_f_solo_num())
    updateCheckboxGroupInput(inputId = "cutting_variable_values_for_solo_numerical", choices = choices)
  })

  chosen_num_var_solo <- reactive(input$numerical_variable_solo)

  value_f_num_var_solo_bins_number <- reactive(

    if (is.numeric(data()[[rlang::sym(chosen_num_var_solo())]]) == TRUE){

      {
        if ((length(data()[[rlang::sym(chosen_num_var_solo())]]) < 200)) {
          as.integer(length(data()[[rlang::sym(chosen_num_var_solo())]]) / 4)
        } else {
          return(50)
        }
      }
    }
  )

  max_f_num_var_solo_bins_number <- reactive({

    if (is.numeric(data()[[rlang::sym(chosen_num_var_solo())]]) == TRUE){

      if (length(data()[[rlang::sym(chosen_num_var_solo())]]) < 200) {
        length(data()[[rlang::sym(chosen_num_var_solo())]])
      } else {
        return(200)
      }

    }
  })

  observeEvent(value_f_num_var_solo_bins_number(), {

    updateSliderInput(
      inputId = "num_var_solo_range",
      value = c(min(data()[[input$numerical_variable_solo]]), max(data()[[input$numerical_variable_solo]])),
      min = floor(min(data()[[input$numerical_variable_solo]][!is.na(data()[[input$numerical_variable_solo]])])),
      max = ceiling(max(data()[[input$numerical_variable_solo]][!is.na(data()[[input$numerical_variable_solo]])]))
    )

    updateSliderInput(
      inputId = "num_var_solo_bins_number",
      value = value_f_num_var_solo_bins_number(),
      min = 2,
      max = max_f_num_var_solo_bins_number()
    )

  })

  corr_regr_method_of_choice <- reactive({input$choice_of_method_of_interest})

  observeEvent(corr_regr_method_of_choice(), {
    updateTabsetPanel(inputId = "corr_regr_choice", selected = input$choice_of_method_of_interest)
  })

  observeEvent(rv_solo_chart$plot, {

    updateTabsetPanel(inputId = "save_solo_chart_button_space", selected = "save_solo_chart_button")

    })

  observeEvent(rv_comparison_cat_var_chart$plot, {

    updateTabsetPanel(inputId = "save_comparison_cat_variables_button_space", selected = "save_comparison_cat_variables_chart_button")

  })

  observeEvent(rv_comparison_num_var_chart$plot, {

    updateTabsetPanel(inputId = "save_comparison_num_variables_button_space", selected = "save_comparison_num_variables_chart_button")

  })

  observeEvent(rv_correalogram$plot, {

    updateTabsetPanel(inputId = "save_corr_chart_button_space", selected = "save_corr_chart_button")

  })


    # создание графика для рассмотрения распределения одной переменной -------------

  cc <- reactive({
    scales::seq_gradient_pal("#112446", "#00CC66", "Lab")(seq(0, 1, length.out =  length(unique(data()[[input$coloring_grouping_variable_for_solo_categorical]]))))
  })

  solo_chart_function <- function(dat, num_var_solo, cat_var_solo, var_dat_type_switcher, cutt_var_f_solo_num, chosen_values_from_cutting_variable_f_solo_num, num_var_bins_number, num_var_solo_range, color_gr_var_f_solo_cat){

    if (num_var_solo == not_sel & cat_var_solo == not_sel) {
      return(NULL)
    } else if (var_dat_type_switcher == "Количественная" & num_var_solo != not_sel & cutt_var_f_solo_num == not_sel) {
      dat |>
        filter(!is.na(.data[[num_var_solo]])) |>
        ggplot(aes(x = .data[[num_var_solo]])) +
        geom_histogram(color = "black", fill = "#112446", bins = num_var_bins_number) +
        labs(y = "Число наблюдений") +
        scale_y_continuous(breaks = seq(0, length(dat[[num_var_solo]]), 2)) +
        xlim(num_var_solo_range) +
        theme_minimal()
    } else if (var_dat_type_switcher == "Количественная" & cutt_var_f_solo_num != not_sel & length(cutt_var_f_solo_num > 0)) {
      dat |>
        filter(!is.na(.data[[num_var_solo]])) |>
        filter(.data[[cutt_var_f_solo_num]] %in% chosen_values_from_cutting_variable_f_solo_num) |>
        ggplot(aes(x = .data[[num_var_solo]])) +
        geom_histogram(color = "black", fill = "#112446", bins = num_var_bins_number) +
        labs(y = "Число наблюдений") +
        scale_y_continuous(breaks = seq(0, length(dat[[num_var_solo]]), 2)) +
        xlim(num_var_solo_range) +
        theme_minimal()
    } else if (var_dat_type_switcher == "Категориальная" & cat_var_solo != not_sel & color_gr_var_f_solo_cat != not_sel) {
      dat |>
        ggplot() +
        geom_bar(aes(x = .data[[cat_var_solo]], fill = .data[[color_gr_var_f_solo_cat]]), color = "black") +
        scale_fill_manual(values = cc()) +
        labs(y = "Число наблюдений") +
        scale_y_continuous(breaks = seq(0, length(dat[[cat_var_solo]]), 2)) +
        theme_minimal()
    } else if (var_dat_type_switcher == "Категориальная" & cat_var_solo != not_sel) {
      dat |>
        ggplot() +
        aes(x = .data[[cat_var_solo]]) +
        geom_bar(color = "black", fill = "#112446") +
        labs(y = "Число наблюдений") +
        scale_y_continuous(breaks = seq(0, length(dat[[cat_var_solo]]), 2)) +
        theme_minimal()
    }
  }

  solo_chart_reactive <- bindEvent(

    x = reactive({

      solo_chart_function(
        dat = data(),
        num_var_solo = input$numerical_variable_solo,
        cat_var_solo = input$categorical_variable_solo,
        var_dat_type_switcher = input$variable_data_type_switcher,
        cutt_var_f_solo_num = input$cutting_variable_for_solo_numerical,
        chosen_values_from_cutting_variable_f_solo_num = input$cutting_variable_values_for_solo_numerical,
        color_gr_var_f_solo_cat = input$coloring_grouping_variable_for_solo_categorical,
        num_var_bins_number = input$num_var_solo_bins_number,
        num_var_solo_range = input$num_var_solo_range
        )

      }),

    {
      input$numerical_variable_solo
      input$categorical_variable_solo
      input$variable_data_type_switcher
      input$cutting_variable_for_solo_numerical
      input$cutting_variable_values_for_solo_numerical
      input$coloring_grouping_variable_for_solo_categorical
      input$num_var_solo_bins_number
      input$num_var_solo_range
    }

  )

  rv_solo_chart <- reactiveValues(plot = NULL)



  output$solo_var_chart <- renderPlot({

    rv_solo_chart$plot <- solo_chart_reactive()

    rv_solo_chart$plot

    })

  observeEvent(input$save_solo_chart_plot, {

    save_ggplot_modal("solo_chart", output_format = c("png", "jpeg", "bmp"))

    })

  save_ggplot_server("solo_chart", rv_solo_chart)



    # создание графиков сравнения групп и расчётов ----------------

  cc2 <- reactive({
    scales::seq_gradient_pal("#112446", "#00CC66", "Lab")(seq(0, 1, length.out =  length(unique(data()[[input$grouping_variable_for_comparison]]))))
  })

  plot_for_comparison_of_numerical_variable_function <- function(dat, num_var_f_comp, gr_var_f_comp){
    if (num_var_f_comp == not_sel) {
      return(NULL)
    } else {
      dat |>
        ggbetweenstats(
          x = !!rlang::sym(gr_var_f_comp),
          y = !!rlang::sym(num_var_f_comp),
          type = "np",
          bf.message = F,
          violin.args = list(width = 0),
          point.args = NULL,
          package = "feathers",
          palette = "princess_parrot"
        )
    }
  }


  plot_for_comparison_of_numerical_variable_reactive <- eventReactive(input$run_comparison, {


    plot_for_comparison_of_numerical_variable_function(
      dat = data(),
      num_var_f_comp = input$numerical_variable_for_comparison,
      gr_var_f_comp = input$grouping_variable_for_comparison)
  })

  rv_comparison_num_var_chart <- reactiveValues(plot = NULL)

  output$plot_num_comp <- renderPlot({

    rv_comparison_num_var_chart$plot <- plot_for_comparison_of_numerical_variable_reactive()
    rv_comparison_num_var_chart$plot

    })

  observeEvent(input$save_comparison_num_variables_chart, {
    save_ggplot_modal("groups_comparison_num_var_chart", output_format = c("png", "jpeg", "bmp"))
  })
  save_ggplot_server("groups_comparison_num_var_chart", rv_comparison_num_var_chart)

  plot_for_comparison_of_categorical_variable_function <- function(dat, cat_var_f_comp, gr_var_f_comp){
    if (cat_var_f_comp == not_sel) {
      return(NULL)
    } else {
      dat |>
        ggbarstats(
          x = !!rlang::sym(gr_var_f_comp),
          y = !!rlang::sym(cat_var_f_comp),
          proportion.test = FALSE,
          bf.message = FALSE,
          legend.title = element_blank(),
          package = "feathers",
          palette = "princess_parrot"
        ) +
        scale_fill_manual(values = cc2())
    }
  }

  plot_for_comparison_of_categorical_variable_reactive <- eventReactive(input$run_comparison, {

    plot_for_comparison_of_categorical_variable_function(
      dat = data(),
      cat_var_f_comp = input$categorical_variable_for_comparison,
      gr_var_f_comp = input$grouping_variable_for_comparison)
  })

  rv_comparison_cat_var_chart <- reactiveValues(plot = NULL)

  output$plot_cat_comp <- renderPlot({

    rv_comparison_cat_var_chart$plot <- plot_for_comparison_of_categorical_variable_reactive()
    rv_comparison_cat_var_chart$plot

    })

  observeEvent(input$save_comparison_cat_variables_chart, {
    save_ggplot_modal("groups_comparison_cat_var_chart", output_format = c("png", "jpeg", "bmp"))
  })
  save_ggplot_server("groups_comparison_cat_var_chart", rv_comparison_cat_var_chart)




    # создание сводной таблицы --------------------

  summary_table_function <- function(dat, gr_var_f_summ, sel_cols_f_summ_t){

    if (length(sel_cols_f_summ_t) == 0 & gr_var_f_summ == not_sel) {
      dat |>
        tbl_summary(
          type = all_continuous() ~ "continuous2"
        ) |>
        add_n() |>
        add_stat_label(label = all_continuous() ~ "Медиана (Межквартильный размах)") |>
        modify_header(
          label = "**Характеристика**"
        ) |>
        as_gt()
    } else if (length(sel_cols_f_summ_t) == 0 & gr_var_f_summ != not_sel) {
      dat |>
        tbl_summary(
          by = all_of(gr_var_f_summ),
          missing = "no",
          type = all_continuous() ~ "continuous2"
        ) |>
        add_p() |>
        add_n() |>
        add_stat_label(label = all_continuous() ~ "Медиана (Межквартильный размах)") |>
        modify_header(
          label = "**Характеристика**",
          p.value = "**p-значение**"
        ) |>
        as_gt()
    } else if (length(sel_cols_f_summ_t) != 0 & gr_var_f_summ != not_sel) {
      dat |>
        select(all_of(sel_cols_f_summ_t)) |>
        tbl_summary(
          by = all_of(gr_var_f_summ),
          missing = "no",
          type = all_continuous() ~ "continuous2"
        ) |>
        add_p() |>
        add_n() |>
        add_stat_label(label = all_continuous() ~ "Медиана (Межквартильный размах)") |>
        modify_header(
          label = "**Характеристика**",
          p.value = "**p-значение**"
        ) |>
        as_gt()
    } else if (length(sel_cols_f_summ_t) != 0 & gr_var_f_summ == not_sel) {
      dat |>
        select(all_of(sel_cols_f_summ_t)) |>
        tbl_summary(
          type = all_continuous() ~ "continuous2"
        ) |>
        add_n() |>
        add_stat_label(label = all_continuous() ~ "Медиана (Межквартильный размах)") |>
        modify_header(
          label = "**Характеристика**"
        ) |>
        as_gt()
    }
  }

  summary_table_reactive <- eventReactive(input$run_summary, {
    summary_table_function(
      dat = data(),
      gr_var_f_summ = input$grouping_variable_for_summary_table,
      sel_cols_f_summ_t = input$selected_columns_for_summary_table
      )
  })

  output$summ_table <- gt::render_gt(summary_table_reactive())
    # страничка с корреалограммой ----


  scatterplot_n_correlation_function <- function(dat, numerical_variable_x, numerical_variable_y){
    dat |>
      ggscatterstats(
        x = !!rlang::sym(numerical_variable_x),
        y = !!rlang::sym(numerical_variable_y),
        bf.message = FALSE,
        type = "np",
        xsidehistogram.args = list(fill = "#112446", color = "black"),
        ysidehistogram.args = list(fill = "#00CC66", color = "black")
      ) +
      ggside::theme_ggside_void()
  }

  scatterplot_n_correlation_reactive <- eventReactive(input$run_correlation, {
    scatterplot_n_correlation_function(
      dat = data(),
      numerical_variable_x = input$numerical_variable_predictor_f_corr,
      numerical_variable_y = input$numerical_variable_dependent_f_corr)
  })

  rv_correalogram <- reactiveValues(plot = NULL)

  output$corr_plot <- renderPlot({

    rv_correalogram$plot <- scatterplot_n_correlation_reactive()
    rv_correalogram$plot

    })

  observeEvent(input$save_corr_chart, {
    save_ggplot_modal("corr_chart", output_format = c("png", "jpeg", "bmp"))
  })
  save_ggplot_server("corr_chart", rv_correalogram)

}

shinyApp(ui, server)
