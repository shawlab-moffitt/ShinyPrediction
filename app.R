type_id <- paste0("v1.0.20240923")


library("shiny")
library("shinycssloaders")
library("shinyjqui")
library("svglite")
library("ggplot2")
library("dplyr")
library("DT")
library("data.table")
library("readr")
library("nnet")
library("caret")
library("Hmisc")

options(shiny.maxRequestSize = 10000*1024^2)

# UI ---------------------------------------------------------------------------
ui <- navbarPage("{ shinyLogisticTest }",
                 # Data Preview -----------------------------------------------
                 tabPanel("Data Preview",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              p(),
                              h3("Training Data"),
                              fileInput("MatFileInput", label = "Matrix Upload:"),
                              div(fileInput("ModelFileInput", label = "Training Model Upload:",
                                            accept = c(".rds",".RData"), placeholder = "Accepts .rds or .RData file"), style = "margin-top:-10px"),
                              div(hr(), style = "margin-top:-10px"),
                              h3("Testing Data"),
                              fileInput("TestMatFileInput", label = "Matrix Upload:"),
                              div(fileInput("TestMetFileInput", label = "Meta Upload:"), style = "margin-top:-10px")
                            ),
                            mainPanel(
                              p(),
                              verbatimTextOutput("FileCheckAlerts"),
                              p(),
                              uiOutput("rendPreviewTabs")
                            )
                          )
                 ),
                 # Prediction -----------------------------------------------
                 tabPanel("Prediction",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              p(),
                              conditionalPanel(condition = "input.PredictionData != '1'",
                                               uiOutput("rendAddMetaCols")
                                               )
                            ),
                            mainPanel(
                              tabsetPanel(id = "PredictionData",
                                          tabPanel("Training Model Fitted",
                                                   p(),
                                                   radioButtons("ModelFittedTabHead",NULL, choices = c("View table head","View entire table"),
                                                                selected = "View entire table", inline = T),
                                                   dataTableOutput("ModelFittedTab"),
                                                   downloadButton("dnldModelFittedTab","Download Table"),
                                                   value = 1
                                          ),
                                          tabPanel("Testing Data Group Centers",
                                                   p(),
                                                   radioButtons("TestGroupCentersTabHead",NULL, choices = c("View table head","View entire table"),
                                                                selected = "View entire table", inline = T),
                                                   dataTableOutput("TestGroupCentersTab"),
                                                   downloadButton("dnldTestGroupCentersTab","Download Table"),
                                                   value = 2
                                          ),
                                          tabPanel("Predictions",
                                                   p(),
                                                   radioButtons("TestPredictionsTabHead",NULL, choices = c("View table head","View entire table"),
                                                                selected = "View entire table", inline = T),
                                                   dataTableOutput("TestPredictionsTab"),
                                                   downloadButton("dnldTestPredictionsTab","Download Table"),
                                                   value = 3
                                          )
                                          )

                            )
                          )
                 )
)


# Server -----------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  FileCheckAlerts_react1 <- reactiveVal(c())
  FileCheckAlerts_react2 <- reactiveVal(c())

  output$FileCheckAlerts <- renderPrint({
    req(FileCheckAlerts_react1())
    req(FileCheckAlerts_react2())
    text <- paste(c(FileCheckAlerts_react1(),FileCheckAlerts_react2()), collapse = "\n")
    cat(text)
  })
  #output$renddownload_notes <- renderUI({
  #  if (length(c(FileCheckAlerts_react1(),FileCheckAlerts_react2())) > 0) {
  #    downloadButton("download_notes","Download data processing notes")
  #  }
  #})
  #output$download_notes <- downloadHandler(
  #  filename = function() {
  #    paste("Merge_Data_Processing_Notes_", Sys.Date(), ".txt", sep = "")
  #  },
  #  content = function(file) {
  #    df <- c(FileCheckAlerts_react1(),FileCheckAlerts_react2())
  #    writeLines(df, file)
  #  }
  #)

  # Load in files --------------------------------------------------------------

  # File names
  model_react_file <- reactiveVal(NULL)
  mat_train_file <- reactiveVal(NULL)
  mat_test_file <-reactiveVal(NULL)
  met_test_file <-reactiveVal(NULL)
  # Raw data
  model_react <- reactiveVal(NULL)
  mat_train_raw <- reactiveVal(NULL)
  mat_test_raw <- reactiveVal(NULL)
  # Formatted Data
  mat_train <- reactiveVal(NULL)
  mat_test <- reactiveVal(NULL)
  met_test <- reactiveVal(NULL)
  # Vals
  feature_react <- reactiveVal(NULL)

  observe({
    print(FileCheckAlerts_react1())
    print(FileCheckAlerts_react2())
  })

  ## URL Input -----------------------------------------------------------------
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query)
    if (isTruthy(query[['train']])) {
      mat_train_file(query[['train']])
    }
    if (isTruthy(query[['model']])) {
      model_react_file(query[['model']])
    }
    if (isTruthy(query[['test']])) {
      mat_test_file(query[['test']])
    }
    if (isTruthy(query[['meta']])) {
      met_test_file (query[['meta']])
    }
  })

  ## File Upload ---------------------------------------------------------------
  observeEvent(input$ModelFileInput, {
    model_react_file(input$ModelFileInput$datapath)
  })
  observeEvent(input$MatFileInput, {
    mat_train_file(input$MatFileInput$datapath)
  })
  observeEvent(input$TestMatFileInput, {
    mat_test_file(input$TestMatFileInput$datapath)
  })
  observeEvent(input$TestMetFileInput, {
    met_test_file(input$TestMetFileInput$datapath)
  })

  output$rendPreviewTabs <- renderUI({
    training_mat_tab <- tabPanel("Training Matrix",
                                 p(),
                                 uiOutput("rendMatHead"),
                                 dataTableOutput("Training_Matrix_Preview"),
                                 downloadButton("dnldTrainMat","Download Table"),
                                 value = 1
    )
    testing_mat_tab <- tabPanel("Testing Matrix",
                                p(),
                                uiOutput("rendTestMatHead"),
                                dataTableOutput("Testing_Matrix_Preview"),
                                downloadButton("dnldTestMat","Download Table"),
                                value = 3
    )
    testing_meta_tab <- tabPanel("Testing Meta",
                                 p(),
                                 uiOutput("rendTestMetHead"),
                                 dataTableOutput("Testing_Meta_Preview"),
                                 downloadButton("dnldTestMet","Download Table"),
                                 value = 4
    )

    if (isTruthy(met_test()) & isTruthy(mat_train_raw()) & isTruthy(mat_test_raw())) {
      tabsetPanel(id = "PreviewTabs",
                  training_mat_tab,
                  testing_mat_tab,
                  testing_meta_tab
      )
    } else if (!isTruthy(met_test()) & isTruthy(mat_train_raw()) & isTruthy(mat_test_raw())) {
      tabsetPanel(id = "PreviewTabs",
                  training_mat_tab,
                  testing_mat_tab
      )
    }

  })

  observeEvent(model_react_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Training Model Uploaded: ",input$ModelFileInput$name)))
    withProgress(message = "Processing", value = 0, {
      incProgress(0.5, detail = "Loading Training Model")
      if (file.exists(model_react_file())) {
        if (tolower(tools::file_ext(model_react_file())) == "rdata") {
          model <- loadRData(model_react_file())
        } else if (tolower(tools::file_ext(model_react_file())) == "rds") {
          model <- readRDS(model_react_file())
        }
      } else {
        if (tolower(tools::file_ext(model_react_file())) == "rdata") {
          model <- loadRData(url(model_react_file()))
        } else if (tolower(tools::file_ext(model_react_file())) == "rds") {
          model <- readRDS(url(model_react_file()))
        }
      }
      incProgress(0.5, detail = "Complete!")
    })
    model_react(model)
    feature <- as.character(model$terms)[2]
    feature <- gsub("^`|`$","",feature)
    if (isTruthy(feature)) {
      feature_react(feature)
    }
  })

  observeEvent(mat_train_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Training Matrix Uploaded: ",input$MatFileInput$name)))
    withProgress(message = "Processing", value = 0, {
      incProgress(0.5, detail = "Loading Training Data")
      if (file.exists(mat_train_file())) {
        if (tolower(tools::file_ext(mat_train_file())) %in% c("zip","gz")) {
          mat_train_raw <- as.data.frame(read_delim(mat_train_file(),delim = '\t', col_names = T))
        } else {
          mat_train_raw <- as.data.frame(fread(mat_train_file()))
        }
      } else {
        if (tools::file_ext(mat_train_file()) %in% c("txt","tsv","TXT","TSV")) {
          mat_train_raw <- as.data.frame(read_delim(url(mat_train_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(mat_train_file()) %in% c("zip","gz","ZIP","GZ")) {
          mat_train_raw <- as.data.frame(read_delim(getZip(mat_train_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(mat_train_file()) %in% c("csv","CSV")) {
          mat_train_raw <- as.data.frame(read_delim(url(mat_train_file()), delim = ',', col_names = T))
        } else if (tools::file_ext(mat_train_file()) %in% c("RData","rdata")) {
          mat_train_raw <- loadRData(url(mat_train_file()))
        } else if (tools::file_ext(mat_train_file()) %in% c("rds","RDS")) {
          mat_train_raw <- readRDS(url(mat_train_file()))
        }
      }
      # Remove Duplicate features
      colnames(mat_train_raw)[1] <- "Feature"
      mat_train_raw_dup <- mat_train_raw[which(mat_train_raw[,1] %in% mat_train_raw[,1][duplicated(mat_train_raw[,1])]),]
      mat_train_raw_nondup <- mat_train_raw[which(!mat_train_raw[,1] %in% mat_train_raw[,1][duplicated(mat_train_raw[,1])]),]
      if (nrow(mat_train_raw_dup) > 0) {
        mat_train_raw_dup <- mat_train_raw_dup %>%
          group_by(Feature) %>%
          summarise_all(max)
      }
      mat_train_raw <- rbind(mat_train_raw_dup,mat_train_raw_nondup)
      if (nrow(mat_train_raw_dup) > 0) {
        message <- paste0(length(unique(mat_train_raw_dup[,1])), " duplicate features found in training matrix. Features reduced to those with maximum average value." )
        FileCheckAlerts_react1(FileCheckAlerts_list,message)
      }
      incProgress(0.5, detail = "Complete!")
    })
    mat_train_raw(mat_train_raw)
  })

  observeEvent(mat_test_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Testing Matrix Uploaded: ",input$TestMatFileInput$name)))
    withProgress(message = "Processing", value = 0, {
      incProgress(0.5, detail = "Loading Testing Data")
      if (file.exists(mat_test_file())) {
        if (tolower(tools::file_ext(mat_test_file())) %in% c("zip","gz")) {
          mat_test_raw <- as.data.frame(read_delim(mat_test_file(),delim = '\t', col_names = T))
        } else {
          mat_test_raw <- as.data.frame(fread(mat_test_file()))
        }
      } else {
        if (tools::file_ext(mat_test_file()) %in% c("txt","tsv","TXT","TSV")) {
          mat_test_raw <- as.data.frame(read_delim(url(mat_test_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(mat_test_file()) %in% c("zip","gz","ZIP","GZ")) {
          mat_test_raw <- as.data.frame(read_delim(getZip(mat_test_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(mat_test_file()) %in% c("csv","CSV")) {
          mat_test_raw <- as.data.frame(read_delim(url(mat_test_file()), delim = ',', col_names = T))
        } else if (tools::file_ext(mat_test_file()) %in% c("RData","rdata")) {
          mat_test_raw <- loadRData(url(mat_test_file()))
        } else if (tools::file_ext(mat_test_file()) %in% c("rds","RDS")) {
          mat_test_raw <- readRDS(url(mat_test_file()))
        }
      }
      # Remove Duplicate features
      colnames(mat_test_raw)[1] <- "Feature"
      mat_test_raw_dup <- mat_test_raw[which(mat_test_raw[,1] %in% mat_test_raw[,1][duplicated(mat_test_raw[,1])]),]
      mat_test_raw_nondup <- mat_test_raw[which(!mat_test_raw[,1] %in% mat_test_raw[,1][duplicated(mat_test_raw[,1])]),]
      if (nrow(mat_test_raw_dup) > 0) {
        mat_test_raw_dup <- mat_test_raw_dup %>%
          group_by(Feature) %>%
          summarise_all(max)
      }
      mat_test_raw <- rbind(mat_test_raw_dup,mat_test_raw_nondup)
      if (nrow(mat_test_raw_dup) > 0) {
        message <- paste0(length(unique(mat_test_raw_dup[,1])), " duplicate features found. Features reduced to those with maximum value." )
        FileCheckAlerts_react1(FileCheckAlerts_list,message)
      }
      incProgress(0.5, detail = "Complete!")
    })
    mat_test_raw(mat_test_raw)
  })

  observeEvent(met_test_file(), {
    FileCheckAlerts_list <- FileCheckAlerts_react1()
    FileCheckAlerts_react1(c(FileCheckAlerts_list,
                             paste0("Testing Meta Uploaded: ",input$TestMetFileInput$name)))
    withProgress(message = "Processing", value = 0, {
      incProgress(0.5, detail = "Loading Meta Data")
      if (file.exists(met_test_file())) {
        if (tolower(tools::file_ext(met_test_file())) %in% c("zip","gz")) {
          met_test_raw <- as.data.frame(read_delim(met_test_file(),delim = '\t', col_names = T))
        } else {
          met_test_raw <- as.data.frame(fread(met_test_file()))
        }
      } else {
        if (tools::file_ext(met_test_file()) %in% c("txt","tsv","TXT","TSV")) {
          met_test_raw <- as.data.frame(read_delim(url(met_test_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(met_test_file()) %in% c("zip","gz","ZIP","GZ")) {
          met_test_raw <- as.data.frame(read_delim(getZip(met_test_file()), delim = '\t', col_names = T))
        } else if (tools::file_ext(met_test_file()) %in% c("csv","CSV")) {
          met_test_raw <- as.data.frame(read_delim(url(met_test_file()), delim = ',', col_names = T))
        } else if (tools::file_ext(met_test_file()) %in% c("RData","rdata")) {
          met_test_raw <- loadRData(url(met_test_file()))
        } else if (tools::file_ext(met_test_file()) %in% c("rds","RDS")) {
          mat_test_raw <- readRDS(url(met_test_file()))
        }
      }
      incProgress(0.5, detail = "Complete!")
    })
    met_test(met_test_raw)
  })

  # Format Data ----------------------------------------------------------------
  observe({

    FileCheckAlerts_list <- c()
    req(mat_train_raw())
    req(mat_test_raw())
    mat_train_raw <- mat_train_raw()
    mat_test_raw <- mat_test_raw()
    rownames(mat_train_raw) <- mat_train_raw[,1]
    mat_train_raw <- mat_train_raw[,-1]
    rownames(mat_test_raw) <- mat_test_raw[,1]
    mat_test_raw <- mat_test_raw[,-1]
    featSame <- intersect(rownames(mat_train_raw),rownames(mat_test_raw))
    mat_train_raw <- mat_train_raw[featSame,]
    mat_test_raw <- mat_test_raw[featSame,]
    if (length(featSame) != nrow(mat_train_raw) | length(featSame) != nrow(mat_test_raw)) {
      message <- paste0("Mistmatching features found between training and testing matrix. Reduced to only similar features (N=",length(featSame),")")
      FileCheckAlerts_list <- c(FileCheckAlerts_list,message)
    }
    FileCheckAlerts_list <- c(FileCheckAlerts_list,
                              paste0("Number of samples in training matrix: ",ncol(mat_train_raw)),
                              paste0("Number of samples in testing matrix: ",ncol(mat_test_raw)),
                              paste0("Number of features in training matrix: ",nrow(mat_train_raw)),
                              paste0("Number of features in testing matrix: ",nrow(mat_test_raw)))

    FileCheckAlerts_react2(FileCheckAlerts_list)
    mat_test(mat_test_raw)
    mat_train(mat_train_raw)

  })

  # View Data ------------------------------------------------------------------

  output$rendMatHead <- renderUI({
    req(mat_train())
    if (ncol(mat_train()) > 301) {
      radioButtons("rendMatHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("rendMatHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Training_Matrix_Preview <- renderDataTable({
    req(mat_train())
    req(input$rendMatHead)
    mat <- as.data.frame(mat_train())
    if (input$rendMatHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T))
  })

  output$rendTestMatHead <- renderUI({
    req(mat_test())
    if (ncol(mat_test()) > 301) {
      radioButtons("TestMatHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("TestMatHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Testing_Matrix_Preview <- renderDataTable({
    req(mat_test())
    req(input$TestMatHead)
    mat <- as.data.frame(mat_test())
    if (input$TestMatHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T))
  })

  output$rendTestMetHead <- renderUI({
    req(met_test())
    if (ncol(met_test()) > 301) {
      radioButtons("TestMetHead",NULL, choices = c("View table head","View entire table"), inline = T)
    } else {
      radioButtons("TestMetHead",NULL, choices = c("View table head","View entire table"), inline = T, selected = "View entire table")
    }
  })
  output$Testing_Meta_Preview <- renderDataTable({
    req(met_test())
    req(input$TestMetHead)
    mat <- as.data.frame(met_test())
    if (input$TestMetHead == "View table head") {
      mat <- head(mat,c(100,100))
    }
    datatable(mat,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 10,
                             scrollX = T),
              rownames = F)
  })

  # Prediction -----------------------------------------------------------------

  ## Inputs --------------------------------------------------------------------

  output$rendAddMetaCols <- renderUI({
    req(met_test())
    met_test <- met_test()
    col_choices <- colnames(met_test)[-1]
    selectInput("AddMetaCols","Add feature from meta data:", choices = col_choices, selected = NULL, multiple = T)
  })

  ## Work ----------------------------------------------------------------------
  pred_obj <- reactive({

    req(feature_react)
    req(model_react)
    req(mat_test)
    mat_test <- mat_test()
    met_test <- met_test()
    model_react <- model_react()
    feature <- feature_react()
    #save(list = ls(), file = "featPredenvTest.RData", envir = environment())
    res <- lrFeaturePredict(model = model_react,
                            data_test = mat_test,
                            meta_test = met_test,
                            feature = feature)
    res

  })

  ## Tables --------------------------------------------------------------------

  ModelFittedTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    df <- res$`Model Fitted`
    df <- cbind(SampleName = rownames(df),df)
    df
  })
  output$ModelFittedTab <- renderDataTable({
    req(pred_obj())
    req(ModelFittedTab_react())
    res <- pred_obj()
    df <- ModelFittedTab_react()
    if (input$ModelFittedTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })

  TestGroupCentersTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_test <- met_test()
    df <- res$`Group Center Testing`
    df <- cbind(SampleName = rownames(df),df)
    if (isTruthy(meta_cols) & isTruthy(met_test)) {
      if (any(meta_cols %in% colnames(met_test))) {
        df <- merge(met_test[,c(colnames(met_test)[1],meta_cols)],df, all.y = T)
        df <- df %>% relocate(any_of(meta_cols) , .after = SampleName)
        colnames(df)[1] <- colnames(met_test)[1]
      }
    }
    df
  })
  output$TestGroupCentersTab <- renderDataTable({
    req(pred_obj())
    req(TestGroupCentersTab_react())
    res <- pred_obj()
    df <- TestGroupCentersTab_react()
    if (input$TestGroupCentersTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = paste0("Group",seq(max(res[["clusters"]]))), digits = 5)
  })

  TestPredictionsTab_react <- reactive({
    req(pred_obj())
    res <- pred_obj()
    meta_cols <- input$AddMetaCols
    met_test <- met_test()
    df <- res$`Prediction Testing`
    df <- df[order(df$Prediction),]
    if (isTruthy(meta_cols) & isTruthy(met_test)) {
      if (any(meta_cols %in% colnames(met_test))) {
        df <- merge(met_test[,c(colnames(met_test)[1],meta_cols)],df, all.y = T)
        df <- df %>% relocate(any_of(meta_cols) , .after = Prediction)
        colnames(df)[1] <- colnames(met_test)[1]
      }
    }
    df
  })
  output$TestPredictionsTab <- renderDataTable({
    req(pred_obj())
    req(TestPredictionsTab_react())
    res <- pred_obj()
    df <- TestPredictionsTab_react()
    if (input$TestPredictionsTabHead == "View table head") {
      df <- head(df,c(100,100))
    }
    datatable(df,
              options = list(lengthMenu = c(5,10, 20, 100, 1000),
                             pageLength = 20,
                             scrollX = T),
              rownames = F) %>%
      formatRound(columns = colnames(res$`Model Fitted`), digits = 5)
  })


  # Downloads ------------------------------------------------------------------

  ## Prediction Tab Tables -----------------------------------------------------

  output$dnldModelFittedTab <- downloadHandler(
    filename = function() {
      feature <- feature_react()
      paste0("ModelFitted_", feature,"_",Sys.Date(),".txt")
    },
    content = function(file) {
      df <- ModelFittedTab_react()
      write.table(df,file, sep = '\t', row.names = F)
    }
  )

  output$dnldTestGroupCentersTab <- downloadHandler(
    filename = function() {
      feature <- feature_react()
      paste0("TestingGroupCenters_", feature,"_",Sys.Date(),".txt")
    },
    content = function(file) {
      df <- TestGroupCentersTab_react()
      write.table(df,file, sep = '\t', row.names = F)
    }
  )

  output$dnldTestPredictionsTab <- downloadHandler(
    filename = function() {
      feature <- feature_react()
      paste0("TestingPredictions_", feature,"_",Sys.Date(),".txt")
    },
    content = function(file) {
      df <- TestPredictionsTab_react()
      write.table(df,file, sep = '\t', row.names = F)
    }
  )



}



# Run the application
shinyApp(ui = ui, server = server)
