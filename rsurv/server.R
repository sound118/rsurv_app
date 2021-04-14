packages <- c("shiny", "thematic", "bslib", "shinyjs", "shinyBS", "shinycssloaders", "shinyscreenshot", "htmltools", "DT", "plotly", "survminer", "survival", "gt")
lapply(packages,library,character.only = TRUE)

server <- function(input, output,session) {
  observeEvent(input$reset_button, {js$reset()})
  
  data_input <- reactive({
    if(input$file == 'example'){
      data <- colon
    }
    else if(input$file == 'load_your_own'){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { data = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { data = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    dataset <- data.frame(data)
    return(as.data.frame(dataset))
  }) 
  
  output$downloadEx <- downloadHandler(
    
    filename <- function() {
      paste('Example ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  # Displaying Data  
  # output$data_display <- DT::renderDataTable({
  #   data_input()
  # })
  
  output$tb <- renderUI({
    if(is.null(data_input()))
      h5("Powered by", tags$img(src='R_logo.png', height=200, width=200), tags$img(src='Shiny1.png', heigth=210, width=210))
    else
      tabsetPanel(
        tabPanel("Data Table", DT::datatable(data_input(), fillContainer = TRUE, style = "bootstrap", rownames = FALSE))
      )
  })
  
  #Survival model variable selection
  output$pop_selection <- renderUI({
    if (is.null(data_input())) return(NULL)
    else {
      data_in <- data_input()
      tagList(
        selectInput('time_var', "Select Time Variable", choices = colnames(data_in), selected = 'time'),
        br(),
        selectInput('event_var', "Select Event Variable", choices = colnames(data_in), selected = 'status'),
        br(),
        selectInput('grp_var', "Select Group Variable", choices = colnames(data_in), selected = 'rx')
      )
    }
  })
  
  v <- reactiveValues(km_static = NULL,
                      km_interactive = NULL)
  
  
  # K-M curve plot
  observeEvent(input$get_kmcurve, {
    # if (input$get_kmcurve == 0) {return()}
    # if (is.null(input$plt_legend)) {return(NULL)}
        
        req(input$get_kmcurve)
        req(input$plt_legend)
   
    # surv_fit(as.formula(paste('Surv(time = time, event = status) ~ ', input$grp_var)), data = data_input())
    model <- eval(parse(text = paste0("survfit(Surv(", input$time_var, ", " , input$event_var, ")"," ~ ", input$grp_var, ", data = data_input())")))
  
      
      fit <- model
      plt <- ggsurvplot(
        fit, 
        data = data_input(), 
        title = "Overall Survival",
        # subtitle = "Stratified by Treatment Group",
        subtitle = input$subtitle,
        font.title = c(22, "bold", "black"),
        ####### Confidence Intervals ########
        conf.int = input$ci_flg, # To Remove conf intervals use "FALSE"
        surv.median.line = input$med_line, # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
        size = 1,                 # change line size
        # palette = c("red", "blue", "green"),# custom color palettes
        palette = unlist(strsplit(input$color, ",")),
        pval = TRUE,              # Add p-value
        pval.size = 5,
        pval.coord = c(0.75*max(unique(colon$time)),0.8),
        risk.table = TRUE,        # Add risk table
        risk.table.col = "strata",# Risk table color by groups
        # legend.labs = c("Ref", "Drug1", "Drug2"),    # Change legend labels
        legend.labs = unlist(strsplit(input$plt_legend, ",")),
        risk.table.height = 0.35, # Useful to change when you have multiple groups
        ggtheme = theme(plot.background = element_rect(fill = "gray", colour = "gray"),
                        panel.background = element_rect(fill = "gray", colour = "gray"),
                        legend.background = element_rect(fill = "gray", colour = "gray")) + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
          theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"))    # Change ggplot2 theme
          # theme(plot.background = element_rect(fill = "gray", colour = "gray"))+
          # theme(panel.background = element_rect(fill = "gray", colour = "gray"))+
          # theme(legend.background = element_rect(fill = "gray", colour = "gray"))
      )
      
      v$km_static <- plt
      v$km_interactive <- plotly::ggplotly(plt[[1]])
  })
  
  output$km_static <- renderPlot({
    if (is.null(v$km_static)) return()
    v$km_static
    # km_plot()
  })
    
  output$km_interactive <- renderPlotly({
    if (is.null(v$km_interactive)) return()
    # p1 <- km_plot()
    v$km_interactive 
  })
  
  output$km_curve <- renderUI({
    if (input$plot_mode == "Static") {
      plotOutput("km_static", width = "100%", height = "580px") %>% withSpinner()
    }
    else if (input$plot_mode == "Interactive") {
      plotlyOutput("km_interactive", width = "100%", height = "580px") %>% withSpinner()
    }
  })
  
  # output$download_km <- downloadHandler(
  #   filename = function() {paste('KM Curve-', Sys.Date(), '.png',sep='')},
  #   content = function(file) {
  #     ggsave(file, plot = km_plot()$plot, width = 20, height = 10,device = "png")
  #   }
  # )
  
  observeEvent(input$screenshot_km, {
    screenshot(filename = paste('KM Curve-', Sys.Date(), sep=''), id = "km_static")
  })
  
  
  #Cox model variable selection
  output$pop_vars_l <- renderUI({
    if (is.null(data_input())) return(NULL)
    else {
      data_in <- data_input()
      tagList(
        selectInput('time_var1', "Select Time Variable", choices = colnames(data_in), selected = 'time'),
        br(),
        selectInput('event_var1', "Select Event Variable", choices = colnames(data_in), selected = 'status')
      )
    }
  })
  
  output$pop_vars_r <- renderUI({
    if (is.null(data_input())) {return(NULL)}
    else {
      return(selectInput("allvars", "Select variables for the model", choices = colnames(data_input()), multiple = TRUE))
    }
  })
    
  #For variable selection
  observe({
    if (input$sel == 0)
      return(NULL)
    isolate({
      
      output$selectInputs <- renderUI({
        s <- ""
        for(i in 1:input$sel) {
          s <- paste(s, selectInput(paste("var", i, sep = ""), paste("Variable ", i, sep = ""), choices = colnames(data_input()) ))
        }
        HTML(s)
      })
      outputOptions(output, 'selectInputs', suspendWhenHidden=FALSE)
    })
  })
  
  #For variable label
  observe({
    
    if (input$obs == 0) 
      return(NULL)
    isolate({
      
      output$selectInputs1 <- renderUI({
        w <- ""
        for(i in 1:input$obs) {
          w <- paste(w, textInput(paste("a", i, sep = ""), paste("Label ", i, sep = ""), value = input[[sprintf("a%d",i)]]))
        }
        HTML(w)
      })
      outputOptions(output, 'selectInputs1', suspendWhenHidden=FALSE)
    })
  })
  
  
  forest_model <- eventReactive(input$get_forestplot, {
    data_in <- data_input()
    # new_data <<- within(data_in, {
    #   # sex <- factor(sex, labels = c("female", "male"))
    #   # differ <- factor(differ, labels = c("well", "moderate", "poor"))
    #   # extent <- factor(extent, labels = c("submuc.", "muscle", "serosa", "contig."))
    #   
    #   for (i in seq(input$sel)) {
    #     input[[paste("var", i, sep = "")]] <- factor(input[[paste("var", i, sep = "")]], labels = unlist(strsplit(input[[paste("a", i, sep = "")]], ",")))
    #   }
    # })
    for (i in seq(input$sel)) {
      data_in[[input[[paste("var", i, sep = "")]]]] <- factor(data_in[[input[[paste("var", i, sep = "")]]]], labels = unlist(strsplit(input[[paste("a", i, sep = "")]], ",")))
    }
    
    new_data <<- data_in
    
   #bimodel <- coxph(Surv(time, status) ~ sex + rx + adhere + differ + extent + node4, data=new_data )
    bigmodel <- eval(parse(text = paste0("coxph(Surv(", input$time_var1, ", ", input$event_var1, ")"," ~ ", paste(input$allvars, collapse = " + "), ", data = new_data)" )))
    bigmodel 
  }
  )
  
  forest_plot <- reactive({
    ggforest(forest_model(), fontsize = 1)
    
  })
  
  output$forest_plot1 <- renderPlot({
    forest_plot()
  })
  
  output$forestplot <- renderUI({
    plotOutput("forest_plot1", width = "100%", height = "580px") %>% withSpinner()
    
  })
  
  output$download_fp <- downloadHandler(
    filename = function() {paste('Forest Plot-', Sys.Date(), '.png',sep='')},
    content = function(file) {
      ggsave(file, plot = forest_plot(), width = 20, height = 10,device = "png")
    }
  )
  
  ###Create gt table
  model_table <- reactive({
    if (input$get_forestplot == 0) return(NULL)
    if (is.null(data_input())) return(NULL)

    covariates <- input$allvars
    univ_formulas <- sapply(covariates,
                            # function(x) as.formula(paste('Surv(input$time_var1, input$event_var1)~', x))
                            function(x) eval(parse(text = paste0("Surv(", input$time_var1, ",", input$event_var1, ")", "~", x )))
                            )
    
    univ_models <- lapply( univ_formulas, function(x){coxph(x, data = data_input())})
    # Extract data 
    univ_results <- lapply(univ_models,
                           function(x){ 
                             x <- summary(x)
                             p.value<-format(round(x$wald["pvalue"], 3), nsmall = 3)
                             wald.test<-signif(x$wald["test"], digits=2)
                             beta<-signif(x$coef[1], digits=2);#coeficient beta
                             HR <- format(signif(x$coef[2], digits=2), nsmall = 2);#exp(beta)
                             HR.confint.lower <- format(signif(x$conf.int[,"lower .95"], 2), nsmall = 2)
                             HR.confint.upper <- format(signif(x$conf.int[,"upper .95"],2), nsmall =2)
                             HR <- paste0(HR, " (", 
                                          HR.confint.lower, "-", HR.confint.upper, ")")
                             res<-c(beta, HR, wald.test, p.value)
                             names(res)<-c("beta", "HR (95% CI)", "wald.test", 
                                           "P value")
                             return(res)
                
                           })
    res <- t(as.data.frame(univ_results, check.names = FALSE))
    res_uni <- as.data.frame(res[,c(2,4)])
    
    
    #### Multivariate analysis
    res.cox <- forest_model()
    sum.cox <- summary(res.cox)
    
    df_l <- round(as.data.frame(sum.cox$conf.int)[, c(1,3,4)], 2)
    
    p.value <- round(as.data.frame(sum.cox$coefficients)[,5], 3)
    
    df1 <-data.frame(paste0(format(df_l$`exp(coef)`, nsmall = 2), " (", format(df_l$`lower .95`, nsmall = 2), "-", format(df_l$`upper .95`, nsmall = 2), ")"))
    
    res_multi <- cbind(df1, p.value)
    names(res_multi) <- c("HR (95% CI) ", "P value ")
    
    cox_res <- cbind(res_uni, res_multi)
    row.names(cox_res) <- unlist(strsplit(input$tbl_labels, ","))
    cox_res$variable <- rownames(cox_res)
    
    ###generate gt table
    gt_tbl <- 
      cox_res%>%
      gt(rowname_col = "variable") %>%
      tab_stubhead(label = "Variable") %>%
      tab_style(style = cell_text(v_align = 'middle'), locations = cells_stubhead()) %>%
      tab_header(
        # title = html("<H5 align='left'>Cox regression analysis</H5>")
        title = input$tbl_title
      ) %>%
      tab_spanner(
        label = "Univariate analysis",
        columns = c("HR (95% CI)", "P value")
      ) %>%
      tab_spanner(
        label = "Multivariate analysis",
        columns = c("HR (95% CI) ", "P value ")
      ) %>%
      tab_footnote(
        footnote = input$tbl_footer,
        locations = cells_title("title")
      ) %>%
      tab_options(row.striping.include_table_body =TRUE,
                  heading.align = "left",
                  heading.border.bottom.width = pct(10),
                  heading.border.bottom.color = "black",
                  row.striping.background_color = "#002B36",
                  table.border.top.color = "white",
                  table.background.color = "#002B36",
                  table_body.hlines.color = "transparent",
                  table_body.border.top.width = pct(10),
                  table_body.border.top.color = "black",
                  table_body.border.bottom.width = pct(10),
                  table_body.border.bottom.color = "black",
                  footnotes.border.bottom.style = "hidden",
                  stub.border.color = "transparent",
                  stub.border.width = pct(0),
                  table.font.size = pct(100),
                  column_labels.border.bottom.width = pct(1),
                  column_labels.border.bottom.color = "black",
                  data_row.padding = px(3)) %>%
      opt_row_striping()
    
    gtsave(gt_tbl, paste("CoxTable-", Sys.Date(), ".html", sep = ""), path = "/root/rsurv/www")
    return(gt_tbl)
  })
  
  output$cox_table <- render_gt(expr = model_table(), height = px(270), width = px(600), align = "left")
  
  output$download_tbl <- downloadHandler(
    filename <- function() {
      paste("CoxTable-", Sys.Date(), ".html", sep = "")
    },
    
    content <- function(file) {
      file.copy(paste("www/", "CoxTable-", Sys.Date(), ".html", sep = ""), file, overwrite = TRUE)
    }
  )
  
  
  output$caption <- renderText({paste("This APP is developed by Jason Yang. It is used to conduct basic clinical trial survival analysis. For any technical issue, please contact jian.yang@genomicarebio.com, thank you!")})

  
  
}
