
packages <- c("shiny", "thematic", "bslib", "shinyjs", "shinyBS", "shinycssloaders", "shinyscreenshot", "htmltools", "DT", "plotly", "survminer", "survival", "gt")
lapply(packages,library,character.only = TRUE)

options(shiny.maxRequestSize= 500*1024^2)
options(shiny.sanitize.errors = TRUE)
options(warn=-1)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" 

thematic_shiny(font = "auto")
theme_set(theme_survminer())
thematic::thematic_on(bg = "#222222", fg = "#222222", accent = "#0CE3AC", font = "Oxanium")

ui <-  fluidPage(
  theme = bslib::bs_theme(version = 3,
    # bg = "#002B36", fg = "white", accent = "#0CE3AC", font = "Oxanium",
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
    # bslib also makes it easy to import CSS fonts
    # base_font = bslib::font_google("Pacifico")
  ),
  
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = "reset"),
  # tags$head(tags$link(rel="stylesheet", type="text/css",href="styles.css")),
  list(tags$head(HTML('<link rel="icon", href="gc_logo.png", type="image/png" />'))),

  # theme = shinytheme("darkly"),
  div(img(src="gc_logo.png", height=25, width=200, style = "float:right; padding-right:25px"),
      titlePanel(
        title="Survival Analysis Application", windowTitle="Genomicare Survival Analysis Application"
      )
  ),
  
  navbarPage(
    "RSurv",
    # tags$head(tags$link(rel="stylesheet", type="text/css",href="styles.css")),
    selected = "Input Data Display",
    # theme = shinytheme("darkly"),
    tabPanel("Input Data Display",
             sidebarPanel(
               # This is intentionally an empty object.
               h4(strong("Input Data File")),
               
               selectInput("file",label= "Select example data file or upload your own data", 
                           choices = c("Use example data" = "example", "Upload your data" = "load_your_own"), selected = "load_your_own"),
               
               conditionalPanel("input.file == 'load_your_own'",
                                fileInput('file1', 'Upload your data', accept=c('.xlsx', '.csv'))),    
               
               conditionalPanel("input.file == 'example'",
                                downloadButton('downloadEx', 'Download example data')),
               
               helpText("Uploaded data file must be in either .xlsx or .csv formt, maximum file size should be less than 500 MB."),
               hr(),
               tags$h4("Restart Application"),
               
               actionButton("reset_button", "Reset")
               ),
            
             # Show a table summarizing the values entered
             mainPanel(
               uiOutput("tb")
             )
    ),
    
    tabPanel("Kaplan Meier Curve",
               sidebarLayout(
                 sidebarPanel(
                   titlePanel('Model Parameter'),
                   wellPanel(
                     uiOutput("pop_selection"),
                     actionButton("get_kmcurve", "Generate KM Curve")
                   )
                 ),
                 mainPanel(
                   wellPanel(
                     titlePanel('Chart Parameter'),
                     fluidRow(
                       column(width = 4, textAreaInput("subtitle", "Chart Subtitle", placeholder = "Enter chart substitle")),
                       column(width = 4, textAreaInput("plt_legend", "Chart Legend", placeholder = "Enter strata values separated by comma")),
                       column(width = 4, textAreaInput("color", "Curve Color", placeholder = "Enter curve colors separated by comma"))
                     ),
                     fluidRow(
                       column(width = 4, style = "text-align: center; margin-top : 20px;", checkboxInput("ci_flg", "Confidence interval on plot", value = TRUE)),
                       column(width = 4, selectInput("med_line", "Median line on plot", choices = c("None"= "none", "Horizontal and vertical"= "hv", "Horizontal only"="h", "Vertical only"="v"), selected = "hv")),
                       column(width = 4, radioButtons("plot_mode", "KM Curve Mode", choices = c("Interactive", "Static"), selected = "Static"))
                     ),
                   
                     uiOutput("km_curve"),
                     br(),
                     # downloadButton(
                     #    "download_km", label = "Download the Chart"
                     # )
                   ),
                   actionButton("screenshot_km", "Download the Chart", icon = icon('download'))
                 )
             )
             ),
    
    tabPanel("Cox PH Model Analysis",
             sidebarLayout(
               sidebarPanel(
                 titlePanel('Parameter Selection'),
                 wellPanel(
                   uiOutput("pop_vars_l"),
                   br(),
                   uiOutput("pop_vars_r"),
                   
                   br(),
                  
                   actionButton("get_forestplot", "Generate Output"),
                   bsTooltip("get_forestplot", "Click this button only after the information for both table and plot components are filled", placement = "bottom", trigger = "hover"),
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table output",
                     wellPanel(
                       titlePanel('Table Components'),
                       fluidRow(
                          column(width = 4, textAreaInput('tbl_title', 'Table title', placeholder = "Enter table title")),
                          column(width = 4, textAreaInput('tbl_labels', 'Table row labels', placeholder = "Enter table row labels separated by comma")),
                          column(width = 4, textAreaInput('tbl_footer', 'Table footnote', placeholder = "Enter table footnote"))
                       ),
                       gt_output("cox_table"),
                       br(),
                       downloadButton("download_tbl", "Download Table")
                       
                     )
                   ),
                   tabPanel("Forest plot",
                      wellPanel(
                        titlePanel('Plot Components'),
                        fluidRow(
                          column(width = 4, actionButton("sel", "Add Variable Dropdown Menu")),
                          column(width = 4,actionButton("obs","Add Label Box"))
                          ),
                        fluidRow(
                          column(width = 4, htmlOutput("selectInputs")),
                          column(width = 4, htmlOutput("selectInputs1"))
                        ),
                        uiOutput("forestplot"),
                        br(),
                        downloadButton(
                          "download_fp", label = "Download the Plot"
                        )
                      )
                   )
                 )
               )
             )
    ),
    
    tabPanel("About the Application", textOutput("caption"))
  )
)

