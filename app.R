# Bilingual VRAG-R application. Scoring is shared with the standalone R API.
library(shiny)
library(bslib)
library(ggplot2)
source("VRAGR.R", local=TRUE)
source("R/workflow.R", local=TRUE)
source("R/i18n.R", local=TRUE)
.app_root <- normalizePath(".",mustWork=TRUE)
options(shiny.maxRequestSize=10*1024^2,shiny.sanitize.errors=TRUE)
manual_link <- function(label="German manual (PDF)",page=NULL) tags$a(label,
  href=paste0(VRAGR_MANUAL_URL,if(!is.null(page)) paste0("#page=",page+2)),target="_blank",rel="noopener noreferrer")
external_link <- function(label,url) tags$a(label,href=url,target="_blank",rel="noopener noreferrer")
notice <- function(...,type="info") div(class=paste("notice",paste0("notice-",type)),...)
intro <- function(kicker,title,text) div(class="section-intro",p(class="eyebrow",kicker),h1(title),p(class="intro-copy",text))
hint <- function(...) p(class="item-hint",...)
csv_note <- function() hint("CSV uses UTF-8, comma separators, decimal points and English column names in both languages. Probabilities are exported between 0 and 1. Invalid dataset rows retain their status and error explanations.")
interpretation_note <- function() notice("These are reference-group rates of violent recidivism, not a certain prediction for an individual. Percentile rank is not a recidivism probability. ",manual_link("Interpretation and limitations",11))
single_group <- function(group) {
  i <- group$item
  controls <- lapply(group$fields,function(field) {
    id <- paste0("single_",field$name)
    label <- tagList(tags$small(class="field-code",field$name)," ",field$en)
    if(field$name %in% vragr_categories || i==12) i18n_select_input(id,label,
      choices=c("Please select"="",choice_labels(field$name),"No information"="__missing__"),selected="")
    else numericInput(id,label,value=NA_real_,min=if(i==7) 1 else 0,step=1)
  })
  extra <- NULL
  if(i %in% c(5,8)) {
    extra <- tagList(i18n_select_input(paste0("single_info_",i),"Information on this item",
      c("Please select"="","Information available"="known","No information"="missing"),selected=""),
      actionButton(paste0("zero_",i),"Set all counts to zero",class="btn-outline-secondary"))
    controls <- conditionalPanel(sprintf("input.single_info_%d === 'known'",i),div(class="form-grid",controls))
  } else if(i %in% c(7,9)) extra <- checkboxInput(paste0("single_missing_",i),"No information",FALSE)
  accordion_panel(paste0("Item ",i," · ",group$en),hint(group$help_en," ",manual_link("Coding guidance",group$page)),
    extra,if(i %in% c(3,10,12)) div(class="form-grid",controls) else controls,value=paste0("item",i))
}
ui <- page_navbar(
  title=div(class="app-brand",span(class="brand-symbol","R"),span("VRAG-R",tags$small("Calculator"))),
  id="main_nav",selected="single",lang="en",fillable=FALSE,window_title="VRAG-R · Calculator",
  theme=bs_theme(version=5,bg="#f5f5f5",fg="#333333",primary="#337ab7",secondary="#60666c",base_font="system-ui"),
  header=tags$head(tags$link(rel="stylesheet",href="app.css"),
    tags$script(id="i18n-catalog",type="application/json",HTML(jsonlite::toJSON(as.list(.translation_catalog),auto_unbox=TRUE))),
    tags$script(src="i18n.js",defer=NA)),
  footer=div(class="app-footer",span(paste("VRAG-R · App",VRAGR_APP_VERSION)),span("Coding: Rettenberger, Gregório Hertz & Eher (2017) · Model: Harris et al. (2015)")),
  nav_panel("Single case",value="single",
    intro("SINGLE CASE","Calculate a VRAG-R result.","Enter the twelve items, explicitly mark unavailable information and click Calculate result. You can then download the result and its inputs as CSV. Consult the manual for case-specific coding decisions."),
    layout_columns(col_widths=c(5,7),
      card(class="input-card",card_header(h2("Case details")),
        hint("Start with empty inputs or load a synthetic example. Missing information is not the same as No or zero."),
        accordion(!!!lapply(.vragr_schema,single_group),open="item1",id="single_items"),
        uiOutput("single_validation"),div(class="action-row",actionButton("single_calculate","Calculate result",class="btn-primary"),
          actionButton("single_example","Load example",class="btn-outline-secondary"),actionButton("single_reset","Reset inputs",class="btn-outline-secondary")),
        uiOutput("example_notice")),
      div(class="result-column",uiOutput("single_results"),interpretation_note(),
        card(card_header(h2("Score and reference distribution")),plotOutput("single_plot",height="300px"),
          hint("The curve shows the published empirical percentile ranks. It does not assume a normal distribution."),
          accordion(open=FALSE,accordion_panel("Download plot",
            i18n_select_input("plot_format","Plot format",c("PNG"="png","JPEG"="jpeg","PDF"="pdf","SVG"="svg","TIFF"="tiff")),
            numericInput("plot_width","Width (inches)",8,min=2,max=30),numericInput("plot_height","Height (inches)",4,min=2,max=30),
            numericInput("plot_dpi","Resolution (DPI)",150,min=72,max=600),uiOutput("plot_download_control"))))))),
  nav_panel("Dataset",value="batch",
    intro("DATASET CALCULATION","Calculate results for a dataset.","Upload a file, map its columns and categories, review the data checks, then calculate and export the results. A downloadable synthetic example contains all 75 input columns."),
    notice(tags$strong("Minimise uploaded data. "),"Only the required fields and an optional pseudonymous case ID are needed. Do not upload names, dates of birth or original case reference numbers."),
    layout_columns(col_widths=c(5,7),
      card(card_header(h2("1 · Choose a file")),fileInput("batch_file","Upload dataset (maximum 10 MiB)",
        accept=c(".xlsx",".csv",".txt",".sav"),buttonLabel="Choose file",placeholder="No file selected"),
        i18n_select_input("batch_format","File format",c("Detect automatically"="auto","Excel (.xlsx)"="xlsx","CSV · Comma-separated"="csv","CSV · Semicolon-separated"="csv2","Text · Tab-separated"="txt","SPSS (.sav)"="sav")),
        numericInput("batch_sheet","Worksheet number (Excel only)",1,min=1,step=1),
        i18n_select_input("batch_decimal","Decimal mark in numeric text",c("Point"=".","Comma"=",")),
        hint("The first row must contain unique column names. Maximum 100,000 data rows. Blank cells mean missing information; an unknown category must be mapped explicitly."),
        downloadButton("example_download","Download example dataset",class="btn-outline-secondary"),uiOutput("import_message")),
      card(card_header(h2("Preview")),hint("The first six rows are shown exactly as imported."),div(class="table-scroll",tableOutput("batch_preview")))),
    card(card_header(h2("2 · Map variables")),uiOutput("column_mapping")),
    card(card_header(h2("3 · Map categories")),hint("Check each suggested category mapping. Stored SPSS values remain the mapping keys; labels can provide suggestions. Unmapped values are invalid, rather than silently scored as No."),uiOutput("category_mapping")),
    card(card_header(h2("4 · Check data and calculate")),uiOutput("batch_validation_summary"),div(class="table-scroll",tableOutput("batch_issues")),actionButton("batch_calculate","Calculate checked data",class="btn-primary")),
    card(card_header(h2("5 · Results and export")),uiOutput("batch_status"),uiOutput("batch_summary"),
      i18n_select_input("batch_outcome","Display distribution",c("VRAG-R score"="Score","Risk category"="Risk_Category")),plotOutput("batch_plot",height="300px"),
      uiOutput("batch_download_control"),csv_note()),interpretation_note()),
  nav_panel("Methods",value="methods",
    intro("METHODS","VRAG-R: instrument and calculation","Read how the items are combined, what the reported measures mean and which limitations apply. The manual provides the full administration and coding rules."),
    layout_columns(col_widths=c(7,5),
      card(card_header(h2("What the VRAG-R measures")),
        p("The VRAG-R combines twelve mainly historical factors to assess violent recidivism. The total score identifies one of nine risk categories. Published rates describe groups with comparable scores; an individual assessment also needs other relevant information."),
        p("Violent recidivism includes new charges for specified violent offences and hands-on sexual offences, and qualifying institutional violence. It is not limited to convictions, and is not the same as general reoffending or sexual recidivism alone."),
        p("The five- and twelve-year rates concern periods with an opportunity to reoffend. They are cumulative reference-group rates, not annual rates or two separate consecutive periods."),
        manual_link("Outcome definition and development sample",7)),
      card(card_header(h2("Measures and missing information")),
        tags$ul(tags$li("Score: sum of the twelve weighted items, from −34 to +46."),
          tags$li("Risk category: 1 to 9, using the published total-score boundaries."),
          tags$li("Percentile: percentage of the reference sample with an equal or lower score. This is not a probability of reoffending."),
          tags$li("At most four whole items may be missing. The total is adjusted using the positive or negative possible scores of the particular missing items."),
          tags$li("Item 3 is prorated when at least one subitem is known, then rounded before coding. Item 12 allows at most one missing PCL-R facet item.")),
        manual_link("Tables and missing-data rules",11))),
    card(card_header(h2("Application and limitations")),
      p("The development sample comprised 1,261 male offenders, largely from Ontario, Canada. Absolute rates may differ between jurisdictions and populations. The cited manual does not establish equivalent calibration for women. Some participants were juveniles at the index offence but almost all were adults when the at-risk period began."),
      p("Professional judgement is needed to establish the index offence and code the items. PCL-R coding requires its own manual and appropriate qualifications. This calculator does not perform a PCL-R assessment."),
      p("The app reports the unadjusted reference rates. It does not adjust for years lived free of violence after release, treatment or other dynamic changes. The manual discusses these matters separately; time in custody is not equivalent to violence-free time in the community."),
      p("For partially unknown indicators in Items 5, 8 and 10, the app conservatively marks the whole item missing. The manual does not give those items a subitem prorating formula. Values cannot simply be replaced with zero."),
      p("Rounding uses the nearest whole number, with exact halves away from zero. The manual specifies rounding but does not explicitly settle midpoint ties. Percentiles for scores 45 and 46 are 100 because the empirical cumulative table already reaches 100 at 44."),
      manual_link("Full application guidance",13)),
    card(card_header(h2("Model and sources")),
      p(manual_link("Rettenberger, Gregório Hertz & Eher (2017)"),". Die deutsche Version des Violence Risk Appraisal Guide-Revised (VRAG-R). BM-Online, volume 8. Kriminologische Zentralstelle."),
      p(external_link("Harris, Rice, Quinsey & Cormier (2015)","https://doi.org/10.1037/14572-000"),". Violent offenders: Appraising and managing risk (3rd ed.). American Psychological Association."),
      p("Scoring tables and coding rules were checked against the publicly available German manual. The full Harris (2015) and Hare (2003) manuals were not available for an independent page-by-page review. Source decisions and regression tests are documented in the repository."),
      p(external_link("Source review and implementation decisions","https://github.com/MertenNeumann/VRAGR_Calculator/blob/main/docs/MODEL_AND_SOURCES.md")))),
  nav_panel("About the app",value="about",
    intro("ABOUT","About this app","Find source code, contact links, issue reporting and information on server-side processing here."),
    layout_columns(col_widths=c(7,5),
      card(card_header(h2("Code and contact")),p("Developed by Merten Neumann."),
        p(external_link("Source code and documentation","https://github.com/MertenNeumann/VRAGR_Calculator")),
        p("Bug reports, feature requests and suggestions for additions are welcome via ",external_link("GitHub Issues","https://github.com/MertenNeumann/VRAGR_Calculator/issues"),". Please do not post personal information or original case data."),
        p(external_link("ResearchGate","https://www.researchgate.net/profile/Merten-Neumann")," · ",external_link("Google Scholar","https://scholar.google.com/citations?user=QjfiK8gAAAAJ&hl=de"))),
      card(card_header(h2("Related calculator")),p("OGRS 3 is available as a separate calculator for general reoffending."),
        p(external_link("OGRS 3 calculator","https://mertenneumann.shinyapps.io/OGRS_Calculator/")),
        p(external_link("OGRS 3 source code","https://github.com/MertenNeumann/OGRS_Calculator")))),
    card(card_header(h2("Data processing")),
      p("Calculations and uploads run on the server. The application code does not create a permanent case database or log case details. Inputs and files remain available for processing during the session."),
      p("Temporary uploads are removed on replacement and recognised session end where the server can execute cleanup. Disconnects, crashes, hosting logs and backups have separate limits; immediate or complete deletion is not guaranteed."),
      p("For shinyapps.io, Posit documents hosting on AWS in us-east-1 (USA). The deployment operator must establish the applicable privacy information, retention and lawful operating conditions before processing real case data."),
      p(external_link("Posit security and compliance documentation","https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html"))),
    card(card_header(h2("AI-assisted development")),
      p("OpenAI Codex assisted with revising the code, interface, documentation and software tests. The app calculates results using the documented R code; no AI model is used to score cases. AI assistance does not replace professional review or case-specific assessment."))),
  nav_spacer(),nav_item(div(class="language-control",i18n_select_input("language","Language / Sprache",c("English"="en","Deutsch"="de"),selected="en",selectize=FALSE))))

server <- function(input,output,session) {
  language <- reactive(if(identical(input$language,"de")) "de" else "en")
  tx <- function(text) translate(text,language())
  render_ui <- function(expr) {
    f <- function() NULL; body(f) <- substitute(expr); environment(f) <- parent.frame()
    renderUI(localize_ui(f(),language()))
  }
  render_inputs <- function(expr) {
    f <- function() NULL; body(f) <- substitute(expr); environment(f) <- parent.frame()
    renderUI(localize_ui(f()))
  }
  single_state <- reactive({
    incomplete <- character()
    values <- setNames(lapply(vragr_fields,function(field) {
      item <- as.integer(strsplit(field,"_",fixed=TRUE)[[1]][2]); value <- input[[paste0("single_",field)]]
      if(item %in% c(5,8)) {
        info <- input[[paste0("single_info_",item)]] %||% ""
        if(info=="missing") return(NA_real_)
        if(info!="known") incomplete <<- c(incomplete,field)
      }
      if(item %in% c(7,9) && isTRUE(input[[paste0("single_missing_",item)]])) return(NA_real_)
      if(identical(value,"__missing__")) return(NA)
      if(is.null(value) || identical(value,"") || (is.numeric(value) && is.na(value))) {
        incomplete <<- c(incomplete,field); return(NA)
      }
      value
    }),vragr_fields)
    list(data=as.data.frame(values,stringsAsFactors=FALSE),incomplete=unique(incomplete))
  })
  single_saved <- reactiveVal(NULL); example_loaded <- reactiveVal(FALSE)
  set_case <- function(data=NULL) {
    for(field in vragr_fields) {
      value <- if(is.null(data)) NA else data[[field]][1]
      if(field %in% vragr_categories || grepl("^Item_12_",field))
        updateSelectInput(session,paste0("single_",field),selected=if(is.null(data)) "" else if(is.na(value)) "__missing__" else as.character(value))
      else updateNumericInput(session,paste0("single_",field),value=if(is.na(value)) NA_real_ else as.numeric(value))
    }
    for(i in c(5,8)) updateSelectInput(session,paste0("single_info_",i),selected=if(is.null(data)) "" else "known")
    for(i in c(7,9)) updateCheckboxInput(session,paste0("single_missing_",i),value=!is.null(data) && is.na(data[[paste0("Item_",i)]][1]))
  }
  observeEvent(input$single_example,{set_case(read_vragr_file(file.path(.app_root,"examples/example-data.csv"),"csv")[1,,drop=FALSE]);example_loaded(TRUE)})
  observeEvent(input$single_reset,{set_case();single_saved(NULL);example_loaded(FALSE)})
  for(item in c(5,8)) local({i <- item; observeEvent(input[[paste0("zero_",i)]],{
    updateSelectInput(session,paste0("single_info_",i),selected="known")
    for(j in seq_len(if(i==5) 27 else 18)) updateNumericInput(session,paste0("single_Item_",i,"_",j),value=0)
  })})
  output$example_notice <- render_ui(if(example_loaded()) hint("Synthetic example loaded. Review the inputs before calculating."))
  single_errors <- reactive({
    state <- single_state()
    if(length(state$incomplete)) return("Complete each input or explicitly mark information as missing.")
    issues <- validate_vragr(state$data)
    if(nrow(issues)) return(paste(issues$field,tx(issues$message),sep=": "))
    result <- vragr(state$data)
    if(result$Status!="calculated") result$Error else character()
  })
  observeEvent(input$single_calculate,{
    state <- single_state()
    if(!length(single_errors())) single_saved(list(data=state$data,scores=vragr(state$data),time=Sys.time()))
  })
  output$single_validation <- render_ui({
    if((input$single_calculate %||% 0)==0) return(NULL)
    errors <- single_errors()
    if(length(errors)) notice(tags$strong("Please check the inputs"),tags$ul(lapply(tx(errors),tags$li)),type="warning")
  })
  single_ready <- reactive(!is.null(single_saved()) && identical(single_saved()$data,single_state()$data) && !length(single_state()$incomplete))
  output$single_results <- render_ui({
    if(!single_ready()) return(card(class="empty-result",card_header(h2("Your result")),
      h3(if(is.null(single_saved())) "No calculation yet." else "Inputs changed."),
      p("Complete the inputs and calculate. Previous results remain hidden until recalculation.")))
    saved <- single_saved(); score <- saved$scores
    tile <- function(label,id,value) div(class="probability",span(label),strong(id=id,value))
    card(class="result-card",card_header(h2("VRAG-R result")),
      div(class="probability-grid",tile("VRAG-R score","score_value",format_number(score$Score,language())),tile("Risk category","category_value",score$Risk_Category)),
      p("Percentile rank: ",strong(id="percentile_value",format_number(score$Percentile,language(),1))," · ","Missing items: ",score$Missing_items),
      h3("Reference-group violent recidivism"),
      div(class="probability-grid",tile("Within five years","prob_five",format_probability(score$Prob_5_years,language())),tile("Within twelve years","prob_twelve",format_probability(score$Prob_12_years,language()))),
      div(class="result-meta",paste(tx("Calculated:"),format_local_time(saved$time,language()))),
      accordion(open=FALSE,accordion_panel("Item scores",div(class="table-scroll",tableOutput("single_item_scores")))),
      downloadButton("single_download","Download result as CSV",class="btn-outline-primary"),csv_note())
  })
  output$single_item_scores <- renderTable({
    req(single_ready());score <- single_saved()$scores
    setNames(data.frame(Item=seq_len(12),Description=vapply(.vragr_schema,function(g) tx(g$en),character(1)),
      Score=format_number(unname(unlist(score[paste0("Item_",1:12,"_score")])),language())),tx(c("Item","Description","Score")))
  },rownames=FALSE)
  output$single_download <- downloadHandler(filename=function(){req(single_ready());vragr_export_filename("Single_case",single_saved()$time)},content=function(file){
    req(single_ready());saved <- single_saved();write.csv(make_vragr_export(saved$data,saved$scores,timestamp=saved$time),file,row.names=FALSE,na="",fileEncoding="UTF-8")
  })
  single_plot <- reactive({
    req(single_ready());score <- single_saved()$scores
    ggplot(data.frame(Score=-34:46,Percentile=vragr_percentiles),aes(Score,Percentile))+
      geom_step(colour="#60666c")+geom_vline(xintercept=score$Score,colour="#b22222",linetype=2)+
      geom_point(data=data.frame(Score=score$Score,Percentile=score$Percentile),colour="#337ab7",size=3)+
      scale_y_continuous(limits=c(0,100))+labs(x=tx("VRAG-R score"),y=tx("Percentile rank"))+theme_minimal(base_size=13)
  })
  output$single_plot <- renderPlot({validate(need(single_ready(),tx("The plot appears after an up-to-date calculation.")));single_plot()},alt=reactive(tx("Empirical percentile ranks and the current VRAG-R score.")))
  output$plot_download_control <- render_ui(if(single_ready()) downloadButton("plot_download","Download plot",class="btn-outline-primary"))
  output$plot_download <- downloadHandler(filename=function(){req(single_ready());format <- input$plot_format %||% "png";req(format %in% c("png","jpeg","pdf","svg","tiff"));sub("csv$",format,vragr_export_filename("Plot",single_saved()$time))},content=function(file){
    req(single_ready());format <- input$plot_format %||% "png";req(format %in% c("png","jpeg","pdf","svg","tiff"))
    width <- input$plot_width; height <- input$plot_height; dpi <- input$plot_dpi
    req(is.finite(width),width>=2,width<=30,is.finite(height),height>=2,height<=30,is.finite(dpi),dpi>=72,dpi<=600)
    ggsave(file,plot=single_plot(),device=if(format=="svg") grDevices::svg else format,width=width,height=height,dpi=dpi,limitsize=TRUE)
  })

  uploaded_paths <- character()
  cleanup <- function(paths) {
    base <- paste0(normalizePath(tempdir()),.Platform$file.sep)
    for(path in paths) {resolved <- normalizePath(path,mustWork=FALSE);if(startsWith(resolved,base) && file.exists(resolved) && !dir.exists(resolved)) unlink(resolved)}
  }
  observeEvent(input$batch_file,{path <- input$batch_file$datapath;cleanup(setdiff(uploaded_paths,path));uploaded_paths <<- union(uploaded_paths,path)},ignoreNULL=FALSE)
  session$onSessionEnded(function() cleanup(uploaded_paths))
  imported <- reactive({
    if(is.null(input$batch_file)) return(list(data=NULL,error=NULL))
    tryCatch(list(data=read_vragr_file(input$batch_file$datapath,input$batch_format %||% "auto",input$batch_file$name,input$batch_sheet %||% 1L),error=NULL),
      error=function(e) list(data=NULL,error="The file could not be read. Check the format, worksheet, unique column names and file size."))
  })
  output$import_message <- render_ui({x <- imported();if(!is.null(x$error)) notice(tx(x$error),type="warning") else if(!is.null(x$data)) hint(paste(nrow(x$data),tx("data rows"),"·",ncol(x$data),tx("columns")))})
  output$batch_preview <- renderTable({req(imported()$data);head(imported()$data,6)},rownames=FALSE)
  output$column_mapping <- render_inputs({
    data <- imported()$data;if(is.null(data)) return(p("Upload a valid file first."))
    choices <- c("Please select"="",setNames(names(data),names(data)))
    controls <- lapply(vragr_fields,function(field) i18n_select_input(paste0("column_",field),tagList(tags$small(class="field-code",field)," ",field_labels()[field]),choices,selected=if(field %in% names(data)) field else ""))
    tagList(accordion(!!!lapply(.vragr_schema,function(group) accordion_panel(paste0("Item ",group$item," · ",group$en),
      div(class="form-grid",controls[match(vapply(group$fields,`[[`,character(1),"name"),vragr_fields)]))),open=FALSE),
      i18n_select_input("batch_id","Optional pseudonymous case ID",c("Use row numbers only"="",setNames(names(data),names(data))),selected=""))
  })
  selected_columns <- reactive(setNames(vapply(vragr_fields,function(field) input[[paste0("column_",field)]] %||% "",character(1)),vragr_fields))
  mapping_values <- reactive({
    data <- imported()$data;columns <- selected_columns()
    if(is.null(data)||any(!columns %in% names(data))||anyDuplicated(columns)) return(NULL)
    setNames(lapply(vragr_categories,function(field) observed_values(data[[columns[[field]]]])),vragr_categories)
  })
  output$category_mapping <- render_inputs({
    values <- mapping_values();if(is.null(values)) return(p("Map all 75 inputs to distinct existing columns."))
    if(any(lengths(values)>100L)) return(notice("More than 100 distinct categories: check the column mapping.",type="warning"))
    raw <- imported()$data;columns <- selected_columns()
    controls <- lapply(vragr_categories,function(field) div(h3(field_labels()[field]),div(class="mapping-grid",lapply(values[[field]],function(value){
      column <- columns[[field]];default <- default_category(value,field,attr(raw[[column]],"labels"))
      i18n_select_input(mapping_input_id(field,column,value),tagList(localize_ui("Value:")," ",span(value,`data-i18n-raw`="true")),
        c("Not mapped"="",choice_labels(field),"No information"="__missing__"),selected=default)
    }))))
    accordion(!!!lapply(.vragr_schema,function(g){fields <- intersect(vapply(g$fields,`[[`,character(1),"name"),vragr_categories);if(!length(fields)) return(NULL)
      accordion_panel(paste0("Item ",g$item," · ",g$en),controls[match(fields,vragr_categories)])}),open=FALSE)
  })
  current_spec <- reactive({
    values <- mapping_values();columns <- selected_columns()
    maps <- setNames(lapply(vragr_categories,function(field){
      if(is.null(values)||length(values[[field]])>100L) return(character())
      setNames(vapply(values[[field]],function(value) input[[mapping_input_id(field,columns[[field]],value)]] %||% "",character(1)),values[[field]])
    }),vragr_categories)
    list(file=if(is.null(input$batch_file)) NULL else as.list(input$batch_file),format=input$batch_format %||% "auto",
      sheet=input$batch_sheet %||% 1L,decimal=input$batch_decimal %||% ".",columns=columns,maps=maps,id=input$batch_id %||% "")
  })
  batch_prepared <- reactive({
    raw <- imported()$data;spec <- current_spec();values <- mapping_values()
    if(is.null(raw)) return(list(data=NULL,error="Please upload a valid file."))
    if(is.null(values)||any(lengths(values)>100L)) return(list(data=NULL,error="Please check the column and category mappings."))
    tryCatch({
      data <- prepare_vragr_data(raw,spec$columns,spec$maps,spec$decimal)
      if(nzchar(spec$id)) {if(!spec$id %in% names(raw)) stop("Please select an existing ID column.");ids <- raw[[spec$id]]
        if(anyNA(ids)||any(!nzchar(trimws(as.character(ids))))||anyDuplicated(ids)) stop("Case IDs must be present and unique in every row.")}
      list(data=data,scores=vragr(data,strict=FALSE),issues=validate_vragr(data),error=NULL)
    },error=function(e) list(data=NULL,error=conditionMessage(e)))
  })
  output$batch_validation_summary <- render_ui({
    x <- batch_prepared();if(is.null(x$data)) return(notice(tx(x$error)))
    good <- sum(x$scores$Status=="calculated");notice(paste(good,tx("scoreable rows"),"·",nrow(x$data)-good,tx("unscoreable rows")),
      "Invalid rows remain in their original order with empty results and error explanations.",type=if(good<nrow(x$data)) "warning" else "success")
  })
  output$batch_issues <- renderTable({
    x <- batch_prepared();req(x$data);bad <- which(x$scores$Status!="calculated");if(!length(bad)) return(NULL)
    rows <- head(bad,30);messages <- vapply(rows,function(i){issues <- x$issues[x$issues$row==i,,drop=FALSE]
      if(nrow(issues)) paste(paste(issues$field,tx(issues$message),sep=": "),collapse=" | ") else tx(x$scores$Error[i])},character(1))
    setNames(data.frame(Row=rows,Issue=messages),tx(c("Row","Issue")))
  },rownames=FALSE)
  batch_saved <- reactiveVal(NULL)
  observeEvent(input$batch_calculate,{
    x <- batch_prepared();if(is.null(x$data)) return(showNotification(tx(x$error),type="warning"))
    if(!any(x$scores$Status=="calculated")) return(showNotification(tx("No rows can be scored. Please review the error explanations."),type="warning"))
    spec <- current_spec();time <- Sys.time();raw <- imported()$data
    export <- make_vragr_export(x$data,x$scores,ids=if(nzchar(spec$id)) raw[[spec$id]] else NULL,timestamp=time,filename=input$batch_file$name)
    batch_saved(list(spec=spec,scores=x$scores,time=time,export=export))
  })
  batch_ready <- reactive(same_batch_spec(if(is.null(batch_saved())) NULL else batch_saved()$spec,current_spec()))
  output$batch_status <- render_ui({
    if(is.null(batch_saved())) return(notice("No calculation yet. Check the file and mappings, then calculate."))
    if(!batch_ready()) return(notice("Results out of date. Recalculate before downloading.",type="warning"))
    notice(paste(tx("Calculated:"),batch_saved()$spec$file$name,"·",format_local_time(batch_saved()$time,language())),type="success")
  })
  output$batch_summary <- render_ui({req(batch_ready());div(class="table-scroll",tableOutput("batch_result_preview"))})
  output$batch_result_preview <- renderTable({
    req(batch_ready());x <- head(batch_saved()$scores,6)
    setNames(data.frame(Row=seq_len(nrow(x)),Status=tx(ifelse(x$Status=="calculated","Calculated","Not calculable")),Score=format_number(x$Score,language()),
      Category=format_number(x$Risk_Category,language()),Five_years=format_probability(x$Prob_5_years,language()),Twelve_years=format_probability(x$Prob_12_years,language())),tx(c("Row","Status","Score","Risk category","Five years","Twelve years")))
  },rownames=FALSE)
  output$batch_download_control <- render_ui(if(batch_ready()) downloadButton("batch_download","Download results",class="btn-primary") else tags$button("Download available after an up-to-date calculation",class="btn btn-secondary",disabled="disabled"))
  output$batch_download <- downloadHandler(filename=function(){req(batch_ready());vragr_export_filename("Results",batch_saved()$time)},content=function(file){req(batch_ready());write.csv(batch_saved()$export,file,row.names=FALSE,na="",fileEncoding="UTF-8")})
  output$example_download <- downloadHandler(filename=function() vragr_export_filename("Example_data",Sys.time()),content=function(file) file.copy(file.path(.app_root,"examples/example-data.csv"),file,overwrite=TRUE))
  output$batch_plot <- renderPlot({
    validate(need(batch_ready(),tx("The plot appears after an up-to-date calculation.")))
    key <- input$batch_outcome %||% "Score";req(key %in% c("Score","Risk_Category"))
    scores <- batch_saved()$scores;values <- scores[[key]][scores$Status=="calculated"]
    ggplot(data.frame(Value=values),aes(Value))+geom_histogram(binwidth=1,boundary=.5,fill="#d3d3d3",colour="#60666c")+
      scale_x_continuous(breaks=if(key=="Risk_Category") 1:9 else waiver())+
      labs(x=tx(if(key=="Score") "VRAG-R score" else "Risk category"),y=tx("Number of cases"))+theme_minimal(base_size=13)
  },alt=reactive(tx("Discrete distribution of calculated VRAG-R scores or risk categories.")))
}
shinyApp(ui=localize_ui(ui),server=server)
