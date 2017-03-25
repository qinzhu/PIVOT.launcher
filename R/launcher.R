#' PIVOT laucher
#'
#' Serves as a gateway for all analysis modules and allow users to monitor the R session state and clean when needed.
#' @examples
#' pivot()
#' @import shiny miniUI rstudioapi PIVOT.data DT
#' @export
pivot <- function(args = "launcher") {

    ui <- miniUI::miniPage(
        gadgetTitleBar("PIVOT Launcher",
                       left = NULL,
                       right = NULL),
        miniTabstripPanel(
            id = "tabstrip",
            miniTabPanel("Main", icon = icon("flask"),
                         miniContentPanel(
                             fillCol(flex = c(NA,NA, 1),
                                     dllUI("module1"),
                                     uiOutput("pkg_info_ui"),
                                     DT::dataTableOutput("workflow_tbl")
                             )
                         )
            ),
            miniTabPanel("Info", icon = icon("map-pin"),
                         miniContentPanel(
                             fillCol(flex = c(NA,1),
                                     dllUI("module2"),
                                     uiOutput("data_status")
                             )
                         )
            ),
            between = list(
                #tags$div(id = "max_dll_monitor", class = "shiny-html-output", style = "background-color: #f2f2f2; text-align: center;"),
                miniButtonBlock(
                    actionButton("launch_module", label = "Launch Module", class = "btn-primary", onclick = "setTimeout(function(){window.close();}, 100); "),
                    #actionButton("launch_module", "Launch Module", class = "btn-primary"),
                    actionButton("clean_session", label = "Clean DLLs", class = "btn-success", onclick = "setTimeout(function(){window.close();}, 100);) ")
                )
            )
        )
    )

    server <- function(input, output, session) {
        # For storing which rows have been excluded
        callModule(dllText,"module1")
        callModule(dllText,"module2")

        # Credits to Yihui Xie, https://github.com/rstudio/DT/issues/93
        shinyInput <- function(FUN, len, id, ...) {
            inputs <- character(len)
            for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(id, i), ...))
            }
            inputs
        }

        shinyInputVal <- function(FUN, len, id, cVal, ...)  {
            inputs <- character(len)
            for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(id, i), value = cVal[i], ...))
            }
            inputs
        }

        shinyValue = function(id, len) {
            unlist(lapply(seq_len(len), function(i) {
                value = input[[paste0(id, i)]]
                if (is.null(value)) NA else value
            }))
        }


        # Handle the Done button being pressed.
        observeEvent(input$done, {
            stopApp()
        })

        # Handle launch_module button

        observe({
            if(not_pressed(input$launch_module)) return()
            # quit R, unless you are running an interactive session

            if(interactive()) {
                isolate({
                    if(input$tabstrip == "Main") {
                        if("monocle" %in% modules()) {
                            require(monocle)
                        }
                        Sys.sleep(1)
                        rstudioapi::sendToConsole("pivot_main()", execute = T)
                        stopApp("All change saved, returning to launcher...")
                    }
                })
            } else {
                stopApp()
                q("no")
            }
        })

        observe({
            if(not_pressed(input$clean_session)) return()
            # quit R, unless you are running an interactive session

            if(interactive()) {
                isolate({
                    stopApp("Performing DLL clean up... (Please use command pivot('clean') if new window does not show up.)")
                    sendToConsole("pivot('clean')", execute = T)
                })
            } else {
                stopApp()
                q("no")
            }
        })

        output$data_status <- renderUI({
            if(!exists("r_data") || is.null(r_data$glb.raw)) {
                return(list(
                    tags$b("Data Input Instructions:"),
                    tags$p("1. Select which modules you want to use for the analysis."),
                    tags$p("2. Press launch button.")
                ))
            } else {
                list(
                    uiOutput("input_file_info"),
                    DT::dataTableOutput("data_info")
                )
            }
        })

        ##### Data Info boxes

        output$data_info <- DT::renderDataTable({
            if(is.null(r_data$sample_meta)) return()
            DT::datatable(r_data$sample_meta, style = "bootstrap",
                          options = list(dom = 'tipr'))
        })

        output$input_file_info <- renderUI({
            if(is.null(r_data$file_path) || is.null(r_data$input_type)) {
                file_path = ""
            } else if(r_data$input_type == "single"){
                file_path = paste("Input file:", r_data$file_path$name)
            }
            else if(r_data$input_type == "dir") {
                file_path = paste("Input Directory:", r_data$file_path[[length(r_data$file_path)]])
            }

            num_features = paste("Number of selected features:", nrow(r_data$raw))
            num_samples = paste("Number of selected samples:", ncol(r_data$raw))
            #num_cats = paste("Number of design categories:", ncol(r_data$glb.meta) - 1)
            return(list(
                tags$li(file_path),
                tags$li(num_features),
                tags$li(num_samples)
                #tags$li(num_cats)
            ))
        })






        info_tbl <- reactive({
            packList <-rownames(installed.packages())

            main_info <- list(
                Module = "PIVOT.analysis",
                Description = "Main analysis module of PIVOT, including data input and management, DE, clustering, heatmap, dimension reduction, etc. "
                #Citation = "Michael I Love, Wolfgang Huber and Simon Anders (2014): Moderated estimation of fold change and dispersion for RNA-Seq data with DESeq2. Genome Biology"
            )

            deseq_info <- list(
                Module = "DESeq2",
                Description = "Differential expression based on a model using the negative binomial distribution by Michael I Love, Wolfgang Huber and Simon Anders (2014)."
                #Citation = "Michael I Love, Wolfgang Huber and Simon Anders (2014): Moderated estimation of fold change and dispersion for RNA-Seq data with DESeq2. Genome Biology"
            )

            scde_info <- list(
                Module = "scde",
                Description = "Single cell differential expression analysis by Kharchenko P and Fan J (2016)."
                #Citation = "Kharchenko P and Fan J (2016). scde: Single Cell Differential Expression. R package version 2.2.0, http://pklab.med.harvard.edu/scde."
            )

            monocle_info <- list(
                Module = "monocle",
                Description = "Clustering, differential expression, and trajectory analysis for single- cell RNA-Seq by Cole Trapnell and Davide Cacchiarelli et al (2014)."
                #Citation = "Cole Trapnell and Davide Cacchiarelli et al (2014): The dynamics and regulators of cell fate decisions are revealed by pseudo-temporal ordering of single cells. Nature Biotechnology."
            )

            network_info <- list(
                Module = "PIVOT.network",
                Description = "Visualization of interactome/regulome network and transdifferentiation factor prediction with Mogrify-like method."
                #Citation = "Rackham, O. J., Firas, J., Fang, H., Oates, M. E., Holmes, M. L., Knaupp, A. S., ... & Petretto, E. (2016). A predictive computational framework for direct reprogramming between human cell types. Nature genetics."
            )

            toolkit_info <- list(
                Module = "PIVOT.toolkit",
                Description = "A set of tools for drawing Venn Diagram, converting gene name to gene id, etc."
            )

            info_tbl <- as.data.frame(rbind(
                main_info, deseq_info, scde_info, monocle_info, network_info, toolkit_info
            ))

            cVal = c(T,T,F,F,F,T)

            info_tbl$Pick <- shinyInputVal(checkboxInput, nrow(info_tbl), 'pick_', cVal = cVal, label = NULL, width = '50px')

            info_tbl$Pick[which(!info_tbl$Module %in% c(packList,"PIVOT.network", "PIVOT.toolkit"))] <- ""

            info_tbl$Status <- shinyInput(actionButton, nrow(info_tbl), 'button_', label = "Install", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')

            info_tbl$Status[which(info_tbl$Module %in% packList)] <- "Installed"
            # Have not separated toolkit from analysis
            info_tbl$Status[which(info_tbl$Module =="PIVOT.toolkit")] <- "Installed"

            # Rearranging the columns
            info_tbl <- info_tbl[,c("Pick", "Module", "Status", "Description")]
            return(info_tbl)
        })


        output$workflow_tbl = DT::renderDataTable(
            info_tbl(), rownames = F, escape = FALSE, selection = 'none', options = list(
                dom = 't', paging = FALSE, ordering = FALSE,
                preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
            )
        )

        observeEvent(input$select_button, {
            selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
            selectedModule <- info_tbl()$Module[selectedRow]

            # Install code here, sort out later
            #stopApp("launching data module... (if new window does not show up, please use command launch_pivotData() or try again)")
            #sendToConsole("launch_pivotData()", execute = T)
            print(selectedModule)
            source("https://bioconductor.org/biocLite.R")
            biocLite("DESeq2", suppressUpdates = T, ask = F)
        })

        modules<-reactive({
            unlist(info_tbl()$Module[which(shinyValue('pick_', nrow(info_tbl())))])
        })

        output$pkg_info_ui <- renderUI({
            if("monocle" %in% modules()) {
                tags$p("Note: Monocle 2 has multiple dependecies. Please limit your module choice.")
            } else {
                tags$p("Please select modules you want to use.")
            }
        })


        # Save selected module to global on exit
        saveModule <- function(session = session) {
            session$onSessionEnded(function() {
                isolate({
                    assign("r_module", modules(), envir = .GlobalEnv)
                })
            })
        }
        saveModule(session)
    }

    if(is.null(args)) {
        # Launch default analysis module
        runGadget(ui, server, viewer = dialogViewer("PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data, v1.0.0"))
    } else {
        if(args == "main"){
            pivot_analysis()
        } else if (args == "launcher") {
            runGadget(ui, server, viewer = dialogViewer("PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data, v1.0.0"))
        } else if (args == "clean") {
            clean_pivotSession()
        } else {
            stop("Args not known, must be one of 'launcher', 'main'.")
        }
    }
}


#' perform session clean
#'
#' @export
clean_pivotSession <- function(){
    detachAllPackages(); .rs.restartR(afterRestartCommand = 'pivot()')
}

#' Launch main interface
#'
#' @export
pivot_main <- function(){
    Sys.sleep(1); pivot('main')
}

