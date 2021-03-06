#' disease_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_disease_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(`data-intro`="In this column you start by deciding what type of analysis you want.", `data-step`=5,
      selectInput(ns("analyse_method"), label = "Type of analysis",
                         choices = c("Disease analysis", "Gene Ontology analysis", "KEGG analysis"))
    ),
    tags$div(`data-intro`="Then you choose enrichment and repository type.", `data-step`=6,
    htmlOutput(ns("enrich_type")),
    htmlOutput(ns("enrich_repo"))),
    htmlOutput(ns("description")),
    tags$br(),
    tags$div(`data-intro`="And lastly you choose the parameters for the analysis and run the enrichment!", `data-step`=7,
    htmlOutput(ns("para"))
    ))
}
    
#' disease_analysis Server Function
#'
#' @noRd 
mod_disease_analysis_server <- function(input, output, session, con, Description1_ui_1, module_overview_ui_1){
  ns <- session$ns
  
  disease_analysis_module <- reactiveValues()
  
  # Call Module only once
  enrichDGN_ui_1 <- callModule(mod_enrichDGN_server, "enrichDGN_ui_1", con = con, Description1_ui_1, module_overview_ui_1) 
  enrichDO_ui_1 <- callModule(mod_enrichDO_server, "enrichDO_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  enrichNCG_ui_1 <- callModule(mod_enrichNCG_server, "enrichNCG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseDGN_ui_1 <- callModule(mod_gseDGN_server, "gseDGN_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseDO_ui_1 <- callModule(mod_gseDO_server, "gseDO_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseNCG_ui_1 <- callModule(mod_gseNCG_server, "gseNCG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  enrichGO_ui_1 <- callModule(mod_enrichGO_server, "enrichGO_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseGO_ui_1 <- callModule(mod_gseGO_server, "gseGO_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  groupGO_ui_1 <- callModule(mod_groupGO_server, "groupGO_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  enrichKEGG_ui_1 <- callModule(mod_enrichKEGG_server, "enrichKEGG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseKEGG_ui_1 <- callModule(mod_gseKEGG_server, "gseKEGG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  enrichMKEGG_ui_1 <- callModule(mod_enrichMKEGG_server, "enrichMKEGG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  gseMKEGG_ui_1 <- callModule(mod_gseMKEGG_server, "gseMKEGG_ui_1", con = con, Description1_ui_1, module_overview_ui_1)
  
  observeEvent(input$analyse_method, {
    
    descrip <- function(method){
      if (input$analyse_method == "Disease analysis"){
        description = tags$div(style = "color: #3f3f40", class = "text-left", 
                        tags$p(tags$b("Over-representation:"), "Method for identifying association of interesting genes."),
                        tags$p(tags$b("Gene set enrichment analysis:"),"Method for identifying association of a set of interesting genes"),

                        tags$p(tags$a("DO:", style = "color: #fec961", href = "https://academic.oup.com/bioinformatics/article/31/4/608/2748221"),
                               "(Disease Ontology) Method for identifying disease association of interesting genes."), 
                        

                        tags$p(tags$a("NCG:", style = "color: #fec961", href = "https://academic.oup.com/nar/article/44/D1/D992/2502624"),
                               "Network of Cancer Gene (NCG), a  manually curated repository containing 1571 cancer genes which has been derived from 175 published studies."),
                        tags$p("The method is used for analyzing gene lists and determining whether they are enriched in genes known to be mutated in a given cancer type. "),
                        
                      
                        tags$p(tags$a("DGN:", style ="color: #fec961", href = "https://academic.oup.com/database/article/doi/10.1093/database/bav028/2433160"),
                               "DisGeNET is a discovery comprehensive platform for dynamical exploration of human disease and their genes. The platform contains over 380 000 associations between >16 000 genes and 13 000 diseases. The enrichment analysis of disease-gene associations is supported by over-representation test and gene set enrichment analysis."),
                        
        
        )
        return(description)
      }
      if (input$analyse_method == "Gene Ontology analysis"){
        description = tags$div(style = "color: #3f3f40", class = "text-left",
                        tags$p(tags$b("Over-representation:"), "Method for identifying association of interesting genes."),
                        tags$p(tags$b("Gene set enrichment analysis:"),"Method for identifying association of a set of interesting genes"),
                        tags$p(tags$b("Classification:"), "A method designed for a gene classification based on GO distribution at a specific level"),
                        
                        tags$p(
                          tags$a(tags$b("GO termfinder"),style ="color: #fec961", href ="https://academic.oup.com/bioinformatics/article/20/18/3710/202612"),
                               "This package uses methods for accessing Gene Ontology information and finding significantly enriched Gene Ontology terms associated with a list of genes."),
                        
                        tags$p("The organism data is collected from",tags$a("Bioconductor OrgDb",style ="color: #fec961", href ="http://bioconductor.org/packages/release/BiocViews.html#___OrgDb")),
                        )
        return(description)
      }
      if (input$analyse_method == "KEGG analysis"){
        description = tags$div(style = "color: #3f3f40", class = "text-left",
                               tags$p(tags$b("Over-representation:"), "Method for identifying association of interesting genes."),
                               tags$p(tags$b("Gene set enrichment analysis:"),"Method for identifying association of a set of interesting genes"),
                               tags$p(
                                 tags$a(tags$b("KEGG:"), style ="color: #fec961", href = "https://www.genome.jp/kegg/"),
                                 "Kyoto Encyclopedia of Genes and Genomes (KEGG), is a database resource for understanding high-level functions and utilities of the biological system."),
                               tags$p("The organism is set to hsa (H. sapiens) by default and is collected from",
                                      tags$a("KEGGdb", style ="color: #fec961", href ="https://www.genome.jp/kegg/catalog/org_list.html"))
        )
        return(description)
      }
    }
    
    description <- function(method, hyperlink){
      tagList(tags$button(paste("Click me to learn more about ", input$analyse_method),
                          class = "link",
                          type = "button",
                          `data-toggle` = "modal",
                          `data-target` = "#descrip_enrich"),
              tags$div(`class` = "modal fade", `id` = "descrip_enrich", `role` = "dialog", `tabindex` = "-1", `aria-hidden` = "true", `style` = "display:none;",
                       tags$div(`class` = "modal-dialog", style="top:20%",
                                tags$div(`class` = "modal-content",
                                         tags$div(`class` = "modal-header",
                                                  tags$button("×", type = "button", class = "close", `data-dismiss` = "modal", `aria-hidden` = "true"),
                                                  tags$h4(method ,class = "modal-title", style = "color: black")),
                                         tags$div(`class` = "modal-body",
                                                  tags$p(descrip(method = method), style = "color: black")),
                                         tags$div(`class` = "modal-footer",
                                                  tags$button("Close", class = "btn btn-default", `data-dismiss` = "modal"),
                                                  tags$a("Learn more", class= "btn btn-default", href=hyperlink, style = "background-color: #2c3e50; border-color: #2c3e50"))))))
    }
    
    
    if (input$analyse_method == "Disease analysis"){
      output$enrich_type <- renderUI({
        tags$div(selectInput(ns("enrich_type1"), label = "Type of enrichment",
                             choices = c("Over-representation test", "Gene Set Enrichment Analysis"))
        )
      })
      
      output$enrich_repo <- renderUI({
        tags$div(selectInput(ns("enrich_repo1"), label = "Type of repository",
                             choices = c("DGN", "DO", "NCG"))
        )
      })
      
      observeEvent(c(input$enrich_type1, input$enrich_repo1), {
        if (input$enrich_type1 == "Over-representation test" & input$enrich_repo1 == "DGN" ){
          output$para <- renderUI({
            mod_enrichDGN_ui(ns("enrichDGN_ui_1"))})
          observeEvent(enrichDGN_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichDGN_ui_1$enrich 
            })
          method <- "DGN Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#enrichdgn-and-enrichdgnv-functions"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
            description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Over-representation test" & input$enrich_repo1 == "DO" ){
          output$para <- renderUI({
            mod_enrichDO_ui(ns("enrichDO_ui_1"))})
          observeEvent(enrichDO_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichDO_ui_1$enrich
            })
          method <- "DO Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#enrichdo-function"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Over-representation test" & input$enrich_repo1 == "NCG" ){
          output$para <- renderUI({
            mod_enrichNCG_ui(ns("enrichNCG_ui_1"))})
          observeEvent(enrichNCG_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichNCG_ui_1$enrich 
           })
          method <- "NCG Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#enrichncg-function"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Gene Set Enrichment Analysis" & input$enrich_repo1 == "DGN" ){
          output$para <- renderUI({
            mod_gseDGN_ui(ns("gseDGN_ui_1"))})
          observeEvent(gseDGN_ui_1$enrich, {
            disease_analysis_module$enrich <- gseDGN_ui_1$enrich
            })
          method <- "DGN Gene Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#gsedgn-fuction"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Gene Set Enrichment Analysis" & input$enrich_repo1 == "DO" ){
          output$para <- renderUI({
            mod_gseDO_ui(ns("gseDO_ui_1"))})
          observeEvent(gseDO_ui_1$enrich, {
            disease_analysis_module$enrich <- gseDO_ui_1$enrich
            })
          method <- "DO Gene Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#gsedo-fuction"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Gene Set Enrichment Analysis" & input$enrich_repo1 == "NCG" ){
          output$para <- renderUI({
            mod_gseNCG_ui(ns("gseNCG_ui_1"))})
          observeEvent(gseNCG_ui_1$enrich, {
            disease_analysis_module$enrich <- gseNCG_ui_1$enrich
            })
          method <- "NCG Gene Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter4.html#gsencg-fuction"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
      })
    }
    
    if (input$analyse_method == "Gene Ontology analysis"){
      output$enrich_type <- renderUI({
        tags$div(selectInput(ns("enrich_type1"), label = "Type of enrichment",
                             choices = c("GO over-representation test", "GO Gene Set Enrichment Analysis", "GO classification"))
        )
      })
      
      output$enrich_repo <- renderUI({})
      
      observeEvent(c(input$enrich_type1), {
        if (input$enrich_type1 == "GO over-representation test"){
          output$para <- renderUI({
            mod_enrichGO_ui(ns("enrichGO_ui_1"))})
          observeEvent(enrichGO_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichGO_ui_1$enrich
            })
          method <- "GO Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter5.html#go-over-representation-test"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "GO Gene Set Enrichment Analysis"){
          output$para <- renderUI({
            mod_gseGO_ui(ns("gseGO_ui_1"))})
          observeEvent(gseGO_ui_1$enrich, {
            disease_analysis_module$enrich <- gseGO_ui_1$enrich
            })
          method <- "GO Gene Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter5.html#go-gene-set-enrichment-analysis"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "GO classification"){
          output$para <- renderUI({
            mod_groupGO_ui(ns("groupGO_ui_1"))})
          observeEvent(groupGO_ui_1$enrich, {
            disease_analysis_module$enrich <- groupGO_ui_1$enrich
            })
          method <- "GO Classification Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter5.html#go-classification"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
      })
    }
    
    if (input$analyse_method == "KEGG analysis"){
      output$enrich_type <- renderUI({
        tags$div(selectInput(ns("enrich_type1"), label = "Type of enrichment",
                             choices = c("Over-representation test", "Gene Set Enrichment Analysis"))
        )
      })
      
      output$enrich_repo <- renderUI({
        tags$div(selectInput(ns("enrich_repo1"), label = "Type of repository",
                             choices = c("KEGG", "MKEGG"))
        )
      })
      
      observeEvent(c(input$enrich_type1, input$enrich_repo1), {
        if (input$enrich_type1 == "Over-representation test" & input$enrich_repo1 == "KEGG"){
          output$para <- renderUI({
            mod_enrichKEGG_ui(ns("enrichKEGG_ui_1"))})
          observeEvent(enrichKEGG_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichKEGG_ui_1$enrich
          })
          method <- "KEGG Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter6.html#kegg-over-representation-test"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Gene Set Enrichment Analysis" & input$enrich_repo1 == "KEGG"){
          output$para <- renderUI({
            mod_gseKEGG_ui(ns("gseKEGG_ui_1"))})
          observeEvent(gseKEGG_ui_1$enrich, {
            disease_analysis_module$enrich <- gseKEGG_ui_1$enrich
            })
          method <- "KEGG Gense Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter6.html#kegg-gene-set-enrichment-analysis"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Over-representation test" & input$enrich_repo1 == "MKEGG"){
          output$para <- renderUI({
            mod_enrichMKEGG_ui(ns("enrichMKEGG_ui_1"))})
          observeEvent(enrichMKEGG_ui_1$enrich, {
            disease_analysis_module$enrich <- enrichMKEGG_ui_1$enrich
            })
          method <- "MKEGG Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter6.html#kegg-module-over-representation-test"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
        if (input$enrich_type1 == "Gene Set Enrichment Analysis" & input$enrich_repo1 == "MKEGG"){
          output$para <- renderUI({
            mod_gseMKEGG_ui(ns("gseMKEGG_ui_1"))})
          observeEvent(gseMKEGG_ui_1$enrich, {
            disease_analysis_module$enrich <- gseMKEGG_ui_1$enrich
            })
          method <- "MKEGG Gense Set Enrichment Method"
          hyperlink <- "https://yulab-smu.github.io/clusterProfiler-book/chapter6.html#kegg-module-gene-set-enrichment-analysis"
          output$description <- renderUI({
            tags$div(style = "text-align: center;",
                     description(method, hyperlink))
          })
        }
      })
    }
  })
  return(disease_analysis_module)
}
    
## To be copied in the UI
# mod_disease_analysis_ui("disease_analysis_ui_1")
    
## To be copied in the server
# callModule(mod_disease_analysis_server, "disease_analysis_ui_1")
 
