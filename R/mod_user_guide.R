#' user_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_guide_ui <- function(id){
  ns <- NS(id)
  tagList(
 uiOutput(ns("user_guide"))
  )
}
    
#' user_guide Server Function
#'
#' @noRd 
mod_user_guide_server <- function(input, output, session){
  ns <- session$ns
 output$user_guide <- renderUI({
   tagList(

   tags$div(`class`="sidenav", style = "margin: 7vh 0vw;", 
            tags$a("1.Input Data", href="#top"),
            tags$a("1.1 Upload", href="#section2", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("1.2 Inference Method", href="#section3", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("1.3 Enrishment Method", href="#section4", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("2. Visualization", href="#section5"),
            tags$a("2.1 Enrichment Method for Visualization", href="#section4", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("2.2 Vizualisation Chart", href="#section6", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("2.3 Results", href="#section7", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("3. Input Objects", href="#section8"),
            tags$a("3.1 Constructing an Input Object", href="#section9", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("3.2 Potential Errors", href="#section10", style= "font-size: 20px; padding: 6px 8px 1px 35px;"),
            tags$a("4. Module Objects", href="#section11"),
            tags$a("5. Enrichment Objects", href="#section12"),
            tags$a("6. PPI networks", href="#section13"),
             ),
   


   tags$div(id="section1", `class`="main", style= "padding-top: 30px; padding-bottom: 10px",
            tags$h1("1. Input Data"),
            tags$p("The input of the web tool is a count matrix. The count matrix that we utilize is slightly different, but it is not a hassle to convert your old ones to ones functional for our tool! It should be noted that XXXTOOOLNAMEXXX is not a preprocessing tool, but an analytical tool, and thus your raw data needs to be refined elsewhere."),
   ),
   
   tags$div(id="section", `class`="main", style= "padding-top: 5px;",
     tags$h2("1.1 Upload"),
     tags$p("Select a file with a given count matrix. The file formats supported are csv, tsv and txt. See the section on input object for further information regarding the count matrix. Once the upload is completed a set of choices will be needed for further analysis. First, set a preferred name for your input object. Second, label your test group and your control group and split the data according to test and reference groups. The header from your input object will help you see which is  which. The left box should correspond to group 1 and the right box should correspond to group 2. Either P-value, Quantile or both can be chosen to be calculated. "),
           ),
   
   tags$div(id="section3", `class`="main",
            tags$h2("1.2 Inferance Method"),
            tags$p("After uploading an input object, select an inference method in the second column. The inference methods are different statistical analyzes that can be performed on the uploaded data and take different amounts of time to perform. If you want to learn more about the different inference methods available, click “Learn more” for more information."),
            tags$br(), 

            tags$p("When choosing parameters for the inference method there are tips beside the parameters."),
            tags$br(), 

            tags$p("For this beta-testing you should only choose MCODE or DIAMoND when trying the inference methods, since the rest will take too long. To try the rest of the inference methods, upload the datasets provided by us.
                   ")
   ),
   
   tags$div(id="section4", `class`="main",
            tags$h2("1.3 Enrichment Method"),
            tags$p("The first option to choose is the type of analysis that is going to be prosecuted.  Disease analysis is used to measure the relationship between disease ontology terms and gene products. Gene Ontology Analysis is a technique to interpret a set of genes and make use of the  Gene Ontology system of classification due to their functional characteristics. KEGG analysis creates a pathway analysis from the KEGG database which examines which pathways and associated functions are likely to be encoded in the genome."),
            tags$br(),

            tags$p("The second option is to choose the type of enrichment method that is going to be used. All analysis consists of the over-representation test and gene set enrichment analysis. The gene ontology analysis also consists of the gene ontology classification. The over-representation test determines whether genes are over-represented (enriched) in the given module. The gene set enrichment analysis is a method to identify classes of genes that are enriched in a given module. The gene ontology (GO) classification is based on GO distribution at a specific level."),
            tags$br(),

            tags$p("The third option to choose from is the kind of repository. In essence which kind of information should the enrichment object consist of:"),
            
            tags$p("DO - Disease Ontology."),
            tags$p("NCG - Network of Cancer Gene."),
            tags$p("DGN - DisGeNET which is comprehensive resources of gene-disease associations."),
            tags$p("KEGG - Kyoto Encyclopedia of Genes and Genomes.
")
   ),
   
   tags$div(id="section5", `class`="main",
            tags$h1("2. Welcome to the user guide!"),
            tags$p("Visualize the enrichment objects via the following graphs.")
   ),
   
   tags$div(id="section6", `class`="main",
            tags$h2("2.1 Enrichment Method for Visualization"),
            tags$p("Choose the enrichment object to visualize.")
   ),
   
   tags$div(id="section7", `class`="main",
            tags$h2("2.2 Visualization Charts"),
            tags$p("2.2.1. Dot plot
Dot plot is used to visualize enriched genes. The vertical axis shows the name of a certain disease. The horizontal shows the gene ratio to each result which also corresponds to the count. The adjusted p-values show the significance of each result. 
2.2.2. Enrichment map
The enrichment map organizes the enriched terms into a network consisting of overlapping gene sets. Therefore overlapping gene sets tend to cluster together and make it easier to identify similarities and functions. 

2.2.3. Gene-concept network
In the gene-concept network the user can see which genes are associated with the significant terms received in the two prior plots. 

2.2.4. Heatmap
The heatmap allows the user to see which genes are associated with the significant terms. The results are the same as in the gene-concept network. 
")
   ),
   
   tags$div(id="section8", `class`="main",
            tags$h1("2.3 Results"),
            tags$p("The result from the enrichment object shown as a data table. ")
   ),
   
   tags$div(id="section9", `class`="main",
            tags$h1("3. Input Object"),
            tags$p("For the tools to work successfully, the input data needs to be in a specific format. This format is that of a count matrix. A count matrix features named columns, each column representing one of the respective samples in your study. The rows however are filled with genes/genomic loci values. These values represent the summarized read counts per genomic region (e.g. gene, transcript) after normalization. Most online tools, such as usegalaxy.org, will make the pre-processing steps very easy (see our example for a quick guidance). So what is it that you need to do?")
   ),
   
   tags$div(id="section10", `class`="main",
            tags$h2("3.1 Construction an Input Objects"),
            tags$p("The easiest way to construct your input object is simply to merge the different tables, one for each sample, that is generated by your pre-processing. This can easily be achieved in a program such as excel. You then change the file format to csv, tsv, or txt before uploading it. Make sure that the input object does not contain any leftover data from the pre-processing!")
   ),
   
   tags$div(id="section11", `class`="main",
            tags$h2("3.2 Potential Errors"),
            tags$p("For the count matrix to be able to function, there can not be any text present in it more than the sample/patient names. In other words, the gene names that usually occupy the first column can be entirely removed. Deletion of entire columns is easily done in excel. Also, make sure that there is no text present in the bottom of the document as this sometimes happens as well.

Make sure before you convert your input object that you only have commas if decimals are present. Excel features a “Search and replace” function which can come in handy. 
")
   ),
   
   tags$div(id="section12", `class`="main",
            tags$h1("4. Module Objects"),
            tags$p("In the tab Module Objects the previously made inference methods are saved. From there, they can be downloaded, deleted or brought back to further analysis.

If a big number of inference methods are saved the user can decide to view more than 10 entries in the top left corner, or search among the inference methods in the top right corner. One can also sort the inference methods on module name, input name, module type or the ppi name. 

Lastly, the user can choose to upload an inference method that has previously been downloaded to the computer. This is done in the bottom left corner.
")
   ),
   
   tags$div(id="section13", `class`="main",
            tags$h1("5. Enrichment Objects"),
            tags$p("In the tab enrichment objects the previously made enrichments are saved. From there, they can be downloaded or brought back to further analysis.

However, it is important to remember that all the enrichments will disappear if the programme is turned off. The user will need to save all necessary enrichments. 

If a big number of enrichments are saved the user can decide to view more than 10 entries in the top left corner, or search among the enrichments in the top right corner. One can also sort the enrichments on module name or the enrichment method.

Lastly, the user can choose to upload an enrichment that has previously been downloaded to the computer. This is done in the bottom left corner.
")
   ),
   
   tags$div(id="section13", `class`="main",
            tags$h1("6. PPI networks"),
            tags$p("Under the PPI-networks tab the available PPI-networks can be viewed and new ones can be uploaded. If no additional PPI-networks have been uploaded there will only be one option called “Default”. Some of the inference methods have PPI-networks as a parameter and the available PPI-networks are the options the user can choose from. ")
   ),
   
   
   )

   
 })
}
    
## To be copied in the UI
# mod_user_guide_ui("user_guide_ui_1")
    
## To be copied in the server
# callModule(mod_user_guide_server, "user_guide_ui_1")
 
