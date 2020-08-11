get_module_genes <- function(module_name, con) {
              MODifieRDB::MODifieR_module_from_db(module_name = module_name, con = con)$module_genes
}
get_sorted_module_genes <- function(module_name, con) {
  MODifieRDB::MODifieR_module_from_db(module_name = module_name, con = con)$module_genes
  input_name <- as.character(MODifieRDB::MODifieR_module_from_db(module_name, con = con)$settings$MODifieR_input)
  input_data <- MODifieRDB::MODifieR_input_from_db(input_name, con = con)$diff_genes
  module_genes <- MODifieRDB::MODifieR_module_from_db(module_name, con = con)$module_genes
  
  subset_genes <- input_data[(input_data$gene %in% module_genes), ]
  
  genes <- subset_genes$pvalue
  names(genes) <- subset_genes$gene
  sort(genes, decreasing = T)
}


get_background_genes <- function(module_name, con) {
  module <- MODifieRDB::MODifieR_module_from_db(module_name = module_name, con = con)
  set_background_genes(module, con = con)
} 

set_background_genes <- function(module, con) {
  UseMethod("set_background_genes", module)
}

#Inference method use ppi-network.
set_background_genes.Mcode <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

set_background_genes.correlation_clique <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

set_background_genes.diamond <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

set_background_genes.module_discoverer <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

#Clique_Sum gets it's ppi from the Clique DB (default PPI)
set_background_genes.Clique_Sum_permutation <- function(module, con) {
  unique(unlist(MODifieRDB::ppi_network_from_db("Default", con = con)[,1:2]))
}

#Inference methods not using ppi-network.  
set_background_genes.DiffCoEx <- function(module, con) {
  input_name <- as.character(module$settings$input_name)
  rownames(MODifieRDB::MODifieR_input_from_db(input_name, con = con)$annotated_exprs_matrix)
}

set_background_genes.WGCNA <- function(module, con) {
  input_name <- as.character(module$settings$input_name)
  rownames(MODifieRDB::MODifieR_input_from_db(input_name, con = con)$annotated_exprs_matrix)
}

set_background_genes.MODA <- function(module, con) {
  input_name <- as.character(module$settings$input_name)
  rownames(MODifieRDB::MODifieR_input_from_db(input_name, con = con)$annotated_exprs_matrix)
}

#Heatmap function.

# CPobj = clusterprofiler enrich object
# NP = number of top pathways
# NG = number of top genes 
gene_heatmap <- function(CPobj, NP, NG, plot_title, pval_color) {
  res_test <- CPobj@result[1:NP,]
  gene_names <- unique(unlist(strsplit(res_test$geneID ,split = "/")))
  test_matrix <-as.data.frame(matrix(data = 0 , ncol = length(res_test$Description) , nrow= length(gene_names)))
  colnames(test_matrix) <- res_test$Description
  rownames(test_matrix) <- gene_names

  if (pval_color == TRUE) {
  for (i in 1:ncol(test_matrix)) {
    test_matrix[,i][rownames(test_matrix) %in% unlist(strsplit(res_test$geneID[i] , split = "/"))] <- -log10(res_test$p.adjust[i])
  }
    test_matrix$rowsums <- rowSums(test_matrix)
    test_matrix <- test_matrix[order(test_matrix$rowsums , decreasing = T) , ]
    test_matrix <- test_matrix[,-(NP+1)]
    test_df <<-  as.data.frame(as.table(as.matrix(test_matrix[1:NG,1:NP])))
    colnames(test_df) <- c("Genes" , "Pathways" , "P.val")
    
    p <- ggplot(test_df, aes(Genes,Pathways)) + 
      geom_tile(aes(fill = P.val), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue", name = "-log10(P-val)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      ggtitle(plot_title)
  
   } else if (pval_color == FALSE) {
    for (i in 1:ncol(test_matrix)) {
      test_matrix[,i][rownames(test_matrix) %in% unlist(strsplit(res_test$geneID[i] , split = "/"))] <- 1
    }
    test_matrix$rowsums <- rowSums(test_matrix)
    test_matrix <- test_matrix[order(test_matrix$rowsums , decreasing = T) , ]
    test_matrix <- test_matrix[,-(NP+1)]
    test_df <<-  as.data.frame(as.table(as.matrix(test_matrix[1:NG,1:NP])))
    colnames(test_df) <- c("Genes" , "Pathways" , "P.val")
    
    p <- ggplot(test_df, aes(Genes,Pathways)) + 
      geom_tile(aes(fill = P.val), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      ggtitle(plot_title)
  }
  
}

#Inspected module object function.

inspect_module <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  UseMethod("inspect_module", inspected_module)
}


inspect_module.Mcode <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_ouput <- renderUI({
    mod_Mcode_post_processing_ui(ns("Mcode_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_ouput)
  return(server_output)
  
}

inspect_module.Correlation_clique <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({
    mod_Correlation_clique_post_processing_ui(ns("Correlation_clique_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_Correlation_clique_post_processing_server, "Correlation_clique_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.DIAMOnD <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({
    mod_DIAMoND_post_processing_ui(ns("DIAMoND_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_DIAMoND_post_processing_server, "DIAMoND_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.module_discoverer <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({
    mod_module_discoverer_post_processing_ui(ns("module_discoverer_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_module_discoverer_post_processing_server, "module_discoverer_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.Clique_Sum_permutation <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({                       
    mod_CliqueSum_post_processing_ui(ns("CliqueSum_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_CliqueSum_post_processing_server, "CliqueSum_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.DiffCoEx <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- renderUI({
    mod_DiffCoEx_post_processing_ui(ns("DiffCoEx_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_DiffCoEx_post_processing_server, "DiffCoEx_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.WGCNA <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({
    mod_WGCNA_post_processing_ui(ns("WGCNA_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_WGCNA_post_processing_server, "WGCNA_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
}

inspect_module.MODA <- function(inspected_module, selected_module_name, ns, con) {
  
  ui_output <- renderUI({
    mod_MODA_post_processing_ui(ns("MODA_post_processing_ui_1"))
  })
  
  server_output <- callModule(mod_MODA_post_processing_server, "MODA_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(ui_output)
  return(server_output)
  
  
  
}

# shinyLink
shinyLink <- function(label) {
  tags$a(
    class = "shiny__link",
    href="javascript:shinyLink()",
    label
  )
}
