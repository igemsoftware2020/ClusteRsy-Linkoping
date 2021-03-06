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

set_background_genes.Correlation_clique <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

set_background_genes.DIAMOnD <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

set_background_genes.module_discoverer <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

#Clique_Sum gets it's ppi from the Clique DB (default PPI)
set_background_genes.Clique_Sum_permutation <- function(module, con) {
  ppi_name <- as.character(MODifieRDB::match_db_loc_to_ppi(module$settings$db, con = con))
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
}

#Inference methods not using ppi-network.  
set_background_genes.DiffCoEx <- function(module, con) {
  input_name <- as.character(module$settings$MODifieR_input)
  rownames(MODifieRDB::MODifieR_input_from_db(input_name, con = con)$annotated_exprs_matrix)
}

set_background_genes.WGCNA <- function(module, con) {
  input_name <- as.character(module$settings$MODifieR_input)
  rownames(MODifieRDB::MODifieR_input_from_db(input_name, con = con)$annotated_exprs_matrix)
}

set_background_genes.MODA <- function(module, con) {
  input_name <- as.character(module$settings$MODifieR_input)
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
    
    p <- ggplot2::ggplot(test_df, ggplot2::aes(Genes,Pathways)) + 
      ggplot2::geom_tile(ggplot2::aes(fill = P.val), colour = "white") + 
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue", name = "-log10(P-val)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)) +
      ggplot2::ggtitle(plot_title)
  
   } else if (pval_color == FALSE) {
    for (i in 1:ncol(test_matrix)) {
      test_matrix[,i][rownames(test_matrix) %in% unlist(strsplit(res_test$geneID[i] , split = "/"))] <- 1
    }
    test_matrix$rowsums <- rowSums(test_matrix)
    test_matrix <- test_matrix[order(test_matrix$rowsums , decreasing = T) , ]
    test_matrix <- test_matrix[,-(NP+1)]
    test_df <<-  as.data.frame(as.table(as.matrix(test_matrix[1:NG,1:NP])))
    colnames(test_df) <- c("Genes" , "Pathways" , "P.val")
    
    p <- ggplot2::ggplot(test_df, ggplot2::aes(Genes,Pathways)) + 
      ggplot2::geom_tile(ggplot2::aes(fill = P.val), colour = "white") + 
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)) +
      ggplot2::ggtitle(plot_title)
  }
  
}

#Inspected module object function.

inspect_module <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  UseMethod("inspect_module", inspected_module)
}


inspect_module.Mcode <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- mod_Mcode_post_processing_ui(ns("Mcode_post_processing_ui_1"))
  
  server_output <- callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.Correlation_clique <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- mod_Correlation_clique_post_processing_ui(ns("Correlation_clique_post_processing_ui_1"))
  
  server_output <- callModule(mod_Correlation_clique_post_processing_server, "Correlation_clique_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.DIAMOnD <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- mod_DIAMoND_post_processing_ui(ns("DIAMoND_post_processing_ui_1"))

  
  server_output <- callModule(mod_DIAMoND_post_processing_server, "DIAMoND_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.module_discoverer <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  #No post processing available, buttons not used.
  inspect_button <- NULL
  post_process_button <- NULL
  
  ui_output <- mod_module_discoverer_post_processing_ui(ns("module_discoverer_post_processing_ui_1"))
  
  server_output <- callModule(mod_module_discoverer_post_processing_server, "module_discoverer_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.Clique_Sum_permutation <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  #No post processing available, buttons not used.
  inspect_button <- NULL
  post_process_button <- NULL
  
  ui_output <- mod_CliqueSum_post_processing_ui(ns("CliqueSum_post_processing_ui_1"))
  
  server_output <- callModule(mod_CliqueSum_post_processing_server, "CliqueSum_post_processing_ui_1", inspected_module, selected_module_name, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.DiffCoEx <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <-  mod_DiffCoEx_post_processing_ui(ns("DiffCoEx_post_processing_ui_1"))
  
  server_output <- callModule(mod_DiffCoEx_post_processing_server, "DiffCoEx_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.WGCNA <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- mod_WGCNA_post_processing_ui(ns("WGCNA_post_processing_ui_1"))
  
  server_output <- callModule(mod_WGCNA_post_processing_server, "WGCNA_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

inspect_module.MODA <- function(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con) {
  
  ui_output <- mod_MODA_post_processing_ui(ns("MODA_post_processing_ui_1"))
  
  server_output <- callModule(mod_MODA_post_processing_server, "MODA_post_processing_ui_1", inspected_module, selected_module_name, inspect_button, post_process_button, con = con)
  
  return(list("ui_output" = ui_output,
              "server_output" = server_output))
  
}

# shinyLink
shinyLink <- function(label, target) {
  tags$a( 
    class = "shiny__link",
    href= paste0("javascript:shinyLink('", target, "')"),
    label
  )
}

# Get input data for the used MODifieR object 

retrieve_input_data <- function(module, con) {
  
  input_data <- MODifieRDB::MODifieR_input_from_db(module$settings$MODifieR_input, con = con)
  
  na.omit(input_data$edgeR_deg_table[module$module_genes, ])
  
}

