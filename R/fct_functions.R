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

set_background_genes.clique_sum <- function(module, con) {
  ppi_name <- as.character(module$settings$ppi_network)
  unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
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


  
