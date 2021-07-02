#--------------------------------------------------
## Get all links from each index ###
find_all_links <- function(pages_pbs) {
  res_pbs = c()
  for (link in pages_pbs){
    tmp <- link %>%
      read_html %>%
      html_nodes('#medicine-item > tbody > tr:nth-child(n) > td > a') %>% 
      html_attr('href') %>%                   
      paste('https://www.pbs.gov.au',.,sep='')
    res_pbs = c(res_pbs, tmp)
  }
  return(res_pbs)
}

#--------------------------------------------------
## Get all sub links within one drug link ###
find_all_sublinks <- function(all_pbs) {
  all_subpbs = c()
  for (link in all_pbs){
    tryCatch(
      tmp_sub <- link %>%
        read_html %>%
        html_nodes('#content > div > div > div:nth-child(4) > div > table > tbody > tr:nth-child(1) > td > ul > li:nth-child(n) > a') %>%
        html_attr('href'), error = function(e){NA} 
    ) #%>%
    #paste('https://www.pbs.gov.au',.,sep='') 
    all_subpbs = c(all_subpbs, tmp_sub)
  }
  return(all_subpbs)
}

#--------------------------------------------------
## Build data table extracting for each link the following information:
# drug_item, drug_name, source, bodysystem_level1, bodysystem_level2, bodysystem_level3,
# product_form, max_qty_packs, max_units, n_repeat, DPMQ, max_safnet, patient_charge

# Parse nodes/columns within main table
get_node <- function(page, x, n=1, n_el = 7){ # expected number of nodes is 7
  if (x != "numerical_summary"){
    return (t(matrix(page %>%
                       html_nodes(x) %>%
                       html_text(), ncol = n)))    
  }
  l = page %>% html_nodes("#medicine-item")
  tmp = lapply(l, function(x) get_first_complete_row(x, n_el))
  return (do.call(rbind, tmp))
}

# Scrape first complete row
get_first_complete_row <- function(x, n){
  tmp = x %>% html_nodes("tr:nth-child(2) > td.align-top") %>% html_text()
  if (length(tmp) == n){
    return (tmp)
  }
  else {
    tmp = lapply(x %>% html_nodes("tr"), function(y) y %>% html_nodes("td") %>% html_text())
    for (item in tmp){
      if (length(item) == n){
        return(item)
      }
    }
  }
}

# Build data frame
build_df <- function(links, res) {
  #res = vector(mode="list", length = length(links))
  for (i in 1:length(links)) {
    if (!is.null(res[[i]])){
      print(paste0("Already scraped: ", links[i]))
      next
    }
    link = links[i]
    download.file(link, destfile="tmp.html", quiet=TRUE)
    page = read_html("tmp.html")
    N = length(page %>% html_nodes("#medicine-item"))
    Nbl = length(page %>% html_nodes("#medicine-summary > tbody > tr:nth-child(2) > td.summary-body > a")) %/% N
    
    ## for 7 nodes
    cols = c("DrugItem", "DrugName", "Source", paste0("BodyLevel", 1:Nbl),
             "ProductForm", "to_drop", "MaxQtyPacks", "MaxUnits", "NRepeat", "DPMQ", "MaxSafeNet", "PatientCharge")
    
    ## for 6 nodes
    # cols = c("DrugItem", "DrugName", "Source", "to_drop", paste0("BodyLevel", 1:Nbl),
    #          "ProductForm", "drug", "MaxAmount", "NRepeat", "DPMA", "MaxSafeNet", "PatientCharge")
    
    nodes = c("span.item-code", 
              "#content > div > div:nth-child(n) > div > h1", 
              "#medicine-summary > tbody > tr:nth-child(n) > td.summary-body > a",
              "span.form-strength",
              "numerical_summary")
    
    tmp = tryCatch(as.data.frame(do.call(cbind, lapply(nodes, function(x) get_node(page, x, N, 7)))), # change to 6 when dealing with 6 nodes
                   error=function(cond) cond)
    if(inherits(tmp, "error")) {
      message(paste0(i, ": The following link broke: ", link))
      next
    }
    if (ncol(tmp) != length(cols)){
      print(paste0("problematic link: ", link))
      next
    }
    colnames(tmp) = cols
    res[[i]] <<- tmp#tmp[, names(tmp) != "to_drop"] ## Assigns in the parent environment, not ideal but connection to PBS is flaky
    #print()
    print(paste0(i,": Successfully scraped ", link))
  }
  #return(res)
}

