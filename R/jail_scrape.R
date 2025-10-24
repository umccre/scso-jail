


library(httr)
#library(readxl)
library(lubridate)
library(purrr)
library(stringi)
library(xml2)
library(dplyr)
library(stringr)
library(tidyr)
#library(fst)

sync.time.label <- lubridate::now() %>% with_tz(tzone = "US/Central")
datestamp <- format(sync.time.label, "%Y%m%d_%H%M%S")


login_func <- function() {
  headers = c(
    `Accept` = '*/*',
    `Accept-Language` = 'en-US,en;q=0.6',
    `Cache-Control` = 'no-cache',
    `Connection` = 'keep-alive',
    `Content-Type` = 'application/x-www-form-urlencoded',
    `Cookie` = 'killmenothing; SULang=en%2CUS',
    `Origin` = 'https://xfer.shelbycountytn.gov',
    `Pragma` = 'no-cache',
    `Referer` = 'https://xfer.shelbycountytn.gov/',
    `Sec-Fetch-Dest` = 'empty',
    `Sec-Fetch-Mode` = 'cors',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-GPC` = '1',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36',
    `X-User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'
  )
  
  sync.time <- Sys.time()
  
  sync.time.milli <- as.numeric(sync.time) *1000
  
  params = list(
    `Command` = 'Login',
    `Sync` = as.character(sync.time.milli)
  )
  
  data = list(
    `user` = 'public',
    `pword` = 'public',
    `language` = 'en,US')
  
  login_res <- httr::POST(url = 'https://xfer.shelbycountytn.gov/Web%20Client/Login.xml', httr::add_headers(.headers=headers), query = params, body = data, encode = 'form',
                          user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'),  httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  
  return(login_res)
}

login_res <- login_func()


# file_struct_res <- GET("https://xfer.shelbycountytn.gov/Web%20Client/ListError.xml?Command=List&Dir=/",
#                        httr::add_headers(.headers=unlist(headers(login_res))),
#                        user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'))
# 
# df <- content(file_struct_res, as = "text") %>%
#   read_xml() %>%
#   # rvest::html_elements("files") %>%
#   as_list() %>%
#   tibble::as_tibble() %>%
#   unnest(cols = everything()) %>%
#   unnest_wider(col = response, names_sep = "") %>%
#   unnest(cols = everything()) %>%
#   unnest(cols = everything()) %>%
#   filter(!is.na(responseFileName))
# 
# colnames(df) <- colnames(df) %>% str_replace_all("response", "")
# 
# dir_file_info <- function(path, login_res) {
#   if(str_detect(path, "Disposition") | str_detect(path, "Health")) {
#     return(tibble())
#   }
#   
#   login_res <- login_res
#   dir_res <- GET(path,
#                  httr::add_headers(.headers=unlist(headers(login_res))),
#                  user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'))
#   
#   if(length(content(dir_res, as = "raw")) < 1000) {
#     login_res <- login_func()
#     print("New session!")
#     dir_file_info(path = path, login_res = login_res)
#   }
#   
#   file_info <- content(dir_res, as = "text") %>%
#     read_xml() %>%
#     #xml_find_all(xpath = "//files") %>%
#     # rvest::html_elements("files") %>%
#     as_list() %>%
#     tibble::as_tibble() %>%
#     unnest(cols = everything()) %>%
#     unnest_wider(col = response, names_sep = "") %>%
#     unnest(cols = everything()) %>%
#     unnest(cols = everything())  %>%
#     filter(!is.na(responseFileName)) %>%
#     mutate(across(everything(), as.character))
#   
#   colnames(file_info) <- colnames(file_info) %>% str_replace_all("response", "")
#   
#   return(file_info)
# }
# 
# 
# all_files <- map_dfr(.x = df$FilePath, .f = function(x){
#   print(x)
#   dir_path <- paste0("https://xfer.shelbycountytn.gov/Web%20Client/ListError.xml?Command=List&Dir=", x)
#   
#   dir_files <- dir_file_info(dir_path, login_res = login_res) %>%
#     mutate(folder = x)
#   
#   return(dir_files)
# })
# 
# all_files <- all_files %>%
#   mutate(name_folder_size = paste0(FileName, folder, FileSize),
#          timestamp = sync.time.label)
# 
# 
# if(!file.exists("data/all_xfer_files.rds")) {
#   
#   new_files <- all_files
#   saveRDS(all_files, file = "data/all_xfer_files.rds")
#   
#   
# } else {
#   all_xfer_files <- readRDS("data/all_xfer_files.rds")
#   
#   new_files <- all_files %>%
#     filter(!name_folder_size %in% all_xfer_files$name_folder_size)
#   
#   all_xfer_files <- bind_rows(all_xfer_files, new_files)
#   saveRDS(all_xfer_files, file = "data/all_xfer_files.rds")
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# walk(.x = unique(all_files$folder), .f = function(x){
#   if(!dir.exists(paste0("./data", xml2::url_parse(x)$path, "/"))) {
#     dir.create(paste0("./data", xml2::url_parse(x)$path, "/"), recursive = T)
#   }
#   
# })
# 
# 
# 
# csv_new_files <- new_files %>%
#   filter(str_detect(FileName, "csv"))
# 
# xls_new_files <- new_files %>%
#   filter(str_detect(FileName, "xls"))
# 
# txt_new_files <- new_files %>%
#   filter(str_detect(FileName, "txt"))

download_with_test <- function(path, login_res, count = 1){
  params = list(
    `Command` = 'Download',
    `File` = xml2::url_parse(path)$path
  )
  
  
  int_file_res <- httr::GET(url = 'https://xfer.shelbycountytn.gov/', httr::add_headers(.headers=unlist(headers(login_res))), query = params,
                            user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'))
  
  if(length(content(int_file_res, as = "raw")) < 1000) {
    if(count >5) {
      print(path)
      return(NA)
    }
    
    login_res <- login_func()
    print("New session!")
    download_with_test(path = path, login_res = login_res, count = count + 1)
  }
  
  return(int_file_res)
  
}




  jail_path <- "%2FSCSO-InJail%2FSCSO-InJail.xls"
    file_res <- download_with_test(path = jail_path, login_res)
    stri_sub(jail_path, nchar(jail_path)-3, nchar(jail_path)-4) <- paste0("_xxx", datestamp)
    writeBin(content(file_res, as = "raw"), con = paste0("data", xml2::url_parse(jail_path)$path))




# if(nrow(csv_new_files) > 0) {
#   walk(.x = csv_new_files$FilePath, .f = function(x){
#     
#     file_res <- download_with_test(path = x, login_res)
#     
#     stri_sub(x, nchar(x)-3, nchar(x)-4) <- paste0("_xxx", datestamp)
#     writeBin(content(file_res, as = "raw"), con = paste0("data/", xml2::url_parse(x)$path))
#   })
# }
# 
# 
# 
# 
# if(nrow(xls_new_files) > 0){
#   walk(.x = xls_new_files$FilePath, .f = function(x){
#     
#     file_res <- download_with_test(path = x, login_res)
#     
#     stri_sub(x, nchar(x)-3, nchar(x)-4) <- paste0("_xxx", datestamp)
#     writeBin(content(file_res, as = "raw"), con = paste0("data/", xml2::url_parse(x)$path))
#   })
# }
# 
# if(nrow(txt_new_files) > 0){
#   walk(.x = txt_new_files$FilePath, .f = function(x){
#     
#     file_res <- download_with_test(path = x, login_res)
#     
#     stri_sub(x, nchar(x)-3, nchar(x)-4) <- paste0("_xxx", datestamp)
#     writeBin(content(file_res, as = "raw"), con = paste0("data/", xml2::url_parse(x)$path))
#   })
# }


