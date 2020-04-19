#' Convert PDF to HTML
#' 
#' This is wrapper around API to \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}.
#' 
#' @param input_file_url A \strong{URL} to pdf file to process. Do \strong{not} supply path to file in your filesystem.
#' @param output_file A name of file to write .html to. Must be of .html extension. By default, the name of the file is extracted from \code{input_file_url}.
#' @param quiet Boolean. Should the method display messages about progress? Errors will be displayed regardless of this option.
#' @param api_key A custom API key obtained from \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}. Make sure to use the key of \strong{version 1}.
#' @param overwrite Boolean. Should the .html file be overwritten?
#' 
#' @details 
#' Create your own account at \href{https://cloudconvert.com/register}{cloudconvert} and obtain your own API key, or convert the file(s) manually.
#' 
#' @return A HTML file with name \code{output_file} is created. It is ready to be passed into \code{\link{extract_code_from_html}} function.
#' @seealso \code{\link{extract_code_from_html}}, \code{\link{extract_code_from_pdf}}
#' @export


convert_pdf_2_html <- function(input_file_url, 
                               output_file = NULL, 
                               quiet = FALSE, 
                               api_key = NULL,
                               overwrite = FALSE){
  input_file_name <- basename(input_file_url)
  if(file.exists(input_file_url)) stop('input_file_url must be URL to file, available online. See ?convert_pdf_to_html')
  if(tolower(tools::file_ext(input_file_name)) != 'pdf') stop(paste0('Input file must be of pdf extension, not', 
                                                                tools::file_ext(input_file_name),
                                                                '.'))
  if(!is.null(output_file)){
    if(tolower(tools::file_ext(output_file)) != 'html') stop(paste0('Output file must be of html extension, not', 
                                                                  tools::file_ext(output_file),
                                                                  '.'))
    if(file.exists(output_file)){
      if(overwrite) file.remove(output_file)
      else stop(paste0(output_file), ' already exists.')
    }
  }
  else{
    output_file <- stringi::stri_replace_last(input_file_name,
                                             'html',
                                             fixed = 'pdf')
  }
  
  if(is.null(api_key)) api_key <- readLines(system.file('extdata/api_key.txt', package = 'CodeExtractoR'))
  base_url <- "https://api.cloudconvert.com/v1/process"
  auth_header <- httr::add_headers(Authorization = paste0('Bearer ',
                                                                   api_key))
  
  # 1) New process ID
  
  req <- api_new_process(base_url, auth_header, quiet)
  req_content <- httr::content(req)
  process_url <- paste0('https:', req_content$url)
  
  # 2) New conversation
  
  req2 <- api_new_conversation(process_url, auth_header, input_file_url, quiet)
  req2_content <- httr::content(req2)
  
  # 3) Check the status & wait for success or failure
  
  req3 <- api_check_status(process_url, auth_header, quiet)
  
  # 4) Download data
  download_url <- paste0('https:',
                         httr::content(req3)$output$url)
  req4 <- api_download(download_url, auth_header, output_file, quiet)
  
  # 5) Write to file
  writeBin(req4$content, con = output_file)
  if(!quiet) message(paste0('Data saved to file', output_file))
} 

api_new_process <- function(base_url, auth_header, quiet){
  req <- httr::POST(base_url,
                    auth_header,
                    httr::content_type_json(),
                    body = '{
                        "inputformat": "pdf",
                        "outputformat": "html"
                      }')
  
  if(httr::http_error(req)){
    stop(sprintf(
      "Initiation of new process failed [%s]", 
      httr::status_code(req)
    ),
    call. = FALSE)
  } else if(!quiet) message('New process initiated')
  req
}

api_new_conversation <- function(process_url, auth_header, input_file_url, quiet){
  req2 <- httr::POST(process_url,
                     auth_header,
                     httr::content_type_json(),
                     body = paste0(
                       '{
                          "input": "download",
                          "file": "',
                       input_file_url,
                       '",
                          "outputformat": "html"
                       }'
                     ))
  
  if(httr::http_error(req2)){
    stop(sprintf(
      "Initiation of new conversation failed [%s]", 
      httr::status_code(req2)
    ),
    call. = FALSE)
  } else if(!quiet) message('New conversation initiated')
  req2
}

api_check_status <- function(process_url, auth_header, quiet){
  req3 <- httr::GET(paste0(process_url, '?wait'),
                    auth_header)
  
  if(httr::http_error(req3)){
    stop(sprintf(
      "Conversion failed [%s]", 
      httr::status_code(req3)
    ),
    call. = FALSE)
  } else if(!quiet) message('Conversion finished. Downloading...')
  req3
}

api_download <- function(download_url, auth_header, output_file, quiet){
  req4 <- httr::GET(download_url,
                    auth_header)
  if(httr::http_error(req4)){
    stop(sprintf(
      "Download failed [%s]", 
      httr::status_code(req4)
    ),
    call. = FALSE)
  } else if(!quiet) message(paste0('Data downloaded, writing into file ', output_file))
  req4
}