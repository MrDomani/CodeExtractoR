#' Convert PDF to HTML
#' 
#' This is wrapper around API to \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}.
#' 
#' @param input_file_url A \strong{URL} to pdf file to process. Do \strong{not} supply path to file in your filesystem.
#' @param output_file A name of file to write .html to. Must be of .html extension. By default, the name of the file is extracted from \code{input_file_url}.
#' @param quiet Boolean. Should the method display messages about progress? Errors will be displayed regardless of this option.
#' @param api_key A custom API key obtained from \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}. Make sure to use the key of \strong{version 1}.
#' 
#' @details 
#' Create your own account at \href{https://cloudconvert.com/register}{cloudconvert} and obtain your own API key, or convert the file(s) manually.
#' 
#' @return A HTML file with name \code{output_file} is created. It is ready to be passed into \code{\link{extract_code_from_html}} function.
#' @seealso \code{\link{extract_code_from_html}}, \code{\link{extract_code_from_pdf}}
#' @export


# Przyjmuje URL do PDFa na wejściu i konwertuje na HTML za pośrednictwem cloudconvert
convert_pdf_2_html <- function(input_file_url, output_file = NULL, quiet = FALSE, api_key = NULL){
  input_file_name <- basename(input_file_url)
  if(file.exists(input_file_url)) stop('input_file_url must be URL to file, available online. See ?convert_pdf_to_html')
  if(tolower(tools::file_ext(input_file_name)) != 'pdf') stop(paste0('Input file must be of pdf extension, not', 
                                                                tools::file_ext(input_file_name),
                                                                '.'))
  if(!is.null(output_file)){
    if(tolower(tools::file_ext(output_file)) != 'html') stop(paste0('Output file must be of html extension, not', 
                                                                  tools::file_ext(output_file),
                                                                  '.'))
    if(file.exists(output_file)) stop(paste0(output_file), 'already exists.')
  }
  else{
    output_file <- stringi::stri_replace_last(input_file_name,
                                             'html',
                                             fixed = 'pdf')
  }
  
  if(is.null(api_key)) api_key <- readLines(system.file('extdata/api_key.txt', package = 'CodeExtractoR'))
  base_url <- "https://api.cloudconvert.com/v1/process"
  authorization_header <- httr::add_headers(Authorization = paste0('Bearer ',
                                                                   api_key))
  
  # 1) New process ID
  
  req <- httr::POST(base_url,
              authorization_header,
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
  } else message('New process initiated')
  req_content <- httr::content(req)
  process_url <- paste0('https:', req_content$url)
  
  # 2) New conversation
  
  req2 <- httr::POST(process_url,
               authorization_header,
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
  } else message('New conversation initiated')
  
  req2_content <- httr::content(req2)
  
  # Check the status & wait for success or failure
  
  req3 <- httr::GET(paste0(process_url, '?wait'),
                       authorization_header)
  
  if(httr::http_error(req3)){
    stop(sprintf(
      "Conversion failed [%s]", 
      httr::status_code(req3)
    ),
    call. = FALSE)
  } else message('Conversion finished. Downloading...')
  
  # Download data
  download_url <- paste0('https:',
                         httr::content(req3)$output$url)
  req4 <- httr::GET(download_url,
              authorization_header)
  if(httr::http_error(req3)){
    stop(sprintf(
      "Download failed [%s]", 
      httr::status_code(req3)
    ),
    call. = FALSE)
  } else message(paste0('Data downloaded, writing into file ', output_file))
  
  # Write to file
  writeBin(req4$content, con = output_file)
  message(paste0('Data saved to file', output_file))
} 
# doc_url <- "https://journal.r-project.org/archive/2014/RJ-2014-011/RJ-2014-011.pdf"
# api_key_v1 <- "WJMTcdHTp9NMB1JazDUDWZBU39q2UJYWx75KSFpwMZ32GhP500tuk6VVxJPh6i9R"
# api_key2 <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjI2ODIzNGQ5NzY0ZDA5YWRjM2U3YjYyMTIyNDU0NGNkMWY5OGEzYTA4ODJjNDZjODg3NzJmMmI2MGM0NTc3NTgwNjg5NDBhMjY5ZmE4YzVkIn0.eyJhdWQiOiIxIiwianRpIjoiMjY4MjM0ZDk3NjRkMDlhZGMzZTdiNjIxMjI0NTQ0Y2QxZjk4YTNhMDg4MmM0NmM4ODc3MmYyYjYwYzQ1Nzc1ODA2ODk0MGEyNjlmYThjNWQiLCJpYXQiOjE1ODYxNzM5NjEsIm5iZiI6MTU4NjE3Mzk2MSwiZXhwIjo0NzQxODQ3NTYxLCJzdWIiOiI0MTM4NTY4MSIsInNjb3BlcyI6WyJ0YXNrLnJlYWQiLCJ0YXNrLndyaXRlIiwicHJlc2V0LnJlYWQiLCJwcmVzZXQud3JpdGUiXX0.eq_HHuXjJKDrP5n8WmgdivOrOBfDTNHJ06HyTCqVZfUV4sjSvm9WojFBXeDTa5IqBcPnlR5WXhBcCST4qhC-E9qkuZ6bAE1EFXRfKhuHlie4vP8Pe4Fpe1jxJmLC_3lPOgv6FgKjSr-wCp4hnFOJwFo2Y2SiDyEmtDiVz4qKY2EGzIBQH1CHkeNGcw9K_Arg1LMXN7E_uFoOTqqrtgxDYn8mSUlunQwNMesyznnLX9g27-yWalYVvQm3Ld0q_HfDNRAoIpUWA8xE308pVC4Hu7AueSL-yCNGQurS2Smh8-_kSq7ErpvcG16lv26MzxxZsum0DpsVrA8cDbN8qbk5USFiGJgdhWFSsCSQKiwF1jVqP82_VICp0Vb9BwvYGlSVbMQGov1dT6bwZfN5eTJli-FT_F1Sm7GEiZP4rzAxKANw28iu7l8aDhkdfzil7j93rgvguV4W2ppkSYDVksqOVGv9q5XkdHUsuwOyu7wTz7nIsY_ua3V65tqBB0AioXPogK0f8qBoitPw8apddtzTinCIS-vtAafVzQ5UtcQrLo8WsQP2Pqp1eaDk4S0r0kpBNgWtzLpDE1GRGE6AJak0j1Tnmcy4vZrUhvumuOUommop-KNLg4wkbI675STwxoB-YM0oXym-gxKNtGQf48020QiO3Vr4PyXPEggHZd2Sd18"
