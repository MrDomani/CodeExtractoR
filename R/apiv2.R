#' Convert PDF to HTML
#' 
#' This is wrapper around API to \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}.
#' 
#' @export


# Przyjmuje PDF na wejściu i konwertuje na HTML za pośrednictwem cloudconvert
convert_pdf_2_html_v2 <- function(input_file, output_file = NULL){
  if(tolower(tools::file_ext(input_file)) != 'pdf') stop(paste0('Input file must be of pdf extension, not', 
                                                                tools::file_ext(input_file),
                                                                '.'))
  if(!is.null(output_file)){
    if(tolower(tools::file_ext(output_file)) != 'html') stop(paste0('Input file must be of html extension, not', 
                                                                    tools::file_ext(input_file),
                                                                    '.'))
    if(file.exists(output_file)) stop(paste0(output_file), 'already exists.')
  }
  else{
    output_file <- stringi::stri_replace_last(input_file,
                                              '%03d.html',
                                              fixed = '.pdf')
  }
  base_url <- "https://api.cloudconvert.com/v2/jobs"
  base_url_upload <- 'https://api.cloudconvert.com/v2/import/upload'
  
  # Initiate job and task
  request_body <- paste0('{
                      "tasks": {
                          "import": {
                              "operation": "import/upload"
                              },
                          "task": {
                              "operation": "convert",
                              "input_format": "pdf",
                              "output_format": "html",
                              "engine": "pdf2htmlex",
                              "input": [
                                  "import"
                              ],
                              "outline": false,
                              "zoom": 1.5,
                              "embed_css": true,
                              "embed_javascript": true,
                              "embed_images": true,
                              "embed_fonts": true,
                              "bg_format": "png",
                              "filename": "',output_file,
                         '"},
                          "export": {
                              "operation": "export/url",
                              "input": [
                                  "task"
                              ],
                              "inline": false,
                              "archive_multiple_files": false
                          }
                      }
                  }')
  request <- httr::POST(url = base_url,
                        httr::add_headers(Authorization = paste0('Bearer ',
                                                                 api_key2)),
                        httr::content_type_json(),
                        body = request_body)
  request_content <- httr::content(request)
  id <- request_content$id
  upload_url <- request_content$data$tasks[[2]]$result$form$url
  # Check if finished:
  check_request <- httr::GET(url = paste(base_url,
                                         id,
                                         'wait',
                                         sep = '/'),
                             httr::add_headers(Authorization = paste0('Bearer ',
                                                                      api_key2)))
  check_request <- httr::POST(url = base_url_upload,
                              httr::add_headers(Authorization = paste0('Bearer ',
                                                                       api_key2)))
  content(check_request) -> con_ch_req
  con_ch_req$data$result$form$url -> upload_url
  form_data <- request_content$data$tasks[[2]]$result$form
  RCurl::postForm(upload_url,
                  expires = form_data$parameters$expires,
                  max_file_count = form_data$parameters$max_file_count,
                  max_file_size = form_data$parameters$max_file_size,
                  signature = form_data$parameters$signature,
                  file = paste0('@', input_file)
  ) -> form_req
  PUT(paste0(upload_url, 
             input_file),
      httr::add_headers(Authorization = paste0('Bearer ',
                                               api_key2))
  ) -> req_put
}


api_key <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjZhY2E5MDYzYWMwYWVmMmY4NDhjMDQ2ZDk5NjQ5NjQyMTI0Y2Y2YTMxYmQ2ZjIxOTE2MDY0ZTVhMGI2Nzg4YzA2NzIwZTIzNTg5MzA1M2VmIn0.eyJhdWQiOiIxIiwianRpIjoiNmFjYTkwNjNhYzBhZWYyZjg0OGMwNDZkOTk2NDk2NDIxMjRjZjZhMzFiZDZmMjE5MTYwNjRlNWEwYjY3ODhjMDY3MjBlMjM1ODkzMDUzZWYiLCJpYXQiOjE1ODYxNzAyNDQsIm5iZiI6MTU4NjE3MDI0NCwiZXhwIjo0NzQxODQzODQ0LCJzdWIiOiI0MTM4NTY4MSIsInNjb3BlcyI6W119.Du40ZkPPTTuub_C5RwNMmqm1PxsF_DdjnUJ2a5g4zf-iNnq83lQNcSwHiXNACqF8jHR37U65pJ3wJulhwoUTNytbSJiXxyaZbBPJj9zDmeBcPXycEKed74-IfV_diicP9eyZCXgpIattmF35MoZyQt4n8K0OrRlUQ0BRU5rM5fcnWAwvtxstpbBwaDdtmmuRY_79wo0OadYRJ1Pz90TG_6vtla3qYanj-z58n8zCRKqNFULBxf-Xqc40jGdA5EyHssHIkTwdw6K3i3mSw91m8aCQ4BZ3SmjpVbkwLwjcXii8wKmENiPycbC3h5_1aFr_sSKs5eSEvyqEbuusN5-_d6vVc2s7Il0syIE_bCCGP66fhk5MlQ4n8QAz0L1gVBXMIOILNg8IhaY8msbOH1fvrGV5L_dEHhYf_58RTL-QSDsvNLUbxumamHbIqcwLzfENtP3hoGMi1D3kitIGdVnvpuMHu3EExNqBmqcrql8aDx2e5W_2unbFNG8RCqiE8F2pNciQ5PXxh4GeDv9zxmFprM0A48PmI9rHKub-z9eN_Ol0J2oNZmkfln0AS-nEPiavg2-qi3L70Px6i40_i5Gn09RVNK6dtk_7Dxe6SN0S8dXe0vt7BkZBZaPwCvItlJflIcDqCsCVhWPUnlTqNj3bhmIjcwGGgf9rT6Ltn90UXvo"
api_key2 <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjI2ODIzNGQ5NzY0ZDA5YWRjM2U3YjYyMTIyNDU0NGNkMWY5OGEzYTA4ODJjNDZjODg3NzJmMmI2MGM0NTc3NTgwNjg5NDBhMjY5ZmE4YzVkIn0.eyJhdWQiOiIxIiwianRpIjoiMjY4MjM0ZDk3NjRkMDlhZGMzZTdiNjIxMjI0NTQ0Y2QxZjk4YTNhMDg4MmM0NmM4ODc3MmYyYjYwYzQ1Nzc1ODA2ODk0MGEyNjlmYThjNWQiLCJpYXQiOjE1ODYxNzM5NjEsIm5iZiI6MTU4NjE3Mzk2MSwiZXhwIjo0NzQxODQ3NTYxLCJzdWIiOiI0MTM4NTY4MSIsInNjb3BlcyI6WyJ0YXNrLnJlYWQiLCJ0YXNrLndyaXRlIiwicHJlc2V0LnJlYWQiLCJwcmVzZXQud3JpdGUiXX0.eq_HHuXjJKDrP5n8WmgdivOrOBfDTNHJ06HyTCqVZfUV4sjSvm9WojFBXeDTa5IqBcPnlR5WXhBcCST4qhC-E9qkuZ6bAE1EFXRfKhuHlie4vP8Pe4Fpe1jxJmLC_3lPOgv6FgKjSr-wCp4hnFOJwFo2Y2SiDyEmtDiVz4qKY2EGzIBQH1CHkeNGcw9K_Arg1LMXN7E_uFoOTqqrtgxDYn8mSUlunQwNMesyznnLX9g27-yWalYVvQm3Ld0q_HfDNRAoIpUWA8xE308pVC4Hu7AueSL-yCNGQurS2Smh8-_kSq7ErpvcG16lv26MzxxZsum0DpsVrA8cDbN8qbk5USFiGJgdhWFSsCSQKiwF1jVqP82_VICp0Vb9BwvYGlSVbMQGov1dT6bwZfN5eTJli-FT_F1Sm7GEiZP4rzAxKANw28iu7l8aDhkdfzil7j93rgvguV4W2ppkSYDVksqOVGv9q5XkdHUsuwOyu7wTz7nIsY_ua3V65tqBB0AioXPogK0f8qBoitPw8apddtzTinCIS-vtAafVzQ5UtcQrLo8WsQP2Pqp1eaDk4S0r0kpBNgWtzLpDE1GRGE6AJak0j1Tnmcy4vZrUhvumuOUommop-KNLg4wkbI675STwxoB-YM0oXym-gxKNtGQf48020QiO3Vr4PyXPEggHZd2Sd18"
