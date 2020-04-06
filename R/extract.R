#' Extract code from PDF file
#' 
#' Extract code content from PDF file and write it to .R file.
#' @param input_file_url A \strong{URL} to pdf file to process. Do \strong{not} supply path to file in your filesystem.
#' @param output_file name of file to write code to. Must be of R extension.
#' @param output_html_file name of HTML file created inside function.
#' \itemize{
#' \item{if \code{NULL}, then the name of the file is extracted from the \code{input_file_url} and the file is created in working directory.
#'       if it exists and \code{clear} is \code{FALSE} then it is assumed to be \strong{product of convertion}, that already took place, and code is extracted from it.}
#' \item{if supplied and a file of that name exists, it is assumed to be a \strong{mistake}.}     
#' }
#' @param quiet Boolean. Should the method display messages about progress? Errors will be displayed regardless of this option.
#' @param api_key A custom API key obtained from \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}. Make sure to use the key of \strong{version 1}.
#' @param clear Should the HTML file be removed? If file exists before conversion, it will be removed too - even when \code{output_html_file} was not supplied.
#' @export
extract_code_from_pdf <- function(input_file_url, output_file, output_html_file = NULL, quiet = FALSE, api_key = NULL, clear = FALSE){
  if(is.null(output_html_file)) output_html_file <- stri_replace_last_fixed(basename(input_file_url),
                                                                            pattern = 'pdf',
                                                                            replacement = 'html')
  if(clear && file.exists(output_html_file)) file.remove(output_html_file)
  if(!file.exists(output_html_file))
    convert_pdf_2_html(input_file_url, output_file = output_html_file, quiet = quiet, api_key = api_key)
  else
    message(paste0(output_html_file,
                   'exists and clear was FALSE; extracting code from it. See ?extract_code_from_pdf'))
  extract_code_from_html(output_html_file, output_file)
  if(clear) file.remove(output_html_file)
}