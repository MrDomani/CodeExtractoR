#' Extract code snippets from HTML file
#' 
#' Search for code snippets, based on their unique font. Some extra may be extracted (like in-line mentions of functions or HTTP links).
#' 
#' @param input_file name of file to extract code from. Must be of HTML extension \strong{and} be produced by converter using pdf2htmlex (like \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}).
#' @param output_file name of file to write code to. Must be of R extension.
#' 
#' @import stringi
#' @export

extract_code_from_html <- function(input_file, output_file){
  
  if(!file.exists(input_file)) stop(paste0(input_file, ' does not exist'))
  if(!tolower(tools::file_ext(input_file)) == 'html') stop(paste0(input_file,
                                                                'is not off .html extension'))
  if(file.exists(output_file)) stop(paste0(output_file, ' exists!'))
  if(!toupper(tools::file_ext(output_file)) == 'R') stop(paste0(output_file,
                                                                ' must be of .R extension'))
  readLines(input_file) -> file_as_chr
  
  div_regex <- rex('<div',
                   not('>'),
                   'ff4',
                   not('>'),
                   '>',
                   not('</div>'),
                   '</div>')
  body <- file_as_chr[1:length(file_as_chr) >= which(stri_detect_fixed(file_as_chr,
                                              pattern = '<body>'))]
  
  # Wyciągamy wszystko, co siedzi w środku divów z klasą z czcionką ff4
  stri_extract_all_regex(body,
                        pattern = div_regex,
                        omit_no_match = TRUE) %>%
    unlist()-> divs
  # Czyścimy z divów na początku i na końcu:
  without_divs <- stri_replace_all_regex(divs,
                                         pattern =  c('^<div[^>]*>',
                                                      '</div>$'),
                                         replacement = c('',''),
                                         vectorize_all = FALSE)
  #Czyścimy z tagów 'span'
  without_spans <- stri_replace_all_regex(without_divs,
                                          pattern = c('<span[^>]*>',
                                                     '</span>'),
                                          replacement = c('',''),
                                          vectorize_all = FALSE)
  
  # zamieniamy HTMLowe entities
  with_correct_chars <- stri_replace_all_fixed(without_spans,
                                               pattern = c('&lt;',
                                                           '&gt;',
                                                           '&quot;',
                                                           '&apos;',
                                                           '&amp;'),
                                               replacement = c('<',
                                                               '>',
                                                               '\"',
                                                               '\'',
                                                               '&'),
                                               vectorize_all = FALSE)
  
  
  with_correct_chars -> final_vector
  writeLines(final_vector, output_file)  
  
}
