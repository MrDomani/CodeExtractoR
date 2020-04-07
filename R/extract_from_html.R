#' Extract code snippets from HTML file
#' 
#' Search for code snippets, based on their unique font. Some extra may be extracted (like in-line mentions of functions or HTTP links).
#' 
#' @param input_file name of file to extract code from. Must be of HTML extension \strong{and} be produced by converter using pdf2htmlex (like \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}).
#' @param output_file name of file to write code to. Must be of R extension.
#' @param filter Boolean. Should all words written with \code{code font} be extracted, or should they be filted first? See Details.
#' @param bibliography Boolean. Should the words be extracted from bibliography too?
#' @param console_char Single character used in document to indicate start of a command. Assumed to be regex. If supported, from all lines starting with this character the character will be removed and all lines \strong{not} starting with this character will be commented. 
#' 
#' @details
#' Often the unfiltered results contain unnecessary occurences of words written with \code{code} font. 
#' If \code{filter} is set to \code{TRUE}, it is assumed, that we are only intersted in words, that are \strong{not} accompanied by words in different fonts in the same line.
#' This prevents us from extracting names of functions mentioned inside paragraphs of ordinary text, for example.
#' 
#' @seealso \code{\link{extract_code_from_pdf}}
#' @import stringi
#' @import rex
#' @export

extract_code_from_html <- function(input_file, 
                                   output_file, 
                                   filter = TRUE, 
                                   bibliography = FALSE,
                                   console_char = NULL){
  
  if(!file.exists(input_file)) stop(paste0(input_file, ' does not exist'))
  if(!tolower(tools::file_ext(input_file)) == 'html') stop(paste0(input_file,
                                                                'is not off .html extension'))
  if(file.exists(output_file)) stop(paste0(output_file, ' exists!'))
  if(!toupper(tools::file_ext(output_file)) == 'R') stop(paste0(output_file,
                                                                ' must be of .R extension'))
  if(!is.logical(filter)) stop(paste0('filter must be logical, not', class(filter)[1]))
  if(!length(filter) == 1) stop(paste0('filter must be of length 1, not', length(filter)))
  if(!is.logical(bibliography)) stop(paste0('bibliography must be logical, not', class(bibliography)[1]))
  if(!length(bibliography) == 1) stop(paste0('bibliography must be of length 1, not', length(bibliography)))
  if(!is.null(console_char)){
    if(!is.character(console_char)) stop(paste0('console_char must be character, not', class(console_char)[1]))
    if(!length(console_char) == 1) stop(paste0('console_char must be of length 1, not', length(console_char)))
  }
  readLines(input_file) -> file_as_chr
  
  div_regex <- rex('<div',
                   not('>'),
                   'y',
                   not('>'),
                   'ff',
                   not('>'),
                   '>',
                   not('<div'),
                   '</div>')
  
  body <- file_as_chr[1:length(file_as_chr) >= which(stri_detect_fixed(file_as_chr,
                                              pattern = '<body>'))]
  
  if(!bibliography){
    bibliographys <- which(stri_detect_fixed(body, 'Bibliography'))
    body <- body[1:length(body) <= bibliographys[length(bibliographys)]]
  }
  # Robimy wektor nisko-poziomowych divów
  
  lowest_divs <- stri_extract_all_regex(body,
                                        pattern = div_regex,
                                        omit_no_match = TRUE)
  lowest_divs <- unlist(lowest_divs)
  
  # Dzielimy divy ze względu na linijkę, w której się znajdują (wartość y w class)
  
  divs_classes <- stri_extract_first_regex(str = lowest_divs,
                                      pattern = 'class=\"[^"]*\"')
  
  y_regex <- rex(alnums %if_prev_is% 'y')
  
  divs_ys <- stri_extract_first_regex(divs_classes,
                                      pattern = y_regex)
  
  # Wysokości są przechowywane jako liczby heksadecymalne!
  
  ys <- base::strtoi(divs_ys, base = 16)
  
  # Dzielimy wektor ze względu na wysokość
  
  divs_splited <- split(lowest_divs,
                        f = factor(ys))
  
  # Wyciągamy wszystko, co siedzi w środku divów z klasą z czcionką ff4 lub ff9
  lapply(divs_splited,
         function(str) stri_detect_regex(str,
                                         pattern = '<div[^>]*class=\"[^"]*ff[49][^"]*\"[^>]*>')) -> l
  if(filter){
    divs <- unlist(divs_splited[sapply(l, all)])
  } else
    divs <- unlist(divs_splited)[unlist(l)]
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
  if(!is.null(console_char)){
    pattern <- paste0('^', console_char)
    is_starting <- stri_detect_regex(with_correct_chars,
                                     pattern)
    final_vector[is_starting] <- stri_replace_first_regex(with_correct_chars[is_starting],
                                                          pattern = console_char,
                                                          replacement = '')
    final_vector[!is_starting] <- paste0('# ', with_correct_chars[!is_starting])
  }
  writeLines(final_vector, output_file)  
}
