#' Extract code snippets from HTML file
#'
#' Search for code snippets, based on their unique font. Some extra may be extracted (like in-line mentions of functions or HTTP links).
#'
#' @param input_file name of file to extract code from. Must be of HTML extension \strong{and} be produced by converter using pdf2htmlex (like \href{https://cloudconvert.com/pdf-to-html}{cloudconvert}).
#' @param output_file name of file to write code to. Must be of R extension. By default its name is extracted from \code{input_file}.
#' @param filter Boolean. Should all words written with \code{code font} be extracted, or should they be filted first? See Details.
#' @param bibliography Boolean. Should the words be extracted from bibliography too?
#' @param console_char Single character used in document to indicate start of a command. Assumed to be regex. If supported, from all lines starting with this character the character will be removed and all lines \strong{not} starting with this character will be commented.
#' @param overwrite Boolean. Should the output file (.R) be overwritten?
#'
#' @details
#' Often the unfiltered results contain unnecessary occurrences of words written with \code{code} font.
#' If \code{filter} is set to \code{TRUE}, it is assumed, that we are only interested in words, that are \strong{not} accompanied by words in different fonts in the same line.
#' This prevents us from extracting names of functions mentioned inside paragraphs of ordinary text, for example.
#' 
#' @return 
#' An .R file is created, containing text \strong{suspected} to be code. Unfortunately, various articles use various fonts for the code included.
#' The texts in .R file are grouped based on their font in the \code{input_file}. 
#' User is encouraged to compare the results with the article.
#'
#' @seealso \code{\link{extract_code_from_pdf}}
#' @import stringi
#' @export

extract_code_from_html <- function(input_file,
                                   output_file = NULL,
                                   filter = TRUE,
                                   bibliography = FALSE,
                                   console_char = NULL,
                                   overwrite = FALSE) {
  if (!file.exists(input_file))
    stop(paste0(input_file, ' does not exist'))
  if (!tolower(tools::file_ext(input_file)) == 'html')
    stop(paste0(input_file,
                'is not off .html extension'))
  output_file <-
    check_output_file(output_file, input_file, overwrite)
  if (!is.logical(filter))
    stop(paste0('filter must be logical, not', class(filter)[1]))
  if (!length(filter) == 1)
    stop(paste0('filter must be of length 1, not', length(filter)))
  if (!is.logical(bibliography))
    stop(paste0('bibliography must be logical, not', class(bibliography)[1]))
  if (!length(bibliography) == 1)
    stop(paste0('bibliography must be of length 1, not', length(bibliography)))
  if (!is.null(console_char)) {
    if (!is.character(console_char))
      stop(paste0('console_char must be character, not', class(console_char)[1]))
    if (!length(console_char) == 1)
      stop(paste0('console_char must be of length 1, not', length(console_char)))
  }
  readLines(input_file) -> file_as_chr
  
  # Stworzone pakietem rex
  # library(rex)
  # div_regex <- rex('<div',
  #                  not('>'),
  #                  'y',
  #                  not('>'),
  #                  'ff',
  #                  not('>'),
  #                  '>',
  #                  not('<div'),
  #                  '</div>')
  div_regex <-
    "<div(?:(?!>).)*y(?:(?!>).)*ff(?:(?!>).)*>(?:(?!<div).)*</div>"
  
  body <-
    file_as_chr[1:length(file_as_chr) >= which(stri_detect_fixed(file_as_chr,
                                                                 pattern = '<body>'))]
  
  if (!bibliography) {
    bibliographys <- which(stri_detect_fixed(body, 'Bibliography'))
    body <-
      body[1:length(body) <= bibliographys[length(bibliographys)]]
  }
  # Robimy wektor nisko-poziomowych divów
  
  lowest_divs <- stri_extract_all_regex(body,
                                        pattern = div_regex,
                                        omit_no_match = TRUE)
  lowest_divs <- unlist(lowest_divs)
  
  # Dzielimy divy ze względu na linijkę, w której się znajdują (wartość y w class)
  
  divs_classes <- stri_extract_first_regex(str = lowest_divs,
                                           pattern = 'class=\"[^"]*\"')
  
  # y_regex <- rex(alnums %if_prev_is% 'y')
  y_regex <- "(?:(?<=y)[[:alnum:]]+)"
  
  divs_ys <- stri_extract_first_regex(divs_classes,
                                      pattern = y_regex)
  
  # Wysokości są przechowywane jako liczby heksadecymalne!
  
  ys <- base::strtoi(divs_ys, base = 16)
  
  # Uwaga: równa wartość y nie znaczy, że w dokumencie elementy rzeczywiście znajdują się koło siebie. Np nagłówek
  
  y_diffs <- diff(ys)
  
  # jeśli natrafiamy na sekwencję n, -n lub -n, n - to spłaszczamy (zamieniamy na 0,0)
  
  #pair_sums <- y_diffs[1:(length(y_diffs) - 1)] + y_diffs[2:length(y_diffs)]
  #y_diffs[sort(unique(c(which(pair_sums == 0), which(pair_sums == 0) + 1)))] <- 0
  
  y_diffs[abs(y_diffs) >= 1] <- 1
  y_classes <- cumsum(c(1, y_diffs))
  
  # Dzielimy wektor ze względu na wysokość
  
  divs_splited <- split(lowest_divs,
                        f = factor(y_classes))
  
  # Wyciągamy wszystko, co siedzi w środku divów z klasą z odpowiednimi czcionkami
  l <- lapply(divs_splited,
              function(str)
                stri_detect_regex(str,
                                  pattern = '<div[^>]*class=\"[^"]*ff[4-9abe][^"]*\"[^>]*>'))
  if (filter) {
    # Jeśli pojawią się spany z innymi czcionkami, to omijamy
    good_divs <- sapply(l , all)
    divs <- unlist(divs_splited[good_divs])
    # divs <- unlist(divs_splited[good_divs & !wrong_spans])
  } else
    divs <- unlist(divs_splited)[unlist(l)]
  
  
  if (filter) {
    wrong_spans <- stri_detect_regex(divs,
                                     pattern = '<span[^>]*class=\"[^"]*ff[^4-9abe][^"]*\"[^>]*>')
    divs <- divs[!wrong_spans]
  }
  
  # Zakładamy, że cały kod jest pisany jedną, tą samą czcionką
  divs_classes <- stri_extract_first_regex(str = divs,
                                           pattern = 'class=\"[^"]*\"')
  
  fonts <- factor(stri_extract_first_regex(divs, pattern = "ff\\w+"))
  divs_splited <- split(divs, fonts)
  divs_sorted <-
    unlist(divs_splited[order(sapply(divs_splited, length), decreasing = TRUE)])
  
  # Czyścimy z divów na początku i na końcu:
  without_divs <- stri_replace_all_regex(
    divs_sorted,
    pattern =  c('^<div[^>]*>',
                 '</div>$'),
    replacement = c('', ''),
    vectorize_all = FALSE
  )
  
  # Czyścimy z tagów 'span'
  without_spans <- stri_replace_all_regex(
    without_divs,
    pattern = c('<span[^>]*>',
                '</span>'),
    replacement = c('', ''),
    vectorize_all = FALSE
  )
  # zamieniamy HTMLowe entities
  with_correct_chars <- stri_replace_all_fixed(
    without_spans,
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
    vectorize_all = FALSE
  )
  with_correct_chars -> final_vector
  if (!is.null(console_char)) {
    pattern <- paste0('^\\s*((', console_char, ')|[+])\\s*')
    is_starting <- stri_detect_regex(with_correct_chars,
                                     pattern)
    final_vector[is_starting] <-
      stri_replace_first_regex(with_correct_chars[is_starting],
                               pattern = pattern,
                               replacement = '')
    final_vector[!is_starting] <-
      paste0('# ', with_correct_chars[!is_starting])
  }
  writeLines(final_vector, output_file)
}

check_output_file <- function(output_file, input_file, overwrite) {
  if (is.null(output_file))
    output_file <- stri_replace_last_fixed(basename(input_file),
                                           pattern = 'html',
                                           replacement = 'R')
  else{
    if (!toupper(tools::file_ext(output_file)) == 'R')
      stop(paste0(output_file,
                  ' must be of .R extension'))
  }
  if (file.exists(output_file)) {
    if (overwrite)
      file.remove(output_file)
    else
      stop(paste0(output_file, ' exists!'))
  }
  output_file
}
