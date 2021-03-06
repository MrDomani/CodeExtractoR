context('convert working properly')

library(httptest)

input_file1 <- system.file('extdata/test.pdf', package = 'CodeExtractoR')
input_file2 <- system.file('extdata/test1.pdf', package = 'CodeExtractoR')

input_url1 <- 'https://journal.r-project.org/archive/2018/RJ-2018-041/RJ-2018-041.pdf'
input_url2 <- 'https://journal.r-project.org/archive/2018/RJ-2018-053/RJ-2018-053.pdf'
file.create('I_already_exist.html')

api_key <- '1234567890'

# httptest::start_capturing()
httptest::with_mock_API(
  test_that('Function working properly',{
    expect_true({
      convert_pdf_2_html(input_url1, 'output1.html', api_key = api_key)
      file.exists('output1.html')
    })
    expect_gt(file.size('output1.html'), 0)
  })
)

httptest::with_mock_API(
  test_that('Incorrect input_file argument caught', {
    expect_error(convert_pdf_2_html(input_file = input_file1,
                                    output_file = 'output1.R', 
                                    api_key = api_key),
                 regexp = 'URL')
    
  })
)
httptest::with_mock_API(
  test_that('Incorrect output_file argument caught',{
    expect_error(convert_pdf_2_html(input_file = input_url1,
                                    output_file = 'output.txt', 
                                    api_key = api_key),
                 regexp = 'extension')
    expect_error(convert_pdf_2_html(input_file = input_url1,
                                       output_file = 'I_already_exist.html',
                                    api_key = api_key),
                 regexp = 'exist')
  })
)
httptest::with_mock_API(
  test_that('Overwrite argument works properly',{
    expect_gt({convert_pdf_2_html(input_file = input_url1,
                                    output_file = 'I_already_exist.html',
                                    overwrite = TRUE,
                                  api_key = api_key)
      file.size('I_already_exist.html')
      },0)
  })
)
# httptest::stop_capturing()
  
if(file.exists('I_already_exist.html')) file.remove('I_already_exist.html')
if(file.exists('output1.html')) file.remove('output1.html')
if(file.exists('output2.html')) file.remove('output2.html')
if(file.exists('RJ-2018-041.html')) file.remove('RJ-2018-041.html')