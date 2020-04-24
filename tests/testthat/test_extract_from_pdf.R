context('extract_from_pdf working properly')

library(httptest)

input_file1 <- system.file('extdata/test.pdf', package = 'CodeExtractoR')
input_file2 <- system.file('extdata/test1.pdf', package = 'CodeExtractoR')

input_url1 <- 'https://journal.r-project.org/archive/2018/RJ-2018-041/RJ-2018-041.pdf'
input_url2 <- 'https://journal.r-project.org/archive/2018/RJ-2018-053/RJ-2018-053.pdf'
file.create('I_already_exist.R')

api_key <- '1234567890'

# httptest::start_capturing()

httptest::with_mock_api(
  test_that('Function working properly',{
    expect_true({
      extract_code_from_pdf(input_url1, 'output1.R', api_key = api_key)
      file.exists('output1.R')
    })
    expect_gt(file.size('output1.R'), 0)
  })
)

test_that('Incorrect input_file argument caught',{
  expect_error(extract_code_from_pdf(input_file = input_file1,
                                      output_file = 'output1.R',
                                     api_key = api_key))
  
})

httptest::with_mock_api(
  test_that('Incorrect output_file argument caught',{
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.txt',
                                       api_key = api_key))
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'I_already_exist.R',
                                       api_key = api_key))
  })
)

httptest::with_mock_API(
  test_that('Incorrect filter argument caught',{
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        filter = 'ABC',
                                       api_key = api_key))
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        filter = c(TRUE, FALSE),
                                       api_key = api_key))
  })
)

httptest::with_mock_api(
  test_that('Incorrect bibliography argument caught',{
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        bibliography = 'ABC',
                                       api_key = api_key))
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        bibliography = c(TRUE, FALSE),
                                       api_key = api_key))
  })
)

httptest::with_mock_API(
  test_that('Incorrect console_char argument caught',{
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        console_char = 123,
                                       api_key = api_key))
    expect_error(extract_code_from_pdf(input_file = input_url1,
                                        output_file = 'output.R',
                                        console_char = LETTERS[1:3],
                                       api_key = api_key))
  })
)
if(file.exists('I_already_exist.R')) file.remove('I_already_exist.R')
if(file.exists('output1.R')) file.remove('output1.R')
if(file.exists('output2.R')) file.remove('output2.R')
if(file.exists('RJ-2018-041.html')) file.remove('RJ-2018-041.html')