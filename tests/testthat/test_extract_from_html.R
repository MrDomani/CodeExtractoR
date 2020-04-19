context('extract_from_html working properly')

input_file1 <- system.file('extdata/RJ-2018-041.html', package = 'CodeExtractoR')
input_file2 <- system.file('extdata/RJ-2018-053.html', package = 'CodeExtractoR')

if(file.exists('I_do_not_exist.html')) file.remove('I_do_not_exist.html')
file.create('I_already_exist.R')

test_that('Function working properly',{
  expect_true({
    extract_code_from_html(input_file1)
    file.exists('RJ-2018-041.R')
  })
  expect_gt(file.size('RJ-2018-041.R'), 0)
  expect_true({
    extract_code_from_html(input_file2)
    file.exists('RJ-2018-053.R')
  })
  expect_gt(file.size('RJ-2018-053.R'), 0)
})

test_that('Output_file argument working',{
  expect_true({
    extract_code_from_html(input_file1, 'output1.R')
    file.exists('output1.R')
  })
  expect_gt(file.size('output1.R'), 0)
})

test_that('Incorrect input_file argument caught',{
  expect_error(extract_code_from_html(input_file = 'I_do_not_exist.html',
                                      output_file = 'output1.R'))
  
})

test_that('Incorrect output_file argument caught',{
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.txt'))
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'I_already_exist.R'))
})

test_that('Incorrect filter argument caught',{
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      filter = 'ABC'))
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      filter = c(TRUE, FALSE)))
})

test_that('Incorrect bibliography argument caught',{
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      bibliography = 'ABC'))
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      bibliography = c(TRUE, FALSE)))
})

test_that('Incorrect console_char argument caught',{
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      console_char = 123))
  expect_error(extract_code_from_html(input_file = input_file1,
                                      output_file = 'output.R',
                                      console_char = LETTERS[1:3]))
})


test_that('Overwrite argument works properly',{
  expect_gt({extract_code_from_html(input_file = input_file1,
                                output_file = 'I_already_exist.R',
                                overwrite = TRUE)
    file.size('I_already_exist.R')
  },0)
})

if(file.exists('I_already_exist.R')) file.remove('I_already_exist.R')
if(file.exists('output1.R')) file.remove('output1.R')
if(file.exists('output2.R')) file.remove('output2.R')
if(file.exists('RJ-2018-041.R')) file.remove('RJ-2018-041.R')
if(file.exists('RJ-2018-053.R')) file.remove('RJ-2018-053.R')