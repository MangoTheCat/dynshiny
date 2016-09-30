
library(shiny)
library(shinytest)                      # github.com/mangothecat/shinytest
library(testthat)

context("dynshiny")

app <- shinyapp$new()

test_that("we have two records to start with", {

  expect_equal(
    app$list_output_widgets(),
    c("buttons", "records")
  )

  expect_equal(
    app$list_input_widgets(),
    c("file", "add", "inp-1", "del-1", "inp-2", "del-2")
  )

})

test_that("cancel and save button shows up on edit", {

  ## For this I would need the complex updates...
  
})
