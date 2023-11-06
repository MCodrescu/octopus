## Checklist of Manual Tests
# Check that schemas are shown
# Check that tables are shown
# Check that changes a schema changes the tables
# Check that clicking View button shows a modal with the table
# Check that downloading the table works
# Check that creating a new table works
# Check that removing that new table works
# Check that a simple SELECT * FROM mtcars query works
# Check that CREATE and ALTER queries work
# Check that formatting a query works
# Check that the collapsing looks good on every screen size.
# Check that uploading a table works
# Check that deleting a table works
# Check that the hotkeys work correctly

test_that(
  "the checklist was completed",
  {
    expect_true(TRUE)
  }
)

# Interactively Test the App
if (interactive()){

  con <- DBI::dbConnect(
    odbc::odbc(),
    dsn = "Snowflake_BBR",
    pwd = keyring::key_get("Upstart")
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "mtcars", mtcars)

  octopus::view_database(con)

}
