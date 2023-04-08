test_that(
  "the drivers list is correct",
  {
    expect_equal(
      c(
        "PqConnection",
        "Snowflake",
        "Vertica Database",
        "duckdb_connection",
        "MySQLConnection",
        "SQLiteConnection",
        "Microsoft SQL Server"
      ),
      list_drivers()
    )
  }
)
