# Initialize the production Postgres database.
#
# Creates the quiz tables (same schema as the SQLite dev database) if they do
# not exist yet, then prints the resulting columns. Safe to re-run: existing
# tables and data are left untouched. Credentials come from the PG_*
# variables in .Renviron.

pkgload::load_all(golem::pkg_path(), quiet = TRUE)

db <- db_connect("postgres", use_pool = FALSE)
db_create_tables(db)
print(db_query(
  db,
  "SELECT table_name, column_name, data_type, is_nullable
   FROM information_schema.columns
   WHERE table_schema = current_schema()
     AND table_name IN ('attempts', 'answers')
   ORDER BY table_name, ordinal_position"
))
print(db_query(
  db,
  "SELECT (SELECT count(*) FROM attempts) AS n_attempts,
          (SELECT count(*) FROM answers) AS n_answers"
))
db_close(db)
