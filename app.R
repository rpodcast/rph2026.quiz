# Ricochet entrypoint (Do not remove this comment)
# Runs the packaged golem app in production mode: verified Ricochet users and
# Postgres. Set GOLEM_CONFIG_ACTIVE to another config to override.

if (!nzchar(Sys.getenv("GOLEM_CONFIG_ACTIVE"))) {
  Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
}

pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE
)
options("golem.app.prod" = TRUE)
# Resolve through the package env so Shiny's R/ autoloading cannot shadow it
pkgload::pkg_env("rpharma2026.quiz")$run_app()
