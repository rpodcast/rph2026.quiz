# rpharma2026.quiz

A timed R/Pharma conference quiz, built with golem for the Ricochet platform.

Each user gets 15 questions, drawn at random from a bank of 50. They see one question at a time, under a single time limit for the whole quiz. Answers are saved as they go, so a user who reloads or switches devices picks up the same questions with the clock still running. When time runs out, the quiz is submitted and unanswered questions count as wrong. The results page shows the score, each question with its correct answer and explanation, and a leaderboard ranked by score, then time taken. Each user gets one attempt.

Answer options are shuffled for each user. In the bank, every correct answer is listed as option `a`.

## Run it locally

```r
golem::run_dev()
```

Development mode uses a mock user ("Test User") and a SQLite file at `persistent/quiz.sqlite3`. Delete that file to take the quiz again.

## Configuration

Settings live in `inst/golem-config.yml`. You can also pass any of them to `run_app()`, for example `run_app(quiz_time_limit_minutes = 2)`.

| Key | Default | Production |
|---|---|---|
| `quiz_bank` | `quiz/dev_quiz.json` (under `inst/app`; silly placeholder questions) | `s3://rpodcast-misc/rpharma_quiz.json` |
| `quiz_n_questions` | 15 | |
| `quiz_time_limit_minutes` | 10 | |
| `quiz_grace_seconds` | 5 | |
| `leaderboard_size` | 10 | |
| `auth_mode` | `mock` | `ricochet` |
| `db_backend` | `sqlite` | `postgres` |
| `sqlite_path` | `persistent/quiz.sqlite3` | |

## Deploying to Ricochet

`app.R` is the entrypoint. It sets `GOLEM_CONFIG_ACTIVE=production` unless that variable is already set. In production:

- The user comes from the `X-Ricochet-User` JWT. The token is verified against `RICOCHET_URL/.well-known/jwks.json`, and its audience must match `RICOCHET_CONTENT_ID`. If there is no valid token, the user sees a sign-in screen and cannot take the quiz.
- The question bank is downloaded once per R process from the private `rpodcast-misc` bucket on Linode Object Storage. Set these Ricochet secrets: `AWS_ACCESS_KEY`, `AWS_SECRET_ACCESS_KEY`, and `AWS_ENDPOINT` (`us-ord-1.linodeobjects.com`). `AWS_REGION` is optional; it defaults to the first part of the endpoint host. If the download or validation fails, the app does not start.
- Answers are stored in Postgres. Set these Ricochet secrets: `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD`, and optionally `PGSSLMODE`. The app creates its tables on first start.

To keep SQLite in production instead, set `db_backend: sqlite` in the `production` section of the config.
