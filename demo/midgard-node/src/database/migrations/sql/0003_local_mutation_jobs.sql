CREATE TABLE local_mutation_jobs (
  job_id TEXT PRIMARY KEY,
  kind TEXT NOT NULL,
  status TEXT NOT NULL,
  plan_hash BYTEA CHECK (plan_hash IS NULL OR octet_length(plan_hash) = 32),
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  attempts INTEGER NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMPTZ,
  CHECK (kind IN ('local_block_finalization', 'confirmed_merge_finalization')),
  CHECK (status IN ('running', 'completed', 'failed')),
  CHECK (
    (status = 'completed' AND completed_at IS NOT NULL)
    OR (status <> 'completed' AND completed_at IS NULL)
  )
);

CREATE INDEX idx_local_mutation_jobs_status_updated
  ON local_mutation_jobs (status, updated_at);
