CREATE TABLE state_queue_mutation_leases (
  token TEXT PRIMARY KEY,
  scope TEXT NOT NULL,
  holder TEXT NOT NULL,
  status TEXT NOT NULL,
  acquired_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMPTZ NOT NULL,
  released_at TIMESTAMPTZ,
  last_error TEXT,
  CHECK (scope IN ('state_queue')),
  CHECK (status IN ('active', 'released', 'failed')),
  CHECK (
    (status = 'active' AND released_at IS NULL)
    OR (status <> 'active' AND released_at IS NOT NULL)
  )
);

CREATE UNIQUE INDEX uniq_state_queue_mutation_leases_active_scope
  ON state_queue_mutation_leases (scope)
  WHERE status = 'active';

CREATE INDEX idx_state_queue_mutation_leases_status_updated
  ON state_queue_mutation_leases (status, acquired_at);
