-- class: D; retention: unchanged (two columns on `node_import_checks`, kept and deleted with the entry)
-- An import check killed at its wall-clock limit has no verdict: the entry
-- waits as `timed_out` until `retry_at_ms`, when the backlog lane runs it
-- again on spare capacity, until its deadline. `timeouts` counts its kills,
-- for the backoff between retries. Both survive a restart.
ALTER TABLE node_import_checks
  DROP CONSTRAINT node_import_checks_state_check,
  ADD CONSTRAINT node_import_checks_state_check CHECK (state IN ('pending',
    'cap_skipped', 'cache_skipped', 'timed_out', 'faulted', 'rejected')),
  ADD COLUMN timeouts INTEGER NOT NULL DEFAULT 0 CHECK (timeouts >= 0),
  ADD COLUMN retry_at_ms BIGINT,
  ADD CONSTRAINT node_import_checks_retry_check
    CHECK (state <> 'timed_out' OR retry_at_ms IS NOT NULL);
