-- class: D-x; retention: one row, updated in place by the follower-change driver
-- The node's write gate (plan §8.1). A guarded write checks the follower
-- view it was computed at (`viewValid`, under `FOR SHARE` on the follower
-- cursor), then takes this row `FOR UPDATE` in the same transaction and
-- requires that no driver recompute is pending and that its permit's epoch
-- is the row's. The driver bumps `epoch` and sets `pending_reason` before a
-- recompute (a rebase, an orphan repair, its first view after a start), so
-- every permit taken before it is refused, and clears them with the view it
-- applied once the recompute and the cache reload are done. `applied_*` is
-- the follower view the driver last applied, the view permits are taken at.
CREATE TABLE node_follower_write_gate (
  singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
  epoch bigint NOT NULL DEFAULT 0,
  applied_generation bigint,
  applied_slot bigint,
  applied_hash bytea,
  applied_height bigint,
  pending_reason text,
  pending_detail text,
  updated_at timestamptz NOT NULL DEFAULT NOW(),
  CHECK ((applied_generation IS NULL) = (applied_slot IS NULL)
    AND (applied_slot IS NULL) = (applied_hash IS NULL)
    AND (applied_hash IS NULL) = (applied_height IS NULL))
);
INSERT INTO node_follower_write_gate (singleton) VALUES (true);

-- The deployment (its manifest id) whose settlement jobs this node enqueues,
-- recorded by the startup preparation before the driver's first view, so
-- before any write that consumes a deposit or finalizes a withdrawal. It
-- replaces the history authority's deployment identity as the enqueue
-- trigger's source; a database that authority served keeps its deployment
-- (a one-time carry-over, not a seed row).
CREATE TABLE node_deployment (
  singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
  deployment_id text NOT NULL CHECK (deployment_id ~ '^[0-9a-f]{64}$'),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);
DO $carry$
BEGIN
  INSERT INTO node_deployment (deployment_id)
    SELECT encode(deployment_identity, 'hex') FROM event_history_authority
    WHERE singleton;
END;
$carry$;
CREATE OR REPLACE FUNCTION enqueue_automatic_settlement() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE event_kind text; initial_phase text;
BEGIN
  IF TG_TABLE_NAME = 'deposits_utxos' THEN
    IF NEW.status <> 'consumed' THEN RETURN NEW; END IF;
    event_kind := 'deposit'; initial_phase := 'absorb';
  ELSE
    IF NEW.status <> 'finalized' OR NEW.validity IS DISTINCT FROM 'WithdrawalIsValid' THEN RETURN NEW; END IF;
    event_kind := 'withdrawal'; initial_phase := 'initialize';
  END IF;
  INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
    SELECT deployment_id, event_kind, encode(NEW.event_id, 'hex'), initial_phase
    FROM node_deployment WHERE singleton
    ON CONFLICT DO NOTHING;
  RETURN NEW;
END;
$$;
