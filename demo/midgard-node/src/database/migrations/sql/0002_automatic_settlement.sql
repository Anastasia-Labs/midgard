-- Operational journals are committed before a signed transaction leaves the node.
CREATE TABLE settlement_jobs (
    deployment_id text NOT NULL,
    kind text NOT NULL CHECK (kind IN ('deposit', 'withdrawal')),
    event_id text NOT NULL,
    phase text NOT NULL CHECK (phase IN ('absorb', 'initialize', 'fund', 'conclude', 'complete')),
    due_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    last_error text,
    failures integer NOT NULL DEFAULT 0,
    verified_generation bigint NOT NULL DEFAULT -1,
    PRIMARY KEY (deployment_id, kind, event_id)
);
CREATE INDEX settlement_jobs_due ON settlement_jobs (deployment_id, due_at, created_at) WHERE phase <> 'complete';
CREATE INDEX settlement_jobs_generation ON settlement_jobs (deployment_id, verified_generation) WHERE phase = 'complete';
CREATE TABLE settlement_attempts (
    deployment_id text NOT NULL,
    tx_hash text PRIMARY KEY CHECK (tx_hash ~ '^[0-9a-f]{64}$'),
    kind text NOT NULL,
    event_id text NOT NULL,
    phase text NOT NULL CHECK (phase IN ('absorb', 'initialize', 'fund', 'conclude')),
    signed_cbor text NOT NULL,
    required_outputs integer[] NOT NULL,
    fee_inputs text[] NOT NULL CHECK (cardinality(fee_inputs) > 0),
    recovery boolean NOT NULL DEFAULT false,
    hold_slot bigint NOT NULL CHECK (hold_slot >= 0),
    status text NOT NULL CHECK (status IN ('pending', 'confirmed', 'expired')),
    created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    FOREIGN KEY (deployment_id, kind, event_id) REFERENCES settlement_jobs(deployment_id, kind, event_id)
);
CREATE UNIQUE INDEX settlement_one_pending ON settlement_attempts (deployment_id) WHERE status = 'pending';
CREATE INDEX settlement_attempts_event ON settlement_attempts (deployment_id, kind, event_id, created_at);
CREATE INDEX settlement_confirmed_fee_inputs ON settlement_attempts USING gin (fee_inputs) WHERE status = 'confirmed';
CREATE TABLE settlement_owners (
    deployment_id text PRIMARY KEY,
    wallet_address text NOT NULL UNIQUE,
    owner_token uuid NOT NULL,
    lease_until timestamptz NOT NULL
);

-- Enqueue with the local finalization transaction. No polling scan of the
-- ever-growing event tables is needed in steady state.
CREATE FUNCTION enqueue_automatic_settlement() RETURNS trigger LANGUAGE plpgsql AS $$
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
    SELECT encode(deployment_identity, 'hex'), event_kind, encode(NEW.event_id, 'hex'), initial_phase
    FROM event_history_authority WHERE singleton
    ON CONFLICT DO NOTHING;
  RETURN NEW;
END;
$$;
CREATE TRIGGER deposit_automatic_settlement AFTER INSERT OR UPDATE OF status ON deposits_utxos
  FOR EACH ROW EXECUTE FUNCTION enqueue_automatic_settlement();
CREATE TRIGGER withdrawal_automatic_settlement AFTER INSERT OR UPDATE OF status, validity ON withdrawal_utxos
  FOR EACH ROW EXECUTE FUNCTION enqueue_automatic_settlement();

-- One-time discovery for an existing deployment; no local or chain reset.
INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
  SELECT encode(a.deployment_identity, 'hex'), 'deposit', encode(d.event_id, 'hex'), 'absorb'
  FROM event_history_authority a CROSS JOIN deposits_utxos d WHERE d.status = 'consumed';
INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
  SELECT encode(a.deployment_identity, 'hex'), 'withdrawal', encode(w.event_id, 'hex'), 'initialize'
  FROM event_history_authority a CROSS JOIN withdrawal_utxos w
  WHERE w.status = 'finalized' AND w.validity = 'WithdrawalIsValid';
