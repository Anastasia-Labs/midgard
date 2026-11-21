-- Event rows bind to the L1 follower's admission identity (plan §5.4, N1)
-- instead of the event-history journal's incarnations: the event key and the
-- immutable admission output, exactly as the follower's never-reuse key set
-- `l1_event_keys` records them (origin_outref = 32-byte tx hash || u16
-- big-endian output index). A row's admission is canonical while
-- `l1_event_keys` holds the same (kind, key, origin_outref); a follower rewind
-- past the admission deletes that key and orphans the row. Null identity is an
-- unassociated local row and is never eligible.
ALTER TABLE deposits_utxos
    ADD COLUMN l1_event_key bytea,
    ADD COLUMN l1_origin_outref bytea;
ALTER TABLE withdrawal_utxos
    ADD COLUMN l1_event_key bytea,
    ADD COLUMN l1_origin_outref bytea;
ALTER TABLE pending_block_finalization_deposits
    ADD COLUMN l1_event_key bytea,
    ADD COLUMN l1_origin_outref bytea;
ALTER TABLE pending_block_finalization_withdrawals
    ADD COLUMN l1_event_key bytea,
    ADD COLUMN l1_origin_outref bytea;

-- Carry every journal association over to the identity it names.
UPDATE deposits_utxos d SET l1_event_key = i.event_key,
    l1_origin_outref = decode(r.rec #>> '{event,outRef,txHash}', 'hex')
      || substring(int4send((r.rec #>> '{event,outRef,outputIndex}')::int) from 3 for 2)
  FROM event_history_incarnations i, LATERAL (SELECT i.incarnation_record::jsonb AS rec) r
  WHERE i.binding_digest = d.history_binding_digest AND i.incarnation_id = d.history_incarnation_id;
UPDATE withdrawal_utxos w SET l1_event_key = i.event_key,
    l1_origin_outref = decode(r.rec #>> '{event,outRef,txHash}', 'hex')
      || substring(int4send((r.rec #>> '{event,outRef,outputIndex}')::int) from 3 for 2)
  FROM event_history_incarnations i, LATERAL (SELECT i.incarnation_record::jsonb AS rec) r
  WHERE i.binding_digest = w.history_binding_digest AND i.incarnation_id = w.history_incarnation_id;
UPDATE pending_block_finalization_deposits m SET l1_event_key = i.event_key,
    l1_origin_outref = decode(r.rec #>> '{event,outRef,txHash}', 'hex')
      || substring(int4send((r.rec #>> '{event,outRef,outputIndex}')::int) from 3 for 2)
  FROM event_history_incarnations i, LATERAL (SELECT i.incarnation_record::jsonb AS rec) r
  WHERE i.binding_digest = m.history_binding_digest AND i.incarnation_id = m.history_incarnation_id;
UPDATE pending_block_finalization_withdrawals m SET l1_event_key = i.event_key,
    l1_origin_outref = decode(r.rec #>> '{event,outRef,txHash}', 'hex')
      || substring(int4send((r.rec #>> '{event,outRef,outputIndex}')::int) from 3 for 2)
  FROM event_history_incarnations i, LATERAL (SELECT i.incarnation_record::jsonb AS rec) r
  WHERE i.binding_digest = m.history_binding_digest AND i.incarnation_id = m.history_incarnation_id;

-- Retained jsonb row images (ledger receipts' consumed deposits, foreign
-- adoptions' event before-images) carry the same identity, so an inverse
-- applied after this migration still matches the row it captured.
CREATE FUNCTION pg_temp.follower_identity_images(images jsonb) RETURNS jsonb
LANGUAGE sql AS $$
  SELECT COALESCE(jsonb_agg((e.image - 'history_binding_digest' - 'history_incarnation_id')
      || jsonb_build_object(
        'l1_event_key', CASE WHEN i.event_key IS NULL THEN NULL
          ELSE '\x' || encode(i.event_key, 'hex') END,
        'l1_origin_outref', CASE WHEN i.event_key IS NULL THEN NULL
          ELSE '\x' || encode(decode(r.rec #>> '{event,outRef,txHash}', 'hex')
            || substring(int4send((r.rec #>> '{event,outRef,outputIndex}')::int) from 3 for 2), 'hex') END)
      ORDER BY e.ordinal), '[]'::jsonb)
  FROM jsonb_array_elements(images) WITH ORDINALITY AS e(image, ordinal)
  LEFT JOIN event_history_incarnations i
    ON i.binding_digest = decode(substring(e.image ->> 'history_binding_digest' from 3), 'hex')
    AND i.incarnation_id = decode(substring(e.image ->> 'history_incarnation_id' from 3), 'hex')
  LEFT JOIN LATERAL (SELECT i.incarnation_record::jsonb AS rec) r ON true
$$;
UPDATE event_history_l2_ledger_receipts
  SET deposits_before = pg_temp.follower_identity_images(deposits_before)
  WHERE jsonb_array_length(deposits_before) > 0;
UPDATE foreign_native_adoptions
  SET event_before = event_before
    || jsonb_build_object('deposits', pg_temp.follower_identity_images(event_before -> 'deposits'))
    || jsonb_build_object('withdrawals', pg_temp.follower_identity_images(event_before -> 'withdrawals'))
  WHERE jsonb_typeof(event_before -> 'deposits') = 'array'
    AND jsonb_typeof(event_before -> 'withdrawals') = 'array';
DROP FUNCTION pg_temp.follower_identity_images(jsonb);

-- A deposit's L1 tx hash is its admission tx (ruling 2): immutable, never the
-- moving location of its list node.
UPDATE deposits_utxos SET deposit_l1_tx_hash = substring(l1_origin_outref from 1 for 32)
  WHERE l1_origin_outref IS NOT NULL;

DROP INDEX idx_deposits_utxos_history_association;
DROP INDEX idx_withdrawal_utxos_history_association;
ALTER TABLE deposits_utxos
    DROP CONSTRAINT deposits_utxos_history_association_fkey,
    DROP CONSTRAINT deposits_utxos_history_association_check,
    DROP COLUMN history_binding_digest,
    DROP COLUMN history_incarnation_id;
ALTER TABLE withdrawal_utxos
    DROP CONSTRAINT withdrawal_utxos_history_association_fkey,
    DROP CONSTRAINT withdrawal_utxos_history_association_check,
    DROP COLUMN history_binding_digest,
    DROP COLUMN history_incarnation_id;
ALTER TABLE pending_block_finalization_deposits
    DROP CONSTRAINT pending_block_finalization_deposits_member_id_fkey,
    DROP CONSTRAINT pending_block_finalization_deposits_history_association_fkey,
    DROP CONSTRAINT pending_block_finalization_deposits_history_association_check,
    DROP COLUMN history_binding_digest,
    DROP COLUMN history_incarnation_id;
ALTER TABLE pending_block_finalization_withdrawals
    DROP CONSTRAINT pending_block_finalization_withdrawals_member_id_fkey,
    DROP CONSTRAINT pending_block_finalization_withdrawals_history_association_fkey,
    DROP CONSTRAINT pending_block_finalization_withdrawals_history_association_check,
    DROP COLUMN history_binding_digest,
    DROP COLUMN history_incarnation_id;

ALTER TABLE deposits_utxos
    ADD CONSTRAINT deposits_utxos_l1_admission_check CHECK (
      (l1_event_key IS NULL AND l1_origin_outref IS NULL) OR
      (l1_event_key IS NOT NULL AND l1_origin_outref IS NOT NULL
       AND octet_length(l1_event_key) = 32 AND octet_length(l1_origin_outref) = 34
       AND substring(l1_origin_outref from 1 for 32) = deposit_l1_tx_hash));
ALTER TABLE withdrawal_utxos
    ADD CONSTRAINT withdrawal_utxos_l1_admission_check CHECK (
      (l1_event_key IS NULL AND l1_origin_outref IS NULL) OR
      (l1_event_key IS NOT NULL AND l1_origin_outref IS NOT NULL
       AND octet_length(l1_event_key) = 32 AND octet_length(l1_origin_outref) = 34));
ALTER TABLE pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_l1_admission_check CHECK (
      (l1_event_key IS NULL AND l1_origin_outref IS NULL) OR
      (l1_event_key IS NOT NULL AND l1_origin_outref IS NOT NULL
       AND octet_length(l1_event_key) = 32 AND octet_length(l1_origin_outref) = 34));
ALTER TABLE pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_l1_admission_check CHECK (
      (l1_event_key IS NULL AND l1_origin_outref IS NULL) OR
      (l1_event_key IS NOT NULL AND l1_origin_outref IS NOT NULL
       AND octet_length(l1_event_key) = 32 AND octet_length(l1_origin_outref) = 34));

-- Orphan repair and the eligibility checks find event rows by admission key;
-- the withdrawal status lookup finds them by admission tx.
CREATE INDEX idx_deposits_utxos_l1_admission
    ON deposits_utxos(l1_event_key);
CREATE INDEX idx_withdrawal_utxos_l1_admission
    ON withdrawal_utxos(l1_event_key);
CREATE INDEX idx_withdrawal_utxos_l1_admission_tx
    ON withdrawal_utxos(substring(l1_origin_outref from 1 for 32));

-- class: D-c; retention: one row, replaced by every follower-change driver run (a full pass rebuilds it)
-- The follower view through which the driver last ingested every admission:
-- the commit horizon never passes it while it is still on the follower's chain.
CREATE TABLE follower_event_ingestion (
  id boolean PRIMARY KEY DEFAULT true CHECK (id),
  generation bigint NOT NULL CHECK (generation >= 0),
  slot bigint NOT NULL CHECK (slot >= 0),
  block_hash bytea NOT NULL CHECK (octet_length(block_hash) = 32),
  height bigint NOT NULL CHECK (height >= 0),
  ingested_through_ms bigint NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT NOW()
);
