-- DA payloads no longer carry the UTxO set (plan DS-1), so the planner and
-- the pre-submit size check no longer need the encoded size of the ledger.
-- The aggregate columns that recorded it, and their pairing constraints, are
-- dropped from the MPF engine state and the own-block journal.
ALTER TABLE mpf_engine_state
    DROP CONSTRAINT mpf_engine_state_utxo_payload_size_pair_check,
    DROP COLUMN utxo_payload_entry_count,
    DROP COLUMN utxo_payload_encoded_tuple_bytes;

ALTER TABLE pending_block_finalizations
    DROP CONSTRAINT pending_block_finalizations_utxo_payload_size_pair_check,
    DROP COLUMN utxo_payload_entry_count,
    DROP COLUMN utxo_payload_encoded_tuple_bytes;
