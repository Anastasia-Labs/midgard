-- Flat changes script identity and retained wire formats. Never reinterpret or
-- rewrite existing transaction bytes, roots, signed DA payloads or journals:
-- the migration runs only on a database that holds no node state, and refuses
-- (SQLSTATE MGF29) any other. A deployment that holds state starts from a
-- fresh genesis on a new database.
DO $flat$
DECLARE
    state_table text;
    has_state boolean;
BEGIN
    FOREACH state_table IN ARRAY ARRAY[
        'node_deployment', 'cek_program_material_entries',
        'cek_program_material_memberships', 'cek_program_material_admission_owners',
        'cek_program_material_retained_state_owners', 'tx_admission_payloads',
        'tx_admissions', 'tx_rejections', 'tx_rejection_causes', 'address_history',
        'deposits_utxos', 'withdrawal_utxos', 'local_mutation_jobs',
        'deposit_submission_attempts', 'event_history_submissions',
        'event_history_submission_inputs', 'settlement_jobs', 'settlement_attempts',
        'settlement_owners', 'node_landed_blocks', 'node_confirmed_ledger_frontier',
        'node_confirmed_ledger_spent', 'node_confirmed_merges',
        'node_import_check_blocks', 'node_import_checks', 'node_import_check_outputs',
        'follower_event_ingestion', 'intent_refusal_holds',
        'operator_membership_observations', 'state_queue_mutation_leases',
        'mempool_tx_deltas', 'mempool', 'processed_mempool', 'immutable',
        'confirmed_ledger', 'mempool_ledger', 'mpf_engine_state', 'blocks',
        'pending_block_finalizations', 'pending_block_finalization_txs',
        'pending_block_finalization_deposits',
        'pending_block_finalization_withdrawals',
        'pending_block_finalization_forced_transactions',
        'pending_block_finalization_transition_trace',
        'pending_block_finalization_event_to_step',
        'pending_block_finalization_validation_traces', 'forced_transaction_utxos',
        'da_payloads', 'da_payload_publications', 'da_payload_announcements'
    ] LOOP
        -- Hold each lock until the migration transaction commits, so no write
        -- lands between this check and the schema change.
        EXECUTE format('LOCK TABLE public.%I IN ACCESS EXCLUSIVE MODE', state_table);
        EXECUTE format('SELECT EXISTS (SELECT 1 FROM public.%I)', state_table)
            INTO has_state;
        IF has_state THEN
            RAISE EXCEPTION 'Flat-script migration needs a fresh node database: % retains state', state_table
                USING ERRCODE = 'MGF29',
                      HINT = 'Flat scripts start from a fresh genesis. Point the node at a new, empty database and run db:migrate there.';
        END IF;
    END LOOP;
END;
$flat$;

-- Flat script bytes remain in canonical transactions and ledger outputs.
-- Derived CEK material is local dispute tooling, not durable node state.
DROP TABLE public.cek_program_material_admission_owners;
DROP TABLE public.cek_program_material_retained_state_owners;
DROP TABLE public.cek_program_material_memberships;
DROP TABLE public.cek_program_material_entries;

ALTER TABLE public.tx_admission_payloads
    DROP CONSTRAINT tx_admission_payloads_cek_program_material_sidecar_check,
    DROP COLUMN cek_program_material_sidecar_cbor,
    DROP COLUMN cek_program_material_sidecar_sha256;

ALTER TABLE public.pending_block_finalization_txs
    DROP CONSTRAINT pending_block_finalization_txs_cek_program_material_check,
    DROP COLUMN cek_program_material_sidecar_cbor,
    DROP COLUMN cek_program_material_sidecar_sha256;

ALTER TABLE public.forced_transaction_utxos
    DROP CONSTRAINT forced_transaction_utxos_exact_proof_material,
    DROP COLUMN cek_program_material_sidecar_cbor,
    DROP COLUMN cek_program_material_sidecar_sha256,
    ADD CONSTRAINT forced_transaction_utxos_exact_transaction CHECK (
        consensus_profile_id = 'midgard-consensus-v1'
        AND octet_length(native_tx_cbor) <= 295041
        AND octet_length(transaction_commitment) = 32
    );

ALTER TABLE public.node_import_checks
    DROP COLUMN program_material_sidecar;
