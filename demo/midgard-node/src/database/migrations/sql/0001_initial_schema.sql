-- Midgard fresh-install schema.
--
-- Pre-launch compatibility policy: this baseline intentionally replaces the
-- historical migration chain. Any existing local database must be discarded;
-- a network carrying prior Midgard state must also be fully redeployed.
CREATE TYPE public.tx_admission_status AS ENUM (
    'queued',
    'validating',
    'accepted',
    'rejected'
);


--
-- Name: address_history; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.address_history (
    tx_id bytea NOT NULL,
    address text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: blocks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.blocks (
    height integer NOT NULL,
    header_hash bytea NOT NULL,
    tx_id bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: blocks_height_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.blocks_height_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: blocks_height_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.blocks_height_seq OWNED BY public.blocks.height;


--
-- Name: commit_build_calibration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.commit_build_calibration (
    id smallint NOT NULL,
    ms_per_tx_ewma double precision NOT NULL,
    sample_count bigint DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT commit_build_calibration_id_check CHECK ((id = 1)),
    CONSTRAINT commit_build_calibration_ms_per_tx_ewma_check CHECK ((ms_per_tx_ewma > (0)::double precision)),
    CONSTRAINT commit_build_calibration_sample_count_check CHECK ((sample_count >= 0))
);


--
-- Name: confirmed_ledger; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.confirmed_ledger (
    tx_id bytea NOT NULL,
    outref bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: da_payload_announcements; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.da_payload_announcements (
    header_hash bytea NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_attempt_at timestamp with time zone,
    next_retry_at timestamp with time zone DEFAULT now(),
    published_at timestamp with time zone,
    last_error text,
    lease_owner text,
    lease_token text,
    lease_expires_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT da_payload_announcements_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT da_payload_announcements_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'failed'::text, 'published'::text])))
);


--
-- Name: da_payload_publications; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.da_payload_publications (
    header_hash bytea NOT NULL,
    peer_id text NOT NULL,
    signer_index integer NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_attempt_at timestamp with time zone,
    next_retry_at timestamp with time zone DEFAULT now(),
    accepted_at timestamp with time zone,
    last_error text,
    lease_owner text,
    lease_expires_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lease_token text,
    CONSTRAINT da_payload_publications_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT da_payload_publications_signer_index_check CHECK ((signer_index >= 0)),
    CONSTRAINT da_payload_publications_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'accepted'::text, 'duplicate'::text, 'conflict'::text, 'rejected'::text, 'transport_error'::text])))
);


--
-- Name: da_payloads; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.da_payloads (
    header_hash bytea NOT NULL,
    version integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    utxos_root text NOT NULL,
    forced_transactions_root text NOT NULL,
    transactions_root text NOT NULL,
    deposits_root text NOT NULL,
    withdrawals_root text NOT NULL,
    transition_trace_root text NOT NULL,
    event_to_step_root text NOT NULL,
    withdrawal_count bigint NOT NULL,
    forced_transaction_count bigint NOT NULL,
    l2_transaction_count bigint NOT NULL,
    deposit_count bigint NOT NULL,
    total_event_count bigint NOT NULL,
    transition_step_count bigint NOT NULL,
    block_start_time timestamp with time zone NOT NULL,
    block_end_time timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT da_payloads_check CHECK ((block_end_time >= block_start_time)),
    CONSTRAINT da_payloads_count_sum_check CHECK ((total_event_count = (((withdrawal_count + forced_transaction_count) + l2_transaction_count) + deposit_count))),
    CONSTRAINT da_payloads_deposit_count_check CHECK ((deposit_count >= 0)),
    CONSTRAINT da_payloads_deposits_root_check CHECK ((deposits_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_event_to_step_root_check CHECK ((event_to_step_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_forced_transaction_count_check CHECK ((forced_transaction_count >= 0)),
    CONSTRAINT da_payloads_forced_transactions_root_check CHECK ((forced_transactions_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT da_payloads_l2_transaction_count_check CHECK ((l2_transaction_count >= 0)),
    CONSTRAINT da_payloads_payload_cbor_check CHECK ((octet_length(payload_cbor) > 0)),
    CONSTRAINT da_payloads_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32)),
    CONSTRAINT da_payloads_total_event_count_check CHECK ((total_event_count >= 0)),
    CONSTRAINT da_payloads_trace_count_check CHECK ((transition_step_count = total_event_count)),
    CONSTRAINT da_payloads_transactions_root_check CHECK ((transactions_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_transition_step_count_check CHECK ((transition_step_count >= 0)),
    CONSTRAINT da_payloads_transition_trace_root_check CHECK ((transition_trace_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_version_v2_check CHECK ((version = 2)),
    CONSTRAINT da_payloads_withdrawal_count_check CHECK ((withdrawal_count >= 0)),
    CONSTRAINT da_payloads_withdrawals_root_check CHECK ((withdrawals_root ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: deposit_submission_attempts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.deposit_submission_attempts (
    tx_hash bytea NOT NULL,
    deposit_event_id bytea NOT NULL,
    signed_tx_cbor bytea NOT NULL,
    expected_deposit_out_ref text NOT NULL,
    expected_l2_address text NOT NULL,
    expected_lovelace text NOT NULL,
    expected_assets jsonb NOT NULL,
    metadata jsonb NOT NULL,
    dependency_out_refs jsonb NOT NULL,
    status text DEFAULT 'prepared'::text NOT NULL,
    prepared_at timestamp with time zone DEFAULT now() NOT NULL,
    attempt_count integer DEFAULT 0 NOT NULL,
    last_submission_at timestamp with time zone,
    submitted_at timestamp with time zone,
    provider_acknowledgement text,
    confirmed_at timestamp with time zone,
    last_reconciled_at timestamp with time zone,
    last_error text,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT deposit_submission_attempts_attempt_count_check CHECK ((attempt_count >= 0)),
    CONSTRAINT deposit_submission_attempts_attempt_timestamp_check CHECK ((((attempt_count = 0) AND (last_submission_at IS NULL)) OR ((attempt_count > 0) AND (last_submission_at IS NOT NULL)))),
    CONSTRAINT deposit_submission_attempts_deposit_event_id_check CHECK ((octet_length(deposit_event_id) > 0)),
    CONSTRAINT deposit_submission_attempts_error_check CHECK (((last_error IS NULL) OR (btrim(last_error) <> ''::text))),
    CONSTRAINT deposit_submission_attempts_dependency_out_refs_check CHECK ((jsonb_typeof(dependency_out_refs) = 'object'::text) AND (jsonb_typeof((dependency_out_refs -> 'spend'::text)) = 'array'::text) AND (jsonb_typeof((dependency_out_refs -> 'collateral'::text)) = 'array'::text) AND (jsonb_typeof((dependency_out_refs -> 'reference'::text)) = 'array'::text) AND ((dependency_out_refs - ARRAY['spend'::text, 'collateral'::text, 'reference'::text]) = '{}'::jsonb)),
    CONSTRAINT deposit_submission_attempts_lifecycle_check CHECK (((status = 'prepared'::text) AND (attempt_count = 0) AND (last_submission_at IS NULL) AND (submitted_at IS NULL) AND (provider_acknowledgement IS NULL) AND (confirmed_at IS NULL) AND (last_reconciled_at IS NULL) AND (last_error IS NULL)) OR ((status = 'submission_unknown'::text) AND (attempt_count > 0) AND (last_submission_at IS NOT NULL) AND (submitted_at IS NULL) AND (provider_acknowledgement IS NULL) AND (confirmed_at IS NULL)) OR ((status = 'submitted'::text) AND (attempt_count > 0) AND (last_submission_at IS NOT NULL) AND (submitted_at IS NOT NULL) AND (provider_acknowledgement IS NOT NULL) AND (confirmed_at IS NULL)) OR ((status = 'confirmed'::text) AND (confirmed_at IS NOT NULL) AND (last_error IS NULL)) OR ((status = 'reconciled_after_timeout'::text) AND (confirmed_at IS NOT NULL) AND (last_reconciled_at IS NOT NULL) AND (last_error IS NULL)) OR ((status = 'ambiguous'::text) AND (confirmed_at IS NULL) AND (last_reconciled_at IS NOT NULL) AND (last_error IS NOT NULL)) OR ((status = 'expired'::text) AND (confirmed_at IS NULL) AND (last_reconciled_at IS NOT NULL) AND (last_error IS NOT NULL))),
    CONSTRAINT deposit_submission_attempts_provider_acknowledgement_check CHECK (((provider_acknowledgement IS NULL) OR (btrim(provider_acknowledgement) <> ''::text))),
    CONSTRAINT deposit_submission_attempts_provider_timestamp_check CHECK ((((submitted_at IS NULL) AND (provider_acknowledgement IS NULL)) OR ((submitted_at IS NOT NULL) AND (provider_acknowledgement IS NOT NULL)))),
    CONSTRAINT deposit_submission_attempts_signed_tx_cbor_check CHECK ((octet_length(signed_tx_cbor) > 0)),
    CONSTRAINT deposit_submission_attempts_status_check CHECK ((status = ANY (ARRAY['prepared'::text, 'submission_unknown'::text, 'submitted'::text, 'confirmed'::text, 'reconciled_after_timeout'::text, 'ambiguous'::text, 'expired'::text]))),
    CONSTRAINT deposit_submission_attempts_timestamp_check CHECK (((last_submission_at IS NULL) OR (last_submission_at >= prepared_at)) AND ((submitted_at IS NULL) OR ((last_submission_at IS NOT NULL) AND (submitted_at >= last_submission_at))) AND ((confirmed_at IS NULL) OR (confirmed_at >= prepared_at)) AND ((last_reconciled_at IS NULL) OR (last_reconciled_at >= prepared_at)) AND (updated_at >= prepared_at)),
    CONSTRAINT deposit_submission_attempts_tx_hash_check CHECK ((octet_length(tx_hash) = 32))
);


--
-- Name: deposits_utxos; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.deposits_utxos (
    event_id bytea NOT NULL,
    event_info bytea NOT NULL,
    inclusion_time timestamp with time zone NOT NULL,
    deposit_l1_tx_hash bytea NOT NULL,
    ledger_tx_id bytea NOT NULL,
    ledger_output bytea NOT NULL,
    ledger_address text NOT NULL,
    projected_header_hash bytea,
    status text NOT NULL,
    CONSTRAINT deposits_utxos_check CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT deposits_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'consumed'::text])))
);


--
-- Name: forced_transaction_utxos; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.forced_transaction_utxos (
    tx_order_id bytea NOT NULL,
    tx_order_l1_tx_hash bytea NOT NULL,
    tx_order_l1_output_index integer NOT NULL,
    asset_name bytea NOT NULL,
    raw_datum bytea NOT NULL,
    tx_id bytea NOT NULL,
    tx_compact bytea NOT NULL,
    forced_inclusion_value bytea NOT NULL,
    operator_validity text NOT NULL,
    inclusion_time timestamp with time zone NOT NULL,
    projected_header_hash bytea,
    status text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forced_transaction_utxos_asset_name_check CHECK (((octet_length(asset_name) >= 1) AND (octet_length(asset_name) <= 32))),
    CONSTRAINT forced_transaction_utxos_check CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT forced_transaction_utxos_operator_validity_check CHECK ((operator_validity = ANY (ARRAY['TxIsValid'::text, 'NonExistentInputUtxo'::text, 'InvalidSignature'::text, 'FailedScript'::text, 'FeeTooLow'::text, 'UnbalancedTx'::text]))),
    CONSTRAINT forced_transaction_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'finalized'::text]))),
    CONSTRAINT forced_transaction_utxos_tx_id_check CHECK ((octet_length(tx_id) = 32)),
    CONSTRAINT forced_transaction_utxos_tx_order_l1_output_index_check CHECK ((tx_order_l1_output_index >= 0)),
    CONSTRAINT forced_transaction_utxos_tx_order_l1_tx_hash_check CHECK ((octet_length(tx_order_l1_tx_hash) = 32))
);


--
-- Name: foreign_tip_reconciliations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.foreign_tip_reconciliations (
    foreign_header_hash bytea NOT NULL,
    replaced_base_header_hash bytea NOT NULL,
    foreign_header_cbor bytea NOT NULL,
    block_start_time timestamp with time zone NOT NULL,
    block_end_time timestamp with time zone NOT NULL,
    deposits_root text NOT NULL,
    forced_transactions_root text NOT NULL,
    withdrawals_root text NOT NULL,
    deposit_count bigint NOT NULL,
    forced_transaction_count bigint NOT NULL,
    withdrawal_count bigint NOT NULL,
    verified_da_payload_cbor bytea,
    verified_da_schema_version integer,
    status text DEFAULT 'awaiting'::text NOT NULL,
    blocking_reason text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    resolved_at timestamp with time zone,
    CONSTRAINT foreign_tip_reconciliations_check CHECK ((block_end_time > block_start_time)),
    CONSTRAINT foreign_tip_reconciliations_check1 CHECK ((((verified_da_payload_cbor IS NULL) AND (verified_da_schema_version IS NULL)) OR ((verified_da_payload_cbor IS NOT NULL) AND (verified_da_schema_version IS NOT NULL)))),
    CONSTRAINT foreign_tip_reconciliations_check2 CHECK ((((status = 'resolved'::text) AND (resolved_at IS NOT NULL) AND (blocking_reason IS NULL)) OR ((status = 'awaiting'::text) AND (resolved_at IS NULL) AND (blocking_reason IS NOT NULL)))),
    CONSTRAINT foreign_tip_reconciliations_deposit_count_check CHECK ((deposit_count >= 0)),
    CONSTRAINT foreign_tip_reconciliations_deposits_root_check CHECK ((deposits_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT foreign_tip_reconciliations_forced_transaction_count_check CHECK ((forced_transaction_count >= 0)),
    CONSTRAINT foreign_tip_reconciliations_forced_transactions_root_check CHECK ((forced_transactions_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT foreign_tip_reconciliations_foreign_header_cbor_check CHECK ((octet_length(foreign_header_cbor) > 0)),
    CONSTRAINT foreign_tip_reconciliations_foreign_header_hash_check CHECK ((octet_length(foreign_header_hash) = 28)),
    CONSTRAINT foreign_tip_reconciliations_replaced_base_header_hash_check CHECK ((octet_length(replaced_base_header_hash) = 28)),
    CONSTRAINT foreign_tip_reconciliations_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'resolved'::text]))),
    CONSTRAINT foreign_tip_reconciliations_withdrawal_count_check CHECK ((withdrawal_count >= 0)),
    CONSTRAINT foreign_tip_reconciliations_withdrawals_root_check CHECK ((withdrawals_root ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: immutable; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.immutable (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: local_mutation_jobs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.local_mutation_jobs (
    job_id text NOT NULL,
    kind text NOT NULL,
    status text NOT NULL,
    plan_hash bytea,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    completed_at timestamp with time zone,
    CONSTRAINT local_mutation_jobs_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT local_mutation_jobs_check CHECK ((((status = 'completed'::text) AND (completed_at IS NOT NULL)) OR ((status <> 'completed'::text) AND (completed_at IS NULL)))),
    CONSTRAINT local_mutation_jobs_kind_check CHECK ((kind = ANY (ARRAY['local_block_finalization'::text, 'confirmed_merge_finalization'::text]))),
    CONSTRAINT local_mutation_jobs_plan_hash_check CHECK (((plan_hash IS NULL) OR (octet_length(plan_hash) = 32))),
    CONSTRAINT local_mutation_jobs_status_check CHECK ((status = ANY (ARRAY['running'::text, 'completed'::text, 'failed'::text])))
);


--
-- Name: mempool; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mempool (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');


--
-- Name: mempool_ledger; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mempool_ledger (
    tx_id bytea NOT NULL,
    outref bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL,
    source_event_id bytea,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');


--
-- Name: mempool_tx_deltas; Type: TABLE; Schema: public; Owner: -
--

CREATE UNLOGGED TABLE public.mempool_tx_deltas (
    tx_id bytea NOT NULL,
    spent_cbor bytea NOT NULL,
    produced_cbor bytea NOT NULL
);


--
-- Name: mpf_engine_state; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mpf_engine_state (
    store_name text NOT NULL,
    migration_version integer NOT NULL,
    root_hex text,
    audit_diverged boolean DEFAULT false NOT NULL,
    last_audit_at timestamp with time zone,
    lease_owner text,
    lease_expires_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_audit_diverged boolean DEFAULT false NOT NULL,
    utxo_payload_entry_count bigint,
    utxo_payload_encoded_tuple_bytes bigint,
    CONSTRAINT mpf_engine_state_migration_version_check CHECK ((migration_version >= 0)),
    CONSTRAINT mpf_engine_state_root_hex_check CHECK (((root_hex IS NULL) OR (root_hex ~ '^[0-9a-f]{64}$'::text))),
    CONSTRAINT mpf_engine_state_utxo_payload_size_pair_check CHECK ((((utxo_payload_entry_count IS NULL) AND (utxo_payload_encoded_tuple_bytes IS NULL)) OR ((utxo_payload_entry_count >= 0) AND (utxo_payload_encoded_tuple_bytes >= 0) AND ((utxo_payload_entry_count <> 0) OR (utxo_payload_encoded_tuple_bytes = 0)))))
);


--
-- Name: pending_block_finalization_deposits; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_deposits (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_deposits_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);


--
-- Name: pending_block_finalization_event_to_step; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_event_to_step (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_event_to_step_ordinal_check CHECK ((ordinal >= 0)),
    CONSTRAINT pending_block_finalization_event_to_step_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);


--
-- Name: pending_block_finalization_forced_transactions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_forced_transactions (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_forced_transact_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32)),
    CONSTRAINT pending_block_finalization_forced_transactions_ordinal_check CHECK ((ordinal >= 0))
);


--
-- Name: pending_block_finalization_transition_trace; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_transition_trace (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_transition_trac_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32)),
    CONSTRAINT pending_block_finalization_transition_trace_ordinal_check CHECK ((ordinal >= 0))
);


--
-- Name: pending_block_finalization_txs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_txs (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_txs_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);


--
-- Name: pending_block_finalization_utxos; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_utxos (
    header_hash bytea NOT NULL,
    outref bytea NOT NULL,
    ordinal integer NOT NULL,
    output bytea NOT NULL
);


--
-- Name: pending_block_finalization_withdrawals; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalization_withdrawals (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_withdrawals_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);


--
-- Name: pending_block_finalizations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_block_finalizations (
    header_hash bytea NOT NULL,
    submitted_tx_hash bytea,
    block_end_time timestamp with time zone NOT NULL,
    status text NOT NULL,
    observed_confirmed_at_ms bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    state_queue_lease_token text NOT NULL,
    base_snapshot_id text NOT NULL,
    base_tail_out_ref text NOT NULL,
    base_tail_header_hash bytea NOT NULL,
    base_tail_datum_cbor text NOT NULL,
    base_utxos_root text NOT NULL,
    base_transactions_root text NOT NULL,
    base_deposits_root text NOT NULL,
    base_withdrawals_root text NOT NULL,
    block_start_time timestamp with time zone NOT NULL,
    expected_utxos_root text NOT NULL,
    expected_transactions_root text NOT NULL,
    expected_deposits_root text NOT NULL,
    expected_withdrawals_root text NOT NULL,
    base_forced_transactions_root text NOT NULL,
    expected_forced_transactions_root text NOT NULL,
    header_cbor bytea NOT NULL,
    expected_transition_trace_root text NOT NULL,
    expected_event_to_step_root text NOT NULL,
    expected_withdrawal_count bigint NOT NULL,
    expected_forced_transaction_count bigint NOT NULL,
    expected_l2_transaction_count bigint NOT NULL,
    expected_deposit_count bigint NOT NULL,
    expected_total_event_count bigint NOT NULL,
    expected_transition_step_count bigint NOT NULL,
    ledger_delta_spent jsonb,
    ledger_delta_produced jsonb,
    utxo_payload_entry_count bigint,
    utxo_payload_encoded_tuple_bytes bigint,
    mpf_owner_schema smallint,
    mpf_owner_binary_sha256 bytea,
    mpf_replay_base_root bytea,
    mpf_replay_candidate_root bytea,
    mpf_replay_event_log bytea,
    mpf_replay_event_log_digest bytea,
    mpf_replay_event_roots bytea,
    mpf_replay_event_count integer,
    CONSTRAINT pending_block_finalizations_base_tail_header_hash_check CHECK ((octet_length(base_tail_header_hash) = 28)),
    CONSTRAINT pending_block_finalizations_expected_count_sum_check CHECK ((expected_total_event_count = (((expected_withdrawal_count + expected_forced_transaction_count) + expected_l2_transaction_count) + expected_deposit_count))),
    CONSTRAINT pending_block_finalizations_expected_deposit_count_check CHECK ((expected_deposit_count >= 0)),
    CONSTRAINT pending_block_finalizations_expected_event_to_step_root_check CHECK ((expected_event_to_step_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT pending_block_finalizations_expected_forced_transaction_c_check CHECK ((expected_forced_transaction_count >= 0)),
    CONSTRAINT pending_block_finalizations_expected_l2_transaction_count_check CHECK ((expected_l2_transaction_count >= 0)),
    CONSTRAINT pending_block_finalizations_expected_total_event_count_check CHECK ((expected_total_event_count >= 0)),
    CONSTRAINT pending_block_finalizations_expected_trace_count_check CHECK ((expected_transition_step_count = expected_total_event_count)),
    CONSTRAINT pending_block_finalizations_expected_transition_step_coun_check CHECK ((expected_transition_step_count >= 0)),
    CONSTRAINT pending_block_finalizations_expected_transition_trace_roo_check CHECK ((expected_transition_trace_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT pending_block_finalizations_expected_withdrawal_count_check CHECK ((expected_withdrawal_count >= 0)),
    CONSTRAINT pending_block_finalizations_header_cbor_check CHECK ((octet_length(header_cbor) > 0)),
    CONSTRAINT pending_block_finalizations_ledger_delta_pair_check CHECK ((((ledger_delta_spent IS NULL) AND (ledger_delta_produced IS NULL)) OR ((ledger_delta_spent IS NOT NULL) AND (ledger_delta_produced IS NOT NULL)))),
    CONSTRAINT pending_block_finalizations_mpf_replay_all_or_none_check CHECK ((((mpf_owner_schema IS NULL) AND (mpf_owner_binary_sha256 IS NULL) AND (mpf_replay_base_root IS NULL) AND (mpf_replay_candidate_root IS NULL) AND (mpf_replay_event_log IS NULL) AND (mpf_replay_event_log_digest IS NULL) AND (mpf_replay_event_roots IS NULL) AND (mpf_replay_event_count IS NULL)) OR ((mpf_owner_schema = 1) AND (octet_length(mpf_owner_binary_sha256) = 32) AND (octet_length(mpf_replay_base_root) = 32) AND (octet_length(mpf_replay_candidate_root) = 32) AND (octet_length(mpf_replay_event_log) >= 92) AND (octet_length(mpf_replay_event_log_digest) = 32) AND (mpf_replay_event_count >= 0) AND (octet_length(mpf_replay_event_roots) = (mpf_replay_event_count * 32))))),
    CONSTRAINT pending_block_finalizations_status_check CHECK ((status = ANY (ARRAY['pending_submission'::text, 'submitted_local_finalization_pending'::text, 'submitted_unconfirmed'::text, 'observed_waiting_stability'::text, 'finalized'::text, 'abandoned'::text]))),
    CONSTRAINT pending_block_finalizations_utxo_payload_size_pair_check CHECK ((((utxo_payload_entry_count IS NULL) AND (utxo_payload_encoded_tuple_bytes IS NULL)) OR ((utxo_payload_entry_count >= 0) AND (utxo_payload_encoded_tuple_bytes >= 0) AND ((utxo_payload_entry_count <> 0) OR (utxo_payload_encoded_tuple_bytes = 0)))))
);


--
-- Name: processed_mempool; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.processed_mempool (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: state_queue_mutation_leases; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.state_queue_mutation_leases (
    token text NOT NULL,
    scope text NOT NULL,
    holder text NOT NULL,
    status text NOT NULL,
    acquired_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    released_at timestamp with time zone,
    last_error text,
    CONSTRAINT state_queue_mutation_leases_check CHECK ((((status = 'active'::text) AND (released_at IS NULL)) OR ((status <> 'active'::text) AND (released_at IS NOT NULL)))),
    CONSTRAINT state_queue_mutation_leases_scope_check CHECK ((scope = 'state_queue'::text)),
    CONSTRAINT state_queue_mutation_leases_status_check CHECK ((status = ANY (ARRAY['active'::text, 'released'::text, 'failed'::text])))
);


--
-- Name: tx_admission_payloads; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tx_admission_payloads (
    tx_id bytea NOT NULL,
    tx_canonical_cbor bytea NOT NULL,
    tx_canonical_cbor_sha256 bytea NOT NULL,
    CONSTRAINT tx_admission_payloads_tx_canonical_cbor_check CHECK ((octet_length(tx_canonical_cbor) > 0)),
    CONSTRAINT tx_admission_payloads_tx_canonical_cbor_sha256_check CHECK ((octet_length(tx_canonical_cbor_sha256) = 32))
);


--
-- Name: tx_admissions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tx_admissions (
    tx_id bytea NOT NULL,
    arrival_seq bigint NOT NULL,
    status public.tx_admission_status NOT NULL,
    first_seen_at timestamp with time zone DEFAULT now() NOT NULL,
    last_seen_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    validation_started_at timestamp with time zone,
    terminal_at timestamp with time zone,
    lease_owner text,
    lease_expires_at timestamp with time zone,
    attempt_count integer DEFAULT 0 NOT NULL,
    next_attempt_at timestamp with time zone DEFAULT now() NOT NULL,
    reject_code text,
    reject_detail text,
    submit_source text NOT NULL,
    request_count bigint DEFAULT 1 NOT NULL,
    CONSTRAINT tx_admissions_attempt_count_check CHECK ((attempt_count >= 0)),
    CONSTRAINT tx_admissions_check CHECK (((last_seen_at >= first_seen_at) AND (updated_at >= first_seen_at))),
    CONSTRAINT tx_admissions_check1 CHECK ((((status = 'validating'::public.tx_admission_status) AND (lease_owner IS NOT NULL) AND (lease_expires_at IS NOT NULL) AND (terminal_at IS NULL)) OR ((status <> 'validating'::public.tx_admission_status) AND (lease_owner IS NULL) AND (lease_expires_at IS NULL)))),
    CONSTRAINT tx_admissions_check2 CHECK ((((status = ANY (ARRAY['accepted'::public.tx_admission_status, 'rejected'::public.tx_admission_status])) AND (terminal_at IS NOT NULL)) OR ((status = ANY (ARRAY['queued'::public.tx_admission_status, 'validating'::public.tx_admission_status])) AND (terminal_at IS NULL)))),
    CONSTRAINT tx_admissions_check3 CHECK ((((status = 'rejected'::public.tx_admission_status) AND (reject_code IS NOT NULL)) OR ((status <> 'rejected'::public.tx_admission_status) AND (reject_code IS NULL) AND (reject_detail IS NULL)))),
    CONSTRAINT tx_admissions_request_count_check CHECK ((request_count >= 1)),
    CONSTRAINT tx_admissions_submit_source_check CHECK ((submit_source = ANY (ARRAY['native'::text, 'backfill'::text]))),
    CONSTRAINT tx_admissions_tx_id_check CHECK ((octet_length(tx_id) = 32))
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');


--
-- Name: tx_admissions_arrival_seq_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.tx_admissions_arrival_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tx_admissions_arrival_seq_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.tx_admissions_arrival_seq_seq OWNED BY public.tx_admissions.arrival_seq;


--
-- Name: tx_rejections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tx_rejections (
    tx_id bytea NOT NULL,
    reject_code text NOT NULL,
    reject_detail text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: withdrawal_utxos; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.withdrawal_utxos (
    event_id bytea NOT NULL,
    raw_event_info bytea NOT NULL,
    settlement_event_info bytea,
    inclusion_time timestamp with time zone NOT NULL,
    withdrawal_l1_tx_hash bytea NOT NULL,
    withdrawal_l1_output_index integer NOT NULL,
    asset_name bytea NOT NULL,
    l2_outref bytea NOT NULL,
    l2_owner bytea NOT NULL,
    l2_value bytea NOT NULL,
    l1_address bytea NOT NULL,
    l1_datum bytea NOT NULL,
    refund_address bytea NOT NULL,
    refund_datum bytea NOT NULL,
    validity text,
    validity_detail jsonb DEFAULT '{}'::jsonb NOT NULL,
    projected_header_hash bytea,
    status text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT withdrawal_utxos_asset_name_check CHECK (((octet_length(asset_name) >= 1) AND (octet_length(asset_name) <= 32))),
    CONSTRAINT withdrawal_utxos_check CHECK (((status = 'awaiting'::text) OR (settlement_event_info IS NOT NULL))),
    CONSTRAINT withdrawal_utxos_check1 CHECK (((status = 'awaiting'::text) OR (validity IS NOT NULL))),
    CONSTRAINT withdrawal_utxos_check2 CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT withdrawal_utxos_l2_owner_check CHECK ((octet_length(l2_owner) = 28)),
    CONSTRAINT withdrawal_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'finalized'::text]))),
    CONSTRAINT withdrawal_utxos_validity_check CHECK (((validity IS NULL) OR (validity = ANY (ARRAY['WithdrawalIsValid'::text, 'NonExistentWithdrawalUtxo'::text, 'SpentWithdrawalUtxo'::text, 'IncorrectWithdrawalOwner'::text, 'IncorrectWithdrawalValue'::text, 'IncorrectWithdrawalSignature'::text, 'TooManyTokensInWithdrawal'::text, 'UnpayableWithdrawalValue'::text])))),
    CONSTRAINT withdrawal_utxos_withdrawal_l1_output_index_check CHECK ((withdrawal_l1_output_index >= 0)),
    CONSTRAINT withdrawal_utxos_withdrawal_l1_tx_hash_check CHECK ((octet_length(withdrawal_l1_tx_hash) = 32))
);


--
-- Name: blocks height; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.blocks ALTER COLUMN height SET DEFAULT nextval('public.blocks_height_seq'::regclass);


--
-- Name: tx_admissions arrival_seq; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tx_admissions ALTER COLUMN arrival_seq SET DEFAULT nextval('public.tx_admissions_arrival_seq_seq'::regclass);


--
-- Name: address_history address_history_tx_id_address_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.address_history
    ADD CONSTRAINT address_history_tx_id_address_key UNIQUE (tx_id, address);


--
-- Name: blocks blocks_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.blocks
    ADD CONSTRAINT blocks_pkey PRIMARY KEY (height);


--
-- Name: blocks blocks_tx_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.blocks
    ADD CONSTRAINT blocks_tx_id_key UNIQUE (tx_id);


--
-- Name: commit_build_calibration commit_build_calibration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.commit_build_calibration
    ADD CONSTRAINT commit_build_calibration_pkey PRIMARY KEY (id);


--
-- Name: confirmed_ledger confirmed_ledger_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.confirmed_ledger
    ADD CONSTRAINT confirmed_ledger_pkey PRIMARY KEY (outref);


--
-- Name: da_payload_announcements da_payload_announcements_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.da_payload_announcements
    ADD CONSTRAINT da_payload_announcements_pkey PRIMARY KEY (header_hash);


--
-- Name: da_payload_publications da_payload_publications_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.da_payload_publications
    ADD CONSTRAINT da_payload_publications_pkey PRIMARY KEY (header_hash, peer_id);


--
-- Name: da_payloads da_payloads_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.da_payloads
    ADD CONSTRAINT da_payloads_pkey PRIMARY KEY (header_hash);


--
-- Name: deposit_submission_attempts deposit_submission_attempts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.deposit_submission_attempts
    ADD CONSTRAINT deposit_submission_attempts_pkey PRIMARY KEY (tx_hash);


--
-- Name: deposits_utxos deposits_utxos_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.deposits_utxos
    ADD CONSTRAINT deposits_utxos_pkey PRIMARY KEY (event_id);


--
-- Name: forced_transaction_utxos forced_transaction_utxos_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.forced_transaction_utxos
    ADD CONSTRAINT forced_transaction_utxos_pkey PRIMARY KEY (tx_order_id);


--
-- Name: forced_transaction_utxos forced_transaction_utxos_tx_order_l1_tx_hash_tx_order_l1_ou_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.forced_transaction_utxos
    ADD CONSTRAINT forced_transaction_utxos_tx_order_l1_tx_hash_tx_order_l1_ou_key UNIQUE (tx_order_l1_tx_hash, tx_order_l1_output_index);


--
-- Name: foreign_tip_reconciliations foreign_tip_reconciliations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.foreign_tip_reconciliations
    ADD CONSTRAINT foreign_tip_reconciliations_pkey PRIMARY KEY (foreign_header_hash);


--
-- Name: immutable immutable_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.immutable
    ADD CONSTRAINT immutable_pkey PRIMARY KEY (tx_id);


--
-- Name: local_mutation_jobs local_mutation_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.local_mutation_jobs
    ADD CONSTRAINT local_mutation_jobs_pkey PRIMARY KEY (job_id);


--
-- Name: mempool_ledger mempool_ledger_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mempool_ledger
    ADD CONSTRAINT mempool_ledger_pkey PRIMARY KEY (outref);


--
-- Name: mempool mempool_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mempool
    ADD CONSTRAINT mempool_pkey PRIMARY KEY (tx_id);


--
-- Name: mempool_tx_deltas mempool_tx_deltas_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mempool_tx_deltas
    ADD CONSTRAINT mempool_tx_deltas_pkey PRIMARY KEY (tx_id);


--
-- Name: mpf_engine_state mpf_engine_state_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mpf_engine_state
    ADD CONSTRAINT mpf_engine_state_pkey PRIMARY KEY (store_name);


--
-- Name: pending_block_finalization_deposits pending_block_finalization_deposits_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_deposits pending_block_finalization_deposits_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalization_event_to_step pending_block_finalization_event_to_ste_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_ste_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_event_to_step pending_block_finalization_event_to_step_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_step_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalization_forced_transactions pending_block_finalization_forced_trans_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_trans_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_forced_transactions pending_block_finalization_forced_transactions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_transactions_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalization_transition_trace pending_block_finalization_transition_t_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_t_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_transition_trace pending_block_finalization_transition_trace_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_trace_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalization_txs pending_block_finalization_txs_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_txs pending_block_finalization_txs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalization_utxos pending_block_finalization_utxos_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_utxos
    ADD CONSTRAINT pending_block_finalization_utxos_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_utxos pending_block_finalization_utxos_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_utxos
    ADD CONSTRAINT pending_block_finalization_utxos_pkey PRIMARY KEY (header_hash, outref);


--
-- Name: pending_block_finalization_withdrawals pending_block_finalization_withdrawals_header_hash_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_header_hash_ordinal_key UNIQUE (header_hash, ordinal);


--
-- Name: pending_block_finalization_withdrawals pending_block_finalization_withdrawals_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_pkey PRIMARY KEY (header_hash, member_id);


--
-- Name: pending_block_finalizations pending_block_finalizations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalizations
    ADD CONSTRAINT pending_block_finalizations_pkey PRIMARY KEY (header_hash);


--
-- Name: pending_block_finalizations pending_block_finalizations_submitted_tx_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalizations
    ADD CONSTRAINT pending_block_finalizations_submitted_tx_hash_key UNIQUE (submitted_tx_hash);


--
-- Name: processed_mempool processed_mempool_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.processed_mempool
    ADD CONSTRAINT processed_mempool_pkey PRIMARY KEY (tx_id);


--
-- Name: state_queue_mutation_leases state_queue_mutation_leases_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.state_queue_mutation_leases
    ADD CONSTRAINT state_queue_mutation_leases_pkey PRIMARY KEY (token);


--
-- Name: tx_admission_payloads tx_admission_payloads_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tx_admission_payloads
    ADD CONSTRAINT tx_admission_payloads_pkey PRIMARY KEY (tx_id);


--
-- Name: tx_admissions tx_admissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tx_admissions
    ADD CONSTRAINT tx_admissions_pkey PRIMARY KEY (tx_id);


--
-- Name: withdrawal_utxos withdrawal_utxos_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.withdrawal_utxos
    ADD CONSTRAINT withdrawal_utxos_pkey PRIMARY KEY (event_id);


--
-- Name: withdrawal_utxos withdrawal_utxos_withdrawal_l1_tx_hash_withdrawal_l1_output_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.withdrawal_utxos
    ADD CONSTRAINT withdrawal_utxos_withdrawal_l1_tx_hash_withdrawal_l1_output_key UNIQUE (withdrawal_l1_tx_hash, withdrawal_l1_output_index);


--
-- Name: idx_address_history_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_address_history_created_at ON public.address_history USING btree (created_at);


--
-- Name: idx_blocks_header_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_blocks_header_hash ON public.blocks USING btree (header_hash);


--
-- Name: idx_blocks_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_blocks_tx_id ON public.blocks USING btree (tx_id);


--
-- Name: idx_confirmed_ledger_address; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_confirmed_ledger_address ON public.confirmed_ledger USING btree (address);


--
-- Name: idx_da_payload_announcements_retry; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_da_payload_announcements_retry ON public.da_payload_announcements USING btree (next_retry_at, lease_expires_at, header_hash) WHERE (status = ANY (ARRAY['pending'::text, 'failed'::text]));


--
-- Name: idx_da_payload_publications_retry; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_da_payload_publications_retry ON public.da_payload_publications USING btree (next_retry_at, lease_expires_at, header_hash, peer_id) WHERE (status = ANY (ARRAY['pending'::text, 'rejected'::text, 'transport_error'::text]));


--
-- Name: idx_da_payloads_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_da_payloads_created_at ON public.da_payloads USING btree (created_at);


--
-- Name: idx_deposit_submission_attempts_active_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_deposit_submission_attempts_active_event_id ON public.deposit_submission_attempts USING btree (deposit_event_id) WHERE (status <> 'expired'::text);


--
-- Name: idx_deposit_submission_attempts_status_prepared_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_deposit_submission_attempts_status_prepared_at ON public.deposit_submission_attempts USING btree (status, prepared_at);


--
-- Name: idx_deposits_utxos_deposit_l1_tx_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_deposits_utxos_deposit_l1_tx_hash ON public.deposits_utxos USING btree (deposit_l1_tx_hash);


--
-- Name: idx_deposits_utxos_projected_header_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_deposits_utxos_projected_header_hash ON public.deposits_utxos USING btree (projected_header_hash);


--
-- Name: idx_deposits_utxos_status_inclusion_time_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_deposits_utxos_status_inclusion_time_event_id ON public.deposits_utxos USING btree (status, inclusion_time, event_id);


--
-- Name: idx_forced_transaction_utxos_projected_header_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_forced_transaction_utxos_projected_header_hash ON public.forced_transaction_utxos USING btree (projected_header_hash);


--
-- Name: idx_forced_transaction_utxos_status_inclusion_time_tx_order_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_forced_transaction_utxos_status_inclusion_time_tx_order_id ON public.forced_transaction_utxos USING btree (status, inclusion_time, tx_order_id);


--
-- Name: idx_forced_transaction_utxos_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_forced_transaction_utxos_tx_id ON public.forced_transaction_utxos USING btree (tx_id);


--
-- Name: idx_foreign_tip_reconciliations_status_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_foreign_tip_reconciliations_status_updated ON public.foreign_tip_reconciliations USING btree (status, updated_at);


--
-- Name: idx_foreign_tip_reconciliations_window; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_foreign_tip_reconciliations_window ON public.foreign_tip_reconciliations USING btree (block_start_time, block_end_time);


--
-- Name: idx_immutable_time_stamp_tz; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_immutable_time_stamp_tz ON public.immutable USING btree (time_stamp_tz);


--
-- Name: idx_local_mutation_jobs_status_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_local_mutation_jobs_status_updated ON public.local_mutation_jobs USING btree (status, updated_at);


--
-- Name: idx_mempool_ledger_address; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mempool_ledger_address ON public.mempool_ledger USING btree (address);


--
-- Name: idx_mempool_time_stamp_tz_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mempool_time_stamp_tz_tx_id ON public.mempool USING btree (time_stamp_tz, tx_id);


--
-- Name: idx_pending_block_finalizations_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pending_block_finalizations_status ON public.pending_block_finalizations USING btree (status);


--
-- Name: idx_processed_mempool_time_stamp_tz_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_processed_mempool_time_stamp_tz_tx_id ON public.processed_mempool USING btree (time_stamp_tz, tx_id);


--
-- Name: idx_state_queue_mutation_leases_status_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_state_queue_mutation_leases_status_updated ON public.state_queue_mutation_leases USING btree (status, acquired_at);


--
-- Name: idx_tx_admissions_active_lease; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_tx_admissions_active_lease ON public.tx_admissions USING btree (lease_owner, tx_id) WHERE (status = 'validating'::public.tx_admission_status);


--
-- Name: idx_tx_admissions_lease; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_tx_admissions_lease ON public.tx_admissions USING btree (lease_expires_at) WHERE (status = 'validating'::public.tx_admission_status);


--
-- Name: idx_tx_admissions_queued_arrival; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_tx_admissions_queued_arrival ON public.tx_admissions USING btree (arrival_seq, tx_id) WHERE (status = 'queued'::public.tx_admission_status);


--
-- Name: idx_tx_rejections_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_tx_rejections_created_at ON public.tx_rejections USING btree (created_at);


--
-- Name: idx_tx_rejections_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_tx_rejections_tx_id ON public.tx_rejections USING btree (tx_id);


--
-- Name: idx_withdrawal_utxos_l2_outref; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_withdrawal_utxos_l2_outref ON public.withdrawal_utxos USING btree (l2_outref);


--
-- Name: idx_withdrawal_utxos_projected_header_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_withdrawal_utxos_projected_header_hash ON public.withdrawal_utxos USING btree (projected_header_hash);


--
-- Name: idx_withdrawal_utxos_status_inclusion_time_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_withdrawal_utxos_status_inclusion_time_event_id ON public.withdrawal_utxos USING btree (status, inclusion_time, event_id);


--
-- Name: idx_withdrawal_utxos_withdrawal_l1_tx_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_withdrawal_utxos_withdrawal_l1_tx_hash ON public.withdrawal_utxos USING btree (withdrawal_l1_tx_hash);


--
-- Name: uniq_mempool_ledger_source_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_mempool_ledger_source_event_id ON public.mempool_ledger USING btree (source_event_id) WHERE (source_event_id IS NOT NULL);


--
-- Name: uniq_pending_block_finalizations_single_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_pending_block_finalizations_single_active ON public.pending_block_finalizations USING btree ((1)) WHERE (status = ANY (ARRAY['pending_submission'::text, 'submitted_local_finalization_pending'::text, 'submitted_unconfirmed'::text, 'observed_waiting_stability'::text]));


--
-- Name: uniq_state_queue_mutation_leases_active_scope; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_state_queue_mutation_leases_active_scope ON public.state_queue_mutation_leases USING btree (scope) WHERE (status = 'active'::text);


--
-- Name: uniq_tx_rejections_tx_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_tx_rejections_tx_id ON public.tx_rejections USING btree (tx_id);


--
-- Name: da_payload_announcements da_payload_announcements_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.da_payload_announcements
    ADD CONSTRAINT da_payload_announcements_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.da_payloads(header_hash) ON DELETE CASCADE;


--
-- Name: da_payload_publications da_payload_publications_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.da_payload_publications
    ADD CONSTRAINT da_payload_publications_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.da_payloads(header_hash) ON DELETE CASCADE;


--
-- Name: mempool_ledger mempool_ledger_source_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mempool_ledger
    ADD CONSTRAINT mempool_ledger_source_event_id_fkey FOREIGN KEY (source_event_id) REFERENCES public.deposits_utxos(event_id) ON DELETE RESTRICT;


--
-- Name: pending_block_finalization_deposits pending_block_finalization_deposits_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_deposits pending_block_finalization_deposits_member_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_member_id_fkey FOREIGN KEY (member_id) REFERENCES public.deposits_utxos(event_id) ON DELETE RESTRICT;


--
-- Name: pending_block_finalization_event_to_step pending_block_finalization_event_to_step_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_step_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_forced_transactions pending_block_finalization_forced_transactions_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_transactions_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_transition_trace pending_block_finalization_transition_trace_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_trace_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_txs pending_block_finalization_txs_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_utxos pending_block_finalization_utxos_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_utxos
    ADD CONSTRAINT pending_block_finalization_utxos_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_withdrawals pending_block_finalization_withdrawals_header_hash_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;


--
-- Name: pending_block_finalization_withdrawals pending_block_finalization_withdrawals_member_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_member_id_fkey FOREIGN KEY (member_id) REFERENCES public.withdrawal_utxos(event_id) ON DELETE RESTRICT;


--
-- Name: tx_admission_payloads tx_admission_payloads_tx_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tx_admission_payloads
    ADD CONSTRAINT tx_admission_payloads_tx_id_fkey FOREIGN KEY (tx_id) REFERENCES public.tx_admissions(tx_id) ON DELETE CASCADE;


--
-- Required singleton seed rows
--

INSERT INTO public.commit_build_calibration (
    id,
    ms_per_tx_ewma,
    sample_count
) VALUES (1, 1.0, 0);
