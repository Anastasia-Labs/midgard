-- The node database schema, created whole on an empty database. A database
-- created from a different text of this file is refused at open; recreate it.

CREATE TYPE public.tx_admission_status AS ENUM (
    'queued',
    'validating',
    'accepted',
    'rejected'
);

-- Enqueues the settlement job of a consumed deposit or a valid finalized
-- withdrawal, for the deployment `node_deployment` records.
CREATE FUNCTION public.enqueue_automatic_settlement() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE event_kind text; initial_phase text;
BEGIN
  IF TG_TABLE_NAME = 'deposits_utxos' THEN
    IF NEW.status <> 'consumed' THEN RETURN NEW; END IF;
    event_kind := 'deposit'; initial_phase := 'absorb';
  ELSE
    IF NEW.status <> 'finalized' OR NEW.validity IS DISTINCT FROM 'WithdrawalIsValid' THEN RETURN NEW; END IF;
    event_kind := 'withdrawal'; initial_phase := 'initialize';
  END IF;
  INSERT INTO settlement_jobs (hub_oracle_policy_id, kind, event_id, phase)
    SELECT hub_oracle_policy_id, event_kind, encode(NEW.event_id, 'hex'), initial_phase
    FROM node_deployment WHERE singleton
    ON CONFLICT DO NOTHING;
  RETURN NEW;
END;
$$;

CREATE TABLE public.address_history (
    tx_id bytea NOT NULL,
    address text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.blocks (
    height integer NOT NULL,
    header_hash bytea NOT NULL,
    tx_id bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);

CREATE SEQUENCE public.blocks_height_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.blocks_height_seq OWNED BY public.blocks.height;

CREATE TABLE public.commit_build_calibration (
    id smallint NOT NULL,
    ms_per_tx_ewma double precision NOT NULL,
    sample_count bigint DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT commit_build_calibration_id_check CHECK ((id = 1)),
    CONSTRAINT commit_build_calibration_ms_per_tx_ewma_check CHECK ((ms_per_tx_ewma > (0)::double precision)),
    CONSTRAINT commit_build_calibration_sample_count_check CHECK ((sample_count >= 0))
);

CREATE TABLE public.confirmed_ledger (
    tx_id bytea NOT NULL,
    outref bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);

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
    consensus_profile_id text NOT NULL,
    validation_traces_root text DEFAULT '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8'::text NOT NULL,
    validation_trace_count bigint DEFAULT 0 NOT NULL,
    block_start_time timestamp with time zone NOT NULL,
    block_end_time timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    block_time timestamp with time zone NOT NULL,
    CONSTRAINT da_payloads_block_time_covers_end CHECK ((block_end_time <= block_time)),
    CONSTRAINT da_payloads_check CHECK ((block_end_time >= block_start_time)),
    CONSTRAINT da_payloads_consensus_profile_id_check CHECK ((consensus_profile_id = 'midgard-consensus-v1'::text)),
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
    CONSTRAINT da_payloads_profile_version_check CHECK (((version = 1) AND (validation_trace_count = (forced_transaction_count + l2_transaction_count)) AND (((validation_trace_count = 0) AND (validation_traces_root = '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8'::text)) OR ((validation_trace_count > 0) AND (validation_traces_root <> '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8'::text))))),
    CONSTRAINT da_payloads_total_event_count_check CHECK ((total_event_count >= 0)),
    CONSTRAINT da_payloads_trace_count_check CHECK ((transition_step_count = total_event_count)),
    CONSTRAINT da_payloads_transactions_root_check CHECK ((transactions_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_transition_step_count_check CHECK ((transition_step_count >= 0)),
    CONSTRAINT da_payloads_transition_trace_root_check CHECK ((transition_trace_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_validation_trace_count_check CHECK ((validation_trace_count >= 0)),
    CONSTRAINT da_payloads_validation_traces_root_check CHECK ((validation_traces_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT da_payloads_withdrawal_count_check CHECK ((withdrawal_count >= 0)),
    CONSTRAINT da_payloads_withdrawals_root_check CHECK ((withdrawals_root ~ '^[0-9a-f]{64}$'::text))
);

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
    l1_event_key bytea,
    l1_origin_outref bytea,
    CONSTRAINT deposits_utxos_check CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT deposits_utxos_l1_admission_check CHECK ((((l1_event_key IS NULL) AND (l1_origin_outref IS NULL)) OR ((l1_event_key IS NOT NULL) AND (l1_origin_outref IS NOT NULL) AND (octet_length(l1_event_key) = 32) AND (octet_length(l1_origin_outref) = 34) AND (SUBSTRING(l1_origin_outref FROM 1 FOR 32) = deposit_l1_tx_hash)))),
    CONSTRAINT deposits_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'consumed'::text])))
);

-- class: D-c; retention: one row, replaced by every follower-change driver run (a full pass rebuilds it)
-- The follower view through which the driver last ingested every admission:
-- the commit horizon never passes it while it is still on the follower's chain.
CREATE TABLE public.follower_event_ingestion (
    id boolean DEFAULT true NOT NULL,
    generation bigint NOT NULL,
    slot bigint NOT NULL,
    block_hash bytea NOT NULL,
    height bigint NOT NULL,
    ingested_through_ms bigint NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT follower_event_ingestion_block_hash_check CHECK ((octet_length(block_hash) = 32)),
    CONSTRAINT follower_event_ingestion_generation_check CHECK ((generation >= 0)),
    CONSTRAINT follower_event_ingestion_height_check CHECK ((height >= 0)),
    CONSTRAINT follower_event_ingestion_id_check CHECK (id),
    CONSTRAINT follower_event_ingestion_slot_check CHECK ((slot >= 0))
);

CREATE TABLE public.forced_transaction_utxos (
    tx_order_id bytea NOT NULL,
    tx_order_l1_tx_hash bytea NOT NULL,
    tx_order_l1_output_index integer NOT NULL,
    asset_name bytea NOT NULL,
    raw_datum bytea NOT NULL,
    tx_id bytea NOT NULL,
    tx_compact bytea NOT NULL,
    forced_inclusion_value bytea NOT NULL,
    consensus_profile_id text NOT NULL,
    native_tx_cbor bytea NOT NULL,
    transaction_commitment bytea NOT NULL,
    inclusion_time timestamp with time zone NOT NULL,
    projected_header_hash bytea,
    status text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forced_transaction_utxos_asset_name_check CHECK (((octet_length(asset_name) >= 1) AND (octet_length(asset_name) <= 32))),
    CONSTRAINT forced_transaction_utxos_check CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT forced_transaction_utxos_exact_transaction CHECK (((consensus_profile_id = 'midgard-consensus-v1'::text) AND (octet_length(native_tx_cbor) <= 295041) AND (octet_length(transaction_commitment) = 32))),
    CONSTRAINT forced_transaction_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'finalized'::text]))),
    CONSTRAINT forced_transaction_utxos_tx_id_check CHECK ((octet_length(tx_id) = 32)),
    CONSTRAINT forced_transaction_utxos_tx_order_l1_output_index_check CHECK ((tx_order_l1_output_index >= 0)),
    CONSTRAINT forced_transaction_utxos_tx_order_l1_tx_hash_check CHECK ((octet_length(tx_order_l1_tx_hash) = 32))
);

CREATE TABLE public.immutable (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
);

-- class: B; retention: one row per family while its refusal stands; deleted on that family's next successful journal record, or once a landed valid tx other than the refused one spends one of the refused tx's inputs (never on a timer)
-- An intent-journal refusal held as a named /readyz reason. Rows are
-- written by whichever process refused the record (the main process, the
-- commit and settlement workers) and read by the main process, whose
-- /readyz reports them; the database is the boundary they cross.
CREATE TABLE public.intent_refusal_holds (
    family text NOT NULL,
    reason text NOT NULL,
    detail text NOT NULL,
    tx_hash bytea NOT NULL,
    inputs bytea[] NOT NULL,
    raised_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT intent_refusal_holds_tx_hash_check CHECK ((octet_length(tx_hash) = 32))
);

-- class: D-x; retention: an `included_by` mark lives while the block it names holds the row (landed and processed, folded but not yet final, or this node's block between its local finalization and its processing); the prune that makes that block's fold final deletes the row, and the rollback that takes the block off the landed chain clears the mark
-- A row a block includes stays, marked by that block's header hash, until
-- the block's fold into `confirmed_ledger` is final. Pending means unmarked:
-- selection, commit building and every pending read skip a marked row.
-- `processed_mempool` carries the same mark.
CREATE TABLE public.mempool (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL,
    included_by bytea,
    CONSTRAINT mempool_included_by_check CHECK ((octet_length(included_by) = 28))
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');

CREATE TABLE public.mempool_ledger (
    tx_id bytea NOT NULL,
    outref bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL,
    source_event_id bytea,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');

CREATE UNLOGGED TABLE public.mempool_tx_deltas (
    tx_id bytea NOT NULL,
    spent_cbor bytea NOT NULL,
    produced_cbor bytea NOT NULL
);

CREATE TABLE public.mpf_engine_state (
    store_name text NOT NULL,
    root_hex text,
    audit_diverged boolean DEFAULT false NOT NULL,
    last_audit_at timestamp with time zone,
    lease_owner text,
    lease_expires_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_audit_diverged boolean DEFAULT false NOT NULL,
    CONSTRAINT mpf_engine_state_root_hex_check CHECK (((root_hex IS NULL) OR (root_hex ~ '^[0-9a-f]{64}$'::text)))
);

-- class: D-x; retention: one row, advanced by every merge folded into confirmed_ledger
-- The landed header whose post-state `confirmed_ledger` holds: the merged
-- queue root, once every block up to it is folded in.
CREATE TABLE public.node_confirmed_ledger_frontier (
    id boolean DEFAULT true NOT NULL,
    header_hash bytea NOT NULL,
    utxos_root text NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT node_confirmed_ledger_frontier_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT node_confirmed_ledger_frontier_id_check CHECK (id),
    CONSTRAINT node_confirmed_ledger_frontier_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text))
);

-- class: D-x; retention: a row lives exactly as long as the `node_confirmed_merges` row of the fold that spent it (deleted with it)
-- The `confirmed_ledger` rows a fold spent, verbatim, so the unfold of that
-- fold restores them.
CREATE TABLE public.node_confirmed_ledger_spent (
    header_hash bytea NOT NULL,
    tx_id bytea NOT NULL,
    outref bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL,
    time_stamp_tz timestamp with time zone NOT NULL
);

-- class: D-x; retention: a row lives while its merge can still be rolled back: it is deleted once the follower's prune boundary reaches its merge slot, or when the unfold of its block rewinds it
-- One row per landed block folded into `confirmed_ledger`, by header: the
-- block's landed row as processing recorded it (so an unfold re-inserts it
-- verbatim), the events the fold marked terminal (so an unfold reopens
-- exactly those), and the slot and output of the merge that made it the
-- queue root (null until landed-block processing read it from the queue
-- history: a merge the merge fiber folded first).
CREATE TABLE public.node_confirmed_merges (
    header_hash bytea NOT NULL,
    parent_header_hash bytea NOT NULL,
    parent_utxos_root text NOT NULL,
    utxos_root text NOT NULL,
    kind text NOT NULL,
    applied boolean NOT NULL,
    spent bytea[] NOT NULL,
    produced_outrefs bytea[] NOT NULL,
    produced_outputs bytea[] NOT NULL,
    deposit_ids bytea[] NOT NULL,
    withdrawals jsonb NOT NULL,
    forced_ids bytea[] NOT NULL,
    tx_ids bytea[] NOT NULL,
    consumed_deposit_ids bytea[] NOT NULL,
    finalized_withdrawal_ids bytea[] NOT NULL,
    finalized_forced_ids bytea[] NOT NULL,
    merge_slot bigint,
    merge_out_ref text,
    folded_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT node_confirmed_merges_check CHECK ((cardinality(produced_outrefs) = cardinality(produced_outputs))),
    CONSTRAINT node_confirmed_merges_check1 CHECK (((merge_slot IS NULL) = (merge_out_ref IS NULL))),
    CONSTRAINT node_confirmed_merges_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT node_confirmed_merges_kind_check CHECK ((kind = ANY (ARRAY['own'::text, 'foreign'::text]))),
    CONSTRAINT node_confirmed_merges_merge_slot_check CHECK ((merge_slot >= 0)),
    CONSTRAINT node_confirmed_merges_parent_header_hash_check CHECK ((octet_length(parent_header_hash) = 28)),
    CONSTRAINT node_confirmed_merges_parent_utxos_root_check CHECK ((parent_utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT node_confirmed_merges_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT node_confirmed_merges_withdrawals_check CHECK ((jsonb_typeof(withdrawals) = 'array'::text))
);

-- The deployment (its hub-oracle policy id) whose settlement jobs this node enqueues,
-- recorded by the startup preparation before the driver's first view, so
-- before any write that consumes a deposit or finalizes a withdrawal.
CREATE TABLE public.node_deployment (
    singleton boolean DEFAULT true NOT NULL,
    hub_oracle_policy_id text NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT node_deployment_hub_oracle_policy_id_check CHECK ((hub_oracle_policy_id ~ '^[0-9a-f]{56}$'::text)),
    CONSTRAINT node_deployment_singleton_check CHECK (singleton)
);

-- class: D-x; retention: one row, updated in place by the follower-change driver
-- The node's write gate. A guarded write checks the follower
-- view it was computed at (`viewValid`, under `FOR SHARE` on the follower
-- cursor), then takes this row `FOR UPDATE` in the same transaction and
-- requires that no driver recompute is pending and that its permit's epoch
-- is the row's. The driver bumps `epoch` and sets `pending_reason` before a
-- recompute (a rebase, an orphan repair, its first view after a start), so
-- every permit taken before it is refused, and clears them with the view it
-- applied once the recompute and the cache reload are done. `applied_*` is
-- the follower view the driver last applied, the view permits are taken at.
CREATE TABLE public.node_follower_write_gate (
    singleton boolean DEFAULT true NOT NULL,
    epoch bigint DEFAULT 0 NOT NULL,
    applied_generation bigint,
    applied_slot bigint,
    applied_hash bytea,
    applied_height bigint,
    pending_reason text,
    pending_detail text,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT node_follower_write_gate_check CHECK ((((applied_generation IS NULL) = (applied_slot IS NULL)) AND ((applied_slot IS NULL) = (applied_hash IS NULL)) AND ((applied_hash IS NULL) = (applied_height IS NULL)))),
    CONSTRAINT node_follower_write_gate_singleton_check CHECK (singleton)
);

-- class: D; retention: as `immutable` keeps this node's own committed txs: no age or retention prune; a row is written once the including block's fold is final, which no rollback reaches, so nothing deletes it
-- A transaction this node accepted that another operator's block included.
-- Its pending row stays, marked by that block (`included_by`), until the
-- block's fold into `confirmed_ledger` is final; the release that deletes the
-- row then (`releaseFinalFolds`) records the inclusion here, so `/tx-status`
-- and the committed-tx count keep answering for it, as `immutable` does for a
-- tx of this node's own blocks.
CREATE TABLE public.node_foreign_tx_inclusions (
    tx_id bytea NOT NULL,
    header_hash bytea NOT NULL,
    CONSTRAINT node_foreign_tx_inclusions_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT node_foreign_tx_inclusions_tx_id_check CHECK ((octet_length(tx_id) = 32))
);

-- class: D; retention: an entry leaves on an accepting local verdict, its block's removal, or its deadline (block end + M/2); a block's row and outputs leave with its last entry
-- The import checker's durable set: each Plutus transaction of an imported
-- foreign block, applied by its committed verdict, until its script runs are
-- accepted locally, its block is removed, or its deadline passes. A locally
-- rejected entry stays, as `rejected`, until its deadline. A transaction is
-- stored once (its bytes and program-material sidecar), and each output a
-- block's checks spend or reference once per block (`node_import_checks`,
-- `node_import_check_outputs`); the checker rebuilds the runs from them with
-- the block's validation context.
CREATE TABLE public.node_import_check_blocks (
    header_hash bytea NOT NULL,
    block_time_ms bigint NOT NULL,
    block_slot numeric NOT NULL,
    network_id numeric NOT NULL,
    min_fee_a numeric NOT NULL,
    min_fee_b numeric NOT NULL,
    slot_zero_time bigint NOT NULL,
    slot_zero_slot bigint NOT NULL,
    slot_length bigint NOT NULL,
    recorded_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.node_import_check_outputs (
    header_hash bytea NOT NULL,
    out_ref bytea NOT NULL,
    output bytea NOT NULL
);

CREATE TABLE public.node_import_checks (
    header_hash bytea NOT NULL,
    tx_id bytea NOT NULL,
    block_position integer NOT NULL,
    deadline_ms bigint NOT NULL,
    state text NOT NULL,
    tx_cbor bytea NOT NULL,
    runs integer NOT NULL,
    budget_cpu numeric NOT NULL,
    run_bytes bigint NOT NULL,
    script_hashes bytea NOT NULL,
    timeouts integer DEFAULT 0 NOT NULL,
    retry_at_ms bigint,
    CONSTRAINT node_import_checks_block_position_check CHECK ((block_position >= 0)),
    CONSTRAINT node_import_checks_budget_cpu_check CHECK ((budget_cpu >= (0)::numeric)),
    CONSTRAINT node_import_checks_retry_check CHECK (((state <> 'timed_out'::text) OR (retry_at_ms IS NOT NULL))),
    CONSTRAINT node_import_checks_run_bytes_check CHECK ((run_bytes >= 0)),
    CONSTRAINT node_import_checks_runs_check CHECK ((runs > 0)),
    CONSTRAINT node_import_checks_script_hashes_check CHECK (((octet_length(script_hashes) % 32) = 0)),
    CONSTRAINT node_import_checks_state_check CHECK ((state = ANY (ARRAY['pending'::text, 'cap_skipped'::text, 'cache_skipped'::text, 'timed_out'::text, 'faulted'::text, 'rejected'::text]))),
    CONSTRAINT node_import_checks_timeouts_check CHECK ((timeouts >= 0)),
    CONSTRAINT node_import_checks_tx_id_check CHECK ((octet_length(tx_id) = 32))
);

-- class: D-x; retention: a row lives while its header is a landed queue node not yet folded into confirmed_ledger, or (state 'removed') until the working-ledger rebase reverts it; folded and reverted rows are deleted
-- One row per landed state-queue block the node processed, exactly once: its
-- net ledger delta (spent and produced outrefs against its parent's ledger)
-- and the events it included. `applied` is whether the working ledger and the
-- native MPF hold it. A foreign row's delta comes from replaying its DA
-- payload against its parent's ledger; an own row's from the node's journal.
-- A removed row of either kind is one the working ledger holds.
CREATE TABLE public.node_landed_blocks (
    header_hash bytea NOT NULL,
    parent_header_hash bytea NOT NULL,
    parent_utxos_root text NOT NULL,
    utxos_root text NOT NULL,
    kind text NOT NULL,
    state text NOT NULL,
    applied boolean NOT NULL,
    spent bytea[] NOT NULL,
    produced_outrefs bytea[] NOT NULL,
    produced_outputs bytea[] NOT NULL,
    deposit_ids bytea[] NOT NULL,
    withdrawals jsonb NOT NULL,
    forced_ids bytea[] NOT NULL,
    tx_ids bytea[] NOT NULL,
    processed_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT node_landed_blocks_check CHECK ((cardinality(produced_outrefs) = cardinality(produced_outputs))),
    CONSTRAINT node_landed_blocks_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT node_landed_blocks_kind_check CHECK ((kind = ANY (ARRAY['own'::text, 'foreign'::text]))),
    CONSTRAINT node_landed_blocks_parent_header_hash_check CHECK ((octet_length(parent_header_hash) = 28)),
    CONSTRAINT node_landed_blocks_parent_utxos_root_check CHECK ((parent_utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT node_landed_blocks_removed_applied CHECK (((state = 'processed'::text) OR applied)),
    CONSTRAINT node_landed_blocks_state_check CHECK ((state = ANY (ARRAY['processed'::text, 'removed'::text]))),
    CONSTRAINT node_landed_blocks_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT node_landed_blocks_withdrawals_check CHECK ((jsonb_typeof(withdrawals) = 'array'::text))
);

-- class: D-x; retention: one row, written once by the install of the node's ledger, in the transaction that stamps it
-- The confirmed header a node's ledger was taken from (the empty ledger, or
-- a peer's snapshot of the header's post-state root) and that header's end
-- time: the node keeps no row for an event included at or before it, and
-- none at all before the row exists.
CREATE TABLE public.node_ledger_snapshot_anchor (
    id boolean DEFAULT true NOT NULL,
    header_hash bytea NOT NULL,
    utxos_root text NOT NULL,
    end_time_ms bigint NOT NULL,
    CONSTRAINT node_ledger_snapshot_anchor_end_time_ms_check CHECK ((end_time_ms >= 0)),
    CONSTRAINT node_ledger_snapshot_anchor_header_hash_check CHECK ((octet_length(header_hash) = 28)),
    CONSTRAINT node_ledger_snapshot_anchor_id_check CHECK (id),
    CONSTRAINT node_ledger_snapshot_anchor_utxos_root_check CHECK ((utxos_root ~ '^[0-9a-f]{64}$'::text))
);

-- class: D-x; retention: rows live while a node without a ledger stamp takes in a peer's ledger snapshot; emptied when a take-in starts and by the install that stamps the ledger
-- The entries of the snapshot being taken in, as `confirmed_ledger` rows:
-- the install moves them into `confirmed_ledger` and `mempool_ledger`.
CREATE TABLE public.node_ledger_snapshot_staging (
    outref bytea NOT NULL,
    tx_id bytea NOT NULL,
    output bytea NOT NULL,
    address text NOT NULL
);

CREATE TABLE public.operator_membership_observations (
    hub_oracle_policy_id bytea NOT NULL,
    operator_key bytea NOT NULL,
    active_block_hash bytea NOT NULL,
    active_block_slot bigint NOT NULL,
    active_block_height bigint NOT NULL,
    CONSTRAINT operator_membership_observations_active_block_hash_check CHECK ((octet_length(active_block_hash) = 32)),
    CONSTRAINT operator_membership_observations_active_block_height_check CHECK ((active_block_height >= 0)),
    CONSTRAINT operator_membership_observations_active_block_slot_check CHECK ((active_block_slot >= 0)),
    CONSTRAINT operator_membership_observations_hub_oracle_policy_id_check CHECK ((octet_length(hub_oracle_policy_id) = 28)),
    CONSTRAINT operator_membership_observations_operator_key_check CHECK ((octet_length(operator_key) = 28))
);

CREATE TABLE public.pending_block_finalization_deposits (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    l1_event_key bytea,
    l1_origin_outref bytea,
    CONSTRAINT pending_block_finalization_deposits_l1_admission_check CHECK ((((l1_event_key IS NULL) AND (l1_origin_outref IS NULL)) OR ((l1_event_key IS NOT NULL) AND (l1_origin_outref IS NOT NULL) AND (octet_length(l1_event_key) = 32) AND (octet_length(l1_origin_outref) = 34)))),
    CONSTRAINT pending_block_finalization_deposits_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);

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

CREATE TABLE public.pending_block_finalization_validation_traces (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalization_validation_traces_ordinal_check CHECK ((ordinal >= 0)),
    CONSTRAINT pending_block_finalization_validation_traces_payload_check CHECK ((octet_length(payload_cbor) > 0)),
    CONSTRAINT pending_block_finalization_validation_traces_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);

CREATE TABLE public.pending_block_finalization_withdrawals (
    header_hash bytea NOT NULL,
    member_id bytea NOT NULL,
    ordinal integer NOT NULL,
    validity text NOT NULL,
    validity_detail jsonb NOT NULL,
    classification_revision integer NOT NULL,
    classification_sha256 bytea NOT NULL,
    payload_cbor bytea NOT NULL,
    payload_sha256 bytea NOT NULL,
    source_table text NOT NULL,
    source_id bytea NOT NULL,
    source_time_stamp_tz timestamp with time zone NOT NULL,
    l1_event_key bytea,
    l1_origin_outref bytea,
    CONSTRAINT pending_block_finalization_withdr_classification_revision_check CHECK ((classification_revision >= 0)),
    CONSTRAINT pending_block_finalization_withdraw_classification_sha256_check CHECK ((octet_length(classification_sha256) = 32)),
    CONSTRAINT pending_block_finalization_withdrawals_l1_admission_check CHECK ((((l1_event_key IS NULL) AND (l1_origin_outref IS NULL)) OR ((l1_event_key IS NOT NULL) AND (l1_origin_outref IS NOT NULL) AND (octet_length(l1_event_key) = 32) AND (octet_length(l1_origin_outref) = 34)))),
    CONSTRAINT pending_block_finalization_withdrawals_payload_sha256_check CHECK ((octet_length(payload_sha256) = 32))
);

CREATE TABLE public.pending_block_finalizations (
    header_hash bytea NOT NULL,
    prepared_tx_hash bytea,
    intended_tx_hash bytea,
    block_end_time timestamp with time zone NOT NULL,
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
    format_version smallint NOT NULL,
    replay_kind text NOT NULL,
    hub_oracle_policy_id text NOT NULL,
    expected_transition_trace_root text NOT NULL,
    expected_event_to_step_root text NOT NULL,
    expected_withdrawal_count bigint NOT NULL,
    expected_forced_transaction_count bigint NOT NULL,
    expected_l2_transaction_count bigint NOT NULL,
    expected_deposit_count bigint NOT NULL,
    expected_total_event_count bigint NOT NULL,
    expected_transition_step_count bigint NOT NULL,
    consensus_profile_id text NOT NULL,
    expected_validation_traces_root text NOT NULL,
    expected_validation_trace_count bigint NOT NULL,
    ledger_delta_spent jsonb NOT NULL,
    ledger_delta_produced jsonb NOT NULL,
    mpf_owner_schema smallint,
    mpf_owner_binary_sha256 bytea,
    mpf_replay_base_root bytea,
    mpf_replay_candidate_root bytea,
    mpf_replay_event_log bytea,
    mpf_replay_event_log_digest bytea,
    mpf_replay_event_roots bytea,
    mpf_replay_event_count integer,
    commit_anchor_hash bytea,
    commit_anchor_height bigint,
    commit_anchor_slot bigint,
    block_time timestamp with time zone NOT NULL,
    CONSTRAINT pending_block_finalizations_base_tail_header_hash_check CHECK ((octet_length(base_tail_header_hash) = 28)),
    CONSTRAINT pending_block_finalizations_block_time_covers_end CHECK ((block_end_time <= block_time)),
    CONSTRAINT pending_block_finalizations_commit_anchor_hash_length CHECK (((commit_anchor_hash IS NULL) OR (octet_length(commit_anchor_hash) = 32))),
    CONSTRAINT pending_block_finalizations_commit_anchor_whole CHECK ((((commit_anchor_hash IS NULL) = (commit_anchor_height IS NULL)) AND ((commit_anchor_height IS NULL) = (commit_anchor_slot IS NULL)))),
    CONSTRAINT pending_block_finalizations_consensus_profile_id_check CHECK ((consensus_profile_id = 'midgard-consensus-v1'::text)),
    CONSTRAINT pending_block_finalizations_hub_oracle_policy_id_check CHECK ((hub_oracle_policy_id ~ '^[0-9a-f]{56}$'::text)),
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
    CONSTRAINT pending_block_finalizations_format_version_check CHECK ((format_version = 1)),
    CONSTRAINT pending_block_finalizations_header_cbor_check CHECK ((octet_length(header_cbor) > 0)),
    CONSTRAINT pending_block_finalizations_mpf_replay_all_or_none_check CHECK ((((replay_kind = 'ledger_delta_v1'::text) AND (mpf_owner_schema IS NULL) AND (mpf_owner_binary_sha256 IS NULL) AND (mpf_replay_base_root IS NULL) AND (mpf_replay_candidate_root IS NULL) AND (mpf_replay_event_log IS NULL) AND (mpf_replay_event_log_digest IS NULL) AND (mpf_replay_event_roots IS NULL) AND (mpf_replay_event_count IS NULL)) OR ((replay_kind = 'ledger_delta_native_mpf_v1'::text) AND (mpf_owner_schema = 1) AND (octet_length(mpf_owner_binary_sha256) = 32) AND (octet_length(mpf_replay_base_root) = 32) AND (encode(mpf_replay_base_root, 'hex'::text) = base_utxos_root) AND (octet_length(mpf_replay_candidate_root) = 32) AND (encode(mpf_replay_candidate_root, 'hex'::text) = expected_utxos_root) AND (octet_length(mpf_replay_event_log) >= 92) AND (octet_length(mpf_replay_event_log_digest) = 32) AND (mpf_replay_event_count >= 0) AND (octet_length(mpf_replay_event_roots) = (mpf_replay_event_count * 32))))),
    CONSTRAINT pending_block_finalizations_prepared_tx_hash_check CHECK (((prepared_tx_hash IS NULL) OR (octet_length(prepared_tx_hash) = 32))),
    CONSTRAINT pending_block_finalizations_profile_validation_check CHECK (((expected_validation_trace_count = (expected_forced_transaction_count + expected_l2_transaction_count)) AND (((expected_validation_trace_count = 0) AND (expected_validation_traces_root = '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8'::text)) OR ((expected_validation_trace_count > 0) AND (expected_validation_traces_root <> '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8'::text))))),
    CONSTRAINT pending_block_finalizations_replay_kind_check CHECK ((replay_kind = ANY (ARRAY['ledger_delta_v1'::text, 'ledger_delta_native_mpf_v1'::text]))),
    CONSTRAINT pending_block_finalizations_validation_count_check CHECK ((expected_validation_trace_count >= 0)),
    CONSTRAINT pending_block_finalizations_validation_root_check CHECK ((expected_validation_traces_root ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT pending_signed_intent_matches_prepared CHECK (((intended_tx_hash IS NULL) OR (prepared_tx_hash IS NULL) OR (intended_tx_hash = prepared_tx_hash)))
);

CREATE TABLE public.processed_mempool (
    tx_id bytea NOT NULL,
    tx bytea NOT NULL,
    time_stamp_tz timestamp with time zone DEFAULT now() NOT NULL,
    included_by bytea,
    CONSTRAINT processed_mempool_included_by_check CHECK ((octet_length(included_by) = 28))
);

-- class: B; retention: a settlement attempt's row is never deleted: it keeps the signed body and its fee reservation
CREATE TABLE public.settlement_attempts (
    hub_oracle_policy_id text NOT NULL,
    tx_hash text NOT NULL,
    kind text NOT NULL,
    event_id text NOT NULL,
    phase text NOT NULL,
    signed_cbor text NOT NULL,
    required_outputs integer[] NOT NULL,
    fee_inputs text[] NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone DEFAULT clock_timestamp() NOT NULL,
    CONSTRAINT settlement_attempts_fee_inputs_check CHECK ((cardinality(fee_inputs) > 0)),
    CONSTRAINT settlement_attempts_phase_check CHECK ((phase = ANY (ARRAY['absorb'::text, 'initialize'::text, 'fund'::text, 'conclude'::text]))),
    CONSTRAINT settlement_attempts_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'final'::text, 'expired'::text]))),
    CONSTRAINT settlement_attempts_tx_hash_check CHECK ((tx_hash ~ '^[0-9a-f]{64}$'::text))
);

-- Operational journals are committed before a signed transaction leaves the node.
CREATE TABLE public.settlement_jobs (
    hub_oracle_policy_id text NOT NULL,
    kind text NOT NULL,
    event_id text NOT NULL,
    phase text NOT NULL,
    due_at timestamp with time zone DEFAULT clock_timestamp() NOT NULL,
    created_at timestamp with time zone DEFAULT clock_timestamp() NOT NULL,
    last_error text,
    failures integer DEFAULT 0 NOT NULL,
    CONSTRAINT settlement_jobs_kind_check CHECK ((kind = ANY (ARRAY['deposit'::text, 'withdrawal'::text]))),
    CONSTRAINT settlement_jobs_phase_check CHECK ((phase = ANY (ARRAY['absorb'::text, 'initialize'::text, 'fund'::text, 'conclude'::text, 'complete'::text])))
);

CREATE TABLE public.settlement_owners (
    hub_oracle_policy_id text NOT NULL,
    wallet_address text NOT NULL,
    owner_token uuid NOT NULL,
    lease_until timestamp with time zone NOT NULL
);

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

CREATE TABLE public.tx_admission_payloads (
    tx_id bytea NOT NULL,
    tx_canonical_cbor bytea NOT NULL,
    CONSTRAINT tx_admission_payloads_tx_canonical_cbor_check CHECK ((octet_length(tx_canonical_cbor) > 0))
);

-- class: D; retention: a terminal row is deleted once it is older than `ADMISSION_TERMINAL_RETENTION_MS` and neither a pending row nor a rejection names its tx; `tx_admission_payloads` keeps a row only until its admission's terminal mark
-- An admission row carries its payload's size and its canonical V1 full
-- hash, so neither the backlog byte quota nor the duplicate check reads the
-- payload table. A `peer` row was forwarded by another active operator: a
-- "not yet" verdict forgets it instead of rejecting it for good.
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
    payload_bytes integer NOT NULL,
    tx_full_hash_v1 bytea NOT NULL,
    CONSTRAINT tx_admissions_attempt_count_check CHECK ((attempt_count >= 0)),
    CONSTRAINT tx_admissions_check CHECK (((last_seen_at >= first_seen_at) AND (updated_at >= first_seen_at))),
    CONSTRAINT tx_admissions_check1 CHECK ((((status = 'validating'::public.tx_admission_status) AND (lease_owner IS NOT NULL) AND (lease_expires_at IS NOT NULL) AND (terminal_at IS NULL)) OR ((status <> 'validating'::public.tx_admission_status) AND (lease_owner IS NULL) AND (lease_expires_at IS NULL)))),
    CONSTRAINT tx_admissions_check2 CHECK ((((status = ANY (ARRAY['accepted'::public.tx_admission_status, 'rejected'::public.tx_admission_status])) AND (terminal_at IS NOT NULL)) OR ((status = ANY (ARRAY['queued'::public.tx_admission_status, 'validating'::public.tx_admission_status])) AND (terminal_at IS NULL)))),
    CONSTRAINT tx_admissions_check3 CHECK ((((status = 'rejected'::public.tx_admission_status) AND (reject_code IS NOT NULL)) OR ((status <> 'rejected'::public.tx_admission_status) AND (reject_code IS NULL) AND (reject_detail IS NULL)))),
    CONSTRAINT tx_admissions_payload_bytes_check CHECK ((payload_bytes > 0)),
    CONSTRAINT tx_admissions_request_count_check CHECK ((request_count >= 1)),
    CONSTRAINT tx_admissions_submit_source_check CHECK ((submit_source = ANY (ARRAY['native'::text, 'backfill'::text, 'peer'::text]))),
    CONSTRAINT tx_admissions_tx_full_hash_v1_check CHECK ((octet_length(tx_full_hash_v1) = 32)),
    CONSTRAINT tx_admissions_tx_id_check CHECK ((octet_length(tx_id) = 32))
)
WITH (autovacuum_vacuum_scale_factor='0.01', autovacuum_vacuum_threshold='50000', autovacuum_analyze_scale_factor='0.02', autovacuum_analyze_threshold='50000', autovacuum_vacuum_cost_delay='0');

CREATE SEQUENCE public.tx_admissions_arrival_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.tx_admissions_arrival_seq_seq OWNED BY public.tx_admissions.arrival_seq;

-- class: D-x; retention: a row lives while its rejection does (it cascades with the `tx_rejections` row)
-- One row per rejected transaction a "dependent" or "batch" rejection
-- follows from: the producer of a rejected output the transaction spends,
-- or a rejected member of the batch it was accepted in. A rejection with no
-- row here is not traced to another transaction. When a landed own block's
-- journal is revived, its members' rejections are deleted, and so is every
-- rejection whose causes are all deleted ones, transitively.
CREATE TABLE public.tx_rejection_causes (
    tx_id bytea NOT NULL,
    cause_tx_id bytea NOT NULL
);

CREATE TABLE public.tx_rejections (
    tx_id bytea NOT NULL,
    reject_code text NOT NULL,
    reject_detail text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.withdrawal_utxos (
    classification_revision integer DEFAULT 0 NOT NULL,
    reopened_from_header_hash bytea,
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
    l1_event_key bytea,
    l1_origin_outref bytea,
    l2_ledger_outref bytea,
    l2_ledger_outref_state text NOT NULL,
    CONSTRAINT withdrawal_ledger_outref_state CHECK ((((l2_ledger_outref_state = 'decoded'::text) AND (l2_ledger_outref IS NOT NULL)) OR ((l2_ledger_outref_state = 'undecodable'::text) AND (l2_ledger_outref IS NULL)))),
    CONSTRAINT withdrawal_utxos_asset_name_check CHECK (((octet_length(asset_name) >= 1) AND (octet_length(asset_name) <= 32))),
    CONSTRAINT withdrawal_utxos_check CHECK (((status = 'awaiting'::text) OR (settlement_event_info IS NOT NULL))),
    CONSTRAINT withdrawal_utxos_check1 CHECK (((status = 'awaiting'::text) OR (validity IS NOT NULL))),
    CONSTRAINT withdrawal_utxos_check2 CHECK (((status <> 'awaiting'::text) OR (projected_header_hash IS NULL))),
    CONSTRAINT withdrawal_utxos_classification_revision_check CHECK ((classification_revision >= 0)),
    CONSTRAINT withdrawal_utxos_l1_admission_check CHECK ((((l1_event_key IS NULL) AND (l1_origin_outref IS NULL)) OR ((l1_event_key IS NOT NULL) AND (l1_origin_outref IS NOT NULL) AND (octet_length(l1_event_key) = 32) AND (octet_length(l1_origin_outref) = 34)))),
    CONSTRAINT withdrawal_utxos_l2_owner_check CHECK ((octet_length(l2_owner) = 28)),
    CONSTRAINT withdrawal_utxos_reopened_from_header_hash_check CHECK ((octet_length(reopened_from_header_hash) = 28)),
    CONSTRAINT withdrawal_utxos_status_check CHECK ((status = ANY (ARRAY['awaiting'::text, 'projected'::text, 'finalized'::text]))),
    CONSTRAINT withdrawal_utxos_validity_check CHECK (((validity IS NULL) OR (validity = ANY (ARRAY['WithdrawalIsValid'::text, 'NonExistentWithdrawalUtxo'::text, 'SpentWithdrawalUtxo'::text, 'IncorrectWithdrawalOwner'::text, 'IncorrectWithdrawalValue'::text, 'IncorrectWithdrawalSignature'::text, 'TooManyTokensInWithdrawal'::text, 'UnpayableWithdrawalValue'::text])))),
    CONSTRAINT withdrawal_utxos_withdrawal_l1_output_index_check CHECK ((withdrawal_l1_output_index >= 0)),
    CONSTRAINT withdrawal_utxos_withdrawal_l1_tx_hash_check CHECK ((octet_length(withdrawal_l1_tx_hash) = 32))
);

ALTER TABLE ONLY public.blocks ALTER COLUMN height SET DEFAULT nextval('public.blocks_height_seq'::regclass);

ALTER TABLE ONLY public.tx_admissions ALTER COLUMN arrival_seq SET DEFAULT nextval('public.tx_admissions_arrival_seq_seq'::regclass);

ALTER TABLE ONLY public.address_history
    ADD CONSTRAINT address_history_tx_id_address_key UNIQUE (tx_id, address);

ALTER TABLE ONLY public.blocks
    ADD CONSTRAINT blocks_pkey PRIMARY KEY (height);

ALTER TABLE ONLY public.blocks
    ADD CONSTRAINT blocks_tx_id_key UNIQUE (tx_id);

ALTER TABLE ONLY public.commit_build_calibration
    ADD CONSTRAINT commit_build_calibration_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.confirmed_ledger
    ADD CONSTRAINT confirmed_ledger_pkey PRIMARY KEY (outref);

ALTER TABLE ONLY public.da_payload_announcements
    ADD CONSTRAINT da_payload_announcements_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.da_payload_publications
    ADD CONSTRAINT da_payload_publications_pkey PRIMARY KEY (header_hash, peer_id);

ALTER TABLE ONLY public.da_payloads
    ADD CONSTRAINT da_payloads_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.deposits_utxos
    ADD CONSTRAINT deposits_utxos_pkey PRIMARY KEY (event_id);

ALTER TABLE ONLY public.follower_event_ingestion
    ADD CONSTRAINT follower_event_ingestion_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.forced_transaction_utxos
    ADD CONSTRAINT forced_transaction_utxos_pkey PRIMARY KEY (tx_order_id);

ALTER TABLE ONLY public.forced_transaction_utxos
    ADD CONSTRAINT forced_transaction_utxos_tx_order_l1_tx_hash_tx_order_l1_ou_key UNIQUE (tx_order_l1_tx_hash, tx_order_l1_output_index);

ALTER TABLE ONLY public.immutable
    ADD CONSTRAINT immutable_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.intent_refusal_holds
    ADD CONSTRAINT intent_refusal_holds_pkey PRIMARY KEY (family);

ALTER TABLE ONLY public.mempool_ledger
    ADD CONSTRAINT mempool_ledger_pkey PRIMARY KEY (outref);

ALTER TABLE ONLY public.mempool
    ADD CONSTRAINT mempool_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.mempool_tx_deltas
    ADD CONSTRAINT mempool_tx_deltas_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.mpf_engine_state
    ADD CONSTRAINT mpf_engine_state_pkey PRIMARY KEY (store_name);

ALTER TABLE ONLY public.node_confirmed_ledger_frontier
    ADD CONSTRAINT node_confirmed_ledger_frontier_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.node_confirmed_ledger_spent
    ADD CONSTRAINT node_confirmed_ledger_spent_pkey PRIMARY KEY (header_hash, outref);

ALTER TABLE ONLY public.node_confirmed_merges
    ADD CONSTRAINT node_confirmed_merges_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.node_deployment
    ADD CONSTRAINT node_deployment_pkey PRIMARY KEY (singleton);

ALTER TABLE ONLY public.node_follower_write_gate
    ADD CONSTRAINT node_follower_write_gate_pkey PRIMARY KEY (singleton);

ALTER TABLE ONLY public.node_foreign_tx_inclusions
    ADD CONSTRAINT node_foreign_tx_inclusions_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.node_import_check_blocks
    ADD CONSTRAINT node_import_check_blocks_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.node_import_check_outputs
    ADD CONSTRAINT node_import_check_outputs_pkey PRIMARY KEY (header_hash, out_ref);

ALTER TABLE ONLY public.node_import_checks
    ADD CONSTRAINT node_import_checks_pkey PRIMARY KEY (header_hash, tx_id);

ALTER TABLE ONLY public.node_landed_blocks
    ADD CONSTRAINT node_landed_blocks_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.node_ledger_snapshot_anchor
    ADD CONSTRAINT node_ledger_snapshot_anchor_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.node_ledger_snapshot_staging
    ADD CONSTRAINT node_ledger_snapshot_staging_pkey PRIMARY KEY (outref);

ALTER TABLE ONLY public.operator_membership_observations
    ADD CONSTRAINT operator_membership_observations_pkey PRIMARY KEY (hub_oracle_policy_id, operator_key);

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_ste_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_step_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_trans_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_transactions_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_t_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_trace_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_validation_traces
    ADD CONSTRAINT pending_block_finalization_validation_traces_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_validation_traces
    ADD CONSTRAINT pending_block_finalization_validation_traces_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_header_hash_ordinal_key UNIQUE (header_hash, ordinal);

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_pkey PRIMARY KEY (header_hash, member_id);

ALTER TABLE ONLY public.pending_block_finalizations
    ADD CONSTRAINT pending_block_finalizations_pkey PRIMARY KEY (header_hash);

ALTER TABLE ONLY public.processed_mempool
    ADD CONSTRAINT processed_mempool_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.settlement_attempts
    ADD CONSTRAINT settlement_attempts_pkey PRIMARY KEY (tx_hash);

ALTER TABLE ONLY public.settlement_jobs
    ADD CONSTRAINT settlement_jobs_pkey PRIMARY KEY (hub_oracle_policy_id, kind, event_id);

ALTER TABLE ONLY public.settlement_owners
    ADD CONSTRAINT settlement_owners_pkey PRIMARY KEY (hub_oracle_policy_id);

ALTER TABLE ONLY public.settlement_owners
    ADD CONSTRAINT settlement_owners_wallet_address_key UNIQUE (wallet_address);

ALTER TABLE ONLY public.state_queue_mutation_leases
    ADD CONSTRAINT state_queue_mutation_leases_pkey PRIMARY KEY (token);

ALTER TABLE ONLY public.tx_admission_payloads
    ADD CONSTRAINT tx_admission_payloads_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.tx_admissions
    ADD CONSTRAINT tx_admissions_pkey PRIMARY KEY (tx_id);

ALTER TABLE ONLY public.tx_rejection_causes
    ADD CONSTRAINT tx_rejection_causes_pkey PRIMARY KEY (tx_id, cause_tx_id);

ALTER TABLE ONLY public.withdrawal_utxos
    ADD CONSTRAINT withdrawal_utxos_pkey PRIMARY KEY (event_id);

ALTER TABLE ONLY public.withdrawal_utxos
    ADD CONSTRAINT withdrawal_utxos_withdrawal_l1_tx_hash_withdrawal_l1_output_key UNIQUE (withdrawal_l1_tx_hash, withdrawal_l1_output_index);

CREATE INDEX idx_address_history_address_tx_id ON public.address_history USING btree (address, tx_id);

CREATE INDEX idx_address_history_created_at ON public.address_history USING btree (created_at);

CREATE INDEX idx_blocks_header_hash ON public.blocks USING btree (header_hash);

CREATE INDEX idx_blocks_header_hash_tx_id ON public.blocks USING btree (header_hash, tx_id);

CREATE INDEX idx_blocks_tx_id ON public.blocks USING btree (tx_id);

CREATE INDEX idx_confirmed_ledger_address ON public.confirmed_ledger USING btree (address);

CREATE INDEX idx_da_payload_announcements_retry ON public.da_payload_announcements USING btree (next_retry_at, lease_expires_at, header_hash) WHERE (status = ANY (ARRAY['pending'::text, 'failed'::text]));

CREATE INDEX idx_da_payload_publications_retry ON public.da_payload_publications USING btree (next_retry_at, lease_expires_at, header_hash, peer_id) WHERE (status = ANY (ARRAY['pending'::text, 'rejected'::text, 'transport_error'::text]));

CREATE INDEX idx_da_payloads_created_at ON public.da_payloads USING btree (created_at);

CREATE INDEX idx_deposits_utxos_deposit_l1_tx_hash ON public.deposits_utxos USING btree (deposit_l1_tx_hash);

CREATE INDEX idx_deposits_utxos_l1_admission ON public.deposits_utxos USING btree (l1_event_key);

CREATE INDEX idx_deposits_utxos_projected_header_hash ON public.deposits_utxos USING btree (projected_header_hash);

CREATE INDEX idx_deposits_utxos_status_inclusion_time_event_id ON public.deposits_utxos USING btree (status, inclusion_time, event_id);

CREATE INDEX idx_forced_transaction_utxos_projected_header_hash ON public.forced_transaction_utxos USING btree (projected_header_hash);

CREATE INDEX idx_forced_transaction_utxos_status_inclusion_time_tx_order_id ON public.forced_transaction_utxos USING btree (status, inclusion_time, tx_order_id);

CREATE INDEX idx_forced_transaction_utxos_tx_id ON public.forced_transaction_utxos USING btree (tx_id);

CREATE INDEX idx_immutable_time_stamp_tz ON public.immutable USING btree (time_stamp_tz);

CREATE INDEX idx_mempool_included_by ON public.mempool USING btree (included_by) WHERE (included_by IS NOT NULL);

CREATE INDEX idx_mempool_ledger_address ON public.mempool_ledger USING btree (address);

CREATE INDEX idx_mempool_ledger_address_outref ON public.mempool_ledger USING btree (address, outref);

CREATE INDEX idx_mempool_time_stamp_tz_tx_id ON public.mempool USING btree (time_stamp_tz, tx_id);

-- The output at an out-ref a fold spent from `confirmed_ledger` (a confirmed
-- root older than the frontier, served to a peer fetching a ledger snapshot
-- while the node folds): one index probe per out-ref.
CREATE INDEX idx_node_confirmed_ledger_spent_outref ON public.node_confirmed_ledger_spent USING btree (outref);

CREATE INDEX idx_node_confirmed_merges_merge_slot ON public.node_confirmed_merges USING btree (merge_slot);

CREATE INDEX idx_node_import_checks_deadline ON public.node_import_checks USING btree (deadline_ms);

CREATE INDEX idx_node_import_checks_state_deadline ON public.node_import_checks USING btree (state, deadline_ms, header_hash, block_position);

-- The output at an out-ref a landed block produced and `confirmed_ledger`
-- does not hold yet (a replay on a ledger the unfolded rows lead to): one
-- index probe per out-ref.
CREATE INDEX idx_node_landed_blocks_produced_outrefs ON public.node_landed_blocks USING gin (produced_outrefs);

CREATE INDEX idx_processed_mempool_included_by ON public.processed_mempool USING btree (included_by) WHERE (included_by IS NOT NULL);

CREATE INDEX idx_processed_mempool_time_stamp_tz_tx_id ON public.processed_mempool USING btree (time_stamp_tz, tx_id);

CREATE INDEX idx_state_queue_mutation_leases_status_updated ON public.state_queue_mutation_leases USING btree (status, acquired_at);

CREATE INDEX idx_tx_admissions_active_lease ON public.tx_admissions USING btree (lease_owner, tx_id) WHERE (status = 'validating'::public.tx_admission_status);

CREATE INDEX idx_tx_admissions_lease ON public.tx_admissions USING btree (lease_expires_at) WHERE (status = 'validating'::public.tx_admission_status);

CREATE INDEX idx_tx_admissions_queued_arrival ON public.tx_admissions USING btree (arrival_seq, tx_id) WHERE (status = 'queued'::public.tx_admission_status);

CREATE INDEX idx_tx_admissions_terminal ON public.tx_admissions USING btree (terminal_at, tx_id) WHERE (status = ANY (ARRAY['accepted'::public.tx_admission_status, 'rejected'::public.tx_admission_status]));

CREATE INDEX idx_tx_rejection_causes_cause ON public.tx_rejection_causes USING btree (cause_tx_id);

CREATE INDEX idx_tx_rejections_created_at ON public.tx_rejections USING btree (created_at);

CREATE INDEX idx_tx_rejections_tx_id ON public.tx_rejections USING btree (tx_id);

CREATE INDEX idx_withdrawal_ledger_outref_pending ON public.withdrawal_utxos USING btree (l2_ledger_outref) WHERE ((l2_ledger_outref_state = 'decoded'::text) AND (status = ANY (ARRAY['awaiting'::text, 'projected'::text])) AND ((validity IS NULL) OR (validity = 'WithdrawalIsValid'::text)));

CREATE INDEX idx_withdrawal_utxos_l1_admission ON public.withdrawal_utxos USING btree (l1_event_key);

CREATE INDEX idx_withdrawal_utxos_l1_admission_tx ON public.withdrawal_utxos USING btree (SUBSTRING(l1_origin_outref FROM 1 FOR 32));

CREATE INDEX idx_withdrawal_utxos_l2_outref ON public.withdrawal_utxos USING btree (l2_outref);

CREATE INDEX idx_withdrawal_utxos_projected_header_hash ON public.withdrawal_utxos USING btree (projected_header_hash);

CREATE INDEX idx_withdrawal_utxos_status_inclusion_time_event_id ON public.withdrawal_utxos USING btree (status, inclusion_time, event_id);

CREATE INDEX idx_withdrawal_utxos_withdrawal_l1_tx_hash ON public.withdrawal_utxos USING btree (withdrawal_l1_tx_hash);

CREATE INDEX settlement_attempts_event ON public.settlement_attempts USING btree (hub_oracle_policy_id, kind, event_id, created_at);

CREATE INDEX settlement_attempts_open ON public.settlement_attempts USING btree (hub_oracle_policy_id, created_at, tx_hash) WHERE (status = 'pending'::text);

CREATE INDEX settlement_jobs_due ON public.settlement_jobs USING btree (hub_oracle_policy_id, due_at, created_at) WHERE (phase <> 'complete'::text);

CREATE UNIQUE INDEX uniq_mempool_ledger_source_event_id ON public.mempool_ledger USING btree (source_event_id) WHERE (source_event_id IS NOT NULL);

CREATE UNIQUE INDEX uniq_node_landed_blocks_processed_parent ON public.node_landed_blocks USING btree (parent_header_hash) WHERE (state = 'processed'::text);

CREATE UNIQUE INDEX uniq_state_queue_mutation_leases_active_scope ON public.state_queue_mutation_leases USING btree (scope) WHERE (status = 'active'::text);

CREATE UNIQUE INDEX uniq_tx_rejections_tx_id ON public.tx_rejections USING btree (tx_id);

CREATE TRIGGER deposit_automatic_settlement AFTER INSERT OR UPDATE OF status ON public.deposits_utxos FOR EACH ROW EXECUTE FUNCTION public.enqueue_automatic_settlement();

CREATE TRIGGER withdrawal_automatic_settlement AFTER INSERT OR UPDATE OF status, validity ON public.withdrawal_utxos FOR EACH ROW EXECUTE FUNCTION public.enqueue_automatic_settlement();

ALTER TABLE ONLY public.da_payload_announcements
    ADD CONSTRAINT da_payload_announcements_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.da_payloads(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.da_payload_publications
    ADD CONSTRAINT da_payload_publications_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.da_payloads(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.mempool_ledger
    ADD CONSTRAINT mempool_ledger_source_event_id_fkey FOREIGN KEY (source_event_id) REFERENCES public.deposits_utxos(event_id) ON DELETE RESTRICT;

ALTER TABLE ONLY public.node_confirmed_ledger_spent
    ADD CONSTRAINT node_confirmed_ledger_spent_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.node_confirmed_merges(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.node_import_check_outputs
    ADD CONSTRAINT node_import_check_outputs_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.node_import_check_blocks(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.node_import_checks
    ADD CONSTRAINT node_import_checks_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.node_import_check_blocks(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_deposits
    ADD CONSTRAINT pending_block_finalization_deposits_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_event_to_step
    ADD CONSTRAINT pending_block_finalization_event_to_step_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_forced_transactions
    ADD CONSTRAINT pending_block_finalization_forced_transactions_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_transition_trace
    ADD CONSTRAINT pending_block_finalization_transition_trace_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_txs
    ADD CONSTRAINT pending_block_finalization_txs_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_validation_traces
    ADD CONSTRAINT pending_block_finalization_validation_traces_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.pending_block_finalization_withdrawals
    ADD CONSTRAINT pending_block_finalization_withdrawals_header_hash_fkey FOREIGN KEY (header_hash) REFERENCES public.pending_block_finalizations(header_hash) ON DELETE CASCADE;

ALTER TABLE ONLY public.settlement_attempts
    ADD CONSTRAINT settlement_attempts_hub_oracle_policy_id_kind_event_id_fkey FOREIGN KEY (hub_oracle_policy_id, kind, event_id) REFERENCES public.settlement_jobs(hub_oracle_policy_id, kind, event_id);

ALTER TABLE ONLY public.tx_admission_payloads
    ADD CONSTRAINT tx_admission_payloads_tx_id_fkey FOREIGN KEY (tx_id) REFERENCES public.tx_admissions(tx_id) ON DELETE CASCADE;

ALTER TABLE ONLY public.tx_rejection_causes
    ADD CONSTRAINT tx_rejection_causes_tx_id_fkey FOREIGN KEY (tx_id) REFERENCES public.tx_rejections(tx_id) ON DELETE CASCADE;

-- Singleton seed rows.
INSERT INTO public.commit_build_calibration (id, ms_per_tx_ewma, sample_count)
VALUES (1, 1.0, 0);

INSERT INTO public.node_follower_write_gate (singleton) VALUES (true);
