import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as MigrationRunner from "../../src/database/migrations/runner.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import type { EventHistorySourceBinding } from "../../src/l1-event-history-source.js";
import { signedIntentReplacementDigest } from "../../src/services/canonical-journal-recovery.js";
import type { HistoryOwnerChange } from "../../src/services/event-history-owner.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "../utils.js";

/**
 * Shared SQL model of the journal recovery suites: journals of this node,
 * and canonical block application receipts, written directly.
 */

export const bytes = (label: string, length = 32) =>
  deterministicFixtureBytes(`before-ttl:${label}`, length);
export const hex = (label: string) => bytes(label).toString("hex");

export const BINDING = hex("binding");
export const binding = { digest: BINDING } as EventHistorySourceBinding;
export const BASE_TX = hex("base-tx");
export const BASE_OUT = `${BASE_TX}#0`;
export const BASE_HEADER = bytes("base-header", 28);
export const ROOT_HEADER = Buffer.alloc(28);
export const UTXOS_ROOT = hex("utxos-root");
export const TTL = 1_000;
export const ZERO_ROOT = "00".repeat(32);
export const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

/** A signed commit spending `spent` with validity upper bound `ttl`. */
export const signedCommit = (spent: string, ttl: number) => {
  const [txHash, index] = spent.split("#");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash!),
      BigInt(index!),
    ),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  body.set_ttl(BigInt(ttl));
  const hash = CML.hash_transaction(body).to_hex();
  const tx = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const cbor = Buffer.from(tx.to_cbor_hex(), "hex");
  tx.free();
  return { hash, cbor };
};

/** Header bytes of the fixture blocks `fixtureHeader` named, by header hash. */
const FIXTURE_HEADER_CBOR = new Map<string, Buffer>();

/**
 * A fixture block's header hash: the hash of a header on `base` moving
 * `baseRoot` to `root`, told apart by `label`. `insertJournal` writes that
 * header's bytes for it, so its journal names the base and roots the journal
 * records by its own header bytes, as the commit worker writes them; any
 * other header hash is journaled with header bytes that bind nothing.
 */
export const fixtureHeader = (
  label: string,
  base: Buffer,
  baseRoot: string = UTXOS_ROOT,
  root: string = ZERO_ROOT,
) => {
  const header: SDK.Header = {
    prevUtxosRoot: baseRoot,
    utxosRoot: root,
    withdrawalsRoot: ZERO_ROOT,
    forcedTransactionsRoot: ZERO_ROOT,
    transactionsRoot: ZERO_ROOT,
    depositsRoot: ZERO_ROOT,
    transitionTraceRoot: ZERO_ROOT,
    eventToStepRoot: ZERO_ROOT,
    validationTracesRoot: ZERO_ROOT,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
    startTime: 1n,
    endTime: 2n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: base.toString("hex"),
    operatorVkey: bytes(`header:${label}`, 28).toString("hex"),
    protocolVersion: 1n,
  };
  const hash = Effect.runSync(SDK.hashBlockHeader(header));
  FIXTURE_HEADER_CBOR.set(
    hash,
    Buffer.from(Data.to(header as never, SDK.Header as never), "hex"),
  );
  return Buffer.from(hash, "hex");
};

export const E = signedCommit(BASE_OUT, TTL);
export const E_HEADER = fixtureHeader("e-header", BASE_HEADER);

export const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect) as Effect.Effect<A, E, never>,
  );

export const insertJournal = (input: {
  readonly header: Buffer;
  readonly status: Pending.Status;
  readonly commit: { readonly hash: string; readonly cbor: Buffer };
  readonly baseOut: string;
  readonly baseHeader: Buffer;
  readonly createdAt: Date;
  /** Why an abandoned journal was abandoned (unattributed by default). */
  readonly abandonment?: "replacement" | "correction";
}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const at = input.createdAt;
    const txHash = Buffer.from(input.commit.hash, "hex");
    const correctionDigest =
      input.abandonment === "replacement"
        ? signedIntentReplacementDigest({
            [Pending.Columns.HEADER_HASH]: input.header,
            [Pending.Columns.INTENDED_TX_HASH]: txHash,
            [Pending.Columns.SIGNED_TX_CBOR]: input.commit.cbor,
          })!
        : input.abandonment === "correction"
          ? hex(`correction:${input.header.toString("hex")}`)
          : null;
    yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
      header_hash: input.header,
      submitted_tx_hash: null,
      prepared_tx_hash: txHash,
      intended_tx_hash: txHash,
      signed_tx_cbor: input.commit.cbor,
      block_end_time: at,
      status: input.status,
      correction_transition_digest: correctionDigest,
      observed_confirmed_at_ms: null,
      created_at: at,
      updated_at: at,
      state_queue_lease_token: `before-ttl:${input.header.toString("hex").slice(0, 8)}`,
      base_snapshot_id: "before-ttl",
      base_tail_out_ref: input.baseOut,
      base_tail_header_hash: input.baseHeader,
      base_tail_datum_cbor: "d87980",
      base_utxos_root: UTXOS_ROOT,
      base_transactions_root: ZERO_ROOT,
      base_deposits_root: ZERO_ROOT,
      base_withdrawals_root: ZERO_ROOT,
      block_start_time: at,
      expected_utxos_root: ZERO_ROOT,
      expected_transactions_root: ZERO_ROOT,
      expected_deposits_root: ZERO_ROOT,
      expected_withdrawals_root: ZERO_ROOT,
      base_forced_transactions_root: ZERO_ROOT,
      expected_forced_transactions_root: ZERO_ROOT,
      header_cbor:
        FIXTURE_HEADER_CBOR.get(input.header.toString("hex")) ??
        Buffer.from("a0", "hex"),
      format_version: 1,
      replay_kind: "ledger_delta_v1",
      deployment_marker_schema_version: "midgard-deployment-marker-v1",
      deployment_manifest_id: hex("manifest"),
      expected_transition_trace_root: ZERO_ROOT,
      expected_event_to_step_root: ZERO_ROOT,
      expected_withdrawal_count: 0n,
      expected_forced_transaction_count: 0n,
      expected_l2_transaction_count: 0n,
      expected_deposit_count: 0n,
      expected_total_event_count: 0n,
      expected_transition_step_count: 0n,
      consensus_profile_id: MIDGARD_CONSENSUS_PROFILE_ID,
      expected_validation_traces_root: EMPTY_MERKLE_ROOT,
      expected_validation_trace_count: 0n,
      ledger_delta_spent: "[]",
      ledger_delta_produced: "[]",
    } as never)}`;
  });

export const activeE = (baseHeader: Buffer = BASE_HEADER) =>
  insertJournal({
    header: E_HEADER,
    status: Pending.Status.PendingSubmission,
    commit: E,
    baseOut: BASE_OUT,
    baseHeader,
    createdAt: new Date(2_000_000),
  });

/** A retained, locally applied journal of the base D (`BASE_HEADER`, on the
 * root) whose root is the fixture base root `UTXOS_ROOT`: the retained parent
 * journal that binds the replay base of a fixture block on D. */
export const retainedBaseJournal = insertJournal({
  header: BASE_HEADER,
  status: Pending.Status.LocallyApplied,
  commit: signedCommit(`${hex("root-tx")}#0`, TTL - 1),
  baseOut: `${hex("root-tx")}#0`,
  baseHeader: ROOT_HEADER,
  createdAt: new Date(1_000_000),
}).pipe(
  Effect.zipRight(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`UPDATE pending_block_finalizations
        SET expected_utxos_root = base_utxos_root,
          block_end_time = block_start_time + INTERVAL '1 second'
        WHERE header_hash = ${BASE_HEADER}`,
    ),
  ),
);

export type ReceiptTx = Readonly<{
  txHash: string;
  spends?: "inputs" | "collaterals";
  inputs: readonly string[];
}>;

export const receipt = (txs: readonly ReceiptTx[], digest = BINDING) =>
  JSON.stringify({
    bindingDigest: digest,
    block: {
      transactions: txs.map((tx) => ({
        txHash: tx.txHash,
        spends: tx.spends ?? "inputs",
        inputs: tx.inputs.map((ref) => {
          const [txHash, index] = ref.split("#");
          return { txHash, outputIndex: Number(index) };
        }),
      })),
    },
  });

/** The canonical application of the block at `height`. */
export const applyBlock = (
  height: number,
  txs: readonly ReceiptTx[],
  options: { readonly canonical?: boolean; readonly digest?: string } = {},
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO event_history_block_applications (
        binding_digest, block_hash, application_revision, parent_hash,
        parent_application_revision, block_slot, block_height,
        before_snapshot_digest, after_snapshot_digest, ledger_receipt,
        ledger_receipt_digest, undo_record, undo_digest, canonical
      ) VALUES (${Buffer.from(BINDING, "hex")}, ${bytes(`block:${height}`)},
        ${height + 1}, ${bytes(`block:${height - 1}`)}, NULL, ${height * 10},
        ${height}, ${bytes("before")}, ${bytes("after")},
        ${receipt(txs, options.digest)}, ${bytes("receipt-digest")},
        '{}', ${bytes("undo")}, ${options.canonical ?? true})`;
  });

export const seed = Effect.gen(function* () {
  yield* MigrationRunner.migrate({
    appVersion: "test",
    actor: "history-expired-intent-release-before-ttl.test",
  });
  yield* resetApplicationTables;
  const sql = yield* SqlClient.SqlClient;
  yield* sql`INSERT INTO event_history_cursor (
      binding_digest, manifest_id, origin_receipt, origin_receipt_digest,
      anchor_hash, anchor_slot, anchor_height, anchor_snapshot_digest,
      head_hash, head_slot, head_height, snapshot_digest, revision, addresses
    ) VALUES (${Buffer.from(BINDING, "hex")}, ${bytes("manifest")},
      'explicit SQL model', ${bytes("origin")}, ${bytes("anchor")}, 0, 0,
      ${bytes("anchor-snapshot")}, ${bytes("anchor")}, 0, 0,
      ${bytes("anchor-snapshot")}, 0, '[]'::jsonb)`;
});

export const point = (height: number, slot = height * 10) =>
  ({ head: { height, slot } }) as never;

export const change = (
  kind: HistoryOwnerChange["kind"],
  to: number,
  options: { readonly from?: number; readonly slot?: number } = {},
) =>
  ({
    kind,
    before: point(options.from ?? to - 1),
    after: point(to, options.slot),
    changes: [],
  }) as unknown as HistoryOwnerChange;

export const FOREIGN = hex("foreign-apply");
