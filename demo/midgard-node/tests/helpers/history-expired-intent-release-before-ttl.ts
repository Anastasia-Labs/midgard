import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as MigrationRunner from "../../src/database/migrations/runner.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import type { EventHistorySourceBinding } from "../../src/l1-event-history-source.js";
import { signedIntentReplacementDigest } from "../../src/services/canonical-journal-recovery.js";
import type { HistoryOwnerChange } from "../../src/services/event-history-owner.js";
import {
  expiredIntentReleaseDisposition,
  type SignedIntentDeferral,
} from "../../src/services/history-expired-intent-release.table.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "../utils.js";

/**
 * Shared SQL model of the pre-TTL signed-intent release suites: journals of
 * this node, and canonical block application receipts, written directly.
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

export const E = signedCommit(BASE_OUT, TTL);
export const E_HEADER = bytes("e-header", 28);

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
      header_cbor: Buffer.from("a0", "hex"),
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

export const dispose = (
  deferral: SignedIntentDeferral,
  historyChange: HistoryOwnerChange,
) =>
  expiredIntentReleaseDisposition({
    binding,
    change: historyChange,
    deferral,
    rewindAuthority: undefined as never,
  });

export const FOREIGN = hex("foreign-apply");
