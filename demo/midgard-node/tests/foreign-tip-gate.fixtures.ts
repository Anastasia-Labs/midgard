import { createHash } from "node:crypto";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
} from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  type DeploymentMarker,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  DepositsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import { ContractDeploymentIdentity } from "../src/services/index.js";
import { computeDaPayloadRoots } from "../src/workers/commit-block-header/da-payload.js";
import { makeDepositEntry } from "./database.test/fixtures.make-deposit-submission-attempt.js";
import { makeHistoryWithdrawalEntry } from "./database.test/fixtures.make-history-withdrawal-entry.js";
import {
  deterministicFixtureOutputReferenceId,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

export const ACTIVE_MARKER = makeDeploymentMarker("ab".repeat(32));
export const OTHER_MARKER = makeDeploymentMarker("cd".repeat(32));
/** A recent foreign block window, inside every challengeability horizon. */
export const WINDOW_END_MS = Math.floor(Date.now() / 1_000) * 1_000 - 120_000;
export const WINDOW_START_MS = WINDOW_END_MS - 10_000;
export const IN_WINDOW = new Date(WINDOW_START_MS + 5_000);
export const INGESTED_PAST_WINDOW = new Date(WINDOW_END_MS + 60_000);
/** A foreign block window far behind every challengeability horizon. */
export const STALE_WINDOW_START_MS = 1_000_000;
export const STALE_WINDOW_END_MS = 1_010_000;
export const NONEMPTY_ROOT = "33".repeat(32);

export const headerFor = (overrides: Partial<SDK.Header> = {}): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
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
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
  ...overrides,
});

/** A foreign header over the fixture window committing to one deposit. */
export const nonEmptyWindowHeader = (
  overrides: Partial<SDK.Header> = {},
): SDK.Header =>
  headerFor({
    depositsRoot: NONEMPTY_ROOT,
    depositCount: 1n,
    totalEventCount: 1n,
    startTime: BigInt(WINDOW_START_MS),
    endTime: BigInt(WINDOW_END_MS),
    ...overrides,
  });

export const oneDepositPayload = async (
  depositId: string,
  headerOverrides: Partial<SDK.Header> = {},
) => {
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const draft: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: headerFor(headerOverrides),
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      deposits: [[depositId, "01"]],
      transition_trace: [],
      event_to_step: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const roots = await Effect.runPromise(computeDaPayloadRoots(draft));
  const header = headerFor({
    ...headerOverrides,
    utxosRoot: roots.utxosRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    validationTracesRoot: roots.validationTracesRoot,
    depositCount: 1n,
    totalEventCount: 1n,
    validationTraceCount: 0n,
  });
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    header,
    payload: {
      ...draft,
      block_body: {
        ...draft.block_body,
        header_hash: headerHash,
        header,
      },
    } satisfies SDK.DaPayload,
  };
};

/** Runs `effect` on a freshly reset database as a node of `marker`. */
export const onNode = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  marker: DeploymentMarker | undefined = ACTIVE_MARKER,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* effect;
      }),
    ).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "derived",
          ...(marker === undefined ? {} : { deploymentMarker: marker }),
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    ) as Effect.Effect<A, E, never>,
  );

/** Retains T2 evidence for `header` exactly as a speculative mismatch does. */
export const recordForeignTip = (
  header: SDK.Header,
  marker: DeploymentMarker = ACTIVE_MARKER,
) =>
  Effect.gen(function* () {
    const foreignHeaderHash = yield* SDK.hashBlockHeader(header);
    yield* ForeignTipReconciliationsDB.recordMismatch({
      foreignHeaderHash,
      replacedBaseHeaderHash: "23".repeat(28),
      foreignHeader: header,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      deploymentMarker: marker,
    });
    return foreignHeaderHash;
  });

/** Indexes one deposit no header has claimed yet. */
export const indexDeposit = (overrides: Partial<DepositsDB.Entry> = {}) =>
  Effect.gen(function* () {
    const entry = makeDepositEntry(overrides);
    yield* DepositsDB.insertEntries([entry]);
    return entry[DepositsDB.Columns.ID].toString("hex");
  });

let eventSequence = 0;
const nextEventBytes = (label: string) =>
  createHash("sha256")
    .update(`foreign-tip-gate.${label}.${(eventSequence += 1).toString()}`)
    .digest();

/** Indexes one forced transaction no header has claimed yet. The gate reads
 * only its window columns, so the proof material is placeholder bytes. */
export const indexForcedTransaction = (
  inclusionTime: Date,
  status: ForcedTransactionsDB.Status = ForcedTransactionsDB.Status.Awaiting,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const id = nextEventBytes("forced");
    const filler = Buffer.from("01", "hex");
    yield* sql`INSERT INTO ${sql(ForcedTransactionsDB.tableName)} ${sql.insert({
      tx_order_id: id,
      tx_order_l1_tx_hash: id,
      tx_order_l1_output_index: 0,
      asset_name: filler,
      raw_datum: filler,
      tx_id: id,
      tx_compact: filler,
      forced_inclusion_value: filler,
      consensus_profile_id: MIDGARD_CONSENSUS_PROFILE_ID,
      native_tx_cbor: filler,
      transaction_commitment: id,
      cek_program_material_sidecar_cbor: filler,
      cek_program_material_sidecar_sha256: id,
      inclusion_time: inclusionTime,
      status,
    })}`;
    return id.toString("hex");
  });

/** Indexes one withdrawal no header has claimed yet. */
export const indexWithdrawal = (
  inclusionTime: Date,
  status: WithdrawalsDB.Status = WithdrawalsDB.Status.Awaiting,
) =>
  Effect.gen(function* () {
    const l1TxHash = nextEventBytes("withdrawal");
    const entry: WithdrawalsDB.Entry = {
      ...makeHistoryWithdrawalEntry(),
      [WithdrawalsDB.Columns.ID]: deterministicFixtureOutputReferenceId(
        `foreign-tip-gate.withdrawal.${l1TxHash.toString("hex")}`,
      ),
      [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: l1TxHash,
      [WithdrawalsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [WithdrawalsDB.Columns.STATUS]: status,
      ...(status === WithdrawalsDB.Status.Awaiting
        ? {}
        : {
            [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: l1TxHash,
            [WithdrawalsDB.Columns.VALIDITY]:
              WithdrawalsDB.Validity.WithdrawalIsValid,
          }),
    };
    yield* WithdrawalsDB.insertEntries([entry]);
    return entry[WithdrawalsDB.Columns.ID].toString("hex");
  });

export const reconciliationRow = (foreignHeaderHash: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      readonly status: string;
      readonly evidence_kind: string;
      readonly blocking_reason: string | null;
      readonly updated_at: Date;
    }>`
      SELECT status, evidence_kind, blocking_reason, updated_at
      FROM foreign_tip_reconciliations
      WHERE foreign_header_hash = ${Buffer.from(foreignHeaderHash, "hex")}
    `;
    return rows[0];
  });

export const depositStatus = (eventIdHex: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly status: string }>`
      SELECT status FROM deposits_utxos
      WHERE event_id = ${Buffer.from(eventIdHex, "hex")}
    `;
    return rows[0]?.status;
  });

/** Makes the foreign block's DA payload locally available, as a peer fetch would. */
export const storeForeignDa = (header: SDK.Header, payload: SDK.DaPayload) =>
  Effect.gen(function* () {
    const headerHash = Buffer.from(yield* SDK.hashBlockHeader(header), "hex");
    const payloadCbor = Buffer.from(
      yield* Effect.promise(() =>
        wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" }),
      ),
    );
    yield* DaPayloadsDB.upsertAvailable({
      [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
      [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
      [DaPayloadsDB.Columns.VERSION]: 1,
      [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payloadCbor,
      [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
        .update(payloadCbor)
        .digest(),
      [DaPayloadsDB.Columns.UTXOS_ROOT]: header.utxosRoot,
      [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
        header.forcedTransactionsRoot,
      [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: header.transactionsRoot,
      [DaPayloadsDB.Columns.DEPOSITS_ROOT]: header.depositsRoot,
      [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: header.withdrawalsRoot,
      [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: header.transitionTraceRoot,
      [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: header.eventToStepRoot,
      [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]:
        header.validationTracesRoot,
      [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: header.withdrawalCount,
      [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
        header.forcedTransactionCount,
      [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: header.l2TransactionCount,
      [DaPayloadsDB.Columns.DEPOSIT_COUNT]: header.depositCount,
      [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: header.totalEventCount,
      // The replay reads only the identity and payload bytes; the row's
      // counts merely satisfy the table's one-step-per-event check.
      [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: header.totalEventCount,
      [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]:
        header.validationTraceCount,
      [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
        Number(header.startTime),
      ),
      [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(Number(header.endTime)),
    });
  });
