import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { SqlClient } from "@effect/sql";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
} from "../src/database/index.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { processMpfs } from "../src/mpf/process.process-mpfs.js";
import type { MidgardMpf } from "../src/mpf/store.midgard-mpf.js";
import {
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.select-commit-tx-candidates.js";
import {
  assertForcedProjectionRows,
  forcedProjectionEntry,
} from "./helpers/commit-da-frame-forced-projection.js";
import {
  deterministicFixtureOutputReferenceId,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

/**
 * Every pass of the DA frame step-down persists its projections and
 * commit-stage rejections. Driving real `processMpfs` against Postgres must
 * match a refused block followed by a fresh selection, with no projection lost
 * or duplicated.
 */

// Phase A accepts normal transactions with their input graphs; Phase B
// applies them in order over the pre-state and rejects one whose input is
// missing, as the real one does.
const graphs = vi.hoisted(
  () =>
    new Map<
      string,
      {
        spentOutRefHexes: readonly string[];
        referenceOutRefHexes: readonly string[];
        produced: readonly LedgerUtils.Entry[];
      }
    >(),
);
const phaseB = vi.hoisted(() => ({ accepted: [] as string[][] }));
vi.mock("@al-ft/midgard-validation", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-validation")
  >("@al-ft/midgard-validation");
  return {
    ...actual,
    runPhaseAValidation: vi.fn(
      (
        txs: readonly { txId: Buffer; sourceKind?: string }[],
        config: Parameters<typeof actual.runPhaseAValidation>[1],
      ) =>
        txs[0]?.sourceKind === "forced"
          ? actual.runPhaseAValidation(
              txs as Parameters<typeof actual.runPhaseAValidation>[0],
              config,
            )
          : Effect.succeed({
              accepted: txs.map((tx) => ({
                ...tx,
                ledgerTx: tx,
                graph: graphs.get(tx.txId.toString("hex"))!,
              })),
              rejected: [],
            }),
    ),
    runPhaseBValidationWithPatch: vi.fn(
      (
        txs: readonly { txId: Buffer }[],
        preState: ReadonlyMap<string, Buffer>,
      ) =>
        Effect.sync(() => {
          const state = new Set(preState.keys());
          const accepted: unknown[] = [];
          const rejected: unknown[] = [];
          for (const { txId } of txs) {
            const graph = graphs.get(txId.toString("hex"))!;
            const spent = graph.spentOutRefHexes;
            if (!spent.every((outRef) => state.has(outRef))) {
              rejected.push({ txId, code: "E_MISSING_INPUT", detail: "" });
              continue;
            }
            for (const outRef of spent) state.delete(outRef);
            for (const entry of graph.produced)
              state.add(entry[LedgerUtils.Columns.OUTREF].toString("hex"));
            accepted.push({
              ledgerTx: { txId },
              graph,
            });
          }
          phaseB.accepted.push(
            accepted.map((a) =>
              (a as { ledgerTx: { txId: Buffer } }).ledgerTx.txId.toString(
                "hex",
              ),
            ),
          );
          return { accepted, rejected };
        }),
    ),
  };
});
// Opaque fixture bytes (no programs, no descriptors) do not affect the
// persisted state compared here.
vi.mock("../src/mpf/ledger-hydration.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/ledger-hydration.js")
  >("../src/mpf/ledger-hydration.js");
  return {
    ...actual,
    encodeTransactionRootValue: vi.fn((txCbor: Buffer) => Buffer.from(txCbor)),
  };
});
const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
const at = (seconds: number) => new Date(T0 + seconds * 1_000);
const outRef = (byte: number) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, byte),
    outputIndex: 0,
  });
const output = (lovelace: bigint) =>
  encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace, assets: new Map() },
  });
const ledgerEntry = (byte: number, txByte = byte): LedgerUtils.Entry => ({
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, txByte),
  [LedgerUtils.Columns.OUTREF]: outRef(byte),
  [LedgerUtils.Columns.OUTPUT]: output(1_000_000n + BigInt(byte)),
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
});

// The withdrawal W withdraws U_w, so T_r (which spends it) is rejected at
// commit. T_c spends T_a's output. Deposits D1 and D2 land between them.
const U1 = ledgerEntry(0x11);
const U2 = ledgerEntry(0x12);
const U3 = ledgerEntry(0x13);
const U_W_LOVELACE = 7_000_000n;
const U_W: LedgerUtils.Entry = {
  ...ledgerEntry(0xaa),
  [LedgerUtils.Columns.OUTPUT]: output(U_W_LOVELACE),
};
const BASE = [U1, U2, U3, U_W];
const tx = (
  byte: number,
  seconds: number,
  spent: readonly LedgerUtils.Entry[],
) => ({
  byte,
  seconds,
  txId: Buffer.alloc(32, byte),
  txCbor: Buffer.from(`ff${byte.toString(16)}`, "hex"),
  spent: spent.map((entry) => entry[LedgerUtils.Columns.OUTREF]),
  produced: [ledgerEntry(byte + 0x40, byte)],
});
const T_A = tx(0x0a, 3, [U1]);
const T_R = tx(0x0b, 4, [U_W]);
const T_B = tx(0x0c, 5, [U2]);
const T_C = tx(0x0d, 7, [T_A.produced[0]!]);
const T_D = tx(0x0e, 8, [U3]);
const TXS = [T_A, T_R, T_B, T_C, T_D];
const hex = (buffer: Buffer) => buffer.toString("hex");
for (const { txId, spent, produced } of TXS)
  graphs.set(hex(txId), {
    spentOutRefHexes: spent.map(hex),
    referenceOutRefHexes: [],
    produced,
  });

const deposit = (label: string, seconds: number): DepositsDB.Entry => ({
  [DepositsDB.Columns.ID]: deterministicFixtureOutputReferenceId(label),
  [DepositsDB.Columns.INFO]: Buffer.alloc(48, 1),
  [DepositsDB.Columns.INCLUSION_TIME]: at(seconds),
  [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.alloc(32, seconds),
  [DepositsDB.Columns.LEDGER_TX_ID]: Buffer.alloc(32, 0x70 + seconds),
  [DepositsDB.Columns.LEDGER_OUTPUT]: output(2_000_000n),
  [DepositsDB.Columns.LEDGER_ADDRESS]: wallet.address,
  [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
});
const D1 = deposit("step-down-persistence.d1", 2);
const D2 = deposit("step-down-persistence.d2", 6);
// D0 was projected by an earlier tick and T_e, still pending beyond this
// block's selection (the batch planner's cut), spent its output: D0 is
// consumed and has no mempool_ledger row. Re-projecting it would bring back
// an output T_e already spent.
const D0 = deposit("step-down-persistence.d0", 1);
const T_E_ID = Buffer.alloc(32, 0x0f);

const seed = (w: WithdrawalsDB.Entry) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* resetApplicationTables;
    yield* MempoolLedgerDB.insert(BASE);
    yield* insertDeposits([D0, D1, D2]);
    const d0Row = yield* DepositsDB.toMempoolLedgerEntry(D0);
    yield* MempoolLedgerDB.reconcileDepositEntries([d0Row]);
    yield* DepositsDB.markAwaitingAsProjected([D0[DepositsDB.Columns.ID]]);
    yield* insertWithdrawals([w]);
    yield* ForcedTransactionsDB.insertEntries([
      yield* forcedProjectionEntry(2, at(2)),
      yield* forcedProjectionEntry(6, at(6)),
    ]);
    // Admission applied every transaction to mempool_ledger and cached its
    // delta, in arrival order.
    const tE = {
      seconds: 9,
      txId: T_E_ID,
      txCbor: Buffer.from("ff0f", "hex"),
      spent: [d0Row[LedgerUtils.Columns.OUTREF]],
      produced: [ledgerEntry(0x4f, 0x0f)],
    };
    for (const { seconds, txId, txCbor, spent, produced } of [...TXS, tE]) {
      yield* MempoolDB.insertMultipleCore([
        { txId, txCbor, spent: [...spent], produced: [...produced] },
      ]);
      yield* MempoolTxDeltasDB.upsertMany([{ txId, spent, produced }]);
      yield* sql`UPDATE mempool SET time_stamp_tz = ${at(seconds)} WHERE tx_id = ${txId}`;
    }
  });

/** The native owner's refusal every build here stops at. */
const NATIVE_STOP = new Error("native owner stop");

const BASE_LEDGER = ledgerOf(BASE);

/**
 * One block build by the real `processMpfs` over `txs`. It runs every
 * database write of the build, then stops where the native owner applies
 * the block's events. Returns the transactions Phase B accepted.
 */
const build = (txs: readonly EntryWithTimeStamp[]) =>
  Effect.gen(function* () {
    phaseB.accepted = [];
    const refused = yield* processMpfs(
      {
        root: () => Effect.succeed(""),
        applyBatch: () => Effect.void,
        resetToRoot: () => Effect.void,
      } as unknown as MidgardMpf,
      txs,
      {
        currentBlockStartTime: at(0),
        baseLedger: yield* Effect.promise(() => BASE_LEDGER),
        nativeMpf: {
          handle: { baseRoot: "22".repeat(32) },
          client: { applyEvents: () => Promise.reject(NATIVE_STOP) },
        } as never,
        validationTraceBuilder: () => Effect.succeed([]),
        forcedValidation: {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 0n,
        },
      },
    ).pipe(Effect.flip);
    expect((refused as { cause?: unknown }).cause).toBe(NATIVE_STOP);
    const accepted = phaseB.accepted.flat();
    return {
      accepted,
      rejectedTxIds: txs
        .map((entry) => entry[TxColumns.TX_ID])
        .filter((txId) => !accepted.includes(hex(txId))),
    };
  });

const snapshot = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return {
    mempool:
      yield* sql`SELECT tx_id, time_stamp_tz FROM mempool ORDER BY tx_id`,
    deltas: yield* sql`SELECT tx_id FROM mempool_tx_deltas ORDER BY tx_id`,
    ledger:
      yield* sql`SELECT tx_id, outref, output, source_event_id FROM mempool_ledger ORDER BY outref`,
    rejections:
      yield* sql`SELECT tx_id, reject_code FROM tx_rejections ORDER BY tx_id`,
    deposits:
      yield* sql`SELECT event_id, status, projected_header_hash FROM deposits_utxos ORDER BY event_id`,
    forced:
      yield* sql`SELECT tx_order_id, status, projected_header_hash, forced_inclusion_value FROM forced_transaction_utxos ORDER BY tx_order_id`,
    withdrawals:
      yield* sql`SELECT event_id, status, validity, classification_revision, settlement_event_info FROM withdrawal_utxos ORDER BY event_id`,
  };
});

const mempoolPage = MempoolDB.retrievePage({ limit: 100 }).pipe(
  Effect.map((page) => page.entries),
);
const ids = (entries: readonly EntryWithTimeStamp[]) =>
  entries.map((entry) => hex(entry[TxColumns.TX_ID]));
const run = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

import { syntheticStepDownStateWriteMeasurement } from "./helpers/commit-da-frame-fixtures.js";
import { insertDeposits, insertWithdrawals } from "./helpers/event-rows.js";
import { ledgerOf } from "./helpers/ledger-of.js";
import { order } from "./process-mpfs-withdrawal.fixture.js";

// Synthetic accounting selects two; production DB helpers persist projections.
const NEXT_TX_COUNT = 2;

describe("DA frame step-down persistence over Postgres", () => {
  it("leaves the state a refused block and a fresh next-tick selection leave, with no projection lost or duplicated", async () => {
    const w = await order(0xc);
    const steppedDown = await run(
      Effect.gen(function* () {
        yield* seed(w);
        const passes: { accepted: readonly string[] }[] = [];
        const result = yield* stepDownCommitSelectionToDaFrame({
          candidateSelection: selectCommitTxCandidates({
            mempoolTxs: (yield* mempoolPage).slice(0, TXS.length),
            processedMempoolTxs: [],
          }),
          maxInnerBytes: 100_000,
          process: (selection) =>
            build(selection.candidateTxs).pipe(
              Effect.tap((built) => passes.push(built)),
            ),
          measure: (built) =>
            Effect.succeed({
              ...syntheticStepDownStateWriteMeasurement(built.accepted),
              rejectedTxIds: built.rejectedTxIds,
            }),
          rebase: Effect.void,
        });
        return {
          nextForcedWindow:
            (yield* ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
              at(9),
            )).map((entry) => hex(entry.tx_order_id)),
          passes: passes.map((pass) => pass.accepted),
          finalSelection: ids(result.candidateSelection.candidateTxs),
          state: yield* snapshot,
          // The next block's window, read without projecting.
          nextWindow: (yield* DepositsDB.retrievePendingHeaderEntriesUpTo(
            at(9),
          )).map((entry) => hex(entry[DepositsDB.Columns.ID])),
        };
      }),
    );
    const crossTick = await run(
      Effect.gen(function* () {
        yield* seed(w);
        // Tick 1 builds the whole selection, which the frame refuses.
        const tick1 = yield* build((yield* mempoolPage).slice(0, TXS.length));
        // Tick 2 selects afresh from the mempool, at the size that fits.
        const tick2Selection = (yield* mempoolPage).slice(0, NEXT_TX_COUNT);
        const tick2 = yield* build(tick2Selection);
        return {
          passes: [tick1.accepted, tick2.accepted],
          finalSelection: ids(tick2Selection),
          state: yield* snapshot,
        };
      }),
    );

    const [a, r, b, c, d] = TXS.map(({ txId }) => hex(txId));
    // Pass 0 rejects T_r (it spends the withdrawn U_w); pass 1 is the first
    // two of the accepted, never the rejected one.
    expect(steppedDown.passes).toEqual([
      [a, b, c, d],
      [a, b],
    ]);
    expect(steppedDown.finalSelection).toEqual([a, b]);
    expect(steppedDown.passes).toEqual(crossTick.passes);
    expect(steppedDown.finalSelection).toEqual(crossTick.finalSelection);
    expect(steppedDown.state).toEqual(crossTick.state);

    const { state } = steppedDown;
    // T_r was rejected once, dropped from the mempool, and its output left
    // mempool_ledger; every other transaction, T_e included, is still pending.
    expect(state.rejections.map((row) => hex(row.tx_id as Buffer))).toEqual([
      r,
    ]);
    expect(state.mempool.map((row) => hex(row.tx_id as Buffer))).toEqual([
      a,
      b,
      c,
      d,
      hex(T_E_ID),
    ]);
    const ledgerOutRefs = state.ledger.map((row) => hex(row.outref as Buffer));
    expect(ledgerOutRefs).not.toContain(
      hex(T_R.produced[0]![LedgerUtils.Columns.OUTREF]),
    );
    // Deposits are projected once; D2 remains pending beyond the final window.
    const statusOf = (id: Buffer) =>
      state.deposits.find(
        (row) => Buffer.compare(row.event_id as Buffer, id) === 0,
      )?.status;
    expect(statusOf(D1[DepositsDB.Columns.ID])).toBe(
      DepositsDB.Status.Projected,
    );
    expect(statusOf(D2[DepositsDB.Columns.ID])).toBe(
      DepositsDB.Status.Projected,
    );
    // Both passes' windows held the consumed D0, and neither projected it
    // again: its spent output stays out of mempool_ledger.
    expect(statusOf(D0[DepositsDB.Columns.ID])).toBe(
      DepositsDB.Status.Consumed,
    );
    expect(
      state.ledger.filter(
        (row) =>
          row.source_event_id !== null &&
          Buffer.compare(
            row.source_event_id as Buffer,
            D0[DepositsDB.Columns.ID],
          ) === 0,
      ),
    ).toEqual([]);
    for (const { [DepositsDB.Columns.ID]: id } of [D1, D2]) {
      expect(
        state.ledger.filter(
          (row) =>
            row.source_event_id !== null &&
            Buffer.compare(row.source_event_id as Buffer, id) === 0,
        ),
      ).toHaveLength(1);
    }
    expect(steppedDown.nextWindow).toContain(hex(D2[DepositsDB.Columns.ID]));
    assertForcedProjectionRows(state.forced);
    expect(steppedDown.nextForcedWindow).toContain(
      hex(deterministicFixtureOutputReferenceId("step-down.f6")),
    );
    // The withdrawal was classified valid and projected, not reclassified.
    expect(state.withdrawals).toHaveLength(1);
    expect(state.withdrawals[0]).toMatchObject({
      status: WithdrawalsDB.Status.Projected,
      validity: WithdrawalsDB.Validity.WithdrawalIsValid,
    });
  }, 120_000);
});
