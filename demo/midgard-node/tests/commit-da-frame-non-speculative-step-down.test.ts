import "./utils.js";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
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
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import {
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.js";
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
 * On the non-speculative path every pass of the DA frame step-down persists
 * its projections and commit-stage rejections. Driving real `processMpfs`
 * against Postgres must match a refused block followed by a fresh selection,
 * with no projection lost or duplicated.
 */

// Phase A accepts normal transactions; Phase B applies them in order over the
// pre-state and rejects one whose input is missing, as the real one does.
const graphs = vi.hoisted(
  () =>
    new Map<
      string,
      { spent: readonly string[]; produced: readonly LedgerUtils.Entry[] }
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
          : Effect.succeed({ accepted: txs, rejected: [] }),
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
            if (!graph.spent.every((outRef) => state.has(outRef))) {
              rejected.push({ txId, code: "E_MISSING_INPUT", detail: "" });
              continue;
            }
            for (const outRef of graph.spent) state.delete(outRef);
            for (const entry of graph.produced)
              state.add(entry[LedgerUtils.Columns.OUTREF].toString("hex"));
            accepted.push({
              ledgerTx: { txId },
              graph: {
                spentOutRefHexes: graph.spent,
                produced: graph.produced,
                referenceOutRefHexes: [],
              },
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
// Opaque fixture bytes do not affect the persisted state compared here.
vi.mock("../src/mpf/ledger-hydration.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/ledger-hydration.js")
  >("../src/mpf/ledger-hydration.js");
  return {
    ...actual,
    encodeTransactionRootValue: vi.fn((txCbor: Buffer) => Buffer.from(txCbor)),
  };
});
vi.mock("../src/database/txAdmissions.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/txAdmissions.js")
  >("../src/database/txAdmissions.js");
  return {
    ...actual,
    retrieveProgramMaterialSidecars: vi.fn((txIds: readonly Buffer[]) =>
      Effect.succeed(
        txIds.map((txId) => ({ txId, sidecarCbor: Buffer.from("80", "hex") })),
      ),
    ),
  };
});

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
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
  graphs.set(hex(txId), { spent: spent.map(hex), produced });

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
const D1 = deposit("non-speculative-step-down.d1", 2);
const D2 = deposit("non-speculative-step-down.d2", 6);
// D0 was projected by an earlier tick and T_e, still pending beyond this
// block's selection (the batch planner's cut), spent its output: D0 is
// consumed and has no mempool_ledger row. Re-projecting it would bring back
// an output T_e already spent.
const D0 = deposit("non-speculative-step-down.d0", 1);
const T_E_ID = Buffer.alloc(32, 0x0f);

const withdrawal = async (): Promise<WithdrawalsDB.Entry> => {
  const value: SDK.Value = new Map([["", new Map([["", U_W_LOVELACE]])]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: privateKey.to_public().hash().to_hex(),
    l2_value: value,
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum: "NoDatum",
  };
  const info: SDK.WithdrawalInfo = {
    body,
    signature: SDK.signWithdrawalBody(privateKey, body),
    validity: "WithdrawalIsValid",
  };
  return {
    [WithdrawalsDB.Columns.ID]: Buffer.from("cc".repeat(32), "hex"),
    raw_event_info: Buffer.from(Data.to(info, SDK.WithdrawalInfo), "hex"),
    settlement_event_info: null,
    inclusion_time: at(1),
    withdrawal_l1_tx_hash: Buffer.from("dd".repeat(32), "hex"),
    withdrawal_l1_output_index: 0,
    asset_name: Buffer.from("ee".repeat(32), "hex"),
    l2_outref: Buffer.from(Data.to(body.l2_outref, SDK.OutputReference), "hex"),
    l2_owner: Buffer.from(body.l2_owner, "hex"),
    l2_value: Buffer.from(Data.to(value, SDK.Value), "hex"),
    l1_address: Buffer.from(Data.to(body.l1_address, SDK.AddressData), "hex"),
    l1_datum: Buffer.from(Data.to(body.l1_datum, SDK.CardanoDatum), "hex"),
    refund_address: Buffer.alloc(0),
    refund_datum: Buffer.alloc(0),
    validity: null,
    validity_detail: {},
    classification_revision: 0,
    reopened_from_header_hash: null,
    projected_header_hash: null,
    status: WithdrawalsDB.Status.Awaiting,
  };
};

const seed = (w: WithdrawalsDB.Entry) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* resetApplicationTables;
    yield* MempoolLedgerDB.insert(BASE);
    yield* DepositsDB.insertEntries([D0, D1, D2]);
    const d0Row = yield* DepositsDB.toMempoolLedgerEntry(D0);
    yield* MempoolLedgerDB.reconcileDepositEntries([d0Row]);
    yield* DepositsDB.markAwaitingAsProjected([D0[DepositsDB.Columns.ID]]);
    yield* WithdrawalsDB.insertEntries([w]);
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

const BASE_ROOT_REFUSAL =
  "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root";

/**
 * One block build by the real `processMpfs`, non-speculative, over `txs`. It
 * runs every database write of the build, then stops at the base-root check
 * (the test's base root differs from the native one) before the native ledger
 * is needed. Returns the transactions Phase B accepted.
 */
const build = (txs: readonly EntryWithTimeStamp[]) =>
  Effect.gen(function* () {
    phaseB.accepted = [];
    const refused = yield* processMpfs(
      { root: () => Effect.succeed("") } as unknown as MidgardMpf,
      txs,
      {
        currentBlockStartTime: at(0),
        initialLedgerEntries: BASE,
        selectedBaseUtxoRoot: "11".repeat(32),
        nativeMpf: { handle: { baseRoot: "22".repeat(32) } } as never,
        forcedValidation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 0n,
        },
        deferDatabaseWrites: false,
      },
    ).pipe(Effect.flip);
    expect(refused.message).toBe(BASE_ROOT_REFUSAL);
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
      yield* sql`SELECT tx_order_id, status, projected_header_hash, forced_inclusion_value, cek_program_material_sidecar_cbor FROM forced_transaction_utxos ORDER BY tx_order_id`,
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

// Synthetic accounting selects two; production DB helpers persist projections.
const NEXT_TX_COUNT = 2;

describe("non-speculative DA frame step-down over Postgres", () => {
  it("leaves the state a refused block and a fresh next-tick selection leave, with no projection lost or duplicated", async () => {
    const w = await withdrawal();
    const steppedDown = await run(
      Effect.gen(function* () {
        yield* seed(w);
        const passes: { accepted: readonly string[] }[] = [];
        const result = yield* stepDownCommitSelectionToDaFrame({
          candidateSelection: selectCommitTxCandidates({
            mempoolTxs: (yield* mempoolPage).slice(0, TXS.length),
            processedMempoolTxs: [],
          }),
          baseUtxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
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
