import "./utils.js";

import {
  assertMidgardCekProgramMaterialBundle,
  encodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  ForcedTransactionsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
} from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import {
  COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
  COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
  COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
} from "../src/mpf/commit-rejection.js";
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import { forcedProjectionEntry } from "./helpers/commit-da-frame-forced-projection.js";
import {
  at,
  BASE,
  lambdaChain,
  T3,
  T4,
  TXS,
} from "./helpers/commit-stage-eviction.fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * A normal transaction that a commit stage cannot process is rejected, with
 * its dependents, and the block is built without it; a forced-transaction
 * store fault still stops the build. Real `processMpfs` over Postgres, with
 * Phase A and Phase B reduced to their ledger effects.
 */

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

const fixture = vi.hoisted(() => ({
  graphs: new Map<
    string,
    { spent: readonly string[]; produced: readonly unknown[] }
  >(),
  phaseADefect: new Set<string>(),
  phaseBFailure: new Set<string>(),
  forcedStoreFault: false,
  // Transaction CBOR hex -> its programs, for the block-wide coverage check.
  envelopesByTxCbor: new Map<string, readonly unknown[]>(),
  sidecarsByTxId: new Map<string, Buffer | null>(),
  phaseBAccepted: [] as string[][],
}));

vi.mock("@al-ft/midgard-validation", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-validation")
  >("@al-ft/midgard-validation");
  return {
    ...actual,
    runPhaseAValidation: vi.fn(
      (
        txs: readonly {
          txId: Buffer;
          txCbor: Buffer;
          sourceKind?: string;
          programMaterialSidecarCbor: Buffer | null;
        }[],
        config: Parameters<typeof actual.runPhaseAValidation>[1],
      ) =>
        txs[0]?.sourceKind === "forced"
          ? actual.runPhaseAValidation(
              txs as Parameters<typeof actual.runPhaseAValidation>[0],
              config,
            )
          : Effect.sync(() => ({
              accepted: txs.map((tx) => {
                if (fixture.phaseADefect.has(hex(tx.txId)))
                  throw new Error("injected Phase A defect");
                const graph = fixture.graphs.get(hex(tx.txId))!;
                return {
                  ledgerTx: { txId: tx.txId },
                  submission: {
                    sourceKind: "normal",
                    txCbor: tx.txCbor,
                    programMaterialSidecarCbor: tx.programMaterialSidecarCbor,
                  },
                  graph: {
                    spentOutRefHexes: graph.spent,
                    referenceOutRefHexes: [],
                    produced: graph.produced,
                  },
                };
              }),
              rejected: [],
            })),
    ),
    runPhaseBValidationWithPatch: vi.fn(
      (
        candidates: readonly {
          ledgerTx: { txId: Buffer };
          graph: {
            spentOutRefHexes: readonly string[];
            produced: readonly LedgerUtils.Entry[];
          };
        }[],
        preState: ReadonlyMap<string, Buffer>,
      ) =>
        candidates.some(({ ledgerTx }) =>
          fixture.phaseBFailure.has(hex(ledgerTx.txId)),
        )
          ? Effect.fail(new Error("injected Phase B failure"))
          : Effect.sync(() => {
              const state = new Set(preState.keys());
              const accepted: (typeof candidates)[number][] = [];
              const rejected: unknown[] = [];
              for (const candidate of candidates) {
                const { spentOutRefHexes, produced } = candidate.graph;
                if (!spentOutRefHexes.every((outRef) => state.has(outRef))) {
                  rejected.push({
                    txId: candidate.ledgerTx.txId,
                    code: "E_MISSING_INPUT",
                    detail: "",
                  });
                  continue;
                }
                for (const outRef of spentOutRefHexes) state.delete(outRef);
                for (const entry of produced)
                  state.add(hex(entry[LedgerUtils.Columns.OUTREF]));
                accepted.push(candidate);
              }
              fixture.phaseBAccepted.push(
                accepted.map(({ ledgerTx }) => hex(ledgerTx.txId)),
              );
              return { accepted, rejected };
            }),
    ),
  };
});
// The fixture transactions are opaque bytes: they decode to their own CBOR,
// and their programs are the fixture's.
vi.mock("@al-ft/midgard-core/codec", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-core/codec")
  >("@al-ft/midgard-core/codec");
  return {
    ...actual,
    decodeMidgardNativeTxFullFromCanonicalCbor: (cbor: Uint8Array) =>
      fixture.envelopesByTxCbor.has(hex(cbor))
        ? { fixtureCbor: hex(cbor) }
        : actual.decodeMidgardNativeTxFullFromCanonicalCbor(cbor),
  };
});
vi.mock("@al-ft/midgard-core/script-proof", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-core/script-proof")
  >("@al-ft/midgard-core/script-proof");
  return {
    ...actual,
    collectMidgardEventProgramEnvelopes: (
      tx: { fixtureCbor?: string },
      ...rest: unknown[]
    ) =>
      tx.fixtureCbor !== undefined
        ? fixture.envelopesByTxCbor.get(tx.fixtureCbor)!
        : (
            actual.collectMidgardEventProgramEnvelopes as (
              ...args: unknown[]
            ) => unknown
          )(tx, ...rest),
  };
});
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
        txIds.flatMap((txId) => {
          const sidecar = fixture.sidecarsByTxId.get(hex(txId));
          return sidecar === null
            ? []
            : [
                {
                  txId,
                  sidecarCbor:
                    sidecar ?? encodeMidgardCekProgramMaterialSidecar([]),
                },
              ];
        }),
      ),
    ),
  };
});
vi.mock("../src/database/forcedTransactions.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/forcedTransactions.js")
  >("../src/database/forcedTransactions.js");
  return {
    ...actual,
    setProofClassifications: vi.fn(
      (...args: Parameters<typeof actual.setProofClassifications>) =>
        fixture.forcedStoreFault
          ? Effect.fail(
              new DatabaseError({
                table: actual.tableName,
                message: "injected forced store fault",
                cause: undefined,
              }),
            )
          : actual.setProofClassifications(...args),
    ),
  };
});

const SELECTION = 4;
const [t1, t2, t3, t4, t5] = TXS.map(({ txId }) => hex(txId));
for (const { txId, spent, produced } of TXS)
  fixture.graphs.set(hex(txId), { spent: spent.map(hex), produced });

beforeEach(() => {
  fixture.phaseADefect.clear();
  fixture.phaseBFailure.clear();
  fixture.forcedStoreFault = false;
  fixture.envelopesByTxCbor.clear();
  for (const { txCbor } of TXS) fixture.envelopesByTxCbor.set(hex(txCbor), []);
  fixture.sidecarsByTxId.clear();
});

const seed = (forced: readonly ForcedTransactionsDB.Entry[] = []) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* resetApplicationTables;
    yield* MempoolLedgerDB.insert(BASE);
    if (forced.length > 0) yield* ForcedTransactionsDB.insertEntries(forced);
    for (const { seconds, txId, txCbor, spent, produced } of TXS) {
      yield* MempoolDB.insertMultipleCore([
        { txId, txCbor, spent: [...spent], produced: [...produced] },
      ]);
      yield* MempoolTxDeltasDB.upsertMany([{ txId, spent, produced }]);
      yield* sql`UPDATE mempool SET time_stamp_tz = ${at(seconds)} WHERE tx_id = ${txId}`;
    }
  });

const BASE_ROOT_REFUSAL =
  "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root";

const config = (
  validationTraceBuilder: (input: {
    readonly transactions: readonly {
      readonly canonicalTransactionCbor: Buffer;
    }[];
  }) => Effect.Effect<readonly never[], DatabaseError> = () =>
    Effect.succeed([]),
) => ({
  currentBlockStartTime: at(0),
  initialLedgerEntries: BASE,
  selectedBaseUtxoRoot: "11".repeat(32),
  nativeMpf: { handle: { baseRoot: "22".repeat(32) } } as never,
  validationTraceBuilder,
  forcedValidation: {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    bucketConcurrency: 1,
    slotForUnixTime: () => 0n,
  },
});

const mempoolPage = MempoolDB.retrievePage({ limit: 100 }).pipe(
  Effect.map((page) => page.entries),
);

/**
 * One block build over the first `count` pending transactions. Every
 * database write of the build runs; the build then stops at the base-root
 * check, past every normal-transaction stage. Returns what Phase B accepted
 * last.
 */
const build = (
  count: number,
  validationTraceBuilder?: Parameters<typeof config>[0],
) =>
  Effect.gen(function* () {
    fixture.phaseBAccepted = [];
    const page: readonly EntryWithTimeStamp[] = (yield* mempoolPage).slice(
      0,
      count,
    );
    const refused = yield* processMpfs(
      { root: () => Effect.succeed("") } as unknown as MidgardMpf,
      page,
      config(validationTraceBuilder) as never,
    ).pipe(Effect.flip);
    expect(refused.message).toBe(BASE_ROOT_REFUSAL);
    return fixture.phaseBAccepted.at(-1) ?? [];
  });

const rejections = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    tx_id: Buffer;
    reject_code: string;
  }>`SELECT tx_id, reject_code FROM tx_rejections ORDER BY tx_id`;
  return Object.fromEntries(
    rows.map((row) => [hex(row.tx_id), row.reject_code]),
  );
});
const pending = mempoolPage.pipe(
  Effect.map((entries) => entries.map((entry) => hex(entry[TxColumns.TX_ID]))),
);

const run = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

describe("commit-stage eviction of normal transactions", () => {
  it("rejects a transaction Phase A cannot process, with its dependents, and the next block commits", async () => {
    fixture.phaseADefect.add(t1);
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        const first = yield* build(SELECTION);
        const afterFirst = yield* rejections;
        const next = yield* build(100);
        return {
          first,
          afterFirst,
          next,
          afterNext: yield* rejections,
          pending: yield* pending,
        };
      }),
    );
    expect(result.first).toEqual([t3, t4]);
    expect(result.afterFirst[t1]).toBe(COMMIT_REJECT_CODE_TRACE_UNAVAILABLE);
    expect(Object.keys(result.afterFirst).sort()).toEqual([t1, t2]);
    expect(result.next).toEqual([t3, t4, t5]);
    expect(result.afterNext).toEqual(result.afterFirst);
    expect(result.pending).toEqual([t3, t4, t5]);
  }, 60_000);

  it("narrows a Phase B failure to the transaction that causes it and rejects its pending dependents", async () => {
    fixture.phaseBFailure.add(t3);
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        const first = yield* build(SELECTION);
        return { first, rejected: yield* rejections, pending: yield* pending };
      }),
    );
    expect(result.first).toEqual([t1, t2, t4]);
    expect(result.rejected).toEqual({
      [t3]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
      [t5]: COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
    });
    expect(result.pending).toEqual([t1, t2, t4]);
  }, 60_000);

  it("rejects a transaction without a validation descriptor or program material", async () => {
    fixture.sidecarsByTxId.set(t1, null);
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        const first = yield* build(SELECTION, (input) =>
          input.transactions.some(
            (transaction) =>
              hex(transaction.canonicalTransactionCbor) === hex(T4.txCbor),
          )
            ? Effect.fail(
                new DatabaseError({
                  table: "pending_block_finalizations",
                  message: "injected descriptor failure",
                  cause: undefined,
                }),
              )
            : Effect.succeed([]),
        );
        return { first, rejected: yield* rejections };
      }),
    );
    expect(result.first).toEqual([t3]);
    expect(result.rejected[t1]).toBe(COMMIT_REJECT_CODE_TRACE_UNAVAILABLE);
    expect(result.rejected[t4]).toBe(COMMIT_REJECT_CODE_TRACE_UNAVAILABLE);
    expect(Object.keys(result.rejected).sort()).toEqual([t1, t2, t4]);
  }, 60_000);

  it("rejects the program variant whose material breaks the block-wide coverage check", async () => {
    // Two variants of one program, each within its own limits; together they
    // declare more node visits than one block's material may.
    const visits = 800_000;
    const chain = lambdaChain(visits + 1);
    const variantA = chain.envelopes[visits - 1]!;
    const variantB = chain.envelopes[visits]!;
    expect(() =>
      assertMidgardCekProgramMaterialBundle(
        [variantA, variantB],
        chain.material,
      ),
    ).toThrow(/aggregate unique-envelope node visits/u);
    fixture.envelopesByTxCbor.set(hex(T3.txCbor), [variantA]);
    fixture.envelopesByTxCbor.set(hex(T4.txCbor), [variantB]);
    fixture.sidecarsByTxId.set(
      t3,
      encodeMidgardCekProgramMaterialSidecar(chain.material.slice(0, visits)),
    );
    fixture.sidecarsByTxId.set(
      t4,
      encodeMidgardCekProgramMaterialSidecar(chain.material),
    );
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        const first = yield* build(SELECTION);
        return { first, rejected: yield* rejections };
      }),
    );
    expect(result.first).toEqual([t1, t2, t3]);
    expect(result.rejected).toEqual({
      [t4]: COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
    });
  }, 180_000);

  it("still stops on a forced-transaction store fault, rejecting nothing", async () => {
    fixture.forcedStoreFault = true;
    const result = await run(
      Effect.gen(function* () {
        yield* seed([yield* forcedProjectionEntry(1, at(1))]);
        const page = (yield* mempoolPage).slice(0, SELECTION);
        const failure = yield* processMpfs(
          { root: () => Effect.succeed("") } as unknown as MidgardMpf,
          page,
          config() as never,
        ).pipe(Effect.flip);
        return {
          failure,
          rejected: yield* rejections,
          pending: yield* pending,
        };
      }),
    );
    expect(result.failure.message).toBe("injected forced store fault");
    expect(result.rejected).toEqual({});
    expect(result.pending).toEqual([t1, t2, t3, t4, t5]);
  }, 60_000);
});
