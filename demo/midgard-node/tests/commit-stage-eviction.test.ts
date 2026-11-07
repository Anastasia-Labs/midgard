import "./utils.js";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { MidgardUplcCapabilityError } from "@al-ft/midgard-validation";
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
import { heldBuildStage } from "../src/mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import {
  COMMIT_REJECT_CODE_DA_OVERSIZE,
  COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
  COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
} from "../src/mpf/commit-rejection.js";
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import { forcedProjectionEntry } from "./helpers/commit-da-frame-forced-projection.js";
import {
  at,
  BASE,
  EXTRA_BASE,
  EXTRA_TXS,
  T1,
  T4,
  TXS,
} from "./helpers/commit-stage-eviction.fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * A normal transaction that a commit stage cannot process is rejected, with
 * its dependents, and the block is built without it, when the failure is the
 * transaction's own. A node fault or a failure that does not happen again
 * holds the build instead, as does a forced-transaction store fault. Past a
 * few rejections at one stage, the rest stay pending for later builds. Real
 * `processMpfs` over Postgres, with Phase A and Phase B reduced to their
 * ledger effects.
 */

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

const fixture = vi.hoisted(() => ({
  graphs: new Map<
    string,
    { spent: readonly string[]; produced: readonly unknown[] }
  >(),
  // Transaction id (CBOR hex for the root) -> a failure injected into the
  // next `times` runs of that stage on it.
  injected: {
    root: new Map<string, { error: unknown; times: number; die?: boolean }>(),
    phaseA: new Map<string, { error: unknown; times: number; die?: boolean }>(),
    phaseB: new Map<string, { error: unknown; times: number; die?: boolean }>(),
  },
  fire: (
    injected: Map<string, { error: unknown; times: number; die?: boolean }>,
    keys: readonly string[],
  ) => {
    const key = keys.find((id) => (injected.get(id)?.times ?? 0) > 0);
    if (key === undefined) return undefined;
    const failure = injected.get(key)!;
    failure.times -= 1;
    return failure;
  },
  forcedStoreFault: false,
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
                const failure = fixture.fire(fixture.injected.phaseA, [
                  hex(tx.txId),
                ]);
                if (failure !== undefined) throw failure.error;
                const graph = fixture.graphs.get(hex(tx.txId))!;
                return {
                  ledgerTx: { txId: tx.txId },
                  submission: {
                    sourceKind: "normal",
                    txCbor: tx.txCbor,
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
      ) => {
        const failure = fixture.fire(
          fixture.injected.phaseB,
          candidates.map(({ ledgerTx }) => hex(ledgerTx.txId)),
        );
        return failure !== undefined
          ? failure.die === true
            ? Effect.die(failure.error)
            : Effect.fail(failure.error)
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
            });
      },
    ),
  };
});
vi.mock("../src/mpf/ledger-hydration.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/ledger-hydration.js")
  >("../src/mpf/ledger-hydration.js");
  return {
    ...actual,
    encodeTransactionRootValue: vi.fn((txCbor: Buffer) => {
      const failure = fixture.fire(fixture.injected.root, [hex(txCbor)]);
      if (failure !== undefined) throw failure.error;
      return Buffer.from(txCbor);
    }),
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
const [e1, e2, e3, e4] = EXTRA_TXS.map(({ txId }) => hex(txId));
for (const { txId, spent, produced } of [...TXS, ...EXTRA_TXS])
  fixture.graphs.set(hex(txId), { spent: spent.map(hex), produced });

beforeEach(() => {
  for (const injected of Object.values(fixture.injected)) injected.clear();
  fixture.forcedStoreFault = false;
});

const seed = (
  forced: readonly ForcedTransactionsDB.Entry[] = [],
  extra: readonly (typeof EXTRA_TXS)[number][] = [],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* resetApplicationTables;
    yield* MempoolLedgerDB.insert(
      extra.length > 0 ? [...BASE, ...EXTRA_BASE] : BASE,
    );
    if (forced.length > 0) yield* ForcedTransactionsDB.insertEntries(forced);
    for (const { seconds, txId, txCbor, spent, produced } of [
      ...TXS,
      ...extra,
    ]) {
      yield* MempoolDB.insertMultipleCore([
        { txId, txCbor, spent: [...spent], produced: [...produced] },
      ]);
      yield* MempoolTxDeltasDB.upsertMany([{ txId, spent, produced }]);
      yield* sql`UPDATE mempool SET time_stamp_tz = ${at(seconds)} WHERE tx_id = ${txId}`;
    }
  });

const BASE_ROOT_REFUSAL =
  "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root";

const config = () => ({
  currentBlockStartTime: at(0),
  initialLedgerEntries: BASE,
  selectedBaseUtxoRoot: "11".repeat(32),
  nativeMpf: { handle: { baseRoot: "22".repeat(32) } } as never,
  validationTraceBuilder: () => Effect.succeed([]),
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

const attemptBuild = (count: number, overrides: object = {}) =>
  Effect.gen(function* () {
    fixture.phaseBAccepted = [];
    const page: readonly EntryWithTimeStamp[] = (yield* mempoolPage).slice(
      0,
      count,
    );
    return yield* processMpfs(
      { root: () => Effect.succeed("") } as unknown as MidgardMpf,
      page,
      { ...config(), ...overrides } as never,
    ).pipe(Effect.flip);
  });

/**
 * One block build over the first `count` pending transactions. Every
 * database write of the build runs; the build then stops at the base-root
 * check, past every normal-transaction stage. Returns what Phase B accepted
 * last.
 */
const build = (count: number, overrides: object = {}) =>
  Effect.gen(function* () {
    const refused = yield* attemptBuild(count, overrides);
    expect(refused.message).toBe(BASE_ROOT_REFUSAL);
    return fixture.phaseBAccepted.at(-1) ?? [];
  });

/** A build that holds before the base-root check: its failure, with what
 * the store then holds. */
const held = (count: number) =>
  Effect.gen(function* () {
    yield* seed();
    const failure = yield* attemptBuild(count);
    expect(failure.message).not.toBe(BASE_ROOT_REFUSAL);
    return {
      failure,
      rejected: yield* rejections,
      pending: yield* pending,
    };
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

const inject = (
  stage: keyof typeof fixture.injected,
  id: string,
  error: unknown,
  times = Number.POSITIVE_INFINITY,
  die = false,
) => fixture.injected[stage].set(id, { error, times, die });

const NOTHING_REJECTED = {
  rejected: {},
  pending: [t1, t2, t3, t4, t5],
};

describe("commit-stage eviction of normal transactions", () => {
  it("rejects a transaction Phase A cannot process, with its dependents, and the next block commits", async () => {
    inject("phaseA", t1, new Error("injected Phase A defect"));
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

  it("rejects a transaction the DA step-down evicts, with its dependents, and the next block commits", async () => {
    const evictions = new Map([
      [t1, { code: COMMIT_REJECT_CODE_DA_OVERSIZE, detail: "oversize" }],
    ]);
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        const first = yield* build(SELECTION, {
          commitEvictions: evictions,
        });
        const rejected = yield* rejections;
        return { first, rejected, next: yield* build(100) };
      }),
    );
    expect(result.first).toEqual([t3, t4]);
    expect(result.rejected[t1]).toBe(COMMIT_REJECT_CODE_DA_OVERSIZE);
    expect(Object.keys(result.rejected).sort()).toEqual([t1, t2]);
    expect(result.next).toEqual([t3, t4, t5]);
  }, 60_000);

  it("narrows a Phase B failure to the transaction that causes it and rejects its pending dependents", async () => {
    inject("phaseB", t3, new Error("injected Phase B failure"));
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

  it("rejects a transaction whose root value cannot be derived, and holds on a root failure that does not happen again", async () => {
    inject("root", hex(T4.txCbor), new Error("injected root failure"));
    const rejected = await run(
      Effect.gen(function* () {
        yield* seed();
        return { first: yield* build(SELECTION), rejected: yield* rejections };
      }),
    );
    expect(rejected.first).toEqual([t1, t2, t3]);
    expect(rejected.rejected).toEqual({
      [t4]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
    });
    inject("root", hex(T4.txCbor), new Error("injected root failure"), 1);
    const transient = await run(held(SELECTION));
    expect(heldBuildStage(transient.failure)).toBe(
      "Transaction root derivation",
    );
    expect(transient).toMatchObject(NOTHING_REJECTED);
  }, 60_000);

  const STAGES = {
    phaseA: "Phase A",
    phaseB: "Phase B",
    root: "Transaction root derivation",
  } as const;
  const ALLOCATION = () => new RangeError("Array buffer allocation failed");
  const TRANSIENT = () => new Error("injected transient failure");
  it.each([
    [
      "phaseB",
      "an unavailable Plutus evaluator",
      () => new MidgardUplcCapabilityError("eval unavailable"),
      Infinity,
    ],
    ["phaseA", "a memory allocation failure", ALLOCATION, Infinity],
    [
      "phaseA",
      "a RangeError other than a stack overflow",
      () => new RangeError("Invalid array length"),
      Infinity,
    ],
    ["phaseA", "a failure that does not happen again", TRANSIENT, 1],
    ["phaseB", "a memory allocation failure", ALLOCATION, Infinity],
    ["phaseB", "a failure that does not happen again", TRANSIENT, 1],
    ["root", "a memory allocation failure", ALLOCATION, Infinity],
  ] as const)(
    "holds the build on %s fault of %s, rejecting nothing",
    async (stage, _kind, error, times) => {
      const id = stage === "root" ? hex(T4.txCbor) : t4;
      inject(stage, id, error(), times, stage === "phaseB");
      const result = await run(held(SELECTION));
      expect(heldBuildStage(result.failure)).toBe(STAGES[stage]);
      expect(result).toMatchObject(NOTHING_REJECTED);
    },
    60_000,
  );

  it("rejects a transaction whose validation overflows the stack again", async () => {
    inject("phaseA", t4, new RangeError("Maximum call stack size exceeded"));
    const result = await run(
      Effect.gen(function* () {
        yield* seed();
        return { first: yield* build(SELECTION), rejected: yield* rejections };
      }),
    );
    expect(result.first).toEqual([t1, t2, t3]);
    expect(result.rejected).toEqual({
      [t4]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
    });
  }, 60_000);

  it("rejects the first four failures one stage confirms, leaves the rest pending with their descendants, and raises the breaker alarm", async () => {
    for (const id of [e1, e2, e3, e4, t3, t4])
      inject("phaseA", id, new Error("injected Phase A defect"));
    const tripped: string[] = [];
    const overrides = {
      initialLedgerEntries: [...BASE, ...EXTRA_BASE],
      onCommitBuildAlarm: (alarm: string, stage: string) =>
        Effect.sync(() => {
          tripped.push(`${alarm}:${stage}`);
        }),
    };
    const result = await run(
      Effect.gen(function* () {
        yield* seed([], EXTRA_TXS);
        const first = yield* build(100, overrides);
        const afterFirst = {
          rejected: yield* rejections,
          pending: yield* pending,
          tripped: [...tripped],
        };
        const next = yield* build(100, overrides);
        return {
          first,
          afterFirst,
          next,
          rejected: yield* rejections,
          pending: yield* pending,
        };
      }),
    );
    const extraRejected = Object.fromEntries(
      [e1, e2, e3, e4].map((id) => [id, COMMIT_REJECT_CODE_TRACE_UNAVAILABLE]),
    );
    // T3 and T4 stay pending, and T5 with T3.
    expect(result.first).toEqual([t1, t2]);
    expect(result.afterFirst).toEqual({
      rejected: extraRejected,
      pending: [t1, t2, t3, t4, t5],
      tripped: [`breaker:${STAGES.phaseA}`],
    });
    // The next build rejects them, and T5 with T3.
    expect(result.next).toEqual([t1, t2]);
    expect(result.rejected).toMatchObject({
      ...extraRejected,
      [t3]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
      [t4]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
    });
    expect(Object.keys(result.rejected).sort()).toEqual(
      [e1, e2, e3, e4, t3, t4, t5].sort(),
    );
    expect(result.pending).toEqual([t1, t2]);
    expect(tripped).toEqual([`breaker:${STAGES.phaseA}`]);
  }, 60_000);

  it("holds the build on a Phase B store fault and on a wasm fault, rejecting nothing", async () => {
    inject(
      "phaseB",
      t1,
      new DatabaseError({
        table: "mempool",
        message: "injected store fault",
        cause: undefined,
      }),
    );
    const store = await run(held(SELECTION));
    expect(store).toMatchObject(NOTHING_REJECTED);
    fixture.injected.phaseB.clear();
    inject("phaseA", t1, new WebAssembly.RuntimeError("unreachable executed"));
    const wasm = await run(held(SELECTION));
    expect(wasm).toMatchObject(NOTHING_REJECTED);
  }, 60_000);

  it("rejects up to four failures Phase B confirms, leaves the rest pending with their descendants, and raises the breaker alarm", async () => {
    for (const id of [e1, e2, e3, e4, t3, t4])
      inject("phaseB", id, new Error("injected Phase B failure"));
    const alarms: string[] = [];
    const result = await run(
      Effect.gen(function* () {
        yield* seed([], EXTRA_TXS);
        yield* build(100, {
          initialLedgerEntries: [...BASE, ...EXTRA_BASE],
          onCommitBuildAlarm: (alarm: string, stage: string) =>
            Effect.sync(() => {
              alarms.push(`${alarm}:${stage}`);
            }),
        });
        return { rejected: yield* rejections, pending: yield* pending };
      }),
    );
    expect(result).toEqual({
      rejected: Object.fromEntries(
        [e1, e2, e3, e4].map((id) => [
          id,
          COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
        ]),
      ),
      pending: [t1, t2, t3, t4, t5],
    });
    expect(alarms).toEqual(["breaker:Phase B"]);
  }, 120_000);

  it("counts each rejected transaction once towards the breaker across the passes that settle a build", async () => {
    for (const id of [e1, e2, e3])
      inject("phaseB", id, new Error("injected Phase B failure"));
    const alarms: string[] = [];
    const result = await run(
      Effect.gen(function* () {
        yield* seed([], EXTRA_TXS);
        // T2 spends T1's output, which the evaluation sees: the rejection of
        // T1 reaches T2 after Phase B accepts it, and the build settles in a
        // second pass.
        const accepted = yield* build(100, {
          initialLedgerEntries: [...BASE, ...EXTRA_BASE, T1.produced[0]!],
          commitEvictions: new Map([
            [t1, { code: COMMIT_REJECT_CODE_DA_OVERSIZE, detail: "oversize" }],
          ]),
          onCommitBuildAlarm: (alarm: string, stage: string) =>
            Effect.sync(() => {
              alarms.push(`${alarm}:${stage}`);
            }),
        });
        return { accepted, rejected: yield* rejections };
      }),
    );
    expect(result.accepted).toEqual([e4, t3, t4, t5]);
    expect(result.rejected).toMatchObject({
      [t1]: COMMIT_REJECT_CODE_DA_OVERSIZE,
      [e1]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
      [e2]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
      [e3]: COMMIT_REJECT_CODE_TRACE_UNAVAILABLE,
    });
    expect(Object.keys(result.rejected).sort()).toEqual(
      [e1, e2, e3, t1, t2].sort(),
    );
    expect(alarms).toEqual([]);
  }, 120_000);

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
