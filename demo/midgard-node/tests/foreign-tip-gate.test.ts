import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { tableName as historyAuthorityTable } from "../src/database/eventHistoryAuthority.js";
import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";
import {
  assessRetainedForeignTipWindows,
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips,
  resolveT2ForeignEventEvidence,
} from "../src/workers/t2-foreign-event-reconciliation.js";
import {
  depositStatus,
  headerFor,
  IN_WINDOW,
  indexDeposit,
  indexForcedTransaction,
  indexWithdrawal,
  INGESTED_PAST_WINDOW,
  NONEMPTY_ROOT,
  nonEmptyWindowHeader,
  oneDepositPayload,
  onNode,
  reconciliationRow,
  recordForeignTip,
  STALE_WINDOW_END_MS,
  STALE_WINDOW_START_MS,
  storeForeignDa,
  WINDOW_END_MS,
  WINDOW_START_MS,
} from "./foreign-tip-gate.fixtures.js";

/** Replaces the per-row replay with `failure` while it is set. */
const injected = vi.hoisted(() => ({
  failure: undefined as Effect.Effect<never, unknown> | undefined,
}));
vi.mock(
  "../src/workers/t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/workers/t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js")
      >();
    return {
      ...actual,
      reconcileRetainedForeignTipEntry: (
        ...args: Parameters<typeof actual.reconcileRetainedForeignTipEntry>
      ) => injected.failure ?? actual.reconcileRetainedForeignTipEntry(...args),
    };
  },
);
afterEach(() => {
  injected.failure = undefined;
});

const gate = () =>
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips({
    eventsIngestedThrough: INGESTED_PAST_WINDOW,
  });

const assess = () =>
  assessRetainedForeignTipWindows({
    eventsIngestedThrough: INGESTED_PAST_WINDOW,
  });

const recentWindow = {
  startTime: BigInt(WINDOW_START_MS),
  endTime: BigInt(WINDOW_END_MS),
};
const staleWindow = {
  startTime: BigInt(STALE_WINDOW_START_MS),
  endTime: BigInt(STALE_WINDOW_END_MS),
};

describe("foreign-tip verdicts the window cannot lift", () => {
  it("classifies a header whose event root and count disagree as invalid from the header alone, for every event kind", async () => {
    const pairs = [
      ["depositsRoot", "depositCount"],
      ["forcedTransactionsRoot", "forcedTransactionCount"],
      ["withdrawalsRoot", "withdrawalCount"],
    ] as const;
    for (const [root, count] of pairs) {
      for (const header of [
        headerFor({ [root]: NONEMPTY_ROOT, totalEventCount: 1n }),
        headerFor({ [count]: 1n, totalEventCount: 1n }),
      ]) {
        const result = await Effect.runPromise(
          Effect.flatMap(SDK.hashBlockHeader(header), (foreignHeaderHash) =>
            resolveT2ForeignEventEvidence({
              foreignHeaderHash,
              header,
              candidateIds: {
                deposits: [],
                forcedTransactions: [],
                withdrawals: [],
              },
            }),
          ),
        );
        expect(result).toMatchObject({
          type: "AwaitingForeignDa",
          reason: "invalid",
          detail: "foreign header event root/count evidence is inconsistent",
        });
      }
    }
  });

  it("keeps refusing a self-inconsistent foreign header with an empty window, before and after the horizon, while an honest one commits", async () => {
    for (const window of [recentWindow, staleWindow]) {
      const result = await onNode(
        Effect.gen(function* () {
          const emptyRootsWithCount = yield* recordForeignTip(
            headerFor({ ...window, depositCount: 1n, totalEventCount: 1n }),
          );
          const first = yield* gate();
          return {
            emptyRootsWithCount,
            first,
            row: yield* reconciliationRow(emptyRootsWithCount),
          };
        }),
      );
      expect(result.first).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.emptyRootsWithCount,
        reason: "invalid",
      });
      expect(result.row?.status).toBe("awaiting");
      const rootWithoutCount = await onNode(
        Effect.gen(function* () {
          const hash = yield* recordForeignTip(
            nonEmptyWindowHeader({ ...window, depositCount: 0n }),
          );
          return {
            hash,
            gate: yield* gate(),
            row: yield* reconciliationRow(hash),
          };
        }),
      );
      expect(rootWithoutCount.gate).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: rootWithoutCount.hash,
        reason: "invalid",
      });
      expect(rootWithoutCount.row?.status).toBe("awaiting");
      const honest = await onNode(
        Effect.flatMap(recordForeignTip(nonEmptyWindowHeader(window)), () =>
          gate(),
        ),
      );
      expect(honest.type).toBe("Ready");
    }
  });

  it("never lets a speculative build past a header its commitment columns show is malformed, even before any replay", async () => {
    const malformed = { depositCount: 1n, totalEventCount: 1n };
    const result = await onNode(
      Effect.gen(function* () {
        yield* recordForeignTip(headerFor({ ...recentWindow, ...malformed }));
        const beforeReplay = yield* assess();
        yield* gate();
        return { beforeReplay, afterReplay: yield* assess() };
      }),
    );
    for (const refusal of [result.beforeReplay, result.afterReplay]) {
      expect(refusal).toMatchObject({
        type: "AwaitingForeignDa",
        reason: "replay_required",
      });
    }
    if (result.afterReplay.type === "AwaitingForeignDa")
      expect(result.afterReplay.detail).toMatch(/^invalid:/u);
  });
});

const occupancyCases = [
  {
    kind: "forced transaction",
    index: (time: Date, status?: ForcedTransactionsDB.Status) =>
      indexForcedTransaction(time, status),
    statuses: [
      ForcedTransactionsDB.Status.Awaiting,
      ForcedTransactionsDB.Status.Projected,
    ],
  },
  {
    kind: "withdrawal",
    index: (time: Date, status?: WithdrawalsDB.Status) =>
      indexWithdrawal(time, status),
    statuses: [WithdrawalsDB.Status.Awaiting, WithdrawalsDB.Status.Projected],
  },
] as const;

describe("foreign window occupancy for every event kind", () => {
  for (const { kind, index, statuses } of occupancyCases) {
    for (const status of statuses) {
      it(`refuses while a ${status} ${kind} lies inside the window and commits once only events outside it remain`, async () => {
        const result = await onNode(
          Effect.gen(function* () {
            const hash = yield* recordForeignTip(nonEmptyWindowHeader());
            // (start, end] is open at the start: these two lie outside it.
            yield* index(new Date(WINDOW_START_MS), status as never);
            yield* index(new Date(WINDOW_END_MS + 1), status as never);
            const outside = yield* gate();
            yield* index(IN_WINDOW, status as never);
            return { hash, outside, inside: yield* gate() };
          }),
        );
        // ... and closed at the end: an event at exactly the end lies inside.
        const atEnd = await onNode(
          Effect.gen(function* () {
            yield* recordForeignTip(nonEmptyWindowHeader());
            yield* index(new Date(WINDOW_END_MS), status as never);
            return yield* gate();
          }),
        );
        expect(result.outside.type).toBe("Ready");
        for (const refusal of [result.inside, atEnd]) {
          expect(refusal).toMatchObject({
            type: "AwaitingForeignDa",
            foreignHeaderHash: result.hash,
            reason: "missing",
          });
          if (refusal.type === "AwaitingForeignDa")
            expect(refusal.detail).toContain("gate=pending_event_in_window");
        }
      });
    }
  }
});

describe("known residual: an honest peer block whose events this node also indexed", () => {
  it("refuses the whole commit on every pass until its DA is available, even past the horizon and once an L2 transaction consumed the deposit", async () => {
    // Pinned on purpose: without the peer's payload the node cannot tell which
    // of its in-window events the peer block already carries. Consumed only
    // means an L2 transaction spent the deposit's mempool output; no header
    // carries it yet. The pruner keeps such a row for the same reason (its
    // sweep needs authenticated history, so that half is asserted in the
    // retention suite: "retains awaiting evidence while an event no header
    // carries yet occupies its window, even once consumed").
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(nonEmptyWindowHeader(staleWindow));
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            STALE_WINDOW_START_MS + 5_000,
          ),
        });
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET status = ${DepositsDB.Status.Consumed}`;
        const passes = [yield* gate(), yield* gate(), yield* gate()];
        return { hash, passes, row: yield* reconciliationRow(hash) };
      }),
    );
    for (const pass of result.passes) {
      expect(pass).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.hash,
        reason: "missing",
      });
      if (pass.type === "AwaitingForeignDa")
        expect(pass.detail).toContain("gate=pending_event_in_window");
    }
    expect(result.row?.status).toBe("awaiting");
  });
});

describe("foreign-tip gate isolation", () => {
  it("commits past this node's own replaced block once its retained payload is pruned, and resolves it while the payload is held", async () => {
    const run = (prunePayload: boolean) =>
      onNode(
        Effect.gen(function* () {
          const { header, payload } = yield* Effect.promise(() =>
            oneDepositPayload("aa".repeat(32), recentWindow),
          );
          const hash = yield* recordForeignTip(header);
          yield* storeForeignDa(header, payload);
          if (prunePayload) {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM da_payloads WHERE header_hash = ${Buffer.from(hash, "hex")}`;
          }
          return { gate: yield* gate(), row: yield* reconciliationRow(hash) };
        }),
      );
    const pruned = await run(true);
    expect(pruned.gate.type).toBe("Ready");
    expect(pruned.row?.evidence_kind).toBe("pending_v1");
    const held = await run(false);
    expect(held.gate.type).toBe("Ready");
    expect(held.row?.evidence_kind).toBe("verified_da_v1");
  });

  it("keeps a row that no longer decodes gating on its own window", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(nonEmptyWindowHeader());
        const sql = yield* SqlClient.SqlClient;
        // Passes the table's checks, fails the V1 decoder.
        yield* sql`UPDATE foreign_tip_reconciliations SET blocking_reason = ''
          WHERE foreign_header_hash = ${Buffer.from(hash, "hex")}`;
        const empty = yield* gate();
        yield* indexDeposit({ [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW });
        return { hash, empty, occupied: yield* gate() };
      }),
    );
    expect(result.empty.type).toBe("Ready");
    expect(result.occupied).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: result.hash,
      reason: "replay_failed",
    });
  });

  it("keeps a resolved row that no longer decodes gating on its own window, on both the ordinary and the speculative path", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const { header, payload } = yield* Effect.promise(() =>
          oneDepositPayload("aa".repeat(32), recentWindow),
        );
        const hash = yield* recordForeignTip(header);
        yield* storeForeignDa(header, payload);
        const resolved = yield* gate();
        const row = yield* reconciliationRow(hash);
        const sql = yield* SqlClient.SqlClient;
        // Passes the table's checks, fails the V1 decoder.
        yield* sql`UPDATE foreign_tip_reconciliations
          SET verified_da_payload_sha256 = ${Buffer.alloc(32, 0x5a)}
          WHERE foreign_header_hash = ${Buffer.from(hash, "hex")}`;
        const empty = { gate: yield* gate(), assess: yield* assess() };
        // Consumed by an L2 transaction but carried by no header: the gate
        // counts it, though it is no late awaiting event.
        yield* indexDeposit({ [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW });
        yield* sql`UPDATE deposits_utxos SET status = ${DepositsDB.Status.Consumed}`;
        return {
          hash,
          resolved,
          status: row?.status,
          empty,
          occupied: { gate: yield* gate(), assess: yield* assess() },
        };
      }),
    );
    expect(result.resolved.type).toBe("Ready");
    expect(result.status).toBe("resolved");
    expect(result.empty.gate.type).toBe("Ready");
    expect(result.empty.assess.type).toBe("Ready");
    for (const refusal of [result.occupied.gate, result.occupied.assess]) {
      expect(refusal).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.hash,
        reason: "replay_failed",
      });
      if (refusal.type === "AwaitingForeignDa")
        expect(refusal.detail).toContain("gate=pending_event_in_window");
    }
  });

  it("hands a speculative build back when a late event lands in a resolved window, and the ordinary replay then releases it", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(headerFor(recentWindow));
        const resolved = yield* gate();
        const eventId = yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW,
        });
        const speculative = yield* assess();
        const statusAfterAssess = yield* depositStatus(eventId);
        return {
          hash,
          eventId,
          resolved,
          speculative,
          statusAfterAssess,
          ordinary: yield* gate(),
        };
      }),
    );
    expect(result.resolved.type).toBe("Ready");
    expect(result.speculative).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: result.hash,
      reason: "replay_required",
      detail: "late_event_in_resolved_window",
    });
    expect(result.statusAfterAssess).toBe(DepositsDB.Status.Awaiting);
    expect(result.ordinary).toEqual({
      type: "Ready",
      absent: {
        deposits: [result.eventId],
        forcedTransactions: [],
        withdrawals: [],
      },
    });
  });

  it("propagates a closed history-producer gate but isolates any other replay failure or defect", async () => {
    const gateClosed = new DatabaseError({
      table: historyAuthorityTable,
      message: "Current authenticated history producer is required",
      cause: new HistoryRecoverySuperseded({ message: "rewinding" }),
    });
    const outcome = (failure: Effect.Effect<never, unknown>) =>
      onNode(
        Effect.gen(function* () {
          yield* recordForeignTip(nonEmptyWindowHeader());
          injected.failure = failure;
          return yield* Effect.either(gate());
        }),
      );
    const closed = await outcome(Effect.fail(gateClosed));
    expect(closed).toMatchObject({ _tag: "Left", left: gateClosed });
    for (const failure of [
      Effect.fail(
        new DatabaseError({
          table: "foreign_tip_reconciliations",
          message: "transient",
          cause: "injected",
        }),
      ),
      Effect.die(new Error("injected defect")),
    ]) {
      const isolated = await outcome(failure);
      expect(isolated).toMatchObject({
        _tag: "Right",
        right: { type: "Ready" },
      });
    }
  });
});
