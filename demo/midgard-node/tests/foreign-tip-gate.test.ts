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
  pruneSettledForeignTipReconciliations,
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
  it("classifies a non-empty root with a zero count as invalid from the header alone", async () => {
    const header = nonEmptyWindowHeader({ depositCount: 0n });
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

  it("never prunes a header its commitment columns show is malformed, even before any replay, nor lets a speculative build past it", async () => {
    const malformed = { depositCount: 1n, totalEventCount: 1n };
    const result = await onNode(
      Effect.gen(function* () {
        const staleMalformed = yield* recordForeignTip(
          headerFor({ ...staleWindow, ...malformed }),
        );
        const staleHonest = yield* recordForeignTip(
          headerFor({ ...staleWindow, prevHeaderHash: "44".repeat(28) }),
        );
        const pruned = yield* pruneSettledForeignTipReconciliations({
          now: new Date(),
          eventsIngestedThrough: INGESTED_PAST_WINDOW,
        });
        const kept = yield* reconciliationRow(staleMalformed);
        const gone = yield* reconciliationRow(staleHonest);
        const recent = yield* recordForeignTip(
          headerFor({ ...recentWindow, ...malformed }),
        );
        const beforeReplay = yield* assess();
        yield* gate();
        return {
          recent,
          pruned,
          kept,
          gone,
          beforeReplay,
          afterReplay: yield* assess(),
        };
      }),
    );
    expect(result.pruned).toBe(1);
    expect(result.kept?.status).toBe("awaiting");
    expect(result.gone).toBeUndefined();
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
        expect(result.outside.type).toBe("Ready");
        expect(result.inside).toMatchObject({
          type: "AwaitingForeignDa",
          foreignHeaderHash: result.hash,
          reason: "missing",
        });
        if (result.inside.type === "AwaitingForeignDa")
          expect(result.inside.detail).toContain(
            "gate=pending_event_in_window",
          );
      });
    }
  }
});

describe("known residual: an honest peer block whose events this node also indexed", () => {
  it("refuses the whole commit on every pass and is never pruned, even past the horizon, until its DA is available", async () => {
    // Pinned on purpose: without the peer's payload the node cannot tell which
    // of its in-window events the peer block already carries.
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(nonEmptyWindowHeader(staleWindow));
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            STALE_WINDOW_START_MS + 5_000,
          ),
        });
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

  it("still answers when the prune fails, and prunes exactly once when it recovers", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const hash = yield* recordForeignTip(headerFor(staleWindow));
        yield* sql.unsafe(`
          CREATE OR REPLACE FUNCTION foreign_tip_gate_refuse_delete()
          RETURNS trigger LANGUAGE plpgsql AS $$
          BEGIN RAISE EXCEPTION 'injected prune failure'; END $$;
          CREATE TRIGGER foreign_tip_gate_refuse_delete
          BEFORE DELETE ON foreign_tip_reconciliations
          FOR EACH ROW EXECUTE FUNCTION foreign_tip_gate_refuse_delete();
        `);
        const dropTrigger = sql.unsafe(`
          DROP TRIGGER IF EXISTS foreign_tip_gate_refuse_delete
            ON foreign_tip_reconciliations;
          DROP FUNCTION IF EXISTS foreign_tip_gate_refuse_delete();
        `);
        const failing = yield* gate().pipe(
          Effect.ensuring(dropTrigger.pipe(Effect.orDie)),
        );
        const retained = yield* reconciliationRow(hash);
        const recovered = yield* gate();
        return {
          failing,
          retained,
          recovered,
          gone: yield* reconciliationRow(hash),
        };
      }),
    );
    expect(result.failing.type).toBe("Ready");
    expect(result.retained).toBeDefined();
    expect(result.recovered.type).toBe("Ready");
    expect(result.gone).toBeUndefined();
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
