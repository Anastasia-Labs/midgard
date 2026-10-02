import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { DatabaseError } from "../src/database/utils/common.js";
import {
  assessRetainedForeignTipWindows,
  pruneSettledForeignTipReconciliations,
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips,
} from "../src/workers/t2-foreign-event-reconciliation.js";
import {
  headerFor,
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
const failReplays = () =>
  Effect.sync(() => {
    injected.failure = Effect.fail(
      new DatabaseError({
        table: "foreign_tip_reconciliations",
        message: "transient",
        cause: "injected",
      }),
    );
  });

const recentWindow = {
  startTime: BigInt(WINDOW_START_MS),
  endTime: BigInt(WINDOW_END_MS),
};
const staleWindow = {
  startTime: BigInt(STALE_WINDOW_START_MS),
  endTime: BigInt(STALE_WINDOW_END_MS),
};

/** Event roots and counts that disagree, on the header's face. */
const malformedShapes = [
  {
    label: "an empty root with a count",
    header: (window: Partial<SDK.Header>) =>
      headerFor({ ...window, depositCount: 1n, totalEventCount: 1n }),
  },
  {
    label: "a root with a zero count",
    header: (window: Partial<SDK.Header>) =>
      nonEmptyWindowHeader({ ...window, depositCount: 0n }),
  },
] as const;

const PAYLOAD_INVALID =
  "foreign DA payload failed header, root, or count verification";

const payloadFor = (depositId: string, window: Partial<SDK.Header>) =>
  Effect.promise(() => oneDepositPayload(depositId, window));

const deleteForeignDa = (foreignHeaderHash: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`DELETE FROM da_payloads WHERE header_hash = ${Buffer.from(foreignHeaderHash, "hex")}`,
  );

/** Retains a foreign block whose header is consistent on its face while the
 * DA payload this node holds for it fails verification against it. */
const recordPayloadInvalidTip = (window: Partial<SDK.Header>) =>
  Effect.gen(function* () {
    const own = yield* payloadFor("aa".repeat(32), window);
    const other = yield* payloadFor("bb".repeat(32), window);
    const hash = yield* recordForeignTip(own.header);
    yield* storeForeignDa(own.header, other.payload);
    return { hash, own };
  });

const expectInvalid = (result: unknown, foreignHeaderHash: string) =>
  expect(result).toMatchObject({
    type: "AwaitingForeignDa",
    foreignHeaderHash,
    reason: "invalid",
    detail: PAYLOAD_INVALID,
  });

describe("a payload-level invalid verdict is as durable as a header-evident one", () => {
  for (const [label, window] of [
    ["recent", recentWindow],
    ["past the horizon", staleWindow],
  ] as const) {
    it(`keeps refusing after the failing payload is gone, and keeps the row, ${label}`, async () => {
      const result = await onNode(
        Effect.gen(function* () {
          const { hash } = yield* recordPayloadInvalidTip(window);
          const first = yield* gate();
          yield* deleteForeignDa(hash);
          const speculative = yield* assess();
          const second = yield* gate();
          const third = yield* gate();
          return {
            hash,
            passes: [first, second, third],
            speculative,
            row: yield* reconciliationRow(hash),
          };
        }),
      );
      for (const pass of result.passes) expectInvalid(pass, result.hash);
      expect(result.speculative).toMatchObject({
        type: "AwaitingForeignDa",
        reason: "replay_required",
        detail: `invalid:${PAYLOAD_INVALID}`,
      });
      expect(result.row).toMatchObject({
        status: "awaiting",
        blocking_reason: `invalid:${PAYLOAD_INVALID}`,
      });
    });
  }

  it("lifts it once the node holds a payload that verifies against the header", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const { hash, own } = yield* recordPayloadInvalidTip(recentWindow);
        const first = yield* gate();
        yield* deleteForeignDa(hash);
        yield* storeForeignDa(own.header, own.payload);
        return {
          hash,
          first,
          second: yield* gate(),
          row: yield* reconciliationRow(hash),
        };
      }),
    );
    expectInvalid(result.first, result.hash);
    expect(result.second.type).toBe("Ready");
    expect(result.row).toMatchObject({
      status: "resolved",
      evidence_kind: "verified_da_v1",
      blocking_reason: null,
    });
  });
});

describe("a refusing row refuses on every path that cannot replay it", () => {
  for (const { label, header } of malformedShapes) {
    it(`refuses on ${label} whose row no longer decodes, with an empty window`, async () => {
      const result = await onNode(
        Effect.gen(function* () {
          const hash = yield* recordForeignTip(header(recentWindow));
          const sql = yield* SqlClient.SqlClient;
          // Passes the table's checks, fails the V1 decoder.
          yield* sql`UPDATE foreign_tip_reconciliations SET blocking_reason = ''
            WHERE foreign_header_hash = ${Buffer.from(hash, "hex")}`;
          return { hash, gate: yield* gate() };
        }),
      );
      expect(result.gate).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.hash,
        reason: "replay_failed",
      });
    });

    it(`refuses on ${label} whose replay fails this pass, with an empty window`, async () => {
      const result = await onNode(
        Effect.gen(function* () {
          const hash = yield* recordForeignTip(header(recentWindow));
          yield* failReplays();
          return { hash, gate: yield* gate() };
        }),
      );
      expect(result.gate).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.hash,
        reason: "replay_failed",
      });
    });
  }

  it("honours a stored invalid verdict on a row consistent on its face, under a speculative assess and a failed replay", async () => {
    const recent = await onNode(
      Effect.gen(function* () {
        const { hash } = yield* recordPayloadInvalidTip(recentWindow);
        yield* gate();
        const speculative = yield* assess();
        yield* failReplays();
        return { hash, speculative, replayFailed: yield* gate() };
      }),
    );
    expect(recent.speculative).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: recent.hash,
      reason: "replay_required",
    });
    expect(recent.replayFailed).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: recent.hash,
      reason: "replay_failed",
    });
  });

  it("honours a stored invalid verdict on a row consistent on its face, under the prune", async () => {
    const stale = await onNode(
      Effect.gen(function* () {
        const { hash } = yield* recordPayloadInvalidTip(staleWindow);
        yield* gate();
        // A pass whose replay fails leaves the stored verdict to the prune.
        yield* failReplays();
        yield* gate();
        const pruned = yield* pruneSettledForeignTipReconciliations({
          now: new Date(),
          eventsIngestedThrough: INGESTED_PAST_WINDOW,
        });
        return { pruned, row: yield* reconciliationRow(hash) };
      }),
    );
    expect(stale.pruned).toBe(0);
    expect(stale.row?.blocking_reason).toBe(`invalid:${PAYLOAD_INVALID}`);
  });
});
