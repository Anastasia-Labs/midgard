import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  BASE_HEADER,
  BASE_OUT,
  E,
  fixtureHeader,
  insertJournal,
  retainedBaseJournal,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  journal,
  queueNode,
  root,
  SOURCE,
  W_COMMIT,
  W_HEADER,
  W_NODE_OUT,
  W_NODE_TX,
  wHoldsTheSlot,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  ledgerRoot,
  observerSees,
  onNode,
  ownerModel,
  plans,
  release,
  revival,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * The signed-intent release reads its target root from the active journal's
 * base root. With no retained journal of its base tail header hash, the
 * journal's own header bytes must hash to its header hash and name its base
 * tail header hash, base root and candidate root as that header's
 * predecessor and roots. Otherwise the release holds under
 * `signed_intent_journal_unbound`: no plan, no native CAS, no SQL write, the
 * journal unchanged. The next evaluation that reads a journal its header
 * binds clears it and releases. The replaced-block revival binds its
 * winner's journal base the same way, under its own source.
 */

const fixture = vi.hoisted(
  (): Fixture => ({ queue: undefined, coverage: "unavailable" }),
);

vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.ledgerSnapshot(original),
  ),
);
vi.mock(
  "../src/services/history-expired-intent-release.signed-commit-node.js",
  (original) =>
    import(
      "./helpers/history-expired-intent-release-preparation.mocks.js"
    ).then((mocks) => mocks.queueAuthentication(original, fixture)),
);
vi.mock("../src/database/eventHistoryCanonicalCoverage.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.canonicalCoverage(original, fixture),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.nodeSerialization(original),
  ),
);

const UNBOUND = "signed_intent_journal_unbound";
const REVIVAL_SOURCE = "history_replaced_block_revival";
const INTEGRITY = "signed_intent_replacement_integrity";

/** The active block, on D (`BASE_HEADER`) at the base root `UTXOS_ROOT`,
 * moving it to `ZERO_ROOT`; its journal carries that header's bytes. */
const HEADER = fixtureHeader("journal-base:active", BASE_HEADER);
/** A root the active block's header does not name. */
const OTHER_ROOT = "33".repeat(32);

/** D is still the queue's tail past the TTL: the active block is replaced. */
const baseUnspent = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      undefined,
      BASE_OUT,
    ),
  ],
} as never;

const setJournal = (columns: Readonly<Record<string, unknown>>) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`UPDATE pending_block_finalizations SET ${sql.update(
        columns as never,
      )} WHERE header_hash = ${HEADER}`,
  );

/** The journal's base root and its native replay's base root, together. */
const setBaseRoot = (baseRoot: string) =>
  setJournal({
    base_utxos_root: baseRoot,
    mpf_replay_base_root: Buffer.from(baseRoot, "hex"),
  });

/** The active journal of `HEADER` with its native replay, and no retained
 * journal of its base D. */
const activeJournal = Effect.gen(function* () {
  yield* insertJournal({
    header: HEADER,
    status: Pending.Status.PendingSubmission,
    commit: E,
    baseOut: BASE_OUT,
    baseHeader: BASE_HEADER,
    createdAt: new Date(2_000_000),
  });
  yield* Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`UPDATE pending_block_finalizations
      SET block_end_time = block_start_time + INTERVAL '1 second'
      WHERE header_hash = ${HEADER}`,
  );
  yield* withNativeReplay(HEADER);
  fixture.queue = baseUnspent;
  fixture.coverage = { head: 10, start: 1, txs: {} };
});

const state = Effect.gen(function* () {
  return {
    status: (yield* statusOf(HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
  };
});

describe("the signed-intent release with no retained parent journal", () => {
  it("releases a journal its own header binds", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* activeJournal;
          const released = yield* release(node);
          return { released, after: yield* state };
        }),
      ZERO_ROOT,
    );
    expect(result.released.failure).toBeUndefined();
    expect(result.released.raised.get(SOURCE)).toBeUndefined();
    expect(result.after).toEqual({
      status: Pending.Status.Abandoned,
      plans: ["applied"],
      ledger: UTXOS_ROOT,
    });
    expect(owner.restores).toBe(1);
    expect(owner.durableRoot).toBe(UTXOS_ROOT);
  });

  it("holds, writing nothing, on a base root its header does not name, and releases once the columns bind again", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* activeJournal;
          yield* setBaseRoot(OTHER_ROOT);
          const attempts = [yield* release(node), yield* release(node)];
          const held = { ...(yield* state), restores: owner.restores };
          yield* setBaseRoot(UTXOS_ROOT);
          const released = yield* release(node);
          return { attempts, held, released, after: yield* state };
        }),
      ZERO_ROOT,
    );
    for (const attempt of result.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBe(UNBOUND);
      expect(attempt.reasons).toContain(UNBOUND);
    }
    expect(
      result.attempts[1]!.reasons.filter((reason) => reason === UNBOUND),
    ).toHaveLength(1);
    expect(result.held).toEqual({
      status: Pending.Status.PendingSubmission,
      plans: [],
      ledger: ZERO_ROOT,
      restores: 0,
    });
    expect(result.released.failure).toBeUndefined();
    expect(result.released.raised.get(SOURCE)).toBeUndefined();
    expect(result.after).toEqual({
      status: Pending.Status.Abandoned,
      plans: ["applied"],
      ledger: UTXOS_ROOT,
    });
    expect(owner.durableRoot).toBe(UTXOS_ROOT);
  });

  it("holds, writing nothing, on header bytes that do not hash to its header hash", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* activeJournal;
          yield* setJournal({
            [Pending.Columns.HEADER_CBOR]: Buffer.from("a0", "hex"),
          });
          const held = yield* release(node);
          return { held, after: yield* state };
        }),
      ZERO_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(SOURCE)).toBe(UNBOUND);
    expect(result.after).toEqual({
      status: Pending.Status.PendingSubmission,
      plans: [],
      ledger: ZERO_ROOT,
    });
    expect(owner.restores).toBe(0);
  });
});

const setWinner = (columns: Readonly<Record<string, unknown>>) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`UPDATE pending_block_finalizations SET ${sql.update(
        columns as never,
      )} WHERE header_hash = ${W_HEADER}`,
  );

const setLedger = (ledger: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`UPDATE mpf_engine_state SET root_hex = ${ledger} WHERE store_name = 'ledger'`,
  );

/** W, on D (`BASE_HEADER`) at the base root `UTXOS_ROOT`, replaced and
 * abandoned, and the only journal; D has no retained journal. The correction
 * observer's cursor queue shows W, whose node output is deep. */
const winnerJournal = Effect.gen(function* () {
  yield* journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
    abandonment: "replacement",
  });
  yield* observerSees([
    { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
  ]);
  fixture.queue = wHoldsTheSlot;
  fixture.coverage = { head: 10, start: 1, txs: { [W_NODE_TX]: 5 } };
});

const winnerState = Effect.gen(function* () {
  return {
    status: (yield* statusOf(W_HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
  };
});

describe("the replaced-block revival with no retained parent journal", () => {
  it("revives a winner its own header binds", async () => {
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* winnerJournal;
          const revived = yield* revival(node);
          return { revived, after: yield* winnerState };
        }),
      UTXOS_ROOT,
    );
    expect(result.revived.failure).toBeUndefined();
    expect(result.revived.raised.get(REVIVAL_SOURCE)).toBeUndefined();
    expect(result.after).toEqual({
      status: Pending.Status.ObservedWaitingStability,
      plans: [],
      ledger: ZERO_ROOT,
    });
  });

  it("holds, writing nothing, on winner header bytes that do not hash to its header hash", async () => {
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* winnerJournal;
          yield* setWinner({
            [Pending.Columns.HEADER_CBOR]: Buffer.from("a0", "hex"),
          });
          const held = yield* revival(node);
          return { held, after: yield* winnerState };
        }),
      UTXOS_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(REVIVAL_SOURCE)).toBe(UNBOUND);
    expect(result.held.reasons).toContain(UNBOUND);
    expect(result.after).toEqual({
      status: Pending.Status.Abandoned,
      plans: [],
      ledger: UTXOS_ROOT,
    });
  });

  it("holds, writing nothing, on a winner base root its header does not name, and revives once the columns bind again", async () => {
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* winnerJournal;
          yield* setWinner({ base_utxos_root: OTHER_ROOT });
          const attempts = [yield* revival(node), yield* revival(node)];
          const held = yield* winnerState;
          yield* setWinner({ base_utxos_root: UTXOS_ROOT });
          yield* setLedger(UTXOS_ROOT);
          const revived = yield* revival(node);
          return { attempts, held, revived, after: yield* winnerState };
        }),
      OTHER_ROOT,
    );
    for (const attempt of result.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(REVIVAL_SOURCE)).toBe(UNBOUND);
    }
    expect(
      result.attempts[1]!.reasons.filter((reason) => reason === UNBOUND),
    ).toHaveLength(1);
    expect(result.held).toEqual({
      status: Pending.Status.Abandoned,
      plans: [],
      ledger: OTHER_ROOT,
    });
    expect(result.revived.failure).toBeUndefined();
    expect(result.revived.raised.get(REVIVAL_SOURCE)).toBeUndefined();
    expect(result.after).toEqual({
      status: Pending.Status.ObservedWaitingStability,
      plans: [],
      ledger: ZERO_ROOT,
    });
  });

  it("holds, writing nothing, on a winner base root other than its retained parent journal's root", async () => {
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* retainedBaseJournal;
          yield* winnerJournal;
          yield* setWinner({ base_utxos_root: OTHER_ROOT });
          const held = yield* revival(node);
          return { held, after: yield* winnerState };
        }),
      OTHER_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(REVIVAL_SOURCE)).toBe(INTEGRITY);
    expect(result.after).toEqual({
      status: Pending.Status.Abandoned,
      plans: [],
      ledger: OTHER_ROOT,
    });
  });
});
