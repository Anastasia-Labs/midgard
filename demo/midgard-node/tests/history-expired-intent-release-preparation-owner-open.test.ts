import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Globals } from "../src/services/globals.js";
import {
  activeE,
  BASE_HEADER,
  BASE_OUT,
  E,
  E_HEADER,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  queueNode,
  root,
  SOURCE,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  onNode,
  type OwnerModel,
  ownerModel,
  plans,
  release,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * The signed-intent release while its retained native owner cannot open yet.
 * With no owner open, the release opens one from its retained bytes; while
 * the store's LevelDB lock is still held (a predecessor owner or process not
 * yet gone) that open fails with LEVEL_LOCKED (the real owner's error, see
 * native-owner-open-wait.test.ts), and the release holds: nothing written,
 * no owner installed, the intent still active (so its disposition keeps the
 * history gate closed). Once the lock is released it proceeds exactly once.
 * A binary digest mismatch stays a failure.
 */

type OpenFixture = Fixture & {
  /** What the next native owner open does. */
  open: "locked" | "mismatch" | OwnerModel;
  opens: number;
};

const fixture = vi.hoisted(
  (): OpenFixture => ({
    queue: undefined,
    coverage: "unavailable",
    open: "locked",
    opens: 0,
  }),
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
vi.mock("../src/services/mpf-native-owner/service.js", async (original) => {
  const actual =
    await original<
      typeof import("../src/services/mpf-native-owner/service.js")
    >();
  const { fakeOwner: model } = await import(
    "./helpers/history-expired-intent-release-preparation.owner.js"
  );
  return {
    ...actual,
    ProductionNativeMpfOwnerService: {
      create: async () => {
        fixture.opens += 1;
        if (fixture.open === "locked")
          throw new Error("Native MPF owner failed to open its store", {
            cause: Object.assign(new Error("Database is locked"), {
              code: "LEVEL_LOCKED",
            }),
          });
        if (fixture.open === "mismatch")
          throw new Error(
            "binarySha256 does not match the pinned owner binary",
          );
        return model(fixture.open);
      },
    },
  };
});

/** E's base D is still the queue's tail past E's TTL: E is replaced. */
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

/** E's own node holds D's slot: E landed. */
const eLanded = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      E_HEADER.toString("hex"),
      BASE_OUT,
    ),
    queueNode(
      E_HEADER.toString("hex"),
      BASE_HEADER.toString("hex"),
      undefined,
      `${E.hash}#0`,
    ),
  ],
} as never;

const activeExpiredE = Effect.gen(function* () {
  yield* activeE();
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE pending_block_finalizations
    SET block_end_time = block_start_time + INTERVAL '1 second'
    WHERE header_hash = ${E_HEADER}`;
  yield* withNativeReplay(E_HEADER);
  fixture.coverage = { head: 10, start: 1, txs: {} };
});

const state = Effect.gen(function* () {
  const globals = yield* Globals;
  return {
    e: (yield* statusOf(E_HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ownerOpen: (yield* Ref.get(globals.NATIVE_MPF_OWNER)) !== undefined,
  };
});

describe("the signed-intent release while its native owner cannot open", () => {
  it("holds while the store's lock is held, writing nothing, then replaces exactly once after it is released", async () => {
    const owner = ownerModel(ZERO_ROOT);
    fixture.opens = 0;
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* activeExpiredE;
        fixture.queue = baseUnspent;
        fixture.open = "locked";
        const locked = [yield* release(node), yield* release(node)];
        const held = yield* state;
        fixture.open = owner;
        const opened = yield* release(node);
        const after = yield* state;
        const again = yield* release(node);
        return { locked, held, opened, after, again, final: yield* state };
      }),
    );
    for (const attempt of result.locked) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBeUndefined();
    }
    expect(result.held).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: [],
      ownerOpen: false,
    });
    expect(result.opened.failure).toBeUndefined();
    expect(result.after).toEqual({
      e: Pending.Status.Abandoned,
      plans: ["applied"],
      ownerOpen: true,
    });
    expect(result.again.failure).toBeUndefined();
    expect(result.final.plans).toEqual(["applied"]);
    expect(owner.restores).toBe(1);
    // Two locked opens, one that succeeded; the open owner is then reused.
    expect(fixture.opens).toBe(3);
  });

  it("still fails on a binary digest mismatch, writing nothing", async () => {
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* activeExpiredE;
        fixture.queue = baseUnspent;
        fixture.open = "mismatch";
        return { attempt: yield* release(node), after: yield* state };
      }),
    );
    expect(result.attempt.failure).toContain(
      "Retained native owner could not open",
    );
    expect(result.attempt.failure).toContain(
      "binarySha256 does not match the pinned owner binary",
    );
    expect(result.after).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: [],
      ownerOpen: false,
    });
  });

  it("holds the landed block's replay over its retained plan while the lock is held, then replays it exactly once", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* activeExpiredE;
        // The replacement's plan is prepared and its CAS runs (the native
        // root at E's base); the node stops before the SQL repair, and
        // restarts with no owner open.
        fixture.queue = baseUnspent;
        fixture.open = owner;
        owner.afterRestore = async () => {
          throw new Error("modelled stop after the native CAS");
        };
        const crashed = yield* release(node);
        owner.afterRestore = undefined;
        yield* Ref.set(globals.NATIVE_MPF_OWNER, undefined);
        const restoredTo = owner.durableRoot;
        // E then shows landed while the store's lock is still held.
        fixture.queue = eLanded;
        fixture.open = "locked";
        const locked = yield* release(node);
        const held = { ...(yield* state), recovers: owner.recovers };
        fixture.open = owner;
        const opened = yield* release(node);
        return {
          crashed,
          restoredTo,
          locked,
          held,
          opened,
          after: yield* state,
        };
      }),
    );
    expect(result.crashed.failure).toContain(
      "modelled stop after the native CAS",
    );
    expect(result.restoredTo).toBe(UTXOS_ROOT);
    expect(result.locked.failure).toBeUndefined();
    expect(result.held).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: ["prepared"],
      ownerOpen: false,
      recovers: 0,
    });
    expect(owner.recovers).toBe(1);
    expect(result.opened.failure).toBeUndefined();
    // Replayed to its candidate once, its plan discarded, and E recorded
    // confirmed.
    expect(result.after.plans).toEqual([]);
    expect(owner.durableRoot).toBe(ZERO_ROOT);
    expect(result.after.e).not.toBe(Pending.Status.PendingSubmission);
    expect(result.after.e).not.toBe(Pending.Status.Abandoned);
  });
});
