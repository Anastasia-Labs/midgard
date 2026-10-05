import { SqlClient } from "@effect/sql";
import { Effect, Runtime } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  activeE,
  BASE_HEADER,
  BASE_OUT,
  E_HEADER,
  hex,
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
 * The signed-intent release between its decision and its plan. The decision
 * is read outside the owned recovery transaction; the plan re-derives it
 * inside, after the native diagnostics. A journal or an observer view that
 * changed in that window is a lost race (`HistoryRecoverySuperseded`, nothing
 * written), and a retained plan whose native root is outside its journal
 * never executes.
 */

const fixture = vi.hoisted((): Fixture & { observer: unknown } => ({
  queue: undefined,
  coverage: "unavailable",
  observer: undefined,
}));

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
/** The correction observer's view: `fixture.observer` once set. */
vi.mock(
  "../src/services/state-queue-correction-rewind.js",
  async (original) => {
    const { Effect } = await import("effect");
    const module =
      await original<
        typeof import("../src/services/state-queue-correction-rewind.js")
      >();
    return {
      ...module,
      loadStateQueueCorrectionObserverState: (
        ...args: Parameters<typeof module.loadStateQueueCorrectionObserverState>
      ) =>
        fixture.observer === undefined
          ? module.loadStateQueueCorrectionObserverState(...args)
          : Effect.succeed(fixture.observer as never),
    };
  },
);

const E = E_HEADER.toString("hex");
const INTEGRITY = "signed_intent_replacement_integrity";
const FOREIGN_ROOT = hex("foreign-durable-root");

/** E's base D is still the queue's tail at the checkpoint, past E's TTL, and
 * E's signed commit is not in the canonical history: E is replaced. */
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

/** An admitted timeout correction that removed E. */
const correctionRemovingE = {
  kind: "observed",
  state: {
    admitted: [
      {
        transitionKind: "timeout_correction",
        removedHeaderHashes: [E],
        transactionHash: hex("correction-of-e"),
      },
    ],
    pending: [],
  },
};

const sql = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => Effect.flatMap(SqlClient.SqlClient, statement);

/** E active, journaled with its native replay, and decided "replace". */
const replaceableE = Effect.gen(function* () {
  yield* activeE();
  yield* sql(
    (sql) => sql`UPDATE pending_block_finalizations
      SET block_end_time = block_start_time + INTERVAL '1 second'
      WHERE header_hash = ${E_HEADER}`,
  );
  yield* withNativeReplay(E_HEADER);
  fixture.queue = baseUnspent;
  fixture.coverage = { head: 10, start: 1, txs: {} };
  fixture.observer = undefined;
});

/** One release where `race` runs once, as the native diagnostics are read
 * (after the decision, before the plan re-derives it); then a second one. */
const raced = (
  owner: OwnerModel,
  race: Effect.Effect<unknown, unknown, SqlClient.SqlClient>,
) =>
  onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        yield* replaceableE;
        const run = Runtime.runPromise(
          yield* Effect.runtime<SqlClient.SqlClient>(),
        );
        owner.beforeDiagnostics = async () => {
          owner.beforeDiagnostics = undefined;
          await run(race);
        };
        const first = yield* release(node);
        const after = {
          e: (yield* statusOf(E_HEADER))?.status,
          plans: (yield* plans).map(({ state }) => state),
          restores: owner.restores,
        };
        const second = yield* release(node);
        return {
          first,
          after,
          second,
          final: {
            e: (yield* statusOf(E_HEADER))?.status,
            plans: (yield* plans).map(({ state }) => state),
            restores: owner.restores,
          },
        };
      }),
    ZERO_ROOT,
  );

/** E's plan prepared and its CAS run from `startRoot`, then the node stops;
 * the native root is then `observedRoot` for two attempts and, when given,
 * `restoredRoot` for a last one. */
const retainedThenObserved = (
  startRoot: string,
  observedRoot: string,
  restoredRoot?: string,
) => {
  const owner = ownerModel(startRoot);
  return onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        yield* replaceableE;
        owner.afterRestore = async () => {
          throw new Error("modelled stop after the native CAS");
        };
        const crashed = yield* release(node);
        owner.afterRestore = undefined;
        const retained = (yield* plans).map(({ state }) => state);
        owner.durableRoot = observedRoot;
        const attempts = [yield* release(node), yield* release(node)];
        const observed = {
          e: (yield* statusOf(E_HEADER))?.status,
          plans: (yield* plans).map(({ state }) => state),
          restores: owner.restores,
          durableRoot: owner.durableRoot,
        };
        if (restoredRoot !== undefined) owner.durableRoot = restoredRoot;
        const restored =
          restoredRoot === undefined ? undefined : yield* release(node);
        return {
          crashed,
          retained,
          attempts,
          observed,
          restored,
          final: {
            e: (yield* statusOf(E_HEADER))?.status,
            plans: (yield* plans).map(({ state }) => state),
            restores: owner.restores,
            durableRoot: owner.durableRoot,
          },
        };
      }),
    ZERO_ROOT,
  );
};

describe("the signed-intent release re-deriving its decision in its plan", () => {
  it("is superseded, writing nothing, when the decision changed from replace to defer, and defers on the next attempt", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await raced(
      owner,
      Effect.sync(() => {
        fixture.observer = correctionRemovingE;
      }),
    );
    expect(result.first.failureTag).toBe("HistoryRecoverySuperseded");
    expect(result.first.failure).toBe(
      `Signed-intent decision for ${E} changed from replace to defer`,
    );
    expect(result.first.raised.get(SOURCE)).toBeUndefined();
    // Nothing written: no plan, no native CAS, E still the active intent.
    expect(result.after).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: [],
      restores: 0,
    });
    // Decided again from fresh state: deferred to the correction path.
    expect(result.second.failure).toBeUndefined();
    expect(result.final).toEqual(result.after);
  });

  it("is superseded, writing nothing, when the journal stopped being replaceable since its decision", async () => {
    const owner = ownerModel(ZERO_ROOT);
    const result = await raced(
      owner,
      sql(
        (sql) => sql`UPDATE pending_block_finalizations
          SET status = ${Pending.Status.Abandoned}
          WHERE header_hash = ${E_HEADER}`,
      ),
    );
    expect(result.first.failureTag).toBe("HistoryRecoverySuperseded");
    expect(result.first.failure).toContain(
      `Signed-intent journal ${E} stopped being replaceable since its decision: `,
    );
    expect(result.first.failure).toContain(
      `has no unlanded status, deployment or native replay matching its journal roots`,
    );
    expect(result.after).toEqual({
      e: Pending.Status.Abandoned,
      plans: [],
      restores: 0,
    });
    // No journal is active any more: nothing to release.
    expect(result.second.failure).toBeUndefined();
    expect(result.final).toEqual(result.after);
  });
});

describe("a retained signed-intent release plan and the native durable root", () => {
  it("never executes while the durable root is outside its journal: held, and resumed once the root is back where its CAS left it", async () => {
    const result = await retainedThenObserved(
      ZERO_ROOT,
      FOREIGN_ROOT,
      UTXOS_ROOT,
    );
    expect(result.crashed.failure).toContain(
      "modelled stop after the native CAS",
    );
    expect(result.retained).toEqual(["prepared"]);
    // Held under the release source, never failing the history owner (whose
    // restart would read the same native store).
    for (const attempt of result.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBe(INTEGRITY);
      expect(attempt.reasons).toContain(INTEGRITY);
    }
    // The plan is still retained, its CAS never ran again, and neither the
    // journal nor the native root moved.
    expect(result.observed).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: ["prepared"],
      restores: 1,
      durableRoot: FOREIGN_ROOT,
    });
    // Evidence the hold's reason is gone resumes it: replaced exactly once.
    expect(result.restored?.failure).toBeUndefined();
    expect(result.restored?.raised.get(SOURCE)).toBeUndefined();
    expect(result.final).toEqual({
      e: Pending.Status.Abandoned,
      plans: ["applied"],
      restores: 2,
      durableRoot: UTXOS_ROOT,
    });
  });

  it("never executes when the durable root is the journal's candidate but the plan's CAS moves from its base", async () => {
    // Prepared with the native root already at E's base: its CAS moves from
    // the base. A durable root back at the candidate is not where the plan
    // left it.
    const result = await retainedThenObserved(UTXOS_ROOT, ZERO_ROOT);
    expect(result.retained).toEqual(["prepared"]);
    for (const attempt of result.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBe(INTEGRITY);
    }
    expect(result.final).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: ["prepared"],
      restores: 1,
      durableRoot: ZERO_ROOT,
    });
  });

  it("resumes, and replaces exactly once, while the durable root is where the plan's CAS left it or moves it from", async () => {
    for (const observedRoot of [UTXOS_ROOT, ZERO_ROOT]) {
      const result = await retainedThenObserved(ZERO_ROOT, observedRoot);
      expect(result.retained).toEqual(["prepared"]);
      for (const attempt of result.attempts) {
        expect(attempt.failure).toBeUndefined();
        expect(attempt.raised.get(SOURCE)).toBeUndefined();
      }
      expect(result.final).toEqual({
        e: Pending.Status.Abandoned,
        plans: ["applied"],
        restores: 2,
        durableRoot: UTXOS_ROOT,
      });
    }
  });

  it("holds only the root refusals: any other failure of the plan's preparation propagates, writing nothing", async () => {
    // The cursor moved under the decision: the plan's checkpoint lock
    // refuses with a failure that says nothing about the native root.
    const owner = ownerModel(ZERO_ROOT);
    const result = await raced(
      owner,
      sql(
        (sql) => sql`UPDATE event_history_cursor SET revision = revision + 1`,
      ),
    );
    expect(result.first.failureTag).toBe("DatabaseError");
    expect(result.first.failure).toBe("Recovery plan checkpoint changed");
    expect(result.first.raised.get(SOURCE)).toBeUndefined();
    expect(result.first.reasons).not.toContain(INTEGRITY);
    expect(result.after).toEqual({
      e: Pending.Status.PendingSubmission,
      plans: [],
      restores: 0,
    });
  });

  it("holds, and never plans, a first release whose durable root is outside its journal", async () => {
    const owner = ownerModel(FOREIGN_ROOT);
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          yield* replaceableE;
          const held = yield* release(node);
          return {
            held,
            plans: (yield* plans).map(({ state }) => state),
            e: (yield* statusOf(E_HEADER))?.status,
          };
        }),
      ZERO_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(SOURCE)).toBe(INTEGRITY);
    expect(result.plans).toEqual([]);
    expect(result.e).toBe(Pending.Status.PendingSubmission);
    expect(owner.restores).toBe(0);
  });
});
