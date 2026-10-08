import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { isRecoverableHistorySourceFailure } from "../src/services/event-history-owner.source-failure.js";
import {
  HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
  HISTORY_SIGNED_INTENT_RELEASE_SOURCE,
  SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
} from "../src/services/liveness-halt.js";
import { NativeMpfRootNotRetained } from "../src/services/mpf-native-owner/protocol.js";
import {
  journal,
  S_COMMIT,
  S_HEADER,
  seedDisplaced,
  W_COMMIT,
  W_HEADER,
  W_NODE_OUT,
  W_NODE_TX,
  wHoldsTheSlot,
  X_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Attempt,
  type Fixture,
  ledgerRoot,
  observerSees,
  onNode,
  type OwnerModel,
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
 * The signed-intent release and the replaced-block revival restore the
 * native MPF root through `executeHistoryDependentRecovery`, after their
 * plan is prepared. A native owner that does not retain the target root in
 * full refuses the restore with `NativeMpfRootNotRetained` before it changes
 * its marker. The preparation then completes with the refusal held under its
 * own source as `signed_intent_target_root_not_retained`: the plan stays
 * prepared, native MPF, the SQL root and the journals stay as they are, and
 * every evaluation retries the restore. Once the native owner retains the
 * root, the next evaluation completes the recovery and clears the reason.
 * (The displacement compensation and the retained displacement's inverse,
 * which also run under the revival source, are held the same way: see
 * history-displacement-compensation.test.ts and
 * history-expired-intent-release-preparation-reversal.test.ts.)
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

const PREFIX_ROOT = "11".repeat(32);

/** W's node output 6 blocks deep at head height 10 (past the depth of 3). */
const deep = { head: 10, start: 1, txs: { [W_NODE_TX]: 5 } };

/** Refuses every restore with `NativeMpfRootNotRetained` until `retain`. */
const refuseUntilRetained = (owner: OwnerModel) => {
  let retained = false;
  owner.beforeRestore = async ({ targetRoot }) => {
    if (!retained) throw new NativeMpfRootNotRetained(targetRoot);
  };
  return () => {
    retained = true;
  };
};

const capturing = () => {
  const logs: string[] = [];
  const layer = Logger.add(
    Logger.make(({ message }) => {
      logs.push([message].flat().map(String).join(" "));
    }),
  );
  return { logs, layer };
};

const state = (owner: OwnerModel, headers: readonly Buffer[]) =>
  Effect.gen(function* () {
    const statuses: (Pending.Status | undefined)[] = [];
    for (const header of headers)
      statuses.push((yield* statusOf(header))?.status);
    return {
      statuses,
      plans: (yield* plans).map(({ state }) => state),
      ledger: yield* ledgerRoot,
      native: owner.durableRoot,
      restores: owner.restores,
    };
  });

const expectHeld = (attempts: readonly Attempt[], source: string) => {
  for (const attempt of attempts) {
    expect(attempt.failure).toBeUndefined();
    expect(attempt.raised.get(source)).toBe(
      SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
    );
  }
  // Raised once: one reason under the source, not one per evaluation.
  expect(
    attempts
      .at(-1)!
      .reasons.filter(
        (reason) => reason === SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
      ),
  ).toHaveLength(1);
};

const expectOperatorText = (logs: readonly string[], target: string) => {
  const raised = logs.find((line) =>
    line.startsWith(`${SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED}:`),
  );
  expect(raised).toContain(
    `Native MPF canonical recovery target root ${target} is not retained in full; refusing to restore`,
  );
  expect(raised).toContain(
    "Operator action is needed: stop the node, install at LEDGER_MPF_DB_PATH a native MPF store that retains this root in full",
  );
};

describe("a NativeMpfRootNotRetained refusal as the history owner classifies it", () => {
  it("is not a failure the owner reconnects after, as a dependent recovery's restore wraps it", () => {
    expect(
      isRecoverableHistorySourceFailure(
        new DatabaseError({
          table: "event_history_recovery_plans",
          message: "Native dependent rollback requires resumable recovery",
          cause: new NativeMpfRootNotRetained(UTXOS_ROOT),
        }),
      ),
    ).toBe(false);
  });
});

describe("the signed-intent release whose native restore is refused as not retained", () => {
  it("holds under the release source with its plan prepared, then completes once the root is retained", async () => {
    fixture.queue = wHoldsTheSlot;
    fixture.coverage = deep;
    const owner = ownerModel(ZERO_ROOT);
    const retain = refuseUntilRetained(owner);
    const { logs, layer } = capturing();
    const headers = [W_HEADER, S_HEADER, X_HEADER];
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* seedDisplaced(Pending.Status.LocallyApplied);
        yield* withNativeReplay(X_HEADER);
        const before = yield* state(owner, headers);
        const attempts = [
          yield* release(node).pipe(Effect.provide(layer)),
          yield* release(node),
        ];
        const held = yield* state(owner, headers);
        retain();
        const completed = yield* release(node);
        return {
          before,
          attempts,
          held,
          completed,
          after: yield* state(owner, headers),
        };
      }),
    );
    expectHeld(result.attempts, HISTORY_SIGNED_INTENT_RELEASE_SOURCE);
    expectOperatorText(logs, UTXOS_ROOT);
    expect(result.held).toEqual({ ...result.before, plans: ["prepared"] });
    expect(result.held.native).toBe(ZERO_ROOT);
    expect(result.held.restores).toBe(0);
    expect(result.completed.failure).toBeUndefined();
    expect(
      result.completed.raised.get(HISTORY_SIGNED_INTENT_RELEASE_SOURCE),
    ).toBeUndefined();
    expect(result.after.plans).toEqual(["applied"]);
    expect(result.after.native).toBe(UTXOS_ROOT);
    expect(result.after.restores).toBe(1);
    expect(result.after.statuses[2]).toBe(Pending.Status.Abandoned);
  });
});

describe("the replaced-block revival whose displaced-chain restore is refused as not retained", () => {
  it("holds under the revival source with its plan prepared, then completes once the root is retained", async () => {
    fixture.queue = wHoldsTheSlot;
    fixture.coverage = deep;
    const owner = ownerModel(PREFIX_ROOT);
    const retain = refuseUntilRetained(owner);
    const { logs, layer } = capturing();
    const headers = [W_HEADER, S_HEADER];
    const result = await onNode(
      owner,
      (node) =>
        Effect.gen(function* () {
          // W replaced and abandoned; S, which moved the ledger root, landed
          // in its place and finalized locally; W holds its base's slot.
          yield* journal(
            W_HEADER,
            Pending.Status.Abandoned,
            W_COMMIT,
            2_000_000,
            { abandonment: "replacement" },
          );
          yield* journal(
            S_HEADER,
            Pending.Status.LocallyApplied,
            S_COMMIT,
            3_000_000,
          );
          yield* Effect.flatMap(
            SqlClient.SqlClient,
            (sql) =>
              sql`UPDATE pending_block_finalizations SET expected_utxos_root = ${PREFIX_ROOT} WHERE header_hash = ${S_HEADER}`,
          );
          yield* withNativeReplay(S_HEADER);
          yield* observerSees([
            { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
          ]);
          const before = yield* state(owner, headers);
          const attempts = [
            yield* revival(node).pipe(Effect.provide(layer)),
            yield* revival(node),
          ];
          const held = yield* state(owner, headers);
          retain();
          const completed = yield* revival(node);
          return {
            before,
            attempts,
            held,
            completed,
            after: yield* state(owner, headers),
          };
        }),
      PREFIX_ROOT,
    );
    expectHeld(result.attempts, HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE);
    expectOperatorText(logs, UTXOS_ROOT);
    expect(result.held).toEqual({ ...result.before, plans: ["prepared"] });
    expect(result.held.native).toBe(PREFIX_ROOT);
    expect(result.held.ledger).toBe(PREFIX_ROOT);
    expect(result.completed.failure).toBeUndefined();
    expect(
      result.completed.raised.get(HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE),
    ).toBeUndefined();
    // Native MPF is back at W's base root, and the SQL marker at W's
    // candidate root, which local finalization replays.
    expect(result.after).toEqual({
      statuses: [
        Pending.Status.ObservedWaitingStability,
        Pending.Status.Abandoned,
      ],
      plans: ["applied"],
      ledger: ZERO_ROOT,
      native: UTXOS_ROOT,
      restores: 1,
    });
  });
});
