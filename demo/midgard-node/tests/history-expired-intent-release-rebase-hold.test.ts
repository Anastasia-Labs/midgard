import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Globals } from "../src/services/globals.js";
import {
  historyRecoveryPass,
  NATIVE_RESTORE_HELD,
} from "../src/services/history-dependent-recovery.js";
import {
  prepareExpiredIntentRelease,
  prepareReplacedBlockRevival,
} from "../src/services/history-expired-intent-release.js";
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
  type Fixture,
  inputs,
  type Node,
  observerSees,
  onNode,
  type OwnerModel,
  ownerModel,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * The history owner's recovery pass (`historyRecoveryPass`, which the
 * production history runtime runs) skips the landed-block rebase in a pass
 * where a recovery held on its native restore: the held plan still owns the
 * native root it restores. Driven here through the real signed-intent
 * release and replaced-block revival preparations, whose native restore the
 * owner refuses as not retained until it retains the root.
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

/** One recovery pass in which only `recovery` (in its own `slot`) has work;
 * what it returned, how many times the rebase ran, and the reason raised
 * under `source`. */
const pass = (
  slot: "expiredIntentRelease" | "replacedBlockRevival",
  recovery: Effect.Effect<unknown, unknown, SqlClient.SqlClient | Globals>,
  source: string,
) =>
  Effect.gen(function* () {
    let returned: unknown;
    let rebases = 0;
    yield* historyRecoveryPass({
      correctionRewind: Effect.void,
      signedHeaderRecovery: Effect.void,
      expiredIntentRelease: Effect.void,
      replacedBlockRevival: Effect.void,
      [slot]: Effect.tap(recovery, (value) =>
        Effect.sync(() => {
          returned = value;
        }),
      ),
      landedBlockRebase: Effect.sync(() => {
        rebases += 1;
      }),
    });
    const globals = yield* Globals;
    return {
      returned,
      rebases,
      raised: (yield* Ref.get(globals.LIVENESS_REASONS)).get(source),
    };
  });

const release = (node: Node) =>
  pass(
    "expiredIntentRelease",
    prepareExpiredIntentRelease(inputs(node)),
    HISTORY_SIGNED_INTENT_RELEASE_SOURCE,
  );

const revival = (node: Node) =>
  pass(
    "replacedBlockRevival",
    prepareReplacedBlockRevival(inputs(node)),
    HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
  );

describe("a history recovery pass whose release or revival holds on its native restore", () => {
  it("skips the landed-block rebase while the release holds, and rebases once it completes", async () => {
    fixture.queue = wHoldsTheSlot;
    fixture.coverage = deep;
    const owner = ownerModel(ZERO_ROOT);
    const retain = refuseUntilRetained(owner);
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* seedDisplaced(Pending.Status.LocallyApplied);
        yield* withNativeReplay(X_HEADER);
        const held = yield* release(node);
        retain();
        const completed = yield* release(node);
        return { held, completed, native: owner.durableRoot };
      }),
    );
    expect(result.held).toEqual({
      returned: NATIVE_RESTORE_HELD,
      rebases: 0,
      raised: SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
    });
    expect(result.completed.returned).not.toBe(NATIVE_RESTORE_HELD);
    expect(result.completed.rebases).toBe(1);
    expect(result.completed.raised).toBeUndefined();
    expect(result.native).toBe(UTXOS_ROOT);
  });

  it("skips the landed-block rebase while the revival holds, and rebases once it completes", async () => {
    fixture.queue = wHoldsTheSlot;
    fixture.coverage = deep;
    const owner = ownerModel(PREFIX_ROOT);
    const retain = refuseUntilRetained(owner);
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
          const held = yield* revival(node);
          retain();
          const completed = yield* revival(node);
          return { held, completed, native: owner.durableRoot };
        }),
      PREFIX_ROOT,
    );
    expect(result.held).toEqual({
      returned: NATIVE_RESTORE_HELD,
      rebases: 0,
      raised: SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
    });
    expect(result.completed.returned).not.toBe(NATIVE_RESTORE_HELD);
    expect(result.completed.rebases).toBe(1);
    expect(result.completed.raised).toBeUndefined();
    expect(result.native).toBe(UTXOS_ROOT);
  });
});
