import { Effect, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { journalAbandonment } from "../src/services/canonical-journal-recovery.js";
import { UTXOS_ROOT } from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  S_HEADER,
  seedDisplaced,
  SOURCE,
  W_HEADER,
  W_NODE_TX,
  wHoldsTheSlot,
  X_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  ledgerRoot,
  onNode,
  ownerModel,
  plans,
  release,
  statusOf,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

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

/** W's node output `depth` blocks deep at head height 10. */
const winnerAt = (depth: number) => ({
  head: 10,
  start: 1,
  txs: { [W_NODE_TX]: 10 - depth + 1 },
});

const displacedState = Effect.gen(function* () {
  yield* seedDisplaced(Pending.Status.Finalized);
  yield* withNativeReplay(X_HEADER);
});

const outcome = Effect.gen(function* () {
  return {
    w: yield* statusOf(W_HEADER),
    s: yield* statusOf(S_HEADER),
    x: yield* statusOf(X_HEADER),
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
    sibling: journalAbandonment(
      Option.getOrThrow(yield* Pending.retrieveByHeaderHash(S_HEADER, true)),
    ),
  };
});

describe("the production release over a locally finalized sibling an L1 rollback displaced", () => {
  it("holds undecided short of the confirmation depth, then re-lands the winner and clears the hold", async () => {
    fixture.queue = wHoldsTheSlot;
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* displacedState;
        fixture.coverage = winnerAt(1);
        const short = yield* release(node);
        const before = yield* outcome;
        fixture.coverage = winnerAt(6);
        const deep = yield* release(node);
        return { short, before, deep, after: yield* outcome };
      }),
    );
    // Short of the depth: held, readiness fails with the reason, nothing
    // written.
    expect(result.short.failure).toBeUndefined();
    expect(result.short.raised.get(SOURCE)).toBe("signed_intent_undecided");
    expect(result.short.reasons).toContain("signed_intent_undecided");
    expect(result.before).toMatchObject({
      w: { status: Pending.Status.Abandoned },
      s: { status: Pending.Status.Finalized },
      x: { status: Pending.Status.PendingSubmission },
      plans: [],
    });
    // Deep enough: one repair abandons X and the displaced S (revivable,
    // under its own replacement digest) and revives W; the hold clears.
    expect(result.deep.failure).toBeUndefined();
    expect(result.deep.raised.get(SOURCE)).toBeUndefined();
    expect(result.deep.reasons).not.toContain("signed_intent_undecided");
    expect(result.after).toMatchObject({
      w: { status: Pending.Status.ObservedWaitingStability },
      s: { status: Pending.Status.Abandoned },
      x: { status: Pending.Status.Abandoned },
      plans: ["applied"],
      sibling: "replacement",
    });
    // The native CAS restored X's base root (which S never moved); the SQL
    // marker then follows the revived W to its candidate root, which local
    // finalization replays natively.
    expect(owner.durableRoot).toBe(UTXOS_ROOT);
    expect(result.after.ledger).toBe(ZERO_ROOT);
    expect(owner.restores).toBe(1);
  });

  it("holds, and never revives, while the coverage that would show the depth is unavailable", async () => {
    fixture.queue = wHoldsTheSlot;
    const result = await onNode(ownerModel(ZERO_ROOT), (node) =>
      Effect.gen(function* () {
        yield* displacedState;
        fixture.coverage = "unavailable";
        const held = yield* release(node);
        return { held, after: yield* outcome };
      }),
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(SOURCE)).toBe("signed_intent_undecided");
    expect(result.after).toMatchObject({
      w: { status: Pending.Status.Abandoned },
      s: { status: Pending.Status.Finalized },
      x: { status: Pending.Status.PendingSubmission },
      plans: [],
    });
  });
});
