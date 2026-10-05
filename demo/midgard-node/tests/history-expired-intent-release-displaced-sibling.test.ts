import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { signedIntentReplacementDigest } from "../src/services/canonical-journal-recovery.js";
import { Globals } from "../src/services/globals.js";
import { decide } from "../src/services/history-expired-intent-release.decide.js";
import type { QueueView } from "../src/services/history-expired-intent-release.signed-commit-node.js";
import {
  activeSignedIntent,
  makeSignedIntentDeferral,
} from "../src/services/history-expired-intent-release.table.js";
import {
  raiseLivenessIncident,
  SIGNED_INTENT_REPLACEMENT_INTEGRITY,
} from "../src/services/liveness-halt.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";
import { reincludeStateQueueCorrectedBlocks } from "../src/services/state-queue-correction-recovery.js";
import {
  bytes,
  change,
  dispose,
  hex,
  insertJournal,
  run,
  seed,
  signedCommit,
  TTL,
  UTXOS_ROOT,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  atDepth,
  decideInTurn,
  decideOnce,
  depthOf,
  evidence,
  failureMessage,
  integrityFailure,
  journal,
  queueNode,
  readStatus,
  root,
  S_COMMIT,
  S_HEADER,
  seedDisplaced,
  SOURCE,
  W_COMMIT,
  W_HEADER,
  W_NODE_TX,
  wHoldsTheSlot,
  X_COMMIT,
  X_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";

/**
 * A locally finalized sibling that an L1 rollback displaced. Base D; this
 * node's block W was signed and journaled, then replaced by S, which landed
 * and finalized locally. A rollback removed S, and W's own signed commit then
 * took D's slot. S changed no ledger state (its expected root is its base
 * root), so after the rollback the node could journal a new block X on D;
 * X's signed commit then expired. The release of X reads W holding D's slot
 * with S still Finalized beside it.
 */

beforeEach(async () => {
  await run(seed);
});

describe("reproduction: a Finalized sibling displaced by a rollback holds the release", () => {
  it("decides undecided on every evaluation and the incident never clears", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const rounds = await decideInTurn([evidence(), evidence(), evidence()]);
    for (const round of rounds) {
      expect(round.kind).toBe("wait");
      expect(round.reason).toContain(
        `${SIGNED_INTENT_UNDECIDED}: this node's replaced block ${W_HEADER.toString("hex")} holds its base's slot, but block ${S_HEADER.toString("hex")} built on the same base is already ${Pending.Status.Finalized}`,
      );
      expect(round.raised).toBe(SIGNED_INTENT_UNDECIDED);
    }
    expect(await readStatus(S_HEADER)).toBe(Pending.Status.Finalized);
    expect(await readStatus(X_HEADER)).toBe(Pending.Status.PendingSubmission);
  });

  it("keeps the history gate closed: the expired intent stays pending at every later point", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const deferral = makeSignedIntentDeferral();
    for (const height of [200, 201, 202]) {
      const disposition = await run(
        dispose(deferral, change("forward", height)),
      );
      expect(disposition?.status).toBe("pending");
    }
  });

  it("refuses to reopen the Finalized sibling as an unlanded block", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const record = Option.getOrThrow(
      await run(Pending.retrieveByHeaderHash(S_HEADER)),
    );
    const exit = await run(
      Effect.exit(
        reincludeStateQueueCorrectedBlocks([
          {
            headerHash: S_HEADER.toString("hex"),
            transitionDigest: signedIntentReplacementDigest(record)!,
            kind: "unlanded",
          },
        ]),
      ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    expect(failureMessage(exit)).toContain(
      `Cannot reopen a unlanded block from journal status ${Pending.Status.Finalized}`,
    );
    expect(await readStatus(S_HEADER)).toBe(Pending.Status.Finalized);
  });
});

describe("reproduction (b): the sibling at ObservedWaitingStability", () => {
  it("cannot arise: the database refuses a second active journal beside an observed one", async () => {
    await run(
      journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
        abandonment: "replacement",
      }),
    );
    await run(
      journal(
        S_HEADER,
        Pending.Status.ObservedWaitingStability,
        S_COMMIT,
        3_000_000,
        { empty: true },
      ),
    );
    const exit = await run(
      Effect.exit(
        journal(
          X_HEADER,
          Pending.Status.PendingSubmission,
          X_COMMIT,
          4_000_000,
        ),
      ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    expect(
      JSON.stringify(
        Cause.squash((exit as Exit.Failure<unknown, unknown>).cause),
      ),
    ).toContain("uniq_pending_block_finalizations_single_active");
    // The observed sibling alone is no signed intent to release.
    expect(await run(activeSignedIntent)).toBeUndefined();
    const disposition = await run(
      dispose(makeSignedIntentDeferral(), change("forward", 200)),
    );
    expect(disposition).toBeUndefined();
  });
});

describe("a Finalized sibling displaced by a rollback, with canonical depth evidence", () => {
  it("waits while the winner's commit is short of the confirmation depth, then revives it and names the sibling displaced", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const rounds = await decideInTurn([atDepth(1n), atDepth(2n), atDepth(3n)]);
    for (const [index, round] of rounds.slice(0, 2).entries()) {
      expect(round.kind).toBe("wait");
      expect(round.reason).toContain(
        `the commit holding the slot is ${(index + 1).toString()} blocks deep, short of the confirmation depth 3`,
      );
      expect(round.raised).toBe(SIGNED_INTENT_UNDECIDED);
    }
    expect(rounds[2]).toEqual({
      kind: "revive",
      displaced: [S_HEADER.toString("hex")],
      raised: undefined,
    });
    // Deciding writes nothing.
    expect(await readStatus(S_HEADER)).toBe(Pending.Status.Finalized);
  });

  it("reads a winner's node output older than the retained chain as deeper than all of it", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const [deep, shallow] = await decideInTurn([
      evidence({ canonicalDepth: depthOf({}, 5n) }),
      evidence({ canonicalDepth: depthOf({}, 1n) }),
    ]);
    expect(deep?.kind).toBe("revive");
    expect(shallow?.kind).toBe("wait");
    expect(shallow?.reason).toContain("2 blocks deep");
  });

  it("names a locally finalized descendant of the displaced sibling displaced after it", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const child = bytes("displaced:t-header", 28);
    await run(
      insertJournal({
        header: child,
        status: Pending.Status.Finalized,
        commit: signedCommit(`${hex("displaced:s-node-tx")}#0`, TTL + 3),
        baseOut: `${hex("displaced:s-node-tx")}#0`,
        baseHeader: S_HEADER,
        createdAt: new Date(3_500_000),
      }).pipe(
        Effect.zipRight(
          Effect.flatMap(
            SqlClient.SqlClient,
            (sql) => sql`UPDATE pending_block_finalizations
              SET block_end_time = block_start_time + INTERVAL '1 second',
                expected_utxos_root = base_utxos_root
              WHERE header_hash = ${child}`,
          ),
        ),
      ),
    );
    const [round] = await decideInTurn([atDepth(3n)]);
    expect(round?.kind).toBe("revive");
    expect(round?.displaced).toEqual([
      S_HEADER.toString("hex"),
      child.toString("hex"),
    ]);
  });

  it("is the integrity failure when a descendant keeps a ledger root other than the base's", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const child = bytes("displaced:t-header", 28);
    const other = "22".repeat(32);
    await run(
      insertJournal({
        header: child,
        status: Pending.Status.Finalized,
        commit: signedCommit(`${hex("displaced:s-node-tx")}#0`, TTL + 3),
        baseOut: `${hex("displaced:s-node-tx")}#0`,
        baseHeader: S_HEADER,
        createdAt: new Date(3_500_000),
      }).pipe(
        Effect.zipRight(
          Effect.flatMap(
            SqlClient.SqlClient,
            (sql) => sql`UPDATE pending_block_finalizations
              SET block_end_time = block_start_time + INTERVAL '1 second',
                base_utxos_root = ${other}, expected_utxos_root = ${other}
              WHERE header_hash = ${child}`,
          ),
        ),
      ),
    );
    // Root-preserving itself, but not over the root S kept: no retained
    // replay connects it to its parent.
    const message = integrityFailure(await decideOnce(atDepth(3n)));
    expect(message).toContain(
      `has no contiguous retained replay from its parent's root ${UTXOS_ROOT} through ${other} to ${other}`,
    );
  });

  it("is the integrity failure, never an undecided wait, when the sibling moved the ledger root", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`UPDATE pending_block_finalizations
          SET expected_utxos_root = ${"11".repeat(32)}
          WHERE header_hash = ${S_HEADER}`,
      ),
    );
    // Short of the depth it still waits: the winner may yet roll back.
    const [short] = await decideInTurn([atDepth(1n)]);
    expect(short?.kind).toBe("wait");
    expect(short?.raised).toBe(SIGNED_INTENT_UNDECIDED);
    // At the depth the altered root has no authenticated retained replay.
    const message = integrityFailure(await decideOnce(atDepth(10n)));
    expect(message).toContain(
      `has no contiguous retained replay from its parent's root ${UTXOS_ROOT} through ${UTXOS_ROOT} to ${"11".repeat(32)}`,
    );
  });

  it("is the integrity failure when the sibling's signed commit is in the canonical history too", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const message = integrityFailure(
      await decideOnce(atDepth(3n, { [S_COMMIT.hash]: 7n })),
    );
    expect(message).toContain(
      "has its signed commit 7 blocks deep in the canonical history",
    );
  });

  it("is the integrity failure when the sibling's node is on the queue too", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const queue = {
      root,
      nodes: [
        ...wHoldsTheSlot.nodes,
        queueNode(
          S_HEADER.toString("hex"),
          W_HEADER.toString("hex"),
          undefined,
          `${hex("displaced:s-node-tx")}#0`,
        ),
      ],
    } as unknown as QueueView;
    const message = integrityFailure(
      await decideOnce(
        evidence({ queue, canonicalDepth: depthOf({ [W_NODE_TX]: 3n }) }),
      ),
    );
    expect(message).toContain("is on the queue");
  });
});

describe("a decided release and the reason raised under its source", () => {
  /** The reason left under the release source after `raised` was raised
   * there and X's release was decided (revived, at depth). */
  const afterDecision = (raised: string) =>
    run(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* raiseLivenessIncident(globals, SOURCE, raised, "earlier");
        const record = Option.getOrThrow(
          yield* Pending.retrieveByHeaderHash(X_HEADER),
        );
        const decision = yield* decide(record, atDepth(3n));
        return {
          kind: decision.kind,
          raised: (yield* Ref.get(globals.LIVENESS_REASONS)).get(SOURCE),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

  it("clears its own undecided reason, never an integrity hold raised under the same source", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    expect(await afterDecision(SIGNED_INTENT_UNDECIDED)).toEqual({
      kind: "revive",
      raised: undefined,
    });
    // Only the preparation's completion with no integrity failure clears it.
    expect(await afterDecision(SIGNED_INTENT_REPLACEMENT_INTEGRITY)).toEqual({
      kind: "revive",
      raised: SIGNED_INTENT_REPLACEMENT_INTEGRITY,
    });
  });
});
