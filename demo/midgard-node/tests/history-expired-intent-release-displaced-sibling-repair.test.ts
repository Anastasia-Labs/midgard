import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  reviveReplacedCanonicalJournal,
  signedIntentReplacementDigest,
} from "../src/services/canonical-journal-recovery.js";
import type { QueueView } from "../src/services/history-expired-intent-release.signed-commit-node.js";
import { reincludeStateQueueCorrectedBlocks } from "../src/services/state-queue-correction-recovery.js";
import {
  BASE_HEADER,
  BASE_OUT,
  hex,
  run,
  seed,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  decideInTurn,
  evidence,
  failureMessage,
  journal,
  queueNode,
  readStatus,
  root,
  S_HEADER,
  seedDisplaced,
  W_COMMIT,
  W_HEADER,
  X_COMMIT,
  X_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";

/**
 * The release of an expired signed intent X whose replaced sibling W holds
 * its base's slot, when X itself, or a displaced sibling S, was already
 * locally finalized: the decision, and the guards its production repair
 * relies on (history-expired-intent-release-preparation-displaced.test.ts
 * drives that repair end to end).
 */

beforeEach(async () => {
  await run(seed);
});

describe("an active block already locally finalized when its replaced sibling holds the slot", () => {
  it("revives the winner: the repair reverses the active block's local finalization", async () => {
    await run(
      journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
        abandonment: "replacement",
      }),
    );
    await run(
      journal(
        X_HEADER,
        Pending.Status.SubmittedUnconfirmed,
        X_COMMIT,
        4_000_000,
      ),
    );
    const [round] = await decideInTurn([evidence()]);
    expect(round).toEqual({ kind: "revive", displaced: [], raised: undefined });
  });

  it("is landed, not revived, when its own node holds the slot", async () => {
    await run(
      journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
        abandonment: "replacement",
      }),
    );
    await run(
      journal(
        X_HEADER,
        Pending.Status.SubmittedUnconfirmed,
        X_COMMIT,
        4_000_000,
      ),
    );
    const queue = {
      root,
      nodes: [
        root,
        queueNode(
          BASE_HEADER.toString("hex"),
          root.headerHash,
          X_HEADER.toString("hex"),
          BASE_OUT,
        ),
        queueNode(
          X_HEADER.toString("hex"),
          BASE_HEADER.toString("hex"),
          undefined,
          `${hex("displaced:x-node-tx")}#0`,
        ),
      ],
    } as unknown as QueueView;
    const [round] = await decideInTurn([evidence({ queue })]);
    expect(round?.kind).toBe("landed");
  });
});

describe("the release's reinclusion and revival over a displaced sibling", () => {
  it("never revives the winner while the displaced sibling is still locally finalized", async () => {
    // The production release abandons the displaced sibling in the revival's
    // own transaction first (see
    // history-expired-intent-release-preparation-displaced.test.ts); the
    // revival itself still refuses a locally finalized sibling.
    await run(seedDisplaced(Pending.Status.Finalized));
    const exit = await run(
      Effect.exit(reviveReplacedCanonicalJournal(W_HEADER)),
    );
    expect(failureMessage(exit)).toContain(
      `block ${S_HEADER.toString("hex")} built on the same base is already ${Pending.Status.Finalized}`,
    );
    expect(await readStatus(S_HEADER)).toBe(Pending.Status.Finalized);
    expect(await readStatus(W_HEADER)).toBe(Pending.Status.Abandoned);
  });

  it("never reopens a block as displaced unless it was locally finalized", async () => {
    await run(seedDisplaced(Pending.Status.Finalized));
    const record = Option.getOrThrow(
      await run(Pending.retrieveByHeaderHash(X_HEADER)),
    );
    const exit = await run(
      Effect.exit(
        reincludeStateQueueCorrectedBlocks([
          {
            headerHash: X_HEADER.toString("hex"),
            transitionDigest: signedIntentReplacementDigest(record)!,
            kind: "displaced",
          },
        ]),
      ),
    );
    expect(failureMessage(exit)).toContain(
      `Cannot reopen a displaced block from journal status ${Pending.Status.PendingSubmission}`,
    );
    expect(await readStatus(X_HEADER)).toBe(Pending.Status.PendingSubmission);
  });
});
