/**
 * The attestation-timeout correction's recovery read (plan §8.2–§8.4): what
 * the node knows of a retained correction attempt, derived from the intent
 * journal's status of that transaction (`deriveIntentStatusIn`) and the
 * follower's facts, in one snapshot of the node database. It reads no L1 and
 * sends nothing: S6 (`l1-follower.intents.ts`) is the only resubmitter, and
 * it resends a live journaled correction and never a dead one.
 */
import {
  type TimeoutCorrectionAttemptObservation,
  type TimeoutCorrectionRecovery,
} from "@al-ft/midgard-fault-proofs";
import {
  deriveIntentStatusIn,
  type IntentStatusRead,
  postgresDialect,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import {
  type DepthParameters,
  isFinal,
  levelAtDepth,
} from "@al-ft/midgard-l1-follower/heads";
import { type SqlClient } from "@effect/sql";
import { type Effect } from "effect";

import { inFollowerSnapshot } from "../database/follower-schema.js";
import {
  type L1BlockBelowCoveredTip,
  l1BlockBelowCoveredTip,
} from "../l1-heads.js";

const pointOf = (point: Readonly<{ slot: number; hash: Buffer }>) => ({
  slot: point.slot,
  hash: point.hash.toString("hex"),
});

/**
 * The observation the journal's status and the release-final block imply.
 * `releaseFinal` is the block at depth k + 1 under the status read's cursor
 * (`l1BlockBelowCoveredTip(k)`): a dead reason at or below its slot is
 * beyond every legal rollback.
 *
 * | status            | observation                                   |
 * | ----------------- | --------------------------------------------- |
 * | `landed`          | `included`, final once depth > k               |
 * | `failed_landed`   | `invalidated` (collateral spent), final likewise |
 * | `conflicted`      | `conflict`, final once the spend is            |
 * | `expired`         | `expired`, final once a final block reaches it |
 * | `dependency_dead` | `invalidated`, never final here                |
 * | `abandoned`       | `abandoned`, never final: it can still land    |
 * | `live`            | `pending`, with its input liveness             |
 * | not journaled     | `abandoned`, not final (see below)             |
 *
 * A dependency's death has no single slot the status names, so it is held
 * short of final: the attempt is abandoned, and a replacement shares an
 * input with it. A correction that is not journaled was recorded before the
 * journal existed, or was pruned k blocks after it became terminal: nothing
 * resends it, and abandoning it is the safe reading either way, since the
 * queue shows a landed one's effect and a replacement excludes a live one.
 */
export const timeoutCorrectionAttemptObservation = (
  read: IntentStatusRead,
  releaseFinal: L1BlockBelowCoveredTip,
  parameters: DepthParameters,
): TimeoutCorrectionAttemptObservation => {
  if (read.cursor === null)
    return {
      status: "unknown",
      final: false,
      canonicalPoint: null,
      releaseFinalPoint: null,
      reason: "the follower store has no covered tip yet",
    };
  const base = {
    canonicalPoint: pointOf(read.cursor.point),
    releaseFinalPoint:
      releaseFinal.kind === "block" ? pointOf(releaseFinal.point) : null,
  };
  const finalAt = (slot: number): boolean =>
    releaseFinal.kind === "block" && slot <= releaseFinal.point.slot;
  if (read.state === null)
    return {
      ...base,
      status: "abandoned",
      final: false,
      reason: "not journaled: recorded before the journal, or pruned",
    };
  const status = read.state.status;
  switch (status.kind) {
    case "landed":
      return {
        ...base,
        status: "included",
        final: isFinal(status.depth, parameters),
        inclusion: {
          slot: status.slot,
          height: status.height,
          depth: status.depth,
          level: levelAtDepth(status.depth, parameters) ?? "landed",
        },
        reason: `landed at depth ${status.depth.toString()}`,
      };
    case "failed_landed":
      return {
        ...base,
        status: "invalidated",
        final: isFinal(status.depth, parameters),
        reason: `failed phase 2 at depth ${status.depth.toString()}`,
      };
    case "conflicted":
      return {
        ...base,
        status: "conflict",
        final: finalAt(status.slot),
        reason: `${status.ownSpender ? "own" : "foreign"} tx ${status.spender.toString("hex")} spent ${status.outRef.txHash.toString("hex")}#${status.outRef.index.toString()} at slot ${status.slot.toString()}`,
      };
    case "expired":
      return {
        ...base,
        status: "expired",
        final: finalAt(status.validToSlot),
        reason: `expired at slot ${status.validToSlot.toString()}`,
      };
    case "dependency_dead":
      return {
        ...base,
        status: "invalidated",
        final: false,
        reason: `dependency ${status.dependency.toString("hex")} is dead`,
      };
    case "abandoned":
      return {
        ...base,
        status: "abandoned",
        final: false,
        reason: "abandoned by the intent reconciler; it can still land",
      };
    case "live":
      return {
        ...base,
        status: "pending",
        final: false,
        inputsAvailable: status.inputsAvailable,
        reason: status.inputsAvailable
          ? "live, every input unspent"
          : "live, an input is not yet a live fact",
      };
  }
};

/** The block at `height` in the snapshot `tx` reads. */
const blockAtHeightIn = async (tx: SqlTx, height: number) => {
  const row = (
    await tx.query(
      "SELECT slot::text AS slot, hash, height::text AS height FROM l1_blocks WHERE height = ?",
      [height],
    )
  )[0];
  return row === undefined
    ? null
    : {
        slot: Number(row.slot),
        hash: row.hash as Buffer,
        height: Number(row.height),
      };
};

/** One attempt's observation, read in one follower snapshot. */
export const observeTimeoutCorrectionAttempt = (
  txHash: string,
  parameters: DepthParameters,
): Effect.Effect<
  TimeoutCorrectionAttemptObservation,
  unknown,
  SqlClient.SqlClient
> =>
  inFollowerSnapshot(async (tx) => {
    const read = await deriveIntentStatusIn(
      tx,
      postgresDialect,
      Buffer.from(txHash, "hex"),
    );
    const releaseFinal = await l1BlockBelowCoveredTip(
      {
        cursor: () => Promise.resolve(read.cursor),
        blockAtHeight: (height) => blockAtHeightIn(tx, height),
      },
      parameters.securityParameter,
    );
    return timeoutCorrectionAttemptObservation(read, releaseFinal, parameters);
  });

/**
 * The recovery the node passes to the timeout-correction workflow: reads
 * only, through `run` (the node runtime's SQL client).
 */
export const intentJournalTimeoutCorrectionRecovery = (
  run: <A>(
    effect: Effect.Effect<A, unknown, SqlClient.SqlClient>,
  ) => Promise<A>,
  parameters: DepthParameters,
): TimeoutCorrectionRecovery => ({
  observeAttempt: ({ transactionHash }) =>
    run(observeTimeoutCorrectionAttempt(transactionHash, parameters)),
});
