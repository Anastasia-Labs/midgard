/**
 * A normal transaction whose block cannot fit the DA frame on its own, when
 * an empty block does, can never be committed: the commit would rebuild it
 * alone on every tick. The exact pre-submit check of that block decides it;
 * the transaction is then rejected and the block is rebuilt without it in the
 * same tick.
 */

import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Data, Effect } from "effect";

import { readDaHardeningConfig } from "../../da/hardening-config.js";
import type { DatabaseError } from "../../database/utils/common.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import { COMMIT_REJECT_CODE_DA_OVERSIZE } from "../../mpf/commit-rejection.js";
import { EMPTY_BLOCK_DA_PAYLOAD_UPPER_BOUND_BYTES } from "../utils/commit-block-planner.js";
import { DA_FRAME_REFUSAL_MESSAGE } from "./submission.assert-pre-submit-da-payload-size.js";

export type CommitEviction = {
  readonly code: string;
  readonly detail: string;
};

/** The exact pre-submit check refused a block of one normal transaction and
 * no other event, and an empty block fits. */
export class CommitDaFrameRefusedNormalTxError extends Data.TaggedError(
  "CommitDaFrameRefusedNormalTxError",
)<{
  readonly txIdHex: string;
  readonly detail: string;
}> {}

/**
 * Turns the exact pre-submit refusal of a block holding one normal
 * transaction and no deposit, withdrawal or forced transaction into a
 * {@link CommitDaFrameRefusedNormalTxError} naming that transaction, when an
 * empty block fits the frame. Any other failure is returned unchanged.
 */
export const attributeDaFrameRefusal = (
  error: DatabaseError,
  block: {
    readonly processedMempoolTxs: readonly {
      readonly [TxColumns.TX_ID]: Uint8Array;
    }[];
    readonly includedDepositEntries: readonly unknown[];
    readonly includedForcedTransactionEntries: readonly unknown[];
    readonly includedWithdrawalEntries: readonly unknown[];
  },
  effectiveInnerLimit: number = maxDaPayloadInnerBytes(
    readDaHardeningConfig().envelopeMode,
  ),
): Effect.Effect<never, DatabaseError | CommitDaFrameRefusedNormalTxError> => {
  const lone = block.processedMempoolTxs[0];
  if (
    error.message !== DA_FRAME_REFUSAL_MESSAGE ||
    lone === undefined ||
    block.processedMempoolTxs.length !== 1 ||
    block.includedDepositEntries.length > 0 ||
    block.includedForcedTransactionEntries.length > 0 ||
    block.includedWithdrawalEntries.length > 0 ||
    EMPTY_BLOCK_DA_PAYLOAD_UPPER_BOUND_BYTES > effectiveInnerLimit
  )
    return Effect.fail(error);
  return Effect.fail(
    new CommitDaFrameRefusedNormalTxError({
      txIdHex: Buffer.from(lone[TxColumns.TX_ID]).toString("hex"),
      detail: `The block holding only this transaction exceeds the DA frame: ${String(
        error.cause,
      )}`,
    }),
  );
};

/**
 * Runs a commit build; while it fails on a lone normal transaction the DA
 * frame refuses, adds that transaction to the evictions the next build
 * rejects before validation, discards the failed build's state, and builds
 * again. Each pass evicts a different transaction, so this ends; a
 * transaction refused again after its eviction fails the build.
 */
export const buildEvictingDaFrameRefusedNormalTx = <A, E, R, E2, R2>({
  build,
  discardFailedBuild,
}: {
  readonly build: (
    evictions: ReadonlyMap<string, CommitEviction>,
  ) => Effect.Effect<A, E, R>;
  readonly discardFailedBuild: Effect.Effect<void, E2, R2>;
}): Effect.Effect<A, E | E2, R | R2> =>
  Effect.gen(function* () {
    const evictions = new Map<string, CommitEviction>();
    for (;;) {
      const result = yield* Effect.either(build(evictions));
      if (result._tag === "Right") return result.right;
      const error = result.left;
      if (
        !(error instanceof CommitDaFrameRefusedNormalTxError) ||
        evictions.has(error.txIdHex)
      )
        return yield* Effect.fail(error);
      evictions.set(error.txIdHex, {
        code: COMMIT_REJECT_CODE_DA_OVERSIZE,
        detail: error.detail,
      });
      yield* Effect.logWarning(
        `🔹 Rejecting tx ${error.txIdHex} whose block cannot fit the DA frame; rebuilding without it. ${error.detail}`,
      );
      yield* discardFailedBuild;
    }
  });
