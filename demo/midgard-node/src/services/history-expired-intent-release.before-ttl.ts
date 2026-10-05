import { Effect } from "effect";

import type { BaseSpend } from "./history-expired-intent-release.base-spend.js";
import type { QueueView } from "./history-expired-intent-release.signed-commit-node.js";
import {
  deferBeforeTtl,
  reportOnce,
  type SignedIntentDeferral,
} from "./history-expired-intent-release.table.js";

type BeforeTtl = Readonly<{
  expired: boolean;
  baseSpend: BaseSpend | undefined;
}>;

/** Before the TTL, why base-spend evidence (of any kind) does not agree with
 * the exact-point queue, which still holds the intent's own base output: the
 * intent can still land, so it is never replaced. Its own retained plan is
 * exempt: that plan is resumed (it binds only the replaced journal). */
export const heldBaseOutput = (
  derived: BeforeTtl & Readonly<{ retainedPlan: boolean }>,
  queue: QueueView,
  outRef: string,
) =>
  !derived.expired &&
  !derived.retainedPlan &&
  derived.baseSpend !== undefined &&
  queue.nodes.some(
    ({ node }) =>
      `${node.utxo.txHash}#${node.utxo.outputIndex.toString()}` === outRef,
  )
    ? `the canonical history shows ${derived.baseSpend.kind === "spent" ? `its base output ${outRef} spent by ${derived.baseSpend.txHash}` : `the signed commit ${derived.baseSpend.txHash} of its replaced sibling included`}, but the exact-point queue still holds its base output ${outRef}`
    : undefined;

/** Before the TTL, base-spend evidence that decides nothing leaves the gate
 * open until other evidence, the TTL or a rollback (see
 * `expiredIntentReleaseDisposition`). False when the TTL is reached or there
 * is no such evidence: the caller decides as before. */
export const declineBeforeTtl = (
  input: BeforeTtl &
    Readonly<{
      deferral: SignedIntentDeferral;
      key: string;
      reportKey: string;
      context: string;
      reason: string;
    }>,
) =>
  Effect.suspend(() => {
    if (input.expired || input.baseSpend === undefined)
      return Effect.succeed(false);
    deferBeforeTtl(input.deferral, input.key, input.baseSpend);
    return reportOnce(
      input.reportKey,
      `Not reconciling ${input.context} before its TTL: ${input.reason}. The history gate stays open and its journal stays active until other evidence, its TTL or a rollback.`,
    ).pipe(Effect.as(true));
  });
