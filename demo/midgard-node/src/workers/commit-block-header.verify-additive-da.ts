import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { processMpfs } from "../mpf/index.js";
import type { Lucid } from "../services/index.js";
import { assertExactCommitRescueDaFrame } from "./commit-block-header.assert-rescue-frame.js";
import type { NotifyCommitWorkerParent } from "./commit-block-header.commit-explicit-block-header-program.js";
import { CommitWorkerInvariantError } from "./commit-block-header.pending-user-event-counts-up-to.js";
import { measureBuiltCommitDaPrefixes } from "./commit-block-header/commit-da-prefix-search.js";
import type { AdditiveCommitFramePlan } from "./utils/commit-block-planner.additive-frame.js";
import { commitDaFrameNoticeForOutcome } from "./utils/commit-block-planner.commit-da-frame-notice.js";

/** Checks the one built candidate against its additive budget. Exact rescues
 * additionally construct the actual header and run its hard frame/count guard. */
export const verifyAdditiveCommitDaFrame = (input: {
  readonly processed: Effect.Effect.Success<ReturnType<typeof processMpfs>>;
  readonly baseRoot: string;
  readonly profile: MidgardConsensusProfile;
  readonly nodeConfig: {
    readonly NETWORK: string;
    readonly MIN_FEE_A: bigint;
    readonly MIN_FEE_B: bigint;
  };
  readonly additivePlan: Extract<AdditiveCommitFramePlan, { kind: "selected" }>;
  readonly latestBlockForSchedulerPlanning: SDK.StateQueueUTxO | undefined;
  readonly lucid: Lucid;
  readonly fixedHistoryEndTime: Date;
  readonly blockTimeMs: number;
  readonly proofValidationSlotConfig: L1SlotConfig;
  readonly daFrameInnerLimit: number;
  readonly notifyParent: NotifyCommitWorkerParent | undefined;
}) =>
  Effect.gen(function* () {
    const {
      processed,
      baseRoot,
      profile,
      nodeConfig,
      additivePlan,
      latestBlockForSchedulerPlanning,
      lucid,
      fixedHistoryEndTime,
      blockTimeMs,
      proofValidationSlotConfig,
      daFrameInnerLimit,
      notifyParent,
    } = input;
    const measurement = yield* measureBuiltCommitDaPrefixes(
      processed,
      baseRoot,
      profile,
      nodeConfig,
    );
    const actualUpperBound = measurement?.innerBytesUpperBound;
    const hasCompleteBound =
      actualUpperBound !== undefined &&
      BigInt(actualUpperBound) <= additivePlan.upperBoundBytes;
    if (
      !hasCompleteBound ||
      (additivePlan.rescue === undefined &&
        actualUpperBound! > daFrameInnerLimit)
    ) {
      const notice = commitDaFrameNoticeForOutcome({
        outcome: "incomplete",
        passes: 1,
        measurement,
        maxInnerBytes: daFrameInnerLimit,
      });
      if (notice !== undefined && notifyParent !== undefined)
        yield* notifyParent(notice);
      yield* Effect.logError(
        "Conservative additive DA bound failed its material check",
      ).pipe(
        Effect.annotateLogs({
          alarm: "da_additive_bound_defect",
          planned_bytes: additivePlan.upperBoundBytes.toString(),
          measured_bytes: actualUpperBound ?? "missing",
        }),
      );
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "da_additive_bound_defect: selected source material exceeds its conservative frame budget",
        }),
      );
    }
    if (additivePlan.rescue !== undefined) {
      // A rescue cannot proceed merely because its conservative bound is large:
      // reconstruct the ACTUAL header and unconditionally check real inner,
      // envelope and request bytes plus protocol counts before proceeding.
      // The live-tail submission path repeats the hard guard before signing.
      const rescueLucid = lucid;
      const exactBytes = yield* assertExactCommitRescueDaFrame({
        processed,
        parent: latestBlockForSchedulerPlanning,
        lucid: rescueLucid,
        profile: profile,
        endTimeMs: fixedHistoryEndTime.getTime(),
        blockTimeMs,
        slotConfig: proofValidationSlotConfig,
        expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: nodeConfig.MIN_FEE_A,
        minFeeB: nodeConfig.MIN_FEE_B,
      }).pipe(
        Effect.tapError((error) =>
          Effect.logError(String(error)).pipe(
            Effect.annotateLogs({
              alarm:
                additivePlan.rescue === "whole_instant"
                  ? "catch_up_whole_instant_frame_hold"
                  : "ordinary_single_frame_hold",
            }),
          ),
        ),
      );
      yield* Effect.logInfo(
        "One exact DA rescue passed its actual-header hard frame guard",
      ).pipe(
        Effect.annotateLogs({
          rescue: additivePlan.rescue,
          conservative_bytes: additivePlan.upperBoundBytes.toString(),
          exact_bytes: exactBytes,
        }),
      );
    }
    const notice = commitDaFrameNoticeForOutcome({
      outcome: "fits",
      passes: 1,
      measurement,
      maxInnerBytes: daFrameInnerLimit,
    });
    if (notice !== undefined && notifyParent !== undefined)
      yield* notifyParent(notice);
  });
