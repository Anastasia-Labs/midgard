import * as SDK from "@al-ft/midgard-sdk/da";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type CommitteeL1Clock,
  committeeL1NowMs,
} from "../l1/follower/l1-clock.js";
import {
  availabilityResponderL1BuildTime,
  availabilityResponderPublicationDeadlineMs,
  availabilityResponderTransactionOperation,
} from "./factory.discover-availability-responder-challenges.js";
import type { AvailabilityResponderAction } from "./responder.js";

/**
 * The plain (non-promise) responder's run of one action through the SDK.
 * As in promise mode, the runner is handed the committee's own read scope:
 * build reads end at the publication deadline on the L1 clock, and the
 * runner's own scope (deadline on its wall clock) is never made. The runner
 * leaves a scope it was handed open; it is closed here.
 */
export const runPlainAvailabilityResponderOperation = async (
  context: SDK.DaAvailabilityOperationContext,
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
  l1Clock: CommitteeL1Clock,
): Promise<SDK.DaAvailabilityOperationResult> => {
  const scope = SDK.createDaAvailabilityReadScope({
    deadlineEpochMs: availabilityResponderPublicationDeadlineMs(action),
    // The runner's own attempt cap for the scope it would otherwise make.
    attemptTimeoutMs: context.leaseDurationMs ?? 300_000,
    nowMs: () => committeeL1NowMs(l1Clock),
    monotonicMs: context.monotonicMs,
  });
  try {
    return await SDK.runDaAvailabilityOperation(context, {
      ...availabilityResponderTransactionOperation(
        lucid,
        deployment,
        action,
        () => availabilityResponderL1BuildTime(l1Clock),
      ),
      preparationScope: scope,
    });
  } finally {
    scope.close();
  }
};
