/**
 * Canonical V1 retention window (GOAL_SPEC 9.4 / Q54).
 *
 * Retained DA and proof evidence must survive the full challenge surface of a
 * block: block maturity plus the worst-case correction (proof) time, plus an
 * operational margin. Every number in this module is derived from the frozen
 * consensus and DA transport profiles - none of them may be re-stated as a
 * literal, so a profile change propagates instead of silently drifting.
 *
 * Authoritative economics: docs/midgard/decisions/0002-canonical-v1-goal-
 * economics-and-margins.md (maturity 604_800_000 ms; worst-case proof-time
 * bound = half maturity = 302_400_000 ms per 3.3 clause 3; RETENTION_DAYS 15).
 *
 * Enforcement is always against the half-maturity BOUND. The measured dispute
 * schedule (`measuredValidationDisputeScheduleMs`, ~11h) is recorded here for
 * observability only and must never be used as the retention floor.
 */

import "./consensus-profile.js";
import "./da-transport.js";
import "./generated-deployment-profiles.js";
import "./retention-window.assert-da-challenge-window-within-maturity.js";
import "./retention-window.da-retention-prune-decision.js";
export {
  assertDaChallengeWindowWithinMaturity,
  assertRetentionDaysCoverWindow,
  assertRetentionWindowCoversDeployment,
  assertWorstCaseProofTimeWithinBound,
  MIDGARD_MIN_RETENTION_DAYS,
  MIDGARD_RETENTION_WINDOW,
  requireRetentionDays,
  RETENTION_KNOWN_HEADER_STATUSES,
  RETENTION_MS_PER_DAY,
  retentionDaysCoverWindow,
  type RetentionDeadline,
  type RetentionHeaderStatus,
  type RetentionWindow,
} from "./retention-window.assert-da-challenge-window-within-maturity.js";
export {
  daRetentionPruneDecision,
  MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS,
  requireRetentionAlertThresholdMs,
  resolveL1ViewFatalMs,
  type RetentionDeadlineAlert,
  retentionDeadlineAlert,
  retentionDeadlineForBlock,
  type RetentionPruneDecision,
  type RetentionPruneInput,
  type RetentionPruneReasonCode,
  type RetentionQueueReference,
} from "./retention-window.da-retention-prune-decision.js";
