/**
 * Readiness catalogue rows of block commitment: the commit worker, its build
 * alarms and plans, and the validation-trace defender
 * (`readiness-catalogue.ts`).
 */
import { DA_PAYLOAD_FRAME_PRESSURE_STAGES } from "@al-ft/midgard-core/da-payload-sizing";

import { COMMIT_ANCHOR_UNAVAILABLE } from "../fibers/block-commitment.commit-anchor-readiness.js";
import { L1_OWN_BLOCK_EVENT_ORPHANED } from "../fibers/block-commitment.own-block-event-orphaned.js";
import {
  COMMIT_BASE_PENDING,
  COMMIT_SCHEDULER_ALIGNMENT_FAILED,
  COMMIT_WINDOW_PENDING,
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_VERIFIER_UNAVAILABLE,
  COMMIT_WORKER_WITHDRAWAL_UNCLASSIFIABLE,
} from "../fibers/block-commitment.worker-readiness.js";
import { VALIDATION_TRACE_DEFENSE_STATES } from "../validation-trace-defense/decide.js";
import type {
  CommitPlanningNotice,
  commitPlanningReadiness,
} from "../workers/commit-planning-notice.js";
import {
  COMMIT_BUILD_HELD,
  COMMIT_STAGE_BREAKER,
} from "./globals.commit-build-alarms.js";
import { COMMIT_DA_FRAME_EVENTS_OVERFLOW } from "./liveness-halt.js";
import {
  both,
  COUNT,
  detail,
  field,
  format,
  hex,
  members,
  oneOf,
  optional,
  otherwise,
  type ReadinessRow,
  reason,
} from "./readiness-catalogue.row.js";

/** The commit stage an alarm names; another stage shows as `unclassified`. */
const COMMIT_STAGE = otherwise(
  oneOf(
    "phase_a",
    "phase_b",
    "transaction_root_derivation",
    "withdrawal_classification",
    "unclassified",
  ),
  "unclassified",
);

/** Why the commit worker's plan is not current, in `commitPlanningProgress`. */
const PLANNING_REASONS = members<
  | Extract<CommitPlanningNotice, { status: "unavailable" }>["reason"]
  | NonNullable<ReturnType<typeof commitPlanningReadiness>["reason"]>
>({
  planning_started: null,
  missing_follower_permit: null,
  missing_admitted_parent: null,
  deployment_identity_unavailable: null,
  no_selected_plan: null,
  follower_recomputing: null,
  follower_authority_changed: null,
  deployment_authority_changed: null,
  clock_invalid: null,
  plan_expired: null,
});

export const COMMIT_READINESS_ROWS: readonly ReadinessRow[] = [
  reason(COMMIT_BASE_PENDING),
  reason(COMMIT_WINDOW_PENDING),
  reason(COMMIT_SCHEDULER_ALIGNMENT_FAILED),
  reason(COMMIT_WORKER_FAILED),
  reason(COMMIT_WORKER_VERIFIER_UNAVAILABLE),
  reason(COMMIT_WORKER_WITHDRAWAL_UNCLASSIFIABLE),
  reason(COMMIT_ANCHOR_UNAVAILABLE),
  reason(COMMIT_DA_FRAME_EVENTS_OVERFLOW),
  reason(L1_OWN_BLOCK_EVENT_ORPHANED),
  reason(COMMIT_BUILD_HELD, COMMIT_STAGE),
  detail(COMMIT_STAGE_BREAKER, COMMIT_STAGE),
  detail(
    "commit_da_frame_pressure",
    oneOf("candidate", "initial_candidate", "required_work"),
    // The stages reached; the producer leaves out 0 (`readiness.ts`).
    oneOf(...DA_PAYLOAD_FRAME_PRESSURE_STAGES.map(String)),
  ),
  ...PLANNING_REASONS.map((code) => field(code)),
  // `<state>:<subject unit>:<deadline ms|none>[:<shortfall lovelace>]`: a
  // state fails readiness or is a detail by its urgency (`health.ts`).
  ...VALIDATION_TRACE_DEFENSE_STATES.map((state) =>
    both(state, hex(56, 120), format("[0-9]+|none"), optional(COUNT)),
  ),
  reason("validation-trace-defense-stalled", COUNT),
];
