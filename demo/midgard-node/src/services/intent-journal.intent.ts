/**
 * The node's L1 intent families and the intent a submission carries (plan
 * §8.2, §8.4): its family, workflow key, plan (S5, §8.1) and content
 * reference, and what a record returns. Re-exported by `intent-journal.ts`.
 */
import type { View } from "@al-ft/midgard-l1-follower";

import type {
  INTENT_JOURNAL_NO_VIEW,
  INTENT_JOURNAL_UNAVAILABLE,
} from "./intent-journal.refusals.js";

/** The node's L1 families (§8.4, Node rows). */
export const NODE_INTENT_FAMILIES = [
  "commit",
  "scheduler_refresh",
  "merge",
  "attest",
  "correction",
  "register",
  "activate",
  "deregister",
  "exit",
  "takeover",
  "retire",
  "recover_bond",
  "reserve_payout",
  "settlement",
  "reference_publication",
  "reference_sweep",
  "reference_funding",
  "script_reward_registration",
  "phas_membership",
  "list_insert",
] as const;

export type NodeIntentFamily = (typeof NODE_INTENT_FAMILIES)[number];

/** The families whose intent names its content: the header, or the event settled. */
export const CONTENT_REF_FAMILIES: ReadonlySet<NodeIntentFamily> = new Set([
  "commit",
  "merge",
  "attest",
  "correction",
  "reserve_payout",
  "settlement",
]);

/**
 * Why a submission is not journaled: no follower runs in this phase or
 * process (protocol initialization before the follower starts, a one-shot
 * CLI command), so nothing would reconcile the intent and the submission
 * awaits its own confirmation.
 */
export type UnjournaledReason = "no_follower";

/**
 * A family's plan (S5, §8.1): the follower generation of the oldest view
 * its reads rest on, taken before its first L1 read (`openPlan`), or the
 * view of follower-derived data it plans from (`intentPlanAt`). `none` when
 * the follower had no view to open it under (no cursor, or the read failed):
 * recording under it is refused with that reason.
 */
export type IntentPlan =
  | Readonly<{ kind: "view"; generation: number }>
  | Readonly<{
      kind: "none";
      reason: typeof INTENT_JOURNAL_NO_VIEW | typeof INTENT_JOURNAL_UNAVAILABLE;
      detail: string;
    }>;

/** The plan of data the follower derived at `view` (an operator set's view). */
export const intentPlanAt = (view: View): IntentPlan => ({
  kind: "view",
  generation: view.generation,
});

export type SubmissionIntent =
  | Readonly<{
      kind: "journaled";
      family: NodeIntentFamily;
      /** The workflow the tx belongs to, e.g. `commit:tail=<outref>`. */
      workflowKey: string;
      /** The family's plan, opened before its first L1 read. */
      plan: IntentPlan;
      /** Class B content the tx commits to (an own block's header hash). */
      contentRef?: Buffer;
    }>
  | Readonly<{
      kind: "unjournaled";
      reason: UnjournaledReason;
      workflowKey: string;
    }>;

export const journaledIntent = (
  family: NodeIntentFamily,
  workflowKey: string,
  plan: IntentPlan,
  contentRef?: Buffer,
): SubmissionIntent => ({
  kind: "journaled",
  family,
  workflowKey,
  plan,
  ...(contentRef === undefined ? {} : { contentRef }),
});

export const unjournaledSubmission = (
  reason: UnjournaledReason,
  workflowKey: string,
): SubmissionIntent => ({ kind: "unjournaled", reason, workflowKey });

/** A short label for logs. */
export const intentLabel = (intent: SubmissionIntent): string =>
  intent.kind === "journaled"
    ? `${intent.family} ${intent.workflowKey}`
    : `unjournaled (${intent.reason}) ${intent.workflowKey}`;

export type RecordOutcome =
  | Readonly<{ kind: "recorded" | "already_recorded" }>
  | Readonly<{ kind: "unjournaled"; reason: UnjournaledReason }>;

/**
 * Why a family records: `send` when it sends the bytes itself right after
 * (the submit seam, the correction workflow), so the record also takes S6's
 * send decision in its transaction; `record_only` when S6 sends them
 * (settlement). `slotTime` maps an L1 slot to its POSIX ms, for the
 * node-behind hold.
 */
export type RecordPurpose =
  | Readonly<{ kind: "send"; slotTime: (slot: number) => number }>
  | Readonly<{ kind: "record_only" }>;
