/**
 * Readiness catalogue rows of the L1 follower, its change driver and the
 * holds and details it reports (`readiness-catalogue.ts`).
 */
import {
  type FollowReadinessReason,
  type LinkedQueueUnhealthyReason,
  WALLET_SEED_PENDING,
} from "@al-ft/midgard-l1-follower";

import { FORCED_ADMISSION_DIVERGENCE } from "../forced-orders/admission-divergence.js";
import {
  FORCED_ORDER_CARRIAGE_PENDING,
  FORCED_ORDER_INGESTION_FAILED,
} from "../forced-orders/ingest.js";
import { L1_OWN_BLOCK_FORCED_ORDER_ORPHANED } from "../forced-orders/own-landed-orphans.js";
import {
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
  EVENTS_HOOK_FAILED,
  EVENTS_INGESTION_FAILED,
  EVENTS_INGESTION_WAITING,
  EVENTS_ORPHAN_RECOVERY,
} from "../l1-events/driver.js";
import { OPERATOR_SET_UNHEALTHY } from "../l1-operator-set/hook.js";
import { OPERATOR_REMOVED } from "../l1-operator-set/membership.js";
import { STATE_QUEUE_UNHEALTHY } from "../l1-state-queue/hook.js";
import {
  CONFIRMED_LEDGER_BASE_MISMATCH,
  CONFIRMED_LEDGER_BEHIND,
  CONFIRMED_LEDGER_OWN_BLOCK_PENDING,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_ORIGIN_AFTER_FRONTIER,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_OWN_REVIVAL_PENDING,
  LANDED_BLOCK_REBASE_FAILED,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCK_REPLAY_HELD,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCK_REPLAY_STUCK,
  LANDED_BLOCK_VERIFIER_UNAVAILABLE,
  LANDED_BLOCKS_WAITING,
} from "../landed-blocks/holds.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FOLLOWER_VIEW_UNAPPLIED,
} from "./follower-write-gate.local.js";
import {
  INTENT_BYTES_MISMATCH,
  INTENT_CONTENT_REF_MISSING,
  INTENT_GATE_UNJOURNALED,
  INTENT_INPUT_UNTRACKED,
  INTENT_JOURNAL_NO_VIEW,
  INTENT_JOURNAL_UNAVAILABLE,
  INTENT_UNBOUNDED_VALIDITY,
  INTENT_UNDECODABLE,
} from "./intent-journal.refusals.js";
import { INTENT_COMMIT_ANCHOR_NOT_DEEP } from "./l1-follower.intent-predicates.js";
import {
  INTENT_RECONCILE_FAILED,
  INTENT_RECONCILE_TRANSIENT,
  INTENT_RESUBMIT_REJECTED,
} from "./l1-follower.intents.js";
import {
  L1_FOLLOWER_UNCONFIGURED,
  L1_NODE_CONFIG_FAILED,
  L1_NODE_CONFIG_UNREADABLE,
} from "./l1-follower.readiness.js";
import {
  DRIVER_RECOMPUTE_FAILED,
  STARTUP_PREPARATION_FAILED,
} from "./l1-follower.recompute.js";
import {
  COUNT,
  detail,
  field,
  members,
  type ReadinessRow,
  reason,
  TEXT,
} from "./readiness-catalogue.row.js";

/** Every reason the follow loop reports (`FollowStatus.readiness`). */
const FOLLOW_REASONS = members<FollowReadinessReason>({
  rollback_beyond_k: null,
  intersection_outside_history: null,
  origin_after_protocol_init: null,
  origin_not_on_chain: null,
  origin_mismatch: null,
  store_integrity: null,
  l1_follower_catching_up: null,
  l1_follower_waiting: null,
  l1_follower_apply_stuck: null,
  l1_follower_migration_failed: null,
  l1_follower_transient_exhausted: null,
  l1_node_unavailable: null,
  l1_node_behind: null,
  tracked_set_changed: null,
  l1_follower_prune_failing: null,
});

/** Why the state queue's live outputs are not one list, in its report. */
const LINKED_QUEUE_UNHEALTHY = members<LinkedQueueUnhealthyReason>({
  no_root: null,
  multiple_roots: null,
  invalid_output: null,
  broken_link: null,
  duplicate_key: null,
  cycle: null,
  orphan_node: null,
  over_cap: null,
});

export const FOLLOWER_READINESS_ROWS: readonly ReadinessRow[] = [
  ...FOLLOW_REASONS.map((code) => reason(code)),
  reason(WALLET_SEED_PENDING),
  reason(L1_FOLLOWER_UNCONFIGURED),
  reason(L1_NODE_CONFIG_UNREADABLE),
  reason(L1_NODE_CONFIG_FAILED),
  reason(FOLLOWER_VIEW_STALE),
  reason(FOLLOWER_VIEW_UNAPPLIED),
  reason(DRIVER_RECOMPUTE_PENDING),
  reason(DRIVER_RECOMPUTE_FAILED),
  reason(STARTUP_PREPARATION_FAILED),
  // The driver's fallback when a failed run named no hold (`l1-follower.ts`).
  reason("l1_driver_transient_failure"),
  reason(EVENTS_HOOK_FAILED),
  reason(EVENTS_INGESTION_FAILED),
  reason(EVENTS_INGESTION_WAITING),
  reason(EVENTS_ORPHAN_RECOVERY),
  reason(EVENT_IDENTITY_CONFLICT),
  detail(EVENT_UNDECODABLE, COUNT),
  // The role prune floors holding the follower's prune back, by the floor's
  // name (`l1-follower.readiness.ts`).
  detail("landed_frontier_prune_floor", COUNT),
  detail("intent_journal_replay_prune_floor", COUNT),
  detail("operator_activity_record_prune_floor", COUNT),
  reason(LANDED_BLOCKS_WAITING),
  reason(LANDED_BLOCK_INVALID),
  reason(LANDED_BLOCK_OWN_JOURNAL_MISMATCH),
  reason(LANDED_BLOCK_OWN_REVIVAL_PENDING),
  reason(LANDED_BLOCK_AWAITING_DA),
  reason(LANDED_BLOCK_DA_REFETCH_PENDING),
  reason(LANDED_BLOCK_EVENT_UNKNOWN),
  reason(LANDED_BLOCK_ORIGIN_AFTER_FRONTIER),
  reason(LANDED_BLOCK_FORCED_ORDER_PENDING),
  reason(LANDED_BLOCK_REPLAY_INCOMPLETE),
  reason(LANDED_BLOCK_LOCAL_FAULT),
  reason(LANDED_BLOCK_REPLAY_HELD),
  reason(LANDED_BLOCK_REPLAY_STUCK),
  reason(LANDED_BLOCK_VERIFIER_UNAVAILABLE),
  reason(LANDED_BLOCK_REPLAY_FAILED),
  reason(LANDED_BLOCK_REBASE_PENDING),
  reason(LANDED_BLOCK_REBASE_FAILED),
  reason(LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING),
  reason(CONFIRMED_LEDGER_BEHIND),
  reason(CONFIRMED_LEDGER_BASE_MISMATCH),
  reason(CONFIRMED_LEDGER_OWN_BLOCK_PENDING),
  reason(INTENT_JOURNAL_NO_VIEW),
  reason(INTENT_JOURNAL_UNAVAILABLE),
  reason(INTENT_INPUT_UNTRACKED),
  reason(INTENT_BYTES_MISMATCH),
  reason(INTENT_UNDECODABLE),
  reason(INTENT_CONTENT_REF_MISSING),
  reason(INTENT_UNBOUNDED_VALIDITY),
  reason(INTENT_GATE_UNJOURNALED),
  reason(INTENT_COMMIT_ANCHOR_NOT_DEEP),
  reason(INTENT_RECONCILE_FAILED),
  reason(INTENT_RECONCILE_TRANSIENT),
  reason(INTENT_RESUBMIT_REJECTED),
  reason(FORCED_ORDER_CARRIAGE_PENDING),
  reason(FORCED_ORDER_INGESTION_FAILED),
  detail(FORCED_ADMISSION_DIVERGENCE, TEXT),
  detail(L1_OWN_BLOCK_FORCED_ORDER_ORPHANED, COUNT),
  reason(STATE_QUEUE_UNHEALTHY),
  ...LINKED_QUEUE_UNHEALTHY.map((code) => field(code)),
  reason(OPERATOR_SET_UNHEALTHY),
  // The operator set before the driver published one (`snapshot.ts`).
  reason("operator_set_unavailable"),
  reason(OPERATOR_REMOVED),
];
