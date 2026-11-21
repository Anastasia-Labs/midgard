/**
 * Readiness catalogue rows of the startup steps, the instance lock and the
 * node's L1 access (`readiness-catalogue.ts`).
 */
import type {
  TRANSPORT_FAILED_REASONS,
  TransportUnreadyReason,
} from "@al-ft/l1-node-transport";

import { L1_TRANSPORT_UNREADY } from "../commands/http/readiness-inputs.js";
import { STATE_QUEUE_UNAVAILABLE } from "../commands/listen-startup.await-landed-state-queue.js";
import { LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE } from "../commands/listen-startup.await-ledger-signature-verifier.js";
import {
  L1_INDEX_UNCONFIGURED,
  L1_NODE_UNCONFIGURED,
  ROLE_NON_FOLLOWER_L1_CONFIG,
} from "./l1-adapter.js";
import {
  NODE_INSTANCE_LOCK_FAILED,
  NODE_INSTANCE_LOCK_HELD_ELSEWHERE,
  NODE_INSTANCE_LOCK_SUSPENDED,
  NODE_INSTANCE_LOCK_UNAVAILABLE,
} from "./node-instance-lock.js";
import {
  field,
  members,
  oneOf,
  type ReadinessRow,
  reason,
} from "./readiness-catalogue.row.js";
import {
  ADMISSION_BACKLOG_UNREAD,
  AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
  DA_CAPABILITY_QUORUM_PENDING,
  DA_PROVIDER_ASSERTIONS_UNAVAILABLE,
  DATABASE_CONNECTION_FAILED,
  DATABASE_SCHEMA_INCOMPATIBLE,
  DATABASE_UNREACHABLE,
  DEPLOYMENT_MANIFEST_MISMATCH,
  DEPLOYMENT_MANIFEST_MISSING,
  DEPLOYMENT_MANIFEST_UNVERIFIABLE,
  L1_NODE_CONFIG_PENDING,
  L1_SLOT_CONFIG_MISMATCH,
  L1_SLOT_MAPPING_PENDING,
  LUCID_INITIALIZATION_PENDING,
  REWARD_ACCOUNT_STATUS_UNAVAILABLE,
  SCHEMA_MIGRATION_IN_PROGRESS,
} from "./startup-waiting.js";

/** Why the node's L1 transport is not ready: a transient state its
 * supervisor retries, or a fatal sidecar code. */
const TRANSPORT_REASONS = members<
  TransportUnreadyReason | (typeof TRANSPORT_FAILED_REASONS)[number]
>({
  sidecar_starting: null,
  sidecar_restarting: null,
  sidecar_unavailable: null,
  node_unreachable: null,
  node_connection_lost: null,
  stopped: null,
  node_handshake_failed: null,
  version_unsupported: null,
  malformed_frame: null,
  client_protocol_violation: null,
});

export const STARTUP_READINESS_ROWS: readonly ReadinessRow[] = [
  // The startup assembler (`listen.startup-http.ts`).
  reason("startup_incomplete"),
  reason("startup_failed"),
  reason(LUCID_INITIALIZATION_PENDING),
  reason(L1_NODE_CONFIG_PENDING),
  reason(L1_SLOT_MAPPING_PENDING),
  reason(L1_SLOT_CONFIG_MISMATCH),
  reason(DATABASE_UNREACHABLE),
  reason(DATABASE_CONNECTION_FAILED),
  reason(SCHEMA_MIGRATION_IN_PROGRESS),
  reason(DATABASE_SCHEMA_INCOMPATIBLE),
  reason(REWARD_ACCOUNT_STATUS_UNAVAILABLE),
  reason(AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED),
  reason(DEPLOYMENT_MANIFEST_MISMATCH),
  reason(DEPLOYMENT_MANIFEST_UNVERIFIABLE),
  reason(DEPLOYMENT_MANIFEST_MISSING),
  reason(DA_PROVIDER_ASSERTIONS_UNAVAILABLE),
  reason(DA_CAPABILITY_QUORUM_PENDING),
  reason(ADMISSION_BACKLOG_UNREAD),
  reason(LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE),
  reason(STATE_QUEUE_UNAVAILABLE),
  reason(NODE_INSTANCE_LOCK_FAILED),
  reason(NODE_INSTANCE_LOCK_HELD_ELSEWHERE),
  reason(NODE_INSTANCE_LOCK_SUSPENDED),
  reason(NODE_INSTANCE_LOCK_UNAVAILABLE),
  reason(L1_INDEX_UNCONFIGURED),
  reason(L1_NODE_UNCONFIGURED),
  reason(ROLE_NON_FOLLOWER_L1_CONFIG),
  reason(L1_TRANSPORT_UNREADY, oneOf(...TRANSPORT_REASONS)),
  ...TRANSPORT_REASONS.map((code) => field(code)),
];
