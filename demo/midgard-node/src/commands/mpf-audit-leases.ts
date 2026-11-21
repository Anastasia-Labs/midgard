import { randomUUID } from "node:crypto";

import { MpfEngineStateDB } from "../database/index.js";

/** The state-queue lease holder and ledger MPF lease owner an MPF audit runs
 * under. */
export type MpfAuditLeases = Readonly<{
  stateQueueHolder: string;
  ledgerStoreOwner: () => string;
}>;

/** The offline `mpf-audit` command's leases. It runs without the history
 * authority, so a live one may exist beside a node, and node startup keeps
 * them. */
export const OFFLINE_MPF_AUDIT_LEASES: MpfAuditLeases = {
  stateQueueHolder: "mpf-payload-audit",
  ledgerStoreOwner: () => `audit:${randomUUID()}`,
};

/** The running node's own payload audit takes names only a node process
 * takes, so a node restarted after a kill mid-audit retires them at startup
 * instead of reporting both stores busy until their hour-long TTL runs out. */
export const NODE_PROCESS_MPF_AUDIT_LEASES: MpfAuditLeases = {
  stateQueueHolder: "node-mpf-payload-audit",
  ledgerStoreOwner: MpfEngineStateDB.nodeProcessAuditLeaseOwner,
};
