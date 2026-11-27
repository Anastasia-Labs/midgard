import { randomUUID } from "node:crypto";

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
