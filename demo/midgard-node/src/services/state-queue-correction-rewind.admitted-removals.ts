import { createHash } from "node:crypto";

import type { StateQueueAuthenticatedTransition } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { type CorrectionRewindMemberKind } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import {
  parseStateQueueCorrectionObserverState,
  type StateQueueCorrectionObserverState,
} from "./state-queue-correction-observer.js";
import { authorizeStateQueueCorrectionReinclusion } from "./state-queue-correction-recovery.js";

export { UNLANDED_STATUSES } from "./state-queue-correction-recovery.js";

/**
 * Architecture G rewind for a state-queue correction (attestation timeout or
 * fraud removal) that removed blocks this node committed. Every such block
 * already advanced the native MPF owner past its replay base, so reincluding
 * its payloads is a canonical rollback of chain-derived local state, not a
 * forward write: the native root returns to the replay base of the EARLIEST
 * removed block, every removed block's payloads are reincluded in chain
 * order, and the source owner reloads the validation cache, all in one
 * recovery generation. The operation is persisted as a recovery plan before
 * the native root moves, so a crash anywhere resumes it (or the observer's
 * chain replay recreates it) to the same end state.
 */

const table = "event_history_recovery_plans";

export const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });

export const sha = (value: string) =>
  createHash("sha256").update(value).digest("hex");

export const C = Pending.Columns;

export const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);

const REMOVABLE_STATUSES: readonly Pending.Status[] = [
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
  Pending.Status.ObservedWaitingStability,
  Pending.Status.LocallyApplied,
];

/** A removed header's journal may still read pending_submission when the
 * process stopped between handing the signed commit to L1 and recording it.
 * The removal proves the commit landed, so the journal is removable only if it
 * retained the signed intent; a journal that never signed cannot be the
 * removed header and stays blocked. */
export const removedStatusBlocked = (
  record: Pending.Record,
): string | undefined => {
  const status = record[C.STATUS];
  if (REMOVABLE_STATUSES.includes(status)) return undefined;
  const header = record[C.HEADER_HASH].toString("hex");
  if (status !== Pending.Status.PendingSubmission)
    return `removed block ${header} has journal status ${status}, not a submitted status`;
  if (record[C.INTENDED_TX_HASH] != null && record[C.SIGNED_TX_CBOR] != null)
    return undefined;
  return `removed block ${header} has journal status pending_submission without a signed intent, so this journal cannot be the removed header`;
};

export type StateQueueCorrectionRewindAuthority = Readonly<{
  manifestId: string;
  stateQueuePolicyId: string;
  requiredFinalityDepth: bigint;
}>;

const observerDocument = (sql: SqlClient.SqlClient) => sql`
  CASE jsonb_typeof(s.state_record)
    WHEN 'string' THEN (s.state_record #>> '{}')::jsonb
    ELSE s.state_record END`;

/** Headers removed by an admitted correction whose local journal was never
 * resolved. Cheap enough for every forward append: one indexed join. */
export const unresolvedRemovedHeaders = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`
      SELECT DISTINCT p.header_hash
      FROM state_queue_terminal_observer_states s
      CROSS JOIN LATERAL jsonb_array_elements(${observerDocument(sql)} -> 'admitted') t(transition)
      CROSS JOIN LATERAL jsonb_array_elements_text(t.transition -> 'removedHeaderHashes') h(value)
      JOIN pending_block_finalizations p
        ON p.header_hash = CASE WHEN h.value ~ '^[0-9a-f]{56}$' THEN decode(h.value, 'hex') END
      WHERE s.deployment_identity_digest = ${Buffer.from(authority.manifestId, "hex")}
        AND s.state_queue_policy_id = ${Buffer.from(authority.stateQueuePolicyId, "hex")}
        AND t.transition ->> 'transitionKind' IN ('timeout_correction', 'fraud_removal')
        AND p.status <> ${Pending.Status.Abandoned}
      ORDER BY p.header_hash`;
    return rows.map((row) => row.header_hash.toString("hex"));
  });

/** Forward-append and resume disposition. A removed block's payloads cannot
 * return to the pending set while the native root still includes it, and no
 * block may commit on that root, so the gate stays closed until the rewind
 * recovery resolves every such journal. */
export const stateQueueCorrectionRewindDisposition = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  unresolvedRemovedHeaders(authority).pipe(
    Effect.map((headers) =>
      headers.length === 0
        ? undefined
        : {
            status: "pending" as const,
            reason: `Admitted state-queue correction removed locally committed block(s) ${headers.join(",")}; the native ledger must rewind before their payloads are reincluded`,
          },
    ),
  );

export type ChainMember = Readonly<{
  record: Pending.Record;
  transitionDigest: string;
  kind: CorrectionRewindMemberKind;
}>;

export type Obligation =
  | Readonly<{ kind: "none" }>
  | Readonly<{ kind: "blocked"; reason: string }>
  | Readonly<{
      kind: "ready";
      chain: readonly ChainMember[];
      parentAggregate: Pending.UtxoPayloadSizeAggregate | undefined;
    }>;

export type AdmittedRemovals = Readonly<{
  kind: "admitted";
  /** header -> digest of the admitted correction that removed it */
  removals: ReadonlyMap<string, string>;
  /** header -> the admitted correction that removed it */
  transitions: ReadonlyMap<string, StateQueueAuthenticatedTransition>;
  state: StateQueueCorrectionObserverState;
}>;

/** The correction observer's persisted authenticated view of the state queue
 * (its cursor queue and every pending and admitted transition: timeout
 * corrections, fraud removals and merges), re-parsed and bound to the
 * configured deployment. This is the observer's durable view, not a fresh L1
 * read. With `lock`, the observer row is held FOR SHARE, so no observer save
 * can change it before the caller's transaction ends. */
export const loadStateQueueCorrectionObserverState = (
  authority: StateQueueCorrectionRewindAuthority,
  lock = false,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ state_record: unknown }>`
      SELECT state_record FROM state_queue_terminal_observer_states
      WHERE deployment_identity_digest = ${Buffer.from(authority.manifestId, "hex")}
        AND state_queue_policy_id = ${Buffer.from(authority.stateQueuePolicyId, "hex")}
      ${lock ? sql`FOR SHARE` : sql``}`;
    if (rows.length !== 1)
      return { kind: "blocked" as const, reason: "no observer state" };
    const raw = rows[0]!.state_record;
    let decoded: unknown;
    try {
      decoded = typeof raw === "string" ? JSON.parse(raw) : raw;
    } catch {
      decoded = null;
    }
    const state = parseStateQueueCorrectionObserverState(decoded);
    if (
      state === null ||
      state.deploymentIdentityDigest !== authority.manifestId ||
      state.stateQueuePolicyId !== authority.stateQueuePolicyId
    )
      return {
        kind: "blocked" as const,
        reason: "the observer state is non-canonical",
      };
    return { kind: "observed" as const, state };
  });

/** Every admitted removal, re-validated from the persisted observer state:
 * each envelope is re-parsed and re-authorized against the configured
 * deployment and release depth. With `lock`, no observer save can retract a
 * removal before the caller's transaction ends. */
export const admittedRemovals = (
  authority: StateQueueCorrectionRewindAuthority,
  lock = false,
) =>
  Effect.gen(function* () {
    const observed = yield* loadStateQueueCorrectionObserverState(
      authority,
      lock,
    );
    if (observed.kind === "blocked") return observed;
    const { state } = observed;
    const removals = new Map<string, string>();
    const transitions = new Map<string, StateQueueAuthenticatedTransition>();
    for (const transition of state.admitted) {
      if (
        transition.transitionKind !== "timeout_correction" &&
        transition.transitionKind !== "fraud_removal"
      )
        continue;
      try {
        authorizeStateQueueCorrectionReinclusion(transition, {
          expectedDeploymentIdentityDigest: authority.manifestId,
          requiredFinalityDepth: authority.requiredFinalityDepth,
        });
      } catch (cause) {
        return {
          kind: "blocked" as const,
          reason: `admitted correction ${transition.transactionHash} is not authorized: ${cause instanceof Error ? cause.message : String(cause)}`,
        };
      }
      for (const header of transition.removedHeaderHashes) {
        const previous = removals.get(header);
        if (previous !== undefined && previous !== transition.transitionDigest)
          return {
            kind: "blocked" as const,
            reason: `block ${header} is removed by two admitted corrections`,
          };
        removals.set(header, transition.transitionDigest);
        transitions.set(header, transition);
      }
    }
    return {
      kind: "admitted" as const,
      removals,
      transitions,
      state,
    } satisfies AdmittedRemovals;
  });

export const journal = (headerHash: string) =>
  Pending.retrieveByHeaderHash(Buffer.from(headerHash, "hex"), true);

export const nonAbandonedChildren = (headerHash: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`
      SELECT header_hash FROM pending_block_finalizations
      WHERE base_tail_header_hash = ${Buffer.from(headerHash, "hex")}
        AND status <> ${Pending.Status.Abandoned}
      ORDER BY header_hash`;
    return rows.map((row) => row.header_hash.toString("hex"));
  });

/** The removed chain's immutable identity: every root and hash that fixes
 * which native state each block moved from and to. Status is excluded; it is
 * checked separately and legitimately advances before the rewind. */
export const chainIdentity = (chain: readonly ChainMember[]) =>
  sha(
    eventHistoryCanonicalJson(
      chain.map(({ record, kind }) => ({
        kind,
        headerHash: record[C.HEADER_HASH].toString("hex"),
        manifestId: record[C.DEPLOYMENT_MANIFEST_ID],
        baseTailHeaderHash: record[C.BASE_TAIL_HEADER_HASH].toString("hex"),
        baseUtxosRoot: record[C.BASE_UTXOS_ROOT],
        expectedUtxosRoot: record[C.EXPECTED_UTXOS_ROOT],
        submittedTxHash: record[C.SUBMITTED_TX_HASH]?.toString("hex") ?? null,
        intendedTxHash: record[C.INTENDED_TX_HASH]?.toString("hex") ?? null,
        replayBaseRoot:
          record.nativeMpfReplay?.baseRoot.toString("hex") ?? null,
        replayCandidateRoot:
          record.nativeMpfReplay?.candidateRoot.toString("hex") ?? null,
        replayEventLogDigest:
          record.nativeMpfReplay?.eventLogDigest.toString("hex") ?? null,
      })),
    ),
  );
