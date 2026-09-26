import { createHash } from "node:crypto";

import type { StateQueueAuthenticatedTransition } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  type CorrectionRewindIntent,
  type CorrectionRewindMember,
  type CorrectionRewindMemberKind,
  prepareCorrectionRewindRecoveryPlan,
  retainedPreparedRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import { DatabaseError } from "../database/utils/common.js";
import { invalidateSpeculativeCommitCandidate } from "../fibers/speculative-commit-builder.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";
import {
  parseStateQueueCorrectionObserverState,
  type StateQueueCorrectionObserverState,
} from "./state-queue-correction-observer.js";
import {
  authorizeStateQueueCorrectionReinclusion,
  reincludeStateQueueCorrectedBlocks,
} from "./state-queue-correction-recovery.js";

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
const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const C = Pending.Columns;
const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);
const REMOVABLE_STATUSES: readonly Pending.Status[] = [
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
  Pending.Status.ObservedWaitingStability,
  Pending.Status.Finalized,
];
/** Journal statuses of a block this node never observed on L1. Observed and
 * finalized journals landed; they are never treated as unlanded. */
const UNLANDED_STATUSES: readonly Pending.Status[] = [
  Pending.Status.PendingSubmission,
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
];
/** A removed header's journal may still read pending_submission when the
 * process stopped between handing the signed commit to L1 and recording it.
 * The removal proves the commit landed, so the journal is removable only if it
 * retained the signed intent; a journal that never signed cannot be the
 * removed header and stays blocked. */
const removedStatusBlocked = (record: Pending.Record): string | undefined => {
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
const unresolvedRemovedHeaders = (
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

type ChainMember = Readonly<{
  record: Pending.Record;
  transitionDigest: string;
  kind: CorrectionRewindMemberKind;
}>;
type Obligation =
  | Readonly<{ kind: "none" }>
  | Readonly<{ kind: "blocked"; reason: string }>
  | Readonly<{
      kind: "ready";
      chain: readonly ChainMember[];
      parentAggregate: Pending.UtxoPayloadSizeAggregate | undefined;
    }>;

type AdmittedRemovals = Readonly<{
  kind: "admitted";
  /** header -> digest of the admitted correction that removed it */
  removals: ReadonlyMap<string, string>;
  /** header -> the admitted correction that removed it */
  transitions: ReadonlyMap<string, StateQueueAuthenticatedTransition>;
  state: StateQueueCorrectionObserverState;
}>;

/** Every admitted removal, re-validated from the persisted observer state:
 * each envelope is re-parsed and re-authorized against the configured
 * deployment and release depth. This is the observer's durable authenticated
 * view, not a fresh L1 read. With `lock`, the observer row is held FOR SHARE,
 * so no observer save can retract a removal before the caller's transaction
 * ends. */
const admittedRemovals = (
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

const journal = (headerHash: string) =>
  Pending.retrieveByHeaderHash(Buffer.from(headerHash, "hex"), true);

const nonAbandonedChildren = (headerHash: string) =>
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
const chainIdentity = (chain: readonly ChainMember[]) =>
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

/** Validates one linear removed chain, earliest first, and the retained
 * parent aggregate of its replay base. Any other shape is not a rewind this
 * node can prove, and stays blocked with its reason. */
const validateChain = (
  chain: readonly ChainMember[],
  manifestId: string,
): Effect.Effect<Obligation, DatabaseError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    for (let index = 0; index < chain.length; index += 1) {
      const { record, kind } = chain[index]!;
      const header = record[C.HEADER_HASH].toString("hex");
      if (kind === "removed") {
        const blocked = removedStatusBlocked(record);
        if (blocked !== undefined) return { kind: "blocked", reason: blocked };
      } else if (!UNLANDED_STATUSES.includes(record[C.STATUS]))
        return {
          kind: "blocked",
          reason: `unlanded descendant ${header} has journal status ${record[C.STATUS]}, which records an L1 observation`,
        };
      if (record[C.DEPLOYMENT_MANIFEST_ID] !== manifestId)
        return {
          kind: "blocked",
          reason: `removed block ${header} belongs to another deployment`,
        };
      const replay = record.nativeMpfReplay;
      if (
        replay === undefined ||
        replay.baseRoot.toString("hex") !== record[C.BASE_UTXOS_ROOT] ||
        replay.candidateRoot.toString("hex") !== record[C.EXPECTED_UTXOS_ROOT]
      )
        return {
          kind: "blocked",
          reason: `removed block ${header} has no native replay matching its journal roots`,
        };
      if (index > 0) {
        const parent = chain[index - 1]!.record;
        if (
          !record[C.BASE_TAIL_HEADER_HASH].equals(parent[C.HEADER_HASH]) ||
          record[C.BASE_UTXOS_ROOT] !== parent[C.EXPECTED_UTXOS_ROOT]
        )
          return {
            kind: "blocked",
            reason: `removed block ${header} does not extend ${parent[C.HEADER_HASH].toString("hex")}`,
          };
      }
    }
    const earliest = chain[0]!.record;
    const baseTail = earliest[C.BASE_TAIL_HEADER_HASH];
    let parentAggregate: Pending.UtxoPayloadSizeAggregate | undefined;
    if (!baseTail.equals(ROOT_TAIL_HEADER_HASH)) {
      const parent = yield* Pending.retrieveByHeaderHash(baseTail);
      if (Option.isSome(parent)) {
        if (
          parent.value[C.STATUS] === Pending.Status.Abandoned ||
          parent.value[C.EXPECTED_UTXOS_ROOT] !== earliest[C.BASE_UTXOS_ROOT]
        )
          return {
            kind: "blocked",
            reason: `the replay base of removed block ${earliest[C.HEADER_HASH].toString("hex")} is not its retained parent's root`,
          };
        parentAggregate = parent.value.utxoPayloadAggregate;
      }
    }
    return { kind: "ready", chain, parentAggregate };
  });

const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

/** True only when the signed transaction's body hashes to `txHash` and spends
 * `outRef`. Undecodable bytes are not evidence. */
const signedTxSpends = (
  cbor: Buffer,
  txHash: Buffer,
  outRef: string,
): boolean => {
  const parsed = OUT_REF.exec(outRef);
  if (parsed === null) return false;
  let tx: CML.Transaction | undefined;
  try {
    tx = CML.Transaction.from_cbor_bytes(cbor);
    const body = tx.body();
    const hash = CML.hash_transaction(body);
    const inputs = body.inputs();
    try {
      if (hash.to_hex() !== txHash.toString("hex")) return false;
      for (let index = 0; index < inputs.len(); index += 1) {
        const input = inputs.get(index);
        const id = input.transaction_id();
        try {
          if (
            id.to_hex() === parsed[1] &&
            input.index().toString() === parsed[2]
          )
            return true;
        } finally {
          id.free();
          input.free();
        }
      }
      return false;
    } finally {
      inputs.free();
      hash.free();
      body.free();
    }
  } catch {
    return false;
  } finally {
    tx?.free();
  }
};

/** A header the authenticated state-queue view still knows: in the current
 * cursor queue, or in any pending or admitted transition's queues. Such a
 * block landed (or may yet be admitted as landed) and is never unlanded. */
const onAuthenticatedQueue = (
  state: StateQueueCorrectionObserverState,
  header: string,
) =>
  state.cursorQueue.some((node) => node.headerHash === header) ||
  [...state.pending, ...state.admitted].some(
    (transition) =>
      transition.removedHeaderHashes.includes(header) ||
      transition.previousQueue.some((node) => node.headerHash === header) ||
      transition.nextQueue.some((node) => node.headerHash === header),
  );

/**
 * Proves that `childHeader`, the only non-abandoned journal extending the
 * removed block `parent`, never reached L1 and never can: its commit spends
 * exactly the queue node of `parent` that the admitted correction consumed.
 * That correction is at the release depth, so the input is gone for as long
 * as the removal itself stands. Anything short of that proof is blocked with
 * its reason; a journal whose header the authenticated view knows is never
 * abandoned.
 */
const proveUnlanded = (
  parent: ChainMember,
  childHeader: string,
  admitted: AdmittedRemovals,
): Effect.Effect<
  | Readonly<{ kind: "blocked"; reason: string }>
  | Readonly<{ kind: "unlanded"; member: ChainMember }>,
  DatabaseError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const parentHeader = parent.record[C.HEADER_HASH].toString("hex");
    const blocked = (reason: string) => ({ kind: "blocked" as const, reason });
    const notYet = `descendant ${childHeader} of removed block ${parentHeader} is not removed by an admitted correction yet`;
    if (parent.kind !== "removed")
      return blocked(
        `descendant ${childHeader} extends unlanded block ${parentHeader}; only a direct descendant of a removed block can be proven unlanded`,
      );
    const found = yield* journal(childHeader);
    if (Option.isNone(found))
      return blocked(`descendant ${childHeader} lost its journal`);
    const record = found.value;
    const status = record[C.STATUS];
    if (!UNLANDED_STATUSES.includes(status))
      return blocked(`${notYet} (journal status ${status})`);
    if (onAuthenticatedQueue(admitted.state, childHeader))
      return blocked(`${notYet}; it is on the authenticated state queue`);
    const removal = admitted.transitions.get(parentHeader);
    const baseTail = record[C.BASE_TAIL_OUT_REF];
    const parentNode = removal?.previousQueue.find(
      (node) => node.headerHash === parentHeader,
    );
    if (
      removal === undefined ||
      removal.transitionDigest !== parent.transitionDigest ||
      parentNode === undefined ||
      parentNode.outRef !== baseTail ||
      !removal.consumedQueueOutRefs.includes(baseTail)
    )
      return blocked(
        `${notYet}; its commit spends ${baseTail}, which the admitted correction of ${parentHeader} did not consume`,
      );
    const intended = record[C.INTENDED_TX_HASH] ?? null;
    const signed = record[C.SIGNED_TX_CBOR] ?? null;
    const submitted = record[C.SUBMITTED_TX_HASH];
    if (intended !== null && signed !== null) {
      if (submitted !== null && !submitted.equals(intended))
        return blocked(
          `${notYet}; it was submitted as ${submitted.toString("hex")}, not its retained signed intent`,
        );
      if (!signedTxSpends(signed, intended, baseTail))
        return blocked(
          `${notYet}; its retained signed commit does not spend ${baseTail}`,
        );
    } else if (
      submitted !== null ||
      status !== Pending.Status.PendingSubmission
    )
      return blocked(
        `${notYet}; it was handed to L1 without retained signed bytes, so it cannot be proven never to land`,
      );
    return {
      kind: "unlanded" as const,
      member: {
        record,
        transitionDigest: removal.transitionDigest,
        kind: "unlanded" as const,
      },
    };
  });

/** The removed chain owed a rewind: an admitted removal whose journal is not
 * abandoned, anchored at the unique owed block whose parent is not owed, with
 * every non-abandoned descendant also removed, or proven never to land (see
 * proveUnlanded). A suffix whose later removals are not admitted yet stays
 * blocked; each admission re-evaluates, so any order of suffix finality
 * reaches the same end state. */
const loadObligation = (authority: StateQueueCorrectionRewindAuthority) =>
  Effect.gen(function* () {
    const unresolved = yield* unresolvedRemovedHeaders(authority);
    if (unresolved.length === 0) return { kind: "none" } satisfies Obligation;
    const admitted = yield* admittedRemovals(authority);
    if (admitted.kind === "blocked") return admitted satisfies Obligation;
    const owed = new Map<string, ChainMember>();
    for (const header of unresolved) {
      const digest = admitted.removals.get(header);
      const record = yield* journal(header);
      if (digest === undefined || Option.isNone(record))
        return {
          kind: "blocked",
          reason: `removed block ${header} lost its admitted correction or journal`,
        } satisfies Obligation;
      owed.set(header, {
        record: record.value,
        transitionDigest: digest,
        kind: "removed",
      });
    }
    const anchors = [...owed.values()].filter(
      ({ record }) =>
        !owed.has(record[C.BASE_TAIL_HEADER_HASH].toString("hex")),
    );
    if (anchors.length !== 1)
      return {
        kind: "blocked",
        reason: `removed blocks ${[...owed.keys()].join(",")} do not form one chain`,
      } satisfies Obligation;
    const chain = [anchors[0]!];
    for (;;) {
      const children = yield* nonAbandonedChildren(
        chain.at(-1)!.record[C.HEADER_HASH].toString("hex"),
      );
      if (children.length === 0) break;
      if (children.length !== 1)
        return {
          kind: "blocked",
          reason: `removed block ${chain.at(-1)!.record[C.HEADER_HASH].toString("hex")} has competing descendants ${children.join(",")}`,
        } satisfies Obligation;
      const child = owed.get(children[0]!);
      if (child === undefined) {
        const unlanded = yield* proveUnlanded(
          chain.at(-1)!,
          children[0]!,
          admitted,
        );
        if (unlanded.kind === "blocked") return unlanded satisfies Obligation;
        chain.push(unlanded.member);
        continue;
      }
      if (chain.at(-1)!.kind === "unlanded")
        return {
          kind: "blocked",
          reason: `removed block ${children[0]!} extends unlanded block ${chain.at(-1)!.record[C.HEADER_HASH].toString("hex")}`,
        } satisfies Obligation;
      if (chain.includes(child))
        return {
          kind: "blocked",
          reason: "removed block ancestry is cyclic",
        } satisfies Obligation;
      chain.push(child);
    }
    if (chain.filter(({ kind }) => kind === "removed").length !== owed.size)
      return {
        kind: "blocked",
        reason: `removed blocks ${[...owed.keys()].join(",")} do not form one chain`,
      } satisfies Obligation;
    return yield* validateChain(chain, authority.manifestId);
  });

/** Re-derives a retained plan's chain from fresh authority. The members are
 * the plan's identity, so a later admission never widens an interrupted one. */
const loadRetainedChain = (
  authority: StateQueueCorrectionRewindAuthority,
  intent: CorrectionRewindIntent,
  lock = false,
) =>
  Effect.gen(function* () {
    const admitted = yield* admittedRemovals(authority, lock);
    if (admitted.kind === "blocked")
      return yield* Effect.fail(
        failure(
          `Retained correction rewind ${intent.headerHash} lost its authority: ${admitted.reason}`,
        ),
      );
    const chain: ChainMember[] = [];
    for (const member of intent.members) {
      const lost = failure(
        `Retained correction rewind member ${member.headerHash} lost its admitted correction or journal`,
      );
      if (member.kind === "unlanded") {
        const proof = yield* proveUnlanded(
          chain.at(-1)!,
          member.headerHash,
          admitted,
        );
        if (
          proof.kind !== "unlanded" ||
          proof.member.transitionDigest !== member.transitionDigest
        )
          return yield* Effect.fail(
            failure(
              `Retained correction rewind member ${member.headerHash} is no longer provably unlanded: ${proof.kind === "blocked" ? proof.reason : "its proving correction changed"}`,
            ),
          );
        chain.push(proof.member);
        continue;
      }
      const record = yield* journal(member.headerHash);
      if (
        admitted.removals.get(member.headerHash) !== member.transitionDigest ||
        Option.isNone(record) ||
        record.value[C.STATUS] === Pending.Status.Abandoned
      )
        return yield* Effect.fail(lost);
      chain.push({
        record: record.value,
        transitionDigest: member.transitionDigest,
        kind: "removed",
      });
    }
    const validated = yield* validateChain(chain, authority.manifestId);
    if (validated.kind !== "ready")
      return yield* Effect.fail(
        failure(
          `Retained correction rewind ${intent.headerHash} is no longer provable: ${validated.kind === "blocked" ? validated.reason : "no chain"}`,
        ),
      );
    if (
      chainIdentity(chain) !== intent.journalDigest ||
      chain[0]!.record[C.BASE_UTXOS_ROOT] !== intent.targetRoot
    )
      return yield* Effect.fail(
        failure(
          `Retained correction rewind ${intent.headerHash} journal identity changed`,
        ),
      );
    return validated;
  });

/** Last blocked reason per history binding. A blocked obligation is
 * re-evaluated on every convergence; it is logged at WARN when its reason
 * first appears or changes, and at debug while it persists. */
const blockedReasons = new Map<string, string>();
const logBlocked = (bindingDigest: string, reason: string | undefined) =>
  Effect.suspend(() => {
    if (reason === undefined) {
      blockedReasons.delete(bindingDigest);
      return Effect.void;
    }
    const message = `State-queue correction rewind is blocked: ${reason}`;
    if (blockedReasons.get(bindingDigest) === reason)
      return Effect.logDebug(message);
    blockedReasons.set(bindingDigest, reason);
    return Effect.logWarning(message);
  });

/** Read-only view of the rewind obligation, for diagnostics and tests. */
export const inspectStateQueueCorrectionRewindObligation = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  loadObligation(authority).pipe(
    Effect.map((obligation) =>
      obligation.kind === "ready"
        ? {
            kind: "ready" as const,
            members: obligation.chain.map(({ record, kind }) => ({
              headerHash: record[C.HEADER_HASH].toString("hex"),
              kind,
            })),
          }
        : obligation,
    ),
  );

/**
 * Recovery preparation: resumes a retained rewind plan, or proves a fresh
 * obligation and executes it. Returns without effect when nothing is owed,
 * when another domain's plan is retained (its owner resumes it first), or
 * when the obligation is blocked (the disposition keeps the gate closed).
 */
export const prepareStateQueueCorrectionRewind = (input: {
  readonly bindingDigest: string;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly authority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation, authority, config } = input;
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    yield* preparation.assertCurrent;
    const derived = yield* owned(
      Effect.gen(function* () {
        const retained = yield* retainedPreparedRecoveryPlan(
          input.bindingDigest,
        );
        if (retained?.kind === "signed_header") return undefined;
        if (retained?.kind === "correction_rewind") {
          const ready = yield* loadRetainedChain(authority, retained.intent);
          return { ready, retained: retained.intent };
        }
        const obligation = yield* loadObligation(authority);
        yield* logBlocked(
          input.bindingDigest,
          obligation.kind === "blocked" ? obligation.reason : undefined,
        );
        return obligation.kind === "ready"
          ? { ready: obligation, retained: undefined }
          : undefined;
      }),
    );
    if (derived === undefined) return;
    const { ready } = derived;
    const records = ready.chain.map(({ record }) => record);
    const members: readonly CorrectionRewindMember[] = ready.chain.map(
      ({ record, transitionDigest, kind }) => ({
        headerHash: record[C.HEADER_HASH].toString("hex"),
        transitionDigest,
        kind,
      }),
    );
    const targetRoot = records[0]![C.BASE_UTXOS_ROOT];
    const acceptedRoots = [
      targetRoot,
      ...records.map((record) => record[C.EXPECTED_UTXOS_ROOT]),
    ];
    const journalDigest = chainIdentity(ready.chain);
    const globals = yield* Globals;
    if (config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
    let owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (owner === undefined) {
      // Open only retained native bytes; never genesis-bootstrap or replay a
      // removed block's journal on this path. Create validates the marker.
      owner = yield* Effect.uninterruptible(
        Effect.gen(function* () {
          const opened = yield* Effect.tryPromise({
            try: () =>
              ProductionNativeMpfOwnerService.create({
                levelPath: config.LEDGER_MPF_DB_PATH,
                binaryPath: config.MPF_NATIVE_OWNER_BINARY_PATH,
                binarySha256: config.MPF_NATIVE_OWNER_BINARY_SHA256,
                maxFrameBytes: config.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
                maxChunkBytes: config.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
                requestTimeoutMs: config.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
                restartLimit: config.MPF_NATIVE_OWNER_RESTART_LIMIT,
                sidecarPath: config.MPF_NATIVE_OWNER_SIDECAR_PATH,
              }),
            catch: (cause) =>
              failure("Retained native rewind owner could not open", cause),
          });
          yield* Ref.set(globals.NATIVE_MPF_OWNER, opened);
          return opened;
        }),
      );
    }
    yield* preparation.assertCurrent;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained native rewind diagnostics failed", cause),
    });
    const durableRoot = diagnostics.durableRoot;
    let expectedRoot: string;
    if (derived.retained !== undefined) {
      expectedRoot = derived.retained.expectedRoot;
      if (durableRoot !== expectedRoot && durableRoot !== targetRoot)
        return yield* Effect.fail(
          failure(
            `Native MPF durable root ${durableRoot} is neither the retained rewind base ${expectedRoot} nor its target ${targetRoot}`,
          ),
        );
    } else {
      // The native root is the removed chain's replay base (a crash before the
      // first promotion) or one of its blocks' roots. Anything else is not a
      // state this rewind can prove it restores from; never guess a base.
      if (!acceptedRoots.includes(durableRoot))
        return yield* Effect.fail(
          failure(
            `Native MPF durable root ${durableRoot} is outside the removed chain ${members.map(({ headerHash }) => headerHash).join(",")}; refusing to rewind`,
          ),
        );
      expectedRoot = durableRoot;
    }
    const intent: CorrectionRewindIntent = {
      bindingDigest: input.bindingDigest,
      manifestId: checkpoint.manifestId,
      headerHash: members[0]!.headerHash,
      members,
      expectedRoot,
      targetRoot,
      journalDigest,
    };
    const evidenceDigest = sha(
      eventHistoryCanonicalJson({
        members,
        point: checkpoint.head,
        snapshot: checkpoint.capture.snapshotDigest,
      }),
    );
    // Re-proves the whole chain inside the caller's transaction with the
    // observer row held FOR SHARE: every removal is still admitted, every
    // unlanded descendant is still provably unlanded, and the journal identity
    // is unchanged. No observer save can retract a removal until the
    // transaction that acts on this proof commits.
    const recheck = loadRetainedChain(
      authority,
      derived.retained ?? intent,
      true,
    ).pipe(Effect.map(({ chain }) => chain));
    const plan = yield* owned(
      recheck.pipe(
        Effect.zipRight(
          prepareCorrectionRewindRecoveryPlan(
            checkpoint,
            derived.retained ?? intent,
            evidenceDigest,
          ),
        ),
      ),
    );
    if (
      plan.intent.targetRoot !== targetRoot ||
      plan.intent.journalDigest !== journalDigest
    )
      return yield* Effect.fail(
        failure("Retained correction rewind identity changed"),
      );
    let submitted: Readonly<{ txHash: string; sinceMs: number }> | undefined;
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        const current = yield* recheck;
        const sql = yield* SqlClient.SqlClient;
        const results = yield* reincludeStateQueueCorrectedBlocks(
          current.map(({ record, transitionDigest, kind }) => ({
            headerHash: record[C.HEADER_HASH].toString("hex"),
            transitionDigest,
            kind,
          })),
        );
        if (
          results.length !== members.length ||
          results.some(({ journalFound }) => !journalFound)
        )
          return yield* Effect.fail(
            failure("Rewind reinclusion did not resolve every removed block"),
          );
        // Removed and unlanded blocks can never be continued; retire only
        // their own leases, atomically with their abandonment.
        for (const { record } of current)
          yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
        // The SQL marker follows the native root, which the plan's CAS already
        // proved. It was stamped by the latest journaled block, which may be
        // a later unsubmitted attempt, so it is replaced, not compared. The
        // aggregate is the replay base's own (its parent journal's) or none,
        // which makes the commit base recompute it from ledger entries.
        const aggregate = ready.parentAggregate;
        const engine = yield* sql`UPDATE mpf_engine_state
          SET root_hex = ${targetRoot},
            utxo_payload_entry_count = ${aggregate?.entryCount ?? null},
            utxo_payload_encoded_tuple_bytes = ${aggregate?.encodedTupleBytes ?? null},
            updated_at = NOW()
          WHERE store_name = 'ledger'
          RETURNING store_name`;
        if (engine.length !== 1)
          return yield* Effect.fail(
            failure("Native SQL marker row is missing"),
          );
        const active = yield* Pending.retrieveActive();
        submitted = Option.match(active, {
          onNone: () => undefined,
          onSome: (record) => ({
            txHash:
              (
                record[C.SUBMITTED_TX_HASH] ?? record[C.INTENDED_TX_HASH]
              )?.toString("hex") ?? "",
            sinceMs: record[C.UPDATED_AT].getTime(),
          }),
        });
      }),
      // The commit preflight re-derives the tail, boundary and any remaining
      // finalization from L1 and the journal once no finalization is pending.
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          submitted?.txHash ?? "",
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          submitted?.sinceMs ?? 0,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
        yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
      }),
    });
    blockedReasons.delete(input.bindingDigest);
    yield* Effect.logInfo(
      `State-queue correction rewind restored native root ${targetRoot} and reincluded block(s) ${members.map(({ headerHash, kind }) => `${headerHash}(${kind})`).join(",")}.`,
    );
  });
