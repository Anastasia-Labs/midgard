import { createHash } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Option, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { loadCanonicalHistoryCoverage } from "../database/eventHistoryCanonicalCoverage.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  discardPreparedHistoryRecoveryPlan,
  prepareRetainedNativeHistoryRecoveryPlan,
  retainedPreparedRecoveryPlan,
  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
} from "../database/eventHistoryRecoveryPlans.js";
import * as MutationJobsDB from "../database/mutationJobs.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import { DatabaseError } from "../database/utils/common.js";
import { recordConfirmedPendingBlock } from "../fibers/block-confirmation.js";
import { invalidateSpeculativeCommitCandidate } from "../fibers/speculative-commit-builder.js";
import {
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import {
  LEDGER_SCAN_TIMEOUT_MS,
  type LedgerSnapshotOutput,
} from "../l1-ledger-snapshot.js";
import {
  type SerializedStateQueueUTxO,
  serializeStateQueueUTxO,
} from "../workers/utils/commit-block-header.js";
import {
  journalAbandonment,
  REVIVAL_BLOCKING_SIBLING_STATUSES,
  reviveReplacedCanonicalJournal,
  signedIntentReplacementDigest,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryOwnerChange } from "./event-history-owner.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";
import {
  loadStateQueueCorrectionObserverState,
  type StateQueueCorrectionRewindAuthority,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.js";

/**
 * Reconciliation of a signed commit that missed its validity window, by the
 * owner ruling "whichever lands wins".
 *
 * Every commit of block E spends the state-queue tail node D it was built on,
 * and the queue validator appends only onto a node whose `next` is empty. So
 * E and any replacement built on D are mutually exclusive on L1: at most one
 * of them ever holds D's slot. The only hazard is local: the node must never
 * build on local state that is not the block that actually landed.
 *
 * This is the single place that decides. It runs in the history owner's
 * pending reconciliation because only there does the node hold an
 * authenticated, exact-point view of the queue bound to the canonical
 * checkpoint it has journaled, with native recovery plans and generation
 * fencing; it also runs at startup before hydration and the local job gate.
 * The confirmation worker only defers to it. Once the observed head slot has
 * reached the signed commit's TTL (its exclusive validity upper bound, so E
 * can never be included in any later block of this chain), authenticated
 * evidence decides, in this order: the exact-point queue, the correction
 * observer's persisted view of every state-queue removal (corrections and
 * merges, pending or admitted), and the canonical history the owner
 * journaled.
 *  - E's own node is on the queue: E landed; its observation is recorded.
 *  - A correction removed E: E landed and was removed; the correction path
 *    owns its journal, so this defers (below).
 *  - E landed and left the queue by a merge: its signed commit is in the
 *    journaled canonical history, the confirmed state is E's header, or an
 *    observed merge folded E. E landed; its observation is recorded against
 *    the node its signed commit created, and local finalization replays it.
 *  - A correction removed D: when this node journals D, the correction path
 *    owns E's journal (its rewind proves and abandons an unlanded descendant
 *    of a removed block), so this defers to it without holding the gate
 *    closed: the deferral is remembered for this journal until a rollback
 *    (the only way D returns) or a runtime restart, both of which re-read
 *    the queue. Without a journal of D nothing else ever resolves E, and the
 *    correction consumed the node E spends: E is replaced.
 *  - D's `next` is still empty, or holds a block that is not this node's
 *    replaced sibling of E (a foreign block): E is replaced. Its journal is
 *    abandoned under its replacement digest, its local-finalization job row
 *    and lease are retired, every member is reopened, the native root and SQL
 *    marker return to its base; the commit worker then builds anew.
 *  - D's `next` is an earlier journal of this node that was replaced on the
 *    same base (any generation): that block won after all (a rollback brought
 *    it back). The active journal is abandoned as above and the winner is
 *    revived with its members taken back; local finalization then replays it.
 *  - D was merged into the confirmed state (an observed merge removed it):
 *    its successor at that merge decides as D's `next` does, and a D merged
 *    while it was still the tail means E can never land: E is replaced.
 *  - D absent before the observer recorded why: nothing is decided at this
 *    source point and the gate stays open (E stays active, so nothing is
 *    built); the next point decides again. The observer runs independently
 *    of the gate, so a recorded removal or merge resolves it.
 *  - Anything else (another own block of a different kind in D's slot, D's
 *    successor node absent) keeps the gate closed and says why.
 * A signed commit is never replaced before its TTL, and never on wall-clock
 * time or queue absence alone.
 */

const table = "event_history_recovery_plans";
const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });
const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");
const C = Pending.Columns;
const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);
const REPLACEMENT_EVIDENCE_DOMAIN =
  "midgard-signed-intent-replacement-evidence-v1";

/** Active journal statuses that record no L1 observation of the commit. */
const UNLANDED_STATUSES: readonly Pending.Status[] = [
  Pending.Status.PendingSubmission,
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
];
const ACTIVE_STATUSES: readonly Pending.Status[] = [
  ...UNLANDED_STATUSES,
  Pending.Status.ObservedWaitingStability,
];

/** The signed validity upper bound (TTL, an exclusive slot bound), or
 * undefined when the bytes do not decode or carry none: such an intent can
 * never be shown unable to land, so it is never replaced. */
const signedTtl = (cbor: Buffer): bigint | undefined => {
  let tx: CML.Transaction | undefined;
  try {
    tx = CML.Transaction.from_cbor_bytes(cbor);
    const body = tx.body();
    const ttl = body.ttl();
    body.free();
    return ttl;
  } catch {
    return undefined;
  } finally {
    tx?.free();
  }
};

type ActiveSignedIntent = Readonly<{
  headerHash: Buffer;
  status: Pending.Status;
  signedTxCbor: Buffer;
  intendedTxHash: Buffer;
}>;

/** The node's single active journal when it holds a signed intent and records
 * no L1 observation. At most one journal is active at a time. */
const activeSignedIntent = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    status: Pending.Status;
    signed_tx_cbor: Buffer | null;
    intended_tx_hash: Buffer | null;
  }>`SELECT header_hash, status, signed_tx_cbor, intended_tx_hash
    FROM pending_block_finalizations WHERE status IN ${sql.in(ACTIVE_STATUSES)}`;
  if (rows.length !== 1) return undefined;
  const row = rows[0]!;
  if (
    !UNLANDED_STATUSES.includes(row.status) ||
    row.signed_tx_cbor === null ||
    row.intended_tx_hash === null
  )
    return undefined;
  return {
    headerHash: row.header_hash,
    status: row.status,
    signedTxCbor: row.signed_tx_cbor,
    intendedTxHash: row.intended_tx_hash,
  } satisfies ActiveSignedIntent;
});

/** Last reported reason per binding and topic: WARN when it first appears or
 * changes, debug while it persists. */
const reported = new Map<string, string>();
const reportOnce = (key: string, message: string | undefined) =>
  Effect.suspend(() => {
    if (message === undefined) {
      reported.delete(key);
      return Effect.void;
    }
    if (reported.get(key) === message) return Effect.logDebug(message);
    reported.set(key, message);
    return Effect.logWarning(message);
  });

/** The one signed intent (header and intended transaction) whose
 * reconciliation defers: to the correction path (`current`, until a rollback),
 * or only at one source point (`atPoint`, while the correction observer has
 * not yet recorded why its base left the queue; the next point decides
 * again). Owned by one history runtime: a restart starts empty. */
export type SignedIntentDeferral = {
  current: string | undefined;
  atPoint: string | undefined;
};

export const makeSignedIntentDeferral = (): SignedIntentDeferral => ({
  current: undefined,
  atPoint: undefined,
});

const deferredAtPoint = (key: string, point: { readonly id: string }) =>
  `${key}@${point.id}`;

const deferralKey = (intent: {
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer;
}) =>
  `${intent.headerHash.toString("hex")}:${intent.intendedTxHash.toString("hex")}`;

/** Forward-append and resume disposition: pending exactly when the active
 * signed intent's TTL has been reached at this checkpoint, so the gate closes
 * and recovery reconciles its base's state-queue slot. Before the TTL the
 * normal confirmation path stays in charge, and after a deferral to the
 * correction path (its base was removed) it stays open until a rollback. SQL
 * only; the reason is stable while the journal is unchanged, so the owner's
 * retry backoff applies. */
export const expiredIntentReleaseDisposition = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly change: HistoryOwnerChange;
  readonly deferral: SignedIntentDeferral;
}) =>
  Effect.gen(function* () {
    // Only a rollback (or a fresh seed) can bring a removed base back.
    if (input.change.kind === "rollback" || input.change.kind === "seed")
      input.deferral.current = undefined;
    const intent = yield* activeSignedIntent;
    if (intent === undefined) return undefined;
    if (
      input.deferral.current === deferralKey(intent) ||
      input.deferral.atPoint ===
        deferredAtPoint(deferralKey(intent), input.change.after.head)
    )
      return undefined;
    const header = intent.headerHash.toString("hex");
    const ttl = signedTtl(intent.signedTxCbor);
    yield* reportOnce(
      `${input.binding.digest}:ttl`,
      ttl === undefined
        ? `Signed commit intent of block ${header} does not decode to a transaction with a finite validity upper bound; it is never replaced and stays fail-closed.`
        : undefined,
    );
    if (ttl === undefined || BigInt(input.change.after.head.slot) < ttl)
      return undefined;
    return {
      status: "pending" as const,
      reason: `Signed commit ${intent.intendedTxHash.toString("hex")} of block ${header} reached its validity upper bound (TTL slot ${ttl.toString()}) unobserved; whichever block holds its base's state-queue slot must be reconciled`,
    };
  });

/** Immutable identity of the active journal: every root and hash fixing the
 * native state it moved from and to, and the exact signed bytes. Status is
 * excluded; it is rechecked separately. */
const journalIdentity = (record: Pending.Record) => {
  const signed = record[C.SIGNED_TX_CBOR];
  return sha(
    eventHistoryCanonicalJson({
      headerHash: record[C.HEADER_HASH].toString("hex"),
      manifestId: record[C.DEPLOYMENT_MANIFEST_ID],
      baseTailHeaderHash: record[C.BASE_TAIL_HEADER_HASH].toString("hex"),
      baseTailOutRef: record[C.BASE_TAIL_OUT_REF],
      baseUtxosRoot: record[C.BASE_UTXOS_ROOT],
      expectedUtxosRoot: record[C.EXPECTED_UTXOS_ROOT],
      intendedTxHash: record[C.INTENDED_TX_HASH]?.toString("hex") ?? null,
      signedTxCborSha256: signed == null ? null : sha(signed),
      submittedTxHash: record[C.SUBMITTED_TX_HASH]?.toString("hex") ?? null,
      stateQueueLeaseToken: record[C.STATE_QUEUE_LEASE_TOKEN],
      replayBaseRoot: record.nativeMpfReplay?.baseRoot.toString("hex") ?? null,
      replayCandidateRoot:
        record.nativeMpfReplay?.candidateRoot.toString("hex") ?? null,
      replayEventLogDigest:
        record.nativeMpfReplay?.eventLogDigest.toString("hex") ?? null,
    }),
  );
};

/** The active journal and the aggregate of its replay base (its retained
 * parent journal's, or none, which makes the commit base recompute it). A
 * journal whose roots or parent do not prove its replay base cannot be
 * rewound and fails loudly: recovery retries it and the gate stays closed. */
const replaceableJournal = (headerHash: Buffer, manifestId: string) =>
  Effect.gen(function* () {
    const header = headerHash.toString("hex");
    const found = yield* Pending.retrieveByHeaderHash(headerHash, true);
    if (Option.isNone(found))
      return yield* Effect.fail(
        failure(`Signed-intent journal ${header} disappeared`),
      );
    const record = found.value;
    const replay = record.nativeMpfReplay;
    if (
      !UNLANDED_STATUSES.includes(record[C.STATUS]) ||
      record[C.DEPLOYMENT_MANIFEST_ID] !== manifestId ||
      record[C.SIGNED_TX_CBOR] == null ||
      record[C.INTENDED_TX_HASH] == null ||
      replay === undefined ||
      replay.baseRoot.toString("hex") !== record[C.BASE_UTXOS_ROOT] ||
      replay.candidateRoot.toString("hex") !== record[C.EXPECTED_UTXOS_ROOT]
    )
      return yield* Effect.fail(
        failure(
          `Signed-intent journal ${header} (status ${record[C.STATUS]}) has no unlanded status, deployment or native replay matching its journal roots`,
        ),
      );
    const baseTail = record[C.BASE_TAIL_HEADER_HASH];
    let parentAggregate: Pending.UtxoPayloadSizeAggregate | undefined;
    if (!baseTail.equals(ROOT_TAIL_HEADER_HASH)) {
      const parent = yield* Pending.retrieveByHeaderHash(baseTail);
      // A pruned parent journal leaves nothing to compare. The replay base is
      // still bound: the native plan's CAS moves the durable root only from
      // this journal's candidate root to its replay base, and the replay base
      // is checked above to equal the journal's recorded base root.
      if (Option.isSome(parent)) {
        if (
          parent.value[C.STATUS] === Pending.Status.Abandoned ||
          parent.value[C.EXPECTED_UTXOS_ROOT] !== record[C.BASE_UTXOS_ROOT]
        )
          return yield* Effect.fail(
            failure(
              `The replay base of signed-intent journal ${header} is not its retained parent's root`,
            ),
          );
        parentAggregate = parent.value.utxoPayloadAggregate;
      }
    }
    return { record, parentAggregate };
  });

type QueueNode = Readonly<{ node: SDK.StateQueueUTxO; headerHash: string }>;
type QueueView = Readonly<{ nodes: readonly QueueNode[]; root: QueueNode }>;
type StateQueueContracts = Pick<SDK.MidgardValidators, "stateQueue">;

/** Authenticates one state-queue output and names it by the header it
 * commits (the root by its confirmed header). */
const authenticateNode = (
  output: LedgerSnapshotOutput,
  contracts: StateQueueContracts,
) =>
  Effect.gen(function* () {
    const { policyId, spendingScriptAddress } = contracts.stateQueue;
    if (
      output.address !== spendingScriptAddress ||
      output.hasReferenceScript ||
      output.datum === undefined ||
      output.datumHash !== undefined
    )
      return yield* Effect.fail(
        failure(
          `State-queue output ${output.txHash}#${output.outputIndex.toString()} is not an inline-datum queue node`,
        ),
      );
    const node = yield* SDK.utxoToStateQueueUTxO(
      {
        txHash: output.txHash,
        outputIndex: output.outputIndex,
        address: output.address,
        assets: { ...output.assets },
        datum: output.datum,
      },
      policyId,
    );
    let headerHash: string;
    if (node.assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME) {
      if (node.datum.key !== "Empty")
        return yield* Effect.fail(failure("State-queue root has a key"));
      headerHash = (yield* SDK.getConfirmedStateFromStateQueueDatum(node.datum))
        .data.headerHash;
    } else {
      headerHash = yield* SDK.hashBlockHeader(
        yield* SDK.getHeaderFromStateQueueDatum(node.datum),
      );
      if (
        node.datum.key === "Empty" ||
        node.datum.key.Key.key !== headerHash ||
        node.assetName !==
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`
      )
        return yield* Effect.fail(
          failure(`State-queue node ${headerHash} is not keyed by its header`),
        );
    }
    return { node, headerHash } satisfies QueueNode;
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : failure("State-queue capture does not authenticate", cause),
    ),
  );

/** Authenticates every state-queue output of the exact-point capture. Any
 * malformed queue output fails the whole view. */
const authenticateQueue = (
  outputs: readonly LedgerSnapshotOutput[],
  contracts: StateQueueContracts,
) =>
  Effect.gen(function* () {
    const { policyId } = contracts.stateQueue;
    const nodes: QueueNode[] = [];
    for (const output of outputs) {
      if (!Object.keys(output.assets).some((unit) => unit.startsWith(policyId)))
        continue;
      const named = yield* authenticateNode(output, contracts);
      if (nodes.some((known) => known.headerHash === named.headerHash))
        return yield* Effect.fail(
          failure(`State-queue header ${named.headerHash} appears twice`),
        );
      nodes.push(named);
    }
    const roots = nodes.filter(
      ({ node }) => node.assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    );
    if (roots.length !== 1)
      return yield* Effect.fail(
        failure("The state queue must have exactly one root"),
      );
    return { nodes, root: roots[0]! } satisfies QueueView;
  });

/** The node a journal's signed commit created for its own block: that
 * commit's output carrying the block's node token, before any later
 * transaction continued or merged it. Local finalization binds it to the
 * journal by every header root. Bytes that do not hash to the journal's
 * intended transaction or create no such node fail closed. */
const signedCommitNode = (
  record: Pending.Record,
  contracts: StateQueueContracts,
) =>
  Effect.gen(function* () {
    const header = record[C.HEADER_HASH].toString("hex");
    const intended = record[C.INTENDED_TX_HASH]?.toString("hex");
    const signed = record[C.SIGNED_TX_CBOR];
    const unit = `${contracts.stateQueue.policyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;
    const outputs = yield* Effect.try({
      try: () => {
        if (intended === undefined || signed == null)
          throw new Error("the journal retains no signed commit");
        const tx = CML.Transaction.from_cbor_bytes(signed);
        const body = tx.body();
        try {
          if (CML.hash_transaction(body).to_hex() !== intended)
            throw new Error("its signed bytes are not its intended commit");
          const all = body.outputs();
          return Array.from({ length: all.len() }, (_, index) =>
            coreToTxOutput(all.get(index)),
          );
        } finally {
          body.free();
          tx.free();
        }
      },
      catch: (cause) =>
        failure(
          `Block ${header} landed, but its node cannot be read from its signed commit`,
          cause,
        ),
    });
    const outputIndex = outputs.findIndex(
      (output) => output.assets[unit] === 1n,
    );
    const output = outputs[outputIndex];
    if (output === undefined)
      return yield* Effect.fail(
        failure(`The signed commit of block ${header} creates no node for it`),
      );
    const created = yield* authenticateNode(
      {
        txHash: intended!,
        outputIndex,
        address: output.address,
        assets: { ...output.assets },
        ...(output.datum == null ? {} : { datum: output.datum }),
        ...(output.datumHash == null ? {} : { datumHash: output.datumHash }),
        hasReferenceScript: output.scriptRef != null,
      },
      contracts,
    );
    if (created.headerHash !== header)
      return yield* Effect.fail(
        failure(`The signed commit of block ${header} creates another node`),
      );
    return created;
  });

type Decision =
  | Readonly<{ kind: "landed"; node: QueueNode; evidence: string }>
  | Readonly<{ kind: "wait"; reason: string }>
  /** `sticky`: the correction path owns the journal, so it is not
   * re-read until a rollback or restart. Otherwise the gate stays open and
   * the next source point decides again. */
  | Readonly<{ kind: "defer"; reason: string; sticky: boolean }>
  | Readonly<{ kind: "replace"; cause: string }>
  | Readonly<{ kind: "revive"; revived: Pending.Record; node: QueueNode }>;

/** Authenticated evidence of the chain at the checkpoint: its exact-point
 * queue, and whether the owner's journaled canonical history holds the
 * signed commit (presence is proof it landed; absence proves nothing, as
 * retention is bounded). The correction observer's view is read in `decide`
 * itself, under a share lock, so a re-derivation sees any change. */
type ReleaseEvidence = Readonly<{
  queue: QueueView;
  inCanonicalHistory: boolean;
  contracts: StateQueueContracts;
  rewindAuthority: StateQueueCorrectionRewindAuthority;
}>;

/** Whether the retained canonical history (complete transaction rosters from
 * its anchor to the checkpoint head) includes `txHash` as a valid (inputs
 * spending) transaction. Unavailable coverage is no evidence. */
const includedInCanonicalHistory = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  txHash: string,
) =>
  loadCanonicalHistoryCoverage(binding, checkpoint).pipe(
    Effect.map((coverage) =>
      coverage.blocks.some((block) =>
        block.transactions.some(
          (tx) => tx.txHash === txHash && tx.spends === "inputs",
        ),
      ),
    ),
    Effect.catchAll((cause) =>
      Effect.logDebug(
        `Canonical history coverage unavailable as signed-intent evidence: ${formatUnknownError(cause)}`,
      ).pipe(Effect.as(false)),
    ),
  );

/** Reads which block holds the active journal's base slot (see the module
 * comment). Read-only; runs in the recovery transaction. */
const decide = (record: Pending.Record, evidence: ReleaseEvidence) =>
  Effect.gen(function* () {
    const { queue, contracts } = evidence;
    const header = record[C.HEADER_HASH].toString("hex");
    const base = record[C.BASE_TAIL_HEADER_HASH].toString("hex");
    const find = (hash: string) =>
      queue.nodes.find((entry) => entry.headerHash === hash);
    const own = find(header);
    if (own !== undefined && own !== queue.root)
      return {
        kind: "landed",
        node: own,
        evidence: "its node is on the queue",
      } satisfies Decision;
    const observer = yield* loadStateQueueCorrectionObserverState(
      evidence.rewindAuthority,
      true,
    );
    const transitions =
      observer.kind === "observed"
        ? [...observer.state.pending, ...observer.state.admitted]
        : [];
    const removing = (hash: string, merge: boolean) =>
      transitions.find(
        (transition) =>
          (transition.transitionKind === "merge") === merge &&
          transition.removedHeaderHashes.includes(hash),
      );
    const correctionOfBlock = removing(header, false);
    if (correctionOfBlock !== undefined)
      return {
        kind: "defer",
        sticky: true,
        reason: `state-queue correction ${correctionOfBlock.transactionHash} removed it after it landed, so the correction path reconciles it`,
      } satisfies Decision;
    const mergeOfBlock = removing(header, true);
    const landed = evidence.inCanonicalHistory
      ? "its signed commit is in the journaled canonical history"
      : queue.root.headerHash === header
        ? "the confirmed state is its header"
        : mergeOfBlock !== undefined
          ? `merge ${mergeOfBlock.transactionHash} folded it into the confirmed state`
          : undefined;
    if (landed !== undefined)
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: landed,
      } satisfies Decision;
    const correctionOfBase = removing(base, false);
    if (correctionOfBase !== undefined) {
      const baseJournal = yield* Pending.retrieveByHeaderHash(
        Buffer.from(base, "hex"),
      );
      if (
        Option.isSome(baseJournal) &&
        baseJournal.value[C.STATUS] !== Pending.Status.Abandoned
      )
        return {
          kind: "defer",
          sticky: true,
          reason: `state-queue correction ${correctionOfBase.transactionHash} removed its base ${base}, and the correction path reconciles this node's removed block with its unlanded descendants`,
        } satisfies Decision;
      return {
        kind: "replace",
        cause: `state-queue correction ${correctionOfBase.transactionHash} consumed the node of its base ${base}, which this node does not journal`,
      } satisfies Decision;
    }

    /** `winner` holds D's slot. A merged slot's winner may have left the
     * queue too; its node is then the one its own signed commit created. */
    const successor = (winner: string, merged: boolean) =>
      Effect.gen(function* () {
        if (winner === header)
          return merged
            ? ({
                kind: "landed",
                node: yield* signedCommitNode(record, contracts),
                evidence: `it was its merged base ${base}'s successor`,
              } satisfies Decision)
            : ({
                kind: "wait",
                reason: `its base ${base} links to it but its node is absent`,
              } satisfies Decision);
        const found = yield* Pending.retrieveByHeaderHash(
          Buffer.from(winner, "hex"),
        );
        if (Option.isNone(found))
          return {
            kind: "replace",
            cause: `foreign block ${winner} holds its base's slot`,
          } satisfies Decision;
        const revived = found.value;
        if (
          revived[C.STATUS] !== Pending.Status.Abandoned ||
          journalAbandonment(revived) !== "replacement" ||
          !revived[C.BASE_TAIL_HEADER_HASH].equals(
            record[C.BASE_TAIL_HEADER_HASH],
          ) ||
          revived[C.BASE_TAIL_OUT_REF] !== record[C.BASE_TAIL_OUT_REF] ||
          revived[C.BASE_UTXOS_ROOT] !== record[C.BASE_UTXOS_ROOT]
        )
          return {
            kind: "wait",
            reason: `this node's block ${winner} (status ${revived[C.STATUS]}, abandonment ${revived[C.STATUS] === Pending.Status.Abandoned ? journalAbandonment(revived) : "none"}) holds its base's slot but is not a replaced block on the same base`,
          } satisfies Decision;
        // Scope limit: reconcile only while nothing built on this base has
        // written local finalization. Fatal before anything is persisted.
        if (record[C.STATUS] === Pending.Status.SubmittedUnconfirmed)
          return yield* Effect.fail(
            new SignedIntentReplacementIntegrityError(
              winner,
              `active block ${header} built on the same base is already locally finalized`,
            ),
          );
        const sql = yield* SqlClient.SqlClient;
        const siblings = yield* sql<{
          header_hash: Buffer;
          status: Pending.Status;
        }>`SELECT header_hash, status FROM pending_block_finalizations
          WHERE base_tail_out_ref = ${record[C.BASE_TAIL_OUT_REF]}
            AND header_hash NOT IN (${record[C.HEADER_HASH]}, ${revived[C.HEADER_HASH]})
          ORDER BY created_at, header_hash`;
        const blocking = siblings.find(({ status }) =>
          REVIVAL_BLOCKING_SIBLING_STATUSES.includes(status),
        );
        if (blocking !== undefined)
          return yield* Effect.fail(
            new SignedIntentReplacementIntegrityError(
              winner,
              `block ${blocking.header_hash.toString("hex")} built on the same base is already ${blocking.status}`,
            ),
          );
        const node =
          find(winner) ??
          (merged ? yield* signedCommitNode(revived, contracts) : undefined);
        if (node === undefined)
          return {
            kind: "wait",
            reason: `the queue names ${winner} as its base's successor but its node is absent`,
          } satisfies Decision;
        return { kind: "revive", revived, node } satisfies Decision;
      });

    const tail = find(base);
    if (tail !== undefined) {
      const next = tail.node.datum.next;
      if (next === "Empty")
        return {
          kind: "replace",
          cause: `its base ${base} is still the queue tail`,
        } satisfies Decision;
      return yield* successor(next.Key.key, false);
    }
    // D left the queue. Only a recorded removal says how; absence alone
    // never decides. The observer records it independently of the history
    // gate, so the gate stays open and the next source point decides again.
    if (observer.kind === "blocked")
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue and the state-queue correction observer has no usable view of why yet (${observer.reason})`,
      } satisfies Decision;
    const mergeOfBase = removing(base, true);
    if (mergeOfBase === undefined)
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue and the state-queue correction observer has recorded neither a correction nor a merge of it yet`,
      } satisfies Decision;
    const index = mergeOfBase.previousQueue.findIndex(
      (node) => node.headerHash === base,
    );
    const merged = mergeOfBase.previousQueue[index + 1]?.headerHash ?? null;
    if (merged === null)
      return {
        kind: "replace",
        cause: `its base ${base} was merged into the confirmed state by ${mergeOfBase.transactionHash} while it was still the queue tail`,
      } satisfies Decision;
    return yield* successor(merged, true);
  });

/**
 * Recovery preparation: once the active signed intent's TTL is reached at
 * this checkpoint, reads the exact-point queue and confirms, replaces or
 * revives (see the module comment). Defers while a correction rewind is owed
 * (it may resolve this very journal) or while another plan is retained (its
 * owner resumes it first).
 */
export const prepareExpiredIntentRelease = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly contracts: Pick<SDK.MidgardValidators, "stateQueue">;
  readonly deferral: SignedIntentDeferral;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation, config } = input;
    const reportKey = `${input.binding.digest}:decision`;
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
        const rewind = yield* stateQueueCorrectionRewindDisposition(
          input.rewindAuthority,
        );
        if (rewind !== undefined) return undefined;
        const intent = yield* activeSignedIntent;
        const retained = yield* retainedPreparedRecoveryPlan(
          input.binding.digest,
        );
        if (
          retained !== undefined &&
          (retained.kind !== "signed_intent_release" ||
            intent === undefined ||
            retained.headerHash !== intent.headerHash.toString("hex"))
        )
          return undefined;
        if (intent === undefined) return undefined;
        const ttl = signedTtl(intent.signedTxCbor);
        if (ttl === undefined || BigInt(checkpoint.head.slot) < ttl)
          return undefined;
        const journal = yield* replaceableJournal(
          intent.headerHash,
          checkpoint.manifestId,
        );
        return {
          ...journal,
          ttl,
          identity: journalIdentity(journal.record),
          retainedPlan: retained !== undefined,
        };
      }),
    );
    if (derived === undefined) return;
    const { record } = derived;
    const headerHash = record[C.HEADER_HASH];
    const header = headerHash.toString("hex");
    const signedTx = record[C.INTENDED_TX_HASH]!.toString("hex");
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
          binding: input.binding,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure(
          `Exact-point state-queue capture failed: ${formatUnknownError(cause, { includeCause: true })}`,
          cause,
        ),
    });
    yield* preparation.assertCurrent;
    const queue = yield* authenticateQueue(
      capture.ledger.outputs,
      input.contracts,
    );
    const evidence: ReleaseEvidence = {
      queue,
      inCanonicalHistory: yield* owned(
        includedInCanonicalHistory(input.binding, checkpoint, signedTx),
      ),
      contracts: input.contracts,
      rewindAuthority: input.rewindAuthority,
    };
    // Re-derives the decision and the unchanged journal inside the caller's
    // transaction.
    const current = (expected: Decision["kind"]) =>
      Effect.gen(function* () {
        const journal = yield* replaceableJournal(
          headerHash,
          checkpoint.manifestId,
        );
        if (journalIdentity(journal.record) !== derived.identity)
          return yield* Effect.fail(
            failure(`Signed-intent journal ${header} identity changed`),
          );
        const decision = yield* decide(journal.record, evidence);
        if (decision.kind !== expected)
          return yield* Effect.fail(
            failure(
              `Signed-intent decision for ${header} changed from ${expected} to ${decision.kind}`,
            ),
          );
        return { ...journal, decision };
      });
    const decision = yield* owned(decide(record, evidence));
    const globals = yield* Globals;
    const context = `signed commit ${signedTx} of block ${header} (TTL slot ${derived.ttl.toString()}, head slot ${checkpoint.head.slot.toString()})`;

    if (decision.kind === "wait") {
      yield* reportOnce(
        reportKey,
        `Cannot reconcile ${context} yet: ${decision.reason}. The history gate stays closed.`,
      );
      return;
    }
    if (decision.kind === "defer") {
      const key = deferralKey({
        headerHash,
        intendedTxHash: record[C.INTENDED_TX_HASH]!,
      });
      if (decision.sticky) input.deferral.current = key;
      else input.deferral.atPoint = deferredAtPoint(key, checkpoint.head);
      yield* reportOnce(
        reportKey,
        decision.sticky
          ? `Not replacing ${context}: ${decision.reason}. The history gate stays open and its journal stays active until the correction path resolves it.`
          : `Not replacing ${context} yet: ${decision.reason}. The history gate stays open, its journal stays active, and the next source point decides again.`,
      );
      return;
    }
    if (decision.kind === "landed") {
      const serialized = yield* serializeStateQueueUTxO(decision.node.node);
      const requiresLocalFinalization = yield* owned(
        current("landed").pipe(
          Effect.tap(() =>
            // A replacement prepared before the block was seen to land may
            // already have restored the base root natively, but its SQL repair
            // never ran, so the journal is intact. Local finalization replays
            // it natively from either root: the replacement is discarded, not
            // resumed.
            derived.retainedPlan
              ? discardPreparedHistoryRecoveryPlan(
                  checkpoint,
                  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
                  header,
                ).pipe(
                  Effect.zipRight(
                    Effect.logWarning(
                      `Discarded the prepared replacement of block ${header}: it landed.`,
                    ),
                  ),
                )
              : Effect.void,
          ),
          Effect.flatMap((journal) =>
            recordConfirmedPendingBlock(
              journal.record,
              Buffer.from(decision.node.node.utxo.txHash, "hex"),
            ),
          ),
        ),
      );
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      yield* Ref.set(
        globals.LOCAL_FINALIZATION_PENDING,
        requiresLocalFinalization,
      );
      yield* Ref.set(
        globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
        requiresLocalFinalization ? serialized : "",
      );
      yield* reportOnce(reportKey, undefined);
      yield* Effect.logInfo(
        `Recorded the L1 observation of ${context}: ${decision.evidence}.`,
      );
      return;
    }

    const revived = decision.kind === "revive" ? decision.revived : undefined;
    const revivedBlock: SerializedStateQueueUTxO | undefined =
      decision.kind === "revive"
        ? yield* serializeStateQueueUTxO(decision.node.node)
        : undefined;
    const replacementDigest = signedIntentReplacementDigest(record)!;
    const targetRoot = record[C.BASE_UTXOS_ROOT];
    const evidenceDigest = sha(
      eventHistoryCanonicalJson({
        domain: REPLACEMENT_EVIDENCE_DOMAIN,
        decision: decision.kind,
        headerHash: header,
        revivedHeaderHash: revived?.[C.HEADER_HASH].toString("hex") ?? null,
        queue: queue.nodes.map(({ node, headerHash }) => ({
          outRef: `${node.utxo.txHash}#${node.utxo.outputIndex.toString()}`,
          headerHash,
        })),
        point: checkpoint.head,
        snapshot: checkpoint.capture.snapshotDigest,
      }),
    );
    if (config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
    let owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (owner === undefined) {
      // Open only retained native bytes; never genesis-bootstrap or replay the
      // replaced journal on this path. Create validates the durable marker.
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
              failure("Retained native owner could not open", cause),
          });
          yield* Ref.set(globals.NATIVE_MPF_OWNER, opened);
          return opened;
        }),
      );
    }
    yield* preparation.assertCurrent;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) => failure("Retained native diagnostics failed", cause),
    });
    // The plan binds only the replaced journal, so a crash between native
    // restoration and the SQL repair resumes it whichever block then wins.
    const plan = yield* owned(
      current(decision.kind).pipe(
        Effect.zipRight(
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            {
              bindingDigest: input.binding.digest,
              manifestId: checkpoint.manifestId,
              headerHash: header,
              signedTransactionHash: signedTx,
              signedTransactionCborSha256: sha(record[C.SIGNED_TX_CBOR]!),
              targetRoot,
              journalDigest: derived.identity,
            },
            evidenceDigest,
            {
              durableRoot: diagnostics.durableRoot,
              candidateRoot: record[C.EXPECTED_UTXOS_ROOT],
            },
            SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
          ),
        ),
      ),
    );
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        const { parentAggregate } = yield* current(decision.kind);
        const sql = yield* SqlClient.SqlClient;
        // Abandons the journal under its replacement digest and reopens every
        // member (L2 transactions, deposits, withdrawals, forced
        // transactions), restoring the speculative ledger, in this
        // transaction. Its signed content is kept.
        const results = yield* reincludeStateQueueCorrectedBlocks([
          {
            headerHash: header,
            transitionDigest: replacementDigest,
            kind: "unlanded",
          },
        ]);
        if (
          results.length !== 1 ||
          !results[0]!.journalFound ||
          results[0]!.abandonedFromStatus === undefined
        )
          return yield* Effect.fail(
            failure(`Signed-intent replacement did not abandon ${header}`),
          );
        yield* MutationJobsDB.abandonLocalBlockFinalization(
          headerHash,
          `${context} can no longer land: ${decision.kind === "replace" ? decision.cause : `replaced block ${revived![C.HEADER_HASH].toString("hex")} holds its base's slot`}`,
        );
        // A replaced block can never be continued; retire only its lease.
        yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
        // The SQL marker follows the native root the plan's CAS proved, from
        // the replaced journal's candidate root (or already at its target
        // when a resumed plan re-runs this repair) and nothing else.
        const engine = yield* sql`UPDATE mpf_engine_state
          SET root_hex = ${targetRoot},
            utxo_payload_entry_count = ${parentAggregate?.entryCount ?? null},
            utxo_payload_encoded_tuple_bytes = ${parentAggregate?.encodedTupleBytes ?? null},
            updated_at = NOW()
          WHERE store_name = 'ledger'
            AND root_hex IN (${record[C.EXPECTED_UTXOS_ROOT]}, ${targetRoot})
          RETURNING store_name`;
        if (engine.length !== 1)
          return yield* Effect.fail(
            failure(
              "Native SQL marker changed before the signed-intent replacement",
            ),
          );
        // The winner takes its members back; its SQL marker moves to its
        // candidate root and native replay follows at local finalization.
        if (revived !== undefined)
          yield* reviveReplacedCanonicalJournal(revived[C.HEADER_HASH]);
      }),
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        // Replaced: no journal is active; the commit preflight re-derives the
        // tail and boundary from L1 and selects the reopened members again.
        // Revived: local finalization replays the winner first.
        yield* Ref.set(
          globals.LOCAL_FINALIZATION_PENDING,
          revivedBlock !== undefined,
        );
        yield* Ref.set(
          globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
          revivedBlock ?? "",
        );
        yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
        yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
      }),
    });
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logWarning(
      decision.kind === "replace"
        ? `Replaced ${context}: ${decision.cause}. Restored native root ${targetRoot} and reopened its members for recommit.`
        : `Revived replaced block ${revived![C.HEADER_HASH].toString("hex")}: it holds the base slot of ${context}, which was abandoned; local finalization replays the winner.`,
    );
  });
