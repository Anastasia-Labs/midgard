import { createHash } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Option, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import {
  CANONICAL_COVERAGE_UNAVAILABLE,
  loadCanonicalHistoryCoverage,
} from "../database/eventHistoryCanonicalCoverage.js";
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
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "./mpf-native-owner/index.js";
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
 * evidence decides. Evidence bound to the checkpoint decides first (the
 * exact-point queue and the canonical history the owner journaled); the
 * correction observer's persisted view of every state-queue removal
 * (corrections and merges, pending or admitted), which is not bound to the
 * checkpoint, only explains a correction of E or why neither E nor D is on
 * the checkpoint's queue.
 *  - E's own node is on the queue, or the confirmed state is E's header: E
 *    landed; its observation is recorded.
 *  - A correction removed E: E landed and was removed; the correction path
 *    owns its journal, so this defers (below). Never a landed block.
 *  - E's signed commit is in the journaled canonical history, or (neither E
 *    nor D on the queue) an observed merge folded E: E landed; its
 *    observation is recorded against the node its signed commit created, and
 *    local finalization replays it.
 *  - A correction removed D (neither E nor D on the queue): when this node
 *    journals D, the correction path owns E's journal (its rewind proves and
 *    abandons an unlanded descendant of a removed block), so this defers to
 *    it without holding the gate closed. An admitted correction's deferral
 *    is remembered for this journal until a rollback (the only way D
 *    returns) or a runtime restart; a pending one only until the observer's
 *    view changes, since it may be retracted. Without a journal of D nothing
 *    else ever resolves E, and the correction consumed the node E spends: E
 *    is replaced.
 *  - D's `next` is still empty, or holds a block that is not this node's
 *    replaced sibling of E (a foreign block): E is replaced. Its journal is
 *    abandoned under its replacement digest, its local-finalization job row
 *    and lease are retired, every member is reopened, the native root and SQL
 *    marker return to its base; the commit worker then builds anew.
 *  - D's `next` is an earlier journal of this node that was replaced on the
 *    same base (any generation): that block won after all (a rollback brought
 *    it back). The active journal is abandoned as above and the winner is
 *    revived with its members taken back; local finalization then replays it.
 *  - Neither E nor D on the queue, and the confirmed state links to D (a
 *    merge sets the confirmed predecessor to the header it folded over): the
 *    block it confirms took D's slot after D was merged, and decides as D's
 *    `next` does. This is checkpoint-bound and covers a D no removal ever
 *    names, such as the root E was built on, but only until the next merge:
 *    no queue output or transition links a confirmed header to anything
 *    after that.
 *  - A replaced sibling of E (this node's block built on the same base
 *    output, so its commit spends what E's spends) shows it landed, by the
 *    evidence the reviver reads: it holds the slot and is revived as D's
 *    `next` would be. This needs no link from the slot to the base.
 *  - E's base output is a root that an observed transition left with an
 *    empty queue (E was built on the root): the next observed transition
 *    names the block appended first after it, which spent that output, and it
 *    decides as D's `next` does (or, removed by a correction, E is replaced;
 *    this node's block only once that correction is admitted). The merge of
 *    D, made before E was built on the root, says nothing about E's slot.
 *  - D was merged into the confirmed state (an observed merge removed it):
 *    its successor at that merge decides as D's `next` does, and a D merged
 *    while it was still the tail means E can never land: E is replaced.
 *  - D absent with none of that recorded: nothing is decided and the gate
 *    stays open (E stays active, so nothing is built) until the observer's
 *    view changes. Nothing ever resolves it when E was built on a root that
 *    no observed transition produced (the genesis root, or one produced
 *    before the observer first ran), a foreign block took that slot, and two
 *    or more merges passed it before this decision.
 *  - Anything else (another own block of a different kind in D's slot, D's
 *    successor node absent) keeps the gate closed and says why.
 * A signed commit is never replaced before its TTL, and never on wall-clock
 * time or queue absence alone. With E's replacement plan already retained, a
 * landed E discards it (after replaying E natively when the plan was prepared
 * from E's candidate root, so its rewind may have run), and a deferral to the
 * correction path resumes it instead, since the correction path waits for
 * every retained plan.
 *
 * With no journal active, a replaced block of this node can still win its
 * base's slot (it landed late, or a rollback brought it back);
 * `prepareReplacedBlockRevival` revives it on the same checkpoint-bound
 * evidence, or an admitted merge that saw it on the queue.
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

/** Whether any journal is active, landed or not. */
const anyActiveJournal = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
    FROM pending_block_finalizations WHERE status IN ${sql.in(ACTIVE_STATUSES)}
    LIMIT 1`;
  return rows.length > 0;
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
 * reconciliation defers: to the correction path (`current`, until a rollback;
 * only on an admitted correction), or until the correction observer's
 * persisted view changes (`untilObserved`: it has not yet recorded why its
 * base left the queue, or has recorded only a correction not yet admitted;
 * without a change of that view or a rollback, the same evidence decides the
 * same way, so nothing is captured again). `revival`: the replaced blocks and
 * source point at which no authenticated evidence showed one of them holding
 * its base's slot. Owned by one history runtime: a restart starts empty. */
export type SignedIntentDeferral = {
  current: string | undefined;
  untilObserved: string | undefined;
  revival: string | undefined;
};

export const makeSignedIntentDeferral = (): SignedIntentDeferral => ({
  current: undefined,
  untilObserved: undefined,
  revival: undefined,
});

/** The correction observer's persisted view, named by its state digest (or
 * why it has none). */
const observerFingerprint = (authority: StateQueueCorrectionRewindAuthority) =>
  loadStateQueueCorrectionObserverState(authority).pipe(
    Effect.map((observer) =>
      observer.kind === "observed"
        ? `observed:${observer.state.stateDigest}`
        : `blocked:${observer.reason}`,
    ),
  );

const deferredUntilObserved = (key: string, fingerprint: string) =>
  `${key}#${fingerprint}`;

const deferralKey = (intent: {
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer;
}) =>
  `${intent.headerHash.toString("hex")}:${intent.intendedTxHash.toString("hex")}`;

/** Forward-append and resume disposition: pending exactly when the active
 * signed intent's TTL has been reached at this checkpoint, so the gate closes
 * and recovery reconciles its base's state-queue slot. Before the TTL the
 * normal confirmation path stays in charge, and after a deferral to the
 * correction path (its base was removed) it stays open until a rollback, and
 * after a deferral to the correction observer until its view changes. SQL
 * only; the reason is stable while the journal is unchanged, so the owner's
 * retry backoff applies. */
export const expiredIntentReleaseDisposition = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly change: HistoryOwnerChange;
  readonly deferral: SignedIntentDeferral;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    // Only a rollback (or a fresh seed) can bring a removed base back, or
    // change the evidence a deferral was decided on without the observer.
    if (input.change.kind === "rollback" || input.change.kind === "seed") {
      input.deferral.current = undefined;
      input.deferral.untilObserved = undefined;
      input.deferral.revival = undefined;
    }
    const intent = yield* activeSignedIntent;
    if (intent === undefined) return undefined;
    const key = deferralKey(intent);
    if (input.deferral.current === key) return undefined;
    if (
      input.deferral.untilObserved?.startsWith(`${key}#`) === true &&
      input.deferral.untilObserved ===
        deferredUntilObserved(
          key,
          yield* observerFingerprint(input.rewindAuthority),
        )
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

/** A queue output named by the header it commits and the header that header
 * links to (for the root, the confirmed header's predecessor, which a merge
 * sets to the header it folded over). */
type QueueNode = Readonly<{
  node: SDK.StateQueueUTxO;
  headerHash: string;
  prevHeaderHash: string;
}>;
type QueueView = Readonly<{ nodes: readonly QueueNode[]; root: QueueNode }>;
type StateQueueContracts = Pick<SDK.MidgardValidators, "stateQueue">;

/** Authenticates one state-queue output and names it by the header it
 * commits and that header's predecessor (the root by its confirmed state). */
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
    let prevHeaderHash: string;
    if (node.assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME) {
      if (node.datum.key !== "Empty")
        return yield* Effect.fail(failure("State-queue root has a key"));
      const confirmed = (yield* SDK.getConfirmedStateFromStateQueueDatum(
        node.datum,
      )).data;
      headerHash = confirmed.headerHash;
      prevHeaderHash = confirmed.prevHeaderHash;
    } else {
      const header = yield* SDK.getHeaderFromStateQueueDatum(node.datum);
      headerHash = yield* SDK.hashBlockHeader(header);
      prevHeaderHash = header.prevHeaderHash;
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
    return { node, headerHash, prevHeaderHash } satisfies QueueNode;
  });

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
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : failure("State-queue capture does not authenticate", cause),
    ),
  );

/** The node a journal's signed commit created for its own block: that
 * commit's output carrying the block's node token, before any later
 * transaction continued or merged it. Local finalization binds it to the
 * journal by every header root. Bytes that do not hash to the journal's
 * intended transaction or create no such node fail closed. Startup hydration
 * re-derives an observed journal's node the same way (a merged node is on no
 * queue to read back). */
export const signedCommitNode = (
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
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : failure(
            `The node the retained signed commit of block ${record[C.HEADER_HASH].toString("hex")} creates does not authenticate`,
            cause,
          ),
    ),
  );

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
 * queue, and which signed commits (the active journal's and its replaced
 * siblings') the owner's journaled canonical history holds (presence is proof
 * a commit landed; absence proves nothing, as retention is bounded). The
 * correction observer's view is read in `decide` itself, under a share lock,
 * so a re-derivation sees any change. */
type ReleaseEvidence = Readonly<{
  queue: QueueView;
  canonicalHistory: ReadonlySet<string>;
  contracts: StateQueueContracts;
  rewindAuthority: StateQueueCorrectionRewindAuthority;
}>;

/** Which of `txHashes` the retained canonical history (complete transaction
 * rosters from its anchor to the checkpoint head) includes as valid (inputs
 * spending) transactions. Unavailable coverage is no evidence (none is
 * included); any other failure (a database error, which aborts the owned
 * transaction) fails the attempt, which recovery retries. */
const includedInCanonicalHistory = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  txHashes: readonly string[],
) =>
  txHashes.length === 0
    ? Effect.succeed<ReadonlySet<string>>(new Set())
    : loadCanonicalHistoryCoverage(binding, checkpoint).pipe(
        Effect.map((coverage): ReadonlySet<string> => {
          const wanted = new Set(txHashes);
          const included = new Set<string>();
          for (const block of coverage.blocks)
            for (const tx of block.transactions)
              if (tx.spends === "inputs" && wanted.has(tx.txHash))
                included.add(tx.txHash);
          return included;
        }),
        Effect.catchIf(
          (cause) =>
            cause instanceof DatabaseError &&
            cause.message === CANONICAL_COVERAGE_UNAVAILABLE,
          (cause) =>
            Effect.logDebug(
              `Canonical history coverage unavailable as signed-intent evidence: ${formatUnknownError(cause)}`,
            ).pipe(Effect.as<ReadonlySet<string>>(new Set())),
        ),
      );

type ObserverView = Effect.Effect.Success<
  ReturnType<typeof loadStateQueueCorrectionObserverState>
>;

/** Authenticated evidence that this node's replaced block `record` landed:
 * its node on the exact-point queue (returned), the confirmed state equal to
 * its header, its signed commit in the journaled canonical history, or an
 * admitted (final) state-queue transition that merged it or saw it on the
 * queue. A block a recorded correction removed never counts: its members stay
 * reopened, which is what that correction's path does anyway. */
const replacedBlockLanding = (
  record: Pending.Record,
  queue: QueueView,
  observer: ObserverView,
  canonicalHistory: ReadonlySet<string>,
): { onQueue: QueueNode | undefined; evidence: string } | undefined => {
  const header = record[C.HEADER_HASH].toString("hex");
  const pending = observer.kind === "observed" ? observer.state.pending : [];
  const admitted = observer.kind === "observed" ? observer.state.admitted : [];
  if (
    [...pending, ...admitted].some(
      (transition) =>
        transition.transitionKind !== "merge" &&
        transition.removedHeaderHashes.includes(header),
    )
  )
    return undefined;
  const onQueue = queue.nodes.find(
    (entry) => entry.headerHash === header && entry !== queue.root,
  );
  if (onQueue !== undefined)
    return { onQueue, evidence: "its node is on the queue" };
  const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
  const seen = admitted.find(
    (transition) =>
      (transition.transitionKind === "merge" &&
        transition.removedHeaderHashes.includes(header)) ||
      [...transition.previousQueue, ...transition.nextQueue].some(
        (node) => node.headerHash === header,
      ),
  );
  const evidence =
    queue.root.headerHash === header
      ? "the confirmed state is its header"
      : signed !== undefined && canonicalHistory.has(signed)
        ? "its signed commit is in the journaled canonical history"
        : seen !== undefined
          ? `admitted state-queue transition ${seen.transactionHash} saw it on the queue`
          : undefined;
  return evidence === undefined ? undefined : { onQueue: undefined, evidence };
};

/** This node's journals built on the same base output as `record` (so their
 * commits spend what its commit spends) that were abandoned for replacement.
 * Sorted by header. */
const replacedSiblings = (record: Pending.Record) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE base_tail_out_ref = ${record[C.BASE_TAIL_OUT_REF]}
        AND status = ${Pending.Status.Abandoned}
        AND header_hash <> ${record[C.HEADER_HASH]}
      ORDER BY header_hash`;
    const siblings: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        siblings.push(found.value);
    }
    return siblings;
  });

/** L1 order of two recorded state-queue transitions. */
const chainOrder = (
  left: SDK.StateQueueAuthenticatedTransition,
  right: SDK.StateQueueAuthenticatedTransition,
) => {
  const block = BigInt(left.blockNo) - BigInt(right.blockNo);
  const index =
    block === 0n
      ? BigInt(left.transactionIndex) - BigInt(right.transactionIndex)
      : block;
  return index === 0n ? 0 : index < 0n ? -1 : 1;
};

/** Reads which block holds the active journal's base slot (see the module
 * comment). Checkpoint-bound evidence decides first; the correction observer's
 * view (not bound to the checkpoint, and pending transitions may still be
 * retracted) only explains a correction of E, or why neither E nor its base D
 * is on the checkpoint's queue. Read-only; runs in the recovery transaction. */
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
    if (queue.root.headerHash === header)
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: "the confirmed state is its header",
      } satisfies Decision;
    const observer = yield* loadStateQueueCorrectionObserverState(
      evidence.rewindAuthority,
      true,
    );
    /** The observed removal of `hash` of one kind, an admitted one first. */
    const removing = (hash: string, merge: boolean) => {
      if (observer.kind !== "observed") return undefined;
      const matches = (transition: SDK.StateQueueAuthenticatedTransition) =>
        (transition.transitionKind === "merge") === merge &&
        transition.removedHeaderHashes.includes(hash);
      const admitted = observer.state.admitted.find(matches);
      if (admitted !== undefined)
        return { transition: admitted, admitted: true };
      const pending = observer.state.pending.find(matches);
      return pending === undefined
        ? undefined
        : { transition: pending, admitted: false };
    };
    /** Only an admitted correction is resolved by the correction path, so
     * only it makes the deferral sticky. A pending one may be retracted; the
     * next change of the observer's view decides again. */
    const deferToCorrection = (
      removal: NonNullable<ReturnType<typeof removing>>,
      removed: string,
    ) =>
      ({
        kind: "defer",
        sticky: removal.admitted,
        reason: removal.admitted
          ? `admitted state-queue correction ${removal.transition.transactionHash} removed ${removed}, and the correction path reconciles this node's removed block with its unlanded descendants`
          : `pending state-queue correction ${removal.transition.transactionHash} removed ${removed}; it is not admitted yet`,
      }) satisfies Decision;
    // A block that landed and was then removed is the correction path's,
    // never a landed block to finalize.
    const correctionOfBlock = removing(header, false);
    if (correctionOfBlock !== undefined)
      return deferToCorrection(correctionOfBlock, "it after it landed");
    const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
    if (signed !== undefined && evidence.canonicalHistory.has(signed))
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: "its signed commit is in the journaled canonical history",
      } satisfies Decision;

    /** `winner` holds D's slot. A merged slot's winner may have left the
     * queue too, or be the confirmed state itself; its node is then the one
     * its own signed commit created (the root is never a block's node). */
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
        const onQueue = find(winner);
        const node =
          onQueue !== undefined && onQueue !== queue.root
            ? onQueue
            : merged
              ? yield* signedCommitNode(revived, contracts)
              : undefined;
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
    // The block that took D's slot links to D by its header. Once that block
    // is merged, the confirmed state names it and links to D (a merge sets
    // the confirmed predecessor to the header it folded over), so the
    // checkpoint's queue still says who won when no removal names D: the root
    // E was built on, whose confirmed header no transition removes, or a D
    // the observer never saw leave. A merged D was never corrected, so this
    // needs no observer. A queue node linking to an absent D is not read:
    // only a correction removes a node and keeps its successor, and the
    // correction arm below decides that once it is recorded.
    if (queue.root.prevHeaderHash === base && queue.root.headerHash !== base)
      return yield* successor(queue.root.headerHash, true);
    // Neither E nor D is on the checkpoint's queue. A recorded removal, a
    // replaced sibling's own landing, or the recorded transitions around a
    // root base say how; absence alone never decides. The observer records
    // independently of the history gate, so the gate stays open and the next
    // change of its view decides again.
    if (observer.kind === "blocked")
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue and the state-queue correction observer has no usable view of why yet (${observer.reason})`,
      } satisfies Decision;
    const mergeOfBlock = removing(header, true);
    if (mergeOfBlock !== undefined)
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: `merge ${mergeOfBlock.transition.transactionHash} folded it into the confirmed state`,
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
        return deferToCorrection(correctionOfBase, `its base ${base}`);
      return {
        kind: "replace",
        cause: `state-queue correction ${correctionOfBase.transition.transactionHash} consumed the node of its base ${base}, which this node does not journal`,
      } satisfies Decision;
    }
    // Every commit built on E's base spends the same output, so whichever of
    // them landed holds the slot and E never can. A replaced sibling of this
    // node on that output shows it by its own landing (read as the reviver
    // reads it), which needs no link from the slot back to the base: once two
    // merges pass a root base, nothing links that confirmed header to the
    // block that took its slot.
    const landedSiblings: string[] = [];
    for (const sibling of yield* replacedSiblings(record))
      if (
        replacedBlockLanding(
          sibling,
          queue,
          observer,
          evidence.canonicalHistory,
        ) !== undefined
      )
        landedSiblings.push(sibling[C.HEADER_HASH].toString("hex"));
    if (landedSiblings.length > 1)
      return yield* Effect.fail(
        new SignedIntentReplacementIntegrityError(
          landedSiblings[0]!,
          `blocks ${landedSiblings.join(", ")} of this node, all built on the base output of block ${header}, landed`,
        ),
      );
    if (landedSiblings.length === 1)
      return yield* successor(landedSiblings[0]!, true);
    // E's base output is a root that a recorded transition left with an empty
    // queue (a merge of the tail, or a correction of the only node): E was
    // built on the root. Only a commit spends a root with an empty queue, so
    // the first block appended after that transition took the slot, and every
    // later transition sees it first on the queue until it leaves: the next
    // recorded transition names it, however many merges followed. The merge
    // of D, made before E was built on the root, says nothing about E's slot,
    // so this decides before it.
    const recorded = [...observer.state.pending, ...observer.state.admitted];
    recorded.sort(chainOrder);
    const emptied = recorded.findIndex(
      ({ nextQueue }) =>
        nextQueue.length === 1 &&
        nextQueue[0]!.headerHash === null &&
        nextQueue[0]!.outRef === record[C.BASE_TAIL_OUT_REF],
    );
    if (emptied >= 0) {
      const after = recorded[emptied + 1];
      const holder = after?.previousQueue[1]?.headerHash ?? null;
      if (holder === null)
        return {
          kind: "defer",
          sticky: false,
          reason: `it was built on the root that state-queue transition ${recorded[emptied]!.transactionHash} left empty, and the state-queue correction observer has recorded no later transition naming the block that took that slot yet`,
        } satisfies Decision;
      const correctionOfHolder = removing(holder, false);
      if (correctionOfHolder !== undefined) {
        const own = yield* Pending.retrieveByHeaderHash(
          Buffer.from(holder, "hex"),
        );
        if (Option.isSome(own) && !correctionOfHolder.admitted)
          return {
            kind: "defer",
            sticky: false,
            reason: `this node's block ${holder} took the slot of its root base, and pending state-queue correction ${correctionOfHolder.transition.transactionHash} removed it; it is not admitted yet`,
          } satisfies Decision;
        return {
          kind: "replace",
          cause: `block ${holder} spent the root output it was built on, and state-queue correction ${correctionOfHolder.transition.transactionHash} then removed that block`,
        } satisfies Decision;
      }
      return yield* successor(holder, true);
    }
    const mergeOfBase = removing(base, true);
    if (mergeOfBase === undefined)
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue, the confirmed state does not link to it, no replaced block of this node on the same base shows it landed, and the state-queue correction observer has recorded neither a correction nor a merge of it, nor a transition that left the root it was built on empty`,
      } satisfies Decision;
    const previous = mergeOfBase.transition.previousQueue;
    const index = previous.findIndex((node) => node.headerHash === base);
    const merged = previous[index + 1]?.headerHash ?? null;
    if (merged === null)
      return {
        kind: "replace",
        cause: `its base ${base} was merged into the confirmed state by ${mergeOfBase.transition.transactionHash} while it was still the queue tail`,
      } satisfies Decision;
    return yield* successor(merged, true);
  });

/** With this intent's own replacement plan retained, a sticky deferral would
 * leave the plan retained while the correction path waits for it. The plan
 * binds only the replaced journal, so it is resumed instead. */
const effective = (decision: Decision, retainedPlan: boolean): Decision =>
  retainedPlan && decision.kind === "defer" && decision.sticky
    ? {
        kind: "replace",
        cause: `${decision.reason}; its retained replacement plan is resumed`,
      }
    : decision;

/** The node's native owner, opening only its retained native bytes when none
 * is open: never a genesis bootstrap or a journal replay. Create validates the
 * durable marker. */
const openRetainedNativeOwner = (globals: Globals, config: NodeConfigDep) =>
  Effect.gen(function* () {
    const open = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (open !== undefined) return open;
    return yield* Effect.uninterruptible(
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
        return opened as NativeMpfOwnerService;
      }),
    );
  });

const persistedReplay = (
  replay: NonNullable<Pending.Record["nativeMpfReplay"]>,
): PersistedNativeMpfReplay => ({
  schema: 1,
  ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
  baseRoot: replay.baseRoot.toString("hex"),
  candidateRoot: replay.candidateRoot.toString("hex"),
  eventLog: replay.eventLog,
  eventLogDigest: replay.eventLogDigest.toString("hex"),
  eventRoots: replay.eventRoots,
  eventCount: replay.eventCount,
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
        // This intent's own retained plan is resumed or discarded first: an
        // owed rewind waits for every retained plan, so waiting for the
        // rewind here would deadlock both.
        if (retained === undefined) {
          const rewind = yield* stateQueueCorrectionRewindDisposition(
            input.rewindAuthority,
          );
          if (rewind !== undefined) return undefined;
        }
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
          // The retained plan's CAS moves the native root from its candidate
          // only when the journal held it there when the plan was prepared
          // (promoted or locally finalized); otherwise the CAS is base to
          // base and never moved it.
          replayRetained:
            retained !== undefined &&
            retained.expectedRoot === journal.record[C.EXPECTED_UTXOS_ROOT],
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
      canonicalHistory: yield* owned(
        replacedSiblings(record).pipe(
          Effect.flatMap((siblings) =>
            includedInCanonicalHistory(input.binding, checkpoint, [
              signedTx,
              ...siblings.flatMap((sibling) => {
                const hash = sibling[C.INTENDED_TX_HASH];
                return hash == null ? [] : [hash.toString("hex")];
              }),
            ]),
          ),
        ),
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
        const decision = effective(
          yield* decide(journal.record, evidence),
          derived.retainedPlan,
        );
        if (decision.kind !== expected)
          return yield* Effect.fail(
            failure(
              `Signed-intent decision for ${header} changed from ${expected} to ${decision.kind}`,
            ),
          );
        return { ...journal, decision };
      });
    const decision = effective(
      yield* owned(decide(record, evidence)),
      derived.retainedPlan,
    );
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
      else
        input.deferral.untilObserved = deferredUntilObserved(
          key,
          yield* owned(observerFingerprint(input.rewindAuthority)),
        );
      yield* reportOnce(
        reportKey,
        decision.sticky
          ? `Not replacing ${context}: ${decision.reason}. The history gate stays open and its journal stays active until the correction path resolves it.`
          : `Not replacing ${context} yet: ${decision.reason}. The history gate stays open, its journal stays active, and the next change of the correction observer's view decides again.`,
      );
      return;
    }
    if (decision.kind === "landed") {
      const serialized = yield* serializeStateQueueUTxO(decision.node.node);
      if (derived.replayRetained) {
        // A replacement prepared from the candidate root before the block was
        // seen to land may already have restored the base root natively (its
        // CAS ran, its SQL repair did not), so the journal is intact but the
        // native root may be at its base. Replay it to the candidate first (a
        // no-op when the CAS never ran): a locally finalized journal is not
        // replayed again at local finalization. The native root stays within
        // the retained plan's two roots, so the plan can still be resumed if
        // this attempt stops before it is discarded. A plan prepared from the
        // base root (a journal never promoted) is base to base: the native
        // root never left the base, local finalization replays the journal,
        // and replaying here would strand the plan outside its roots.
        const owner = yield* openRetainedNativeOwner(globals, config);
        yield* preparation.assertCurrent;
        yield* Effect.tryPromise({
          try: () => owner.recover(persistedReplay(record.nativeMpfReplay!)),
          catch: (cause) =>
            failure(
              `Native replay of landed block ${header} over its discarded replacement failed`,
              cause,
            ),
        }).pipe(Effect.uninterruptible);
        yield* preparation.assertCurrent;
      }
      const requiresLocalFinalization = yield* owned(
        current("landed").pipe(
          Effect.tap(() =>
            // The native root is where the journal's status says it is again;
            // the replacement is discarded, not resumed. The plan must still
            // be the one the replay choice was made for.
            derived.retainedPlan
              ? retainedPreparedRecoveryPlan(input.binding.digest)
                  .pipe(
                    Effect.flatMap((retained) =>
                      retained?.kind === "signed_intent_release" &&
                      retained.headerHash === header &&
                      (retained.expectedRoot ===
                        record[C.EXPECTED_UTXOS_ROOT]) ===
                        derived.replayRetained
                        ? discardPreparedHistoryRecoveryPlan(
                            checkpoint,
                            SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
                            header,
                          )
                        : Effect.fail(
                            failure(
                              `The retained replacement plan of landed block ${header} changed`,
                            ),
                          ),
                    ),
                  )
                  .pipe(
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
    const owner = yield* openRetainedNativeOwner(globals, config);
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

/** This node's replacement-abandoned journals whose blocks the correction
 * observer's persisted view shows on a state queue: its cursor queue, a queue
 * a recorded transition saw, or a block a recorded merge folded. A block a
 * recorded correction removed is excluded: its members stay reopened, which
 * is what that correction's path does anyway. A hint only: it is not bound to
 * the checkpoint, so it only selects which blocks the checkpoint-bound
 * evidence is read for. Sorted by header. */
const revivalCandidates = (
  authority: StateQueueCorrectionRewindAuthority,
  lock = false,
) =>
  Effect.gen(function* () {
    const observer = yield* loadStateQueueCorrectionObserverState(
      authority,
      lock,
    );
    if (observer.kind !== "observed") return [];
    const { cursorQueue, pending, admitted } = observer.state;
    const transitions = [...pending, ...admitted];
    const corrected = new Set(
      transitions
        .filter((transition) => transition.transitionKind !== "merge")
        .flatMap((transition) => transition.removedHeaderHashes),
    );
    const seen = new Set<string>();
    for (const node of [
      ...cursorQueue,
      ...transitions.flatMap((transition) => [
        ...transition.previousQueue,
        ...transition.nextQueue,
      ]),
    ])
      if (node.headerHash !== null) seen.add(node.headerHash);
    for (const transition of transitions)
      if (transition.transitionKind === "merge")
        for (const hash of transition.removedHeaderHashes) seen.add(hash);
    const hashes = [...seen].filter((hash) => !corrected.has(hash)).sort();
    if (hashes.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE status = ${Pending.Status.Abandoned}
        AND header_hash IN ${sql.in(hashes.map((hash) => Buffer.from(hash, "hex")))}
      ORDER BY header_hash`;
    const records: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        records.push(found.value);
    }
    return records;
  });

const revivalKey = (
  candidates: readonly Pending.Record[],
  point: { readonly id: string },
) =>
  `${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(",")}@${point.id}`;

/** Forward-append and resume disposition: pending when no journal is active
 * and the correction observer's view shows one of this node's replaced blocks
 * on a state queue (whichever lands wins: a replaced block that won its base's
 * slot, by landing late or through a rollback, is revived, otherwise the
 * commit worker refuses to build on it for good). Once no checkpoint-bound
 * evidence showed a candidate at a source point, it stays open until the next
 * point. SQL only. */
export const replacedBlockRevivalDisposition = (input: {
  readonly change: HistoryOwnerChange;
  readonly deferral: SignedIntentDeferral;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    if (input.change.kind === "rollback" || input.change.kind === "seed")
      input.deferral.revival = undefined;
    if (yield* anyActiveJournal) return undefined;
    const candidates = yield* revivalCandidates(input.rewindAuthority);
    if (candidates.length === 0) return undefined;
    if (
      input.deferral.revival === revivalKey(candidates, input.change.after.head)
    )
      return undefined;
    return {
      status: "pending" as const,
      reason: `The state-queue correction observer shows replaced block ${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node on the state queue while no journal is active; a replaced block that holds its base's slot must be revived`,
    };
  });

/**
 * Recovery preparation: revives this node's replaced block when authenticated
 * evidence bound to the checkpoint shows it landed, while no journal is active
 * (with one active, `prepareExpiredIntentRelease` reconciles its base's slot,
 * reviving there). Evidence: its node on the exact-point queue, the confirmed
 * state equal to its header, its signed commit in the journaled canonical
 * history, or an admitted (final) merge that folded it or saw it on the
 * queue. The revival abandons nothing: with no journal active every sibling on
 * its base is already abandoned, which the revival itself enforces (a sibling
 * that landed or was locally finalized is an integrity failure). Its members
 * are taken back, its SQL marker moves to its candidate root, and local
 * finalization replays it natively. Defers while a correction rewind is owed
 * or any plan is retained.
 */
export const prepareReplacedBlockRevival = (input: {
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
    const reportKey = `${input.binding.digest}:revival`;
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    // Every candidate that is not excluded, or undefined when another recovery
    // goes first or a journal is active.
    const derive = Effect.gen(function* () {
      if (
        (yield* stateQueueCorrectionRewindDisposition(
          input.rewindAuthority,
        )) !== undefined ||
        (yield* retainedPreparedRecoveryPlan(input.binding.digest)) !==
          undefined ||
        (yield* anyActiveJournal)
      )
        return undefined;
      return yield* revivalCandidates(input.rewindAuthority, true);
    });
    yield* preparation.assertCurrent;
    const candidates = yield* owned(derive);
    if (candidates === undefined || candidates.length === 0) return;
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
    const canonicalHistory = yield* owned(
      includedInCanonicalHistory(
        input.binding,
        checkpoint,
        candidates.flatMap((record) => {
          const hash = record[C.INTENDED_TX_HASH];
          return hash == null ? [] : [hash.toString("hex")];
        }),
      ),
    );
    // Which candidates the evidence shows landed, with their nodes. Read in
    // the recovery transaction, re-derived before anything is written.
    const landed = Effect.gen(function* () {
      const current = yield* derive;
      if (current === undefined) return [];
      const observer = yield* loadStateQueueCorrectionObserverState(
        input.rewindAuthority,
        true,
      );
      const found: {
        record: Pending.Record;
        node: QueueNode;
        evidence: string;
      }[] = [];
      for (const record of current) {
        const landing = replacedBlockLanding(
          record,
          queue,
          observer,
          canonicalHistory,
        );
        if (landing !== undefined)
          found.push({
            record,
            node:
              landing.onQueue ??
              (yield* signedCommitNode(record, input.contracts)),
            evidence: landing.evidence,
          });
      }
      if (found.length > 1)
        return yield* Effect.fail(
          failure(
            `Replaced blocks ${found.map(({ record }) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node all landed`,
          ),
        );
      return found;
    });
    const winners = yield* owned(landed);
    const header = winners[0]?.record[C.HEADER_HASH].toString("hex");
    if (header === undefined) {
      input.deferral.revival = revivalKey(candidates, checkpoint.head);
      yield* reportOnce(
        reportKey,
        `The correction observer shows replaced block ${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node on the state queue, but no evidence bound to checkpoint ${checkpoint.head.id} shows it landed; the next source point decides again.`,
      );
      return;
    }
    const winner = winners[0]!;
    const serialized = yield* serializeStateQueueUTxO(winner.node.node);
    const globals = yield* Globals;
    if (config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
    yield* owned(
      landed.pipe(
        Effect.flatMap((current) =>
          current.length === 1 &&
          current[0]!.record[C.HEADER_HASH].toString("hex") === header &&
          current[0]!.node.node.utxo.txHash === winner.node.node.utxo.txHash &&
          current[0]!.node.node.utxo.outputIndex ===
            winner.node.node.utxo.outputIndex
            ? reviveReplacedCanonicalJournal(winner.record[C.HEADER_HASH])
            : Effect.fail(
                failure(`The revival evidence for block ${header} changed`),
              ),
        ),
      ),
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
    yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
    yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, serialized);
    yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
    yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
    input.deferral.revival = undefined;
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logWarning(
      `Revived replaced block ${header}: ${winner.evidence}, and no journal was active; local finalization replays it.`,
    );
  });
