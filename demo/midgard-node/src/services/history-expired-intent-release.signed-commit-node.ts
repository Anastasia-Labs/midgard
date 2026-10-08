import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Data, Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { type LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";
import {
  C,
  type CanonicalDepth,
  failure,
  ROOT_TAIL_HEADER_HASH,
  sha,
  UNLANDED_STATUSES,
} from "./history-expired-intent-release.table.js";
import { unboundJournalReason } from "./journal-header-binding.js";
import { type StateQueueCorrectionRewindAuthority } from "./state-queue-correction-rewind.js";

/** Immutable identity of the active journal: every root and hash fixing the
 * native state it moved from and to, and the exact signed bytes. Status is
 * excluded; it is rechecked separately. */
export const journalIdentity = (record: Pending.Record) => {
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

/** The only mutable identity evolution accepted by an already prepared
 * release: acknowledgement of its exact retained signed transaction. */
export const journalIdentityBeforeSubmissionAck = (record: Pending.Record) =>
  record[C.SUBMITTED_TX_HASH] != null &&
  record[C.INTENDED_TX_HASH] != null &&
  record[C.SUBMITTED_TX_HASH]!.equals(record[C.INTENDED_TX_HASH]!)
    ? journalIdentity({ ...record, [C.SUBMITTED_TX_HASH]: null })
    : undefined;

/** A journal's replay base is bound neither by a retained parent journal
 * (none has its base tail header hash) nor by its own header (see
 * `unboundJournalReason`). The signed-intent release and the replaced-block
 * revival read their target root from that base, so they hold (see
 * `heldOnIntegrityFailure`), with nothing written. */
export class SignedIntentJournalUnbound extends Data.TaggedError(
  "SignedIntentJournalUnbound",
)<{ readonly message: string }> {}

/** Fails with `SignedIntentJournalUnbound` unless `record`'s own header
 * bytes bind its replay base: for a journal no retained parent journal binds
 * (the root base included). `label` names the journal in the message. */
export const requireHeaderBoundBase = (record: Pending.Record, label: string) =>
  Effect.flatMap(unboundJournalReason(record), (unbound) =>
    unbound === undefined
      ? Effect.void
      : Effect.fail(
          new SignedIntentJournalUnbound({
            message: `The replay base of ${label} ${record[C.HEADER_HASH].toString("hex")} is bound by no retained parent journal and not by its header: ${unbound}.`,
          }),
        ),
  );

/** The active journal and the aggregate of its replay base (its retained
 * parent journal's, or none, which makes the commit base recompute it). A
 * journal whose roots or parent do not prove its replay base cannot be
 * rewound and fails loudly: recovery retries it and the gate stays closed.
 * With no retained parent journal (the root base included), its own header
 * bytes must bind the base, or it fails with `SignedIntentJournalUnbound`. */
export const replaceableJournal = (headerHash: Buffer, manifestId: string) =>
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
    const parent = baseTail.equals(ROOT_TAIL_HEADER_HASH)
      ? Option.none()
      : yield* Pending.retrieveByHeaderHash(baseTail);
    if (Option.isNone(parent)) {
      yield* requireHeaderBoundBase(record, "signed-intent journal");
      return { record, parentAggregate: undefined };
    }
    if (
      parent.value[C.STATUS] === Pending.Status.Abandoned ||
      parent.value[C.EXPECTED_UTXOS_ROOT] !== record[C.BASE_UTXOS_ROOT]
    )
      return yield* Effect.fail(
        failure(
          `The replay base of signed-intent journal ${header} is not its retained parent's root`,
        ),
      );
    return { record, parentAggregate: parent.value.utxoPayloadAggregate };
  });

/** A queue output named by the header it commits and the header that header
 * links to (for the root, the confirmed header's predecessor, which a merge
 * sets to the header it folded over). */
export type QueueNode = Readonly<{
  node: SDK.StateQueueUTxO;
  headerHash: string;
  prevHeaderHash: string;
}>;

export type QueueView = Readonly<{
  nodes: readonly QueueNode[];
  root: QueueNode;
}>;

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
export const authenticateQueue = (
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

export type Decision =
  | Readonly<{ kind: "landed"; node: QueueNode; evidence: string }>
  | Readonly<{ kind: "wait"; reason: string }>
  /** `sticky`: the correction path owns the journal, so it is not
   * re-read until a rollback or restart. Otherwise the gate stays open and
   * the next source point decides again. */
  | Readonly<{ kind: "defer"; reason: string; sticky: boolean }>
  | Readonly<{ kind: "replace"; cause: string }>
  /** `displaced`: this node's locally finalized blocks on the same base (and
   * their descendants), earliest first, that an L1 rollback took off the
   * chain while `revived` now holds the base's slot at confirmation depth.
   * The repair abandons them before it revives `revived`. */
  | Readonly<{
      kind: "revive";
      revived: Pending.Record;
      node: QueueNode;
      displaced: readonly Pending.Record[];
    }>;

/** Authenticated evidence of the chain at the checkpoint: its exact-point
 * queue, which signed commits (the active journal's and its replaced
 * siblings') the owner's journaled canonical history holds (presence is proof
 * a commit landed; absence proves nothing, as retention is bounded), and the
 * transaction that history shows spending the active journal's base output,
 * when one other than its signed commit did. `canonicalDepth`, when given,
 * says how deep that history holds a transaction; only with it can a locally
 * finalized sibling be shown displaced (see `decide`). The correction
 * observer's view is read in `decide` itself, under a share lock, so a
 * re-derivation sees any change. */
export type ReleaseEvidence = Readonly<{
  queue: QueueView;
  baseSpend?: string | undefined;
  canonicalHistory: ReadonlySet<string>;
  canonicalDepth?: CanonicalDepth | undefined;
  contracts: StateQueueContracts;
  rewindAuthority: StateQueueCorrectionRewindAuthority;
}>;
