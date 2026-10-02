import {
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type LocalKupmiosOutRefsAtPoint,
  type readAdmittedLocalKupmiosBoundary,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { rawRedeemers } from "./authenticated-state-queue-observation.authenticate-persisted-bootstrap-topology.js";
import {
  RELEASE_FINALITY_DEPTH,
  WATCHER_STATE_QUEUE_REMOVAL_KINDS,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherReleasedHeaderProof,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  mintPolicyIds,
  outputHasUnit,
  outputReferences,
} from "./authenticated-state-queue-observation.parse-persisted-observation.js";

export type WatcherMergedHeaderReaders = Readonly<{
  readBoundary(): ReturnType<typeof readAdmittedLocalKupmiosBoundary>;
  readOutRefs(
    outRefs: readonly string[],
    point: FraudProofRawL1Point,
  ): Promise<LocalKupmiosOutRefsAtPoint>;
  readTransaction(
    txHash: string,
    point: FraudProofRawL1Point,
  ): Promise<FraudProofRawL1Transaction>;
}>;

/** The single state-queue mint redeemer of `body`, or null. */
const stateQueueMintRedeemer = (
  body: CML.TransactionBody,
  witnessSetCbor: string,
  stateQueuePolicyId: string,
): SDK.StateQueueRedeemer | null => {
  const policyIndex = mintPolicyIds(body).indexOf(stateQueuePolicyId);
  const redeemers = rawRedeemers(witnessSetCbor).filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (policyIndex < 0 || redeemers.length !== 1) return null;
  try {
    return Data.from(redeemers[0]!.cborHex, SDK.StateQueueRedeemer);
  } catch {
    return null;
  }
};

const nodeMintQuantity = (
  body: CML.TransactionBody,
  stateQueuePolicyId: string,
  headerHash: string,
): bigint | undefined =>
  body
    .mint()
    ?.get(
      CML.ScriptHash.from_hex(stateQueuePolicyId),
      CML.AssetName.from_hex(
        `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
      ),
    );

/**
 * The confirmed-state output `raw` produces when it is exactly the
 * MergeToConfirmedStateV1 of `headerHash` over `rootOutRef`, burning that
 * header's node; null for any other spend of the root.
 */
const mergedConfirmedState = ({
  raw,
  rootOutRef,
  headerHash,
  stateQueuePolicyId,
}: {
  raw: FraudProofRawL1Transaction;
  rootOutRef: string;
  headerHash: string;
  stateQueuePolicyId: string;
}): string | null => {
  if (raw.confirmationDepth < RELEASE_FINALITY_DEPTH) return null;
  const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
  if (!outputReferences(body.inputs()).includes(rootOutRef)) return null;
  const decoded = stateQueueMintRedeemer(
    body,
    raw.witnessSetCbor,
    stateQueuePolicyId,
  );
  if (
    typeof decoded !== "object" ||
    decoded === null ||
    !("MergeToConfirmedStateV1" in decoded)
  )
    return null;
  const merge = decoded.MergeToConfirmedStateV1;
  const consumed = merge.confirmed_state_input_outref;
  const outputs = body.outputs();
  const outputIndex = merge.confirmed_state_output_index;
  if (
    merge.header_node_key !== headerHash ||
    `${consumed.transactionId}#${consumed.outputIndex.toString()}` !==
      rootOutRef ||
    nodeMintQuantity(body, stateQueuePolicyId, headerHash) !== -1n ||
    outputIndex < 0n ||
    outputIndex >= BigInt(outputs.len()) ||
    !outputHasUnit(
      outputs.get(Number(outputIndex)),
      `${stateQueuePolicyId}${SDK.STATE_QUEUE_ROOT_ASSET_NAME}`,
    )
  )
    return null;
  return `${raw.txHash}#${outputIndex.toString()}`;
};

/** A queue element: the confirmed-state root (null) or a header's node. */
type QueueElement = string | null;

/**
 * Headers of `observation` that left the L1 queue at release finality,
 * proven from the watcher's own release-final view, never from a node or DA
 * provider. The walk follows every queue element of the observation, the
 * confirmed-state root and each queued header's node, applying the
 * transactions that spend them one at a time in chain order:
 *
 * - a MergeToConfirmedStateV1 of the queue head over the root merges that
 *   header and names the next root, since merges land in queue order;
 * - a removal redeemer that burns exactly one followed header's node removes
 *   that header, wherever it sits in the queue;
 * - any other element a transaction consumes, including a removal's anchor
 *   and the root of a removed queue head, continues at the one output that
 *   carries its unit.
 *
 * A transaction's consumed elements are its inputs among the outputs the
 * walk follows when it applies it. The next transaction is the earliest
 * spend of a followed output; within one block it is one whose every
 * followed-unit input is already followed, so a spend of an output an
 * earlier transaction of the block produced waits for that transaction. A
 * transaction below release depth stops the walk, since every later one is
 * shallower still. Any other burn of a followed header's node, or a node
 * whose unit cannot be followed, ends the walk; it never throws on a
 * transaction it does not recognise. Nothing is cached, so a rollback below
 * a merge or removal returns its header to the queue.
 */
export const resolveMergedHeadersAtBoundary = async ({
  observation,
  authority,
  readers,
}: {
  observation: WatcherAuthenticatedStateQueueObservation;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  readers: WatcherMergedHeaderReaders;
}): Promise<ReadonlyMap<string, WatcherReleasedHeaderProof>> => {
  const released = new Map<string, WatcherReleasedHeaderProof>();
  const boundary = await readers.readBoundary();
  const point = boundary.kupoCheckpoint;
  // A transition at or below the boundary is already applied to any
  // observation taken at or after it.
  if (BigInt(observation.nativePoint.blockNo) >= BigInt(point.blockNo))
    return released;
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const [root, ...nodes] = observation.finalizedQueue;
  const queued: string[] = [];
  for (const { headerHash } of nodes) {
    if (headerHash === null) return released;
    queued.push(headerHash);
  }
  if (root === undefined || root.headerHash !== null) return released;
  const live = new Map<QueueElement, string>([
    [null, root.outRef],
    ...nodes.map(({ headerHash, outRef }) => [headerHash, outRef] as const),
  ]);
  const unit = (element: QueueElement): string =>
    element === null
      ? `${stateQueuePolicyId}${SDK.STATE_QUEUE_ROOT_ASSET_NAME}`
      : `${stateQueuePolicyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${element}`;

  /**
   * False while `raw` spends a followed element's unit from an output the
   * walk has not reached yet: a transaction of the same block produced it.
   */
  const ready = (raw: FraudProofRawL1Transaction): boolean =>
    raw.resolvedInputs.every(({ outRef, outputCbor }) => {
      const output = CML.TransactionOutput.from_cbor_hex(outputCbor);
      return [...live].every(
        ([element, followed]) =>
          followed === outRef || !outputHasUnit(output, unit(element)),
      );
    });

  /** Applies one release-final transaction; false ends the walk. */
  const apply = (raw: FraudProofRawL1Transaction): boolean => {
    const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
    const inputs = outputReferences(body.inputs());
    const consumed = [...live].flatMap(([element, outRef]) =>
      inputs.includes(outRef) ? [element] : [],
    );
    if (consumed.length === 0) return false;
    const handled = new Set<QueueElement>();
    const rootOutRef = live.get(null);
    const head = queued.find((headerHash) => !released.has(headerHash));
    const nextRoot =
      rootOutRef === undefined || head === undefined || !consumed.includes(null)
        ? null
        : mergedConfirmedState({
            raw,
            rootOutRef,
            headerHash: head,
            stateQueuePolicyId,
          });
    if (nextRoot !== null && head !== undefined) {
      released.set(
        head,
        Object.freeze({
          headerHash: head,
          mergeTransactionHash: raw.txHash,
          mergeBlockHash: raw.inclusionPoint.blockHash,
          mergeSlot: raw.inclusionPoint.slot,
          mergeBlockNo: raw.inclusionPoint.blockNo,
          confirmationDepth: raw.confirmationDepth.toString(),
        }),
      );
      live.set(null, nextRoot);
      live.delete(head);
      handled.add(null).add(head);
    } else {
      const redeemer = stateQueueMintRedeemer(
        body,
        raw.witnessSetCbor,
        stateQueuePolicyId,
      );
      const removalKind =
        typeof redeemer === "object" && redeemer !== null
          ? WATCHER_STATE_QUEUE_REMOVAL_KINDS.find((kind) => kind in redeemer)
          : undefined;
      const burned = queued.filter(
        (headerHash) =>
          !released.has(headerHash) &&
          nodeMintQuantity(body, stateQueuePolicyId, headerHash) === -1n,
      );
      const removed = burned[0];
      if (
        removalKind !== undefined &&
        burned.length === 1 &&
        removed !== undefined &&
        consumed.includes(removed)
      ) {
        released.set(
          removed,
          Object.freeze({
            headerHash: removed,
            removalTransactionHash: raw.txHash,
            removalKind,
            removalBlockHash: raw.inclusionPoint.blockHash,
            removalSlot: raw.inclusionPoint.slot,
            removalBlockNo: raw.inclusionPoint.blockNo,
            confirmationDepth: raw.confirmationDepth.toString(),
          }),
        );
        live.delete(removed);
        handled.add(removed);
      }
    }
    // Any other burn of a followed node is a transition this walk does not
    // prove; it ends here rather than guess the queue past it.
    if (
      queued.some(
        (headerHash) =>
          !released.has(headerHash) &&
          (nodeMintQuantity(body, stateQueuePolicyId, headerHash) ?? 0n) < 0n,
      )
    )
      return false;
    const outputs = body.outputs();
    for (const element of consumed) {
      if (handled.has(element)) continue;
      const carriers: number[] = [];
      for (let index = 0; index < outputs.len(); index += 1)
        if (outputHasUnit(outputs.get(index), unit(element)))
          carriers.push(index);
      if (carriers.length !== 1) return false;
      live.set(element, `${raw.txHash}#${carriers[0]!.toString()}`);
    }
    return true;
  };

  const transactions = new Map<string, FraudProofRawL1Transaction>();
  while (released.size < queued.length && live.size > 0) {
    const followed = new Set(live.values());
    const { spends } = await readers.readOutRefs([...followed], point);
    const spenders = new Map<string, FraudProofRawL1Point>();
    for (const spend of spends)
      if (followed.has(spend.outRef))
        spenders.set(spend.spendingTxHash, spend.spendPoint);
    if (spenders.size === 0) break;
    const earliest = [...spenders.values()]
      .map(({ blockNo }) => BigInt(blockNo))
      .reduce((least, blockNo) => (blockNo < least ? blockNo : least));
    const candidates: FraudProofRawL1Transaction[] = [];
    // Hash order only makes the choice among independent spends of one
    // block deterministic; readiness orders dependent ones.
    for (const [txHash, spendPoint] of [...spenders].sort(([left], [right]) =>
      left < right ? -1 : left > right ? 1 : 0,
    )) {
      if (BigInt(spendPoint.blockNo) !== earliest) continue;
      const raw =
        transactions.get(txHash) ??
        (await readers.readTransaction(txHash, spendPoint));
      transactions.set(txHash, raw);
      candidates.push(raw);
    }
    const next = candidates.find(ready);
    if (
      next === undefined ||
      next.confirmationDepth < RELEASE_FINALITY_DEPTH ||
      !apply(next)
    )
      return released;
  }
  return released;
};
