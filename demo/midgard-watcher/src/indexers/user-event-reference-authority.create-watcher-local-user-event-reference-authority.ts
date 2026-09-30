import { CML } from "@lucid-evolution/lucid";

import { type WatcherLocalBackfillFinalityReceipt } from "../l1/finality-engine.js";
import {
  isWatcherL1AdapterNormalizedBlock,
  WATCHER_L1_ADAPTER_BOUNDS,
  type WatcherLocalBackfillObservationReceipt,
  type WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import {
  readWatcherResolvedBlockObservation,
  resolveWatcherBlockObservationTransactions,
  type WatcherResolvedBlockObservation,
} from "../l1/resolved-block-observation.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";

export const WATCHER_USER_EVENT_REFERENCE_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-user-event-reference-authority-v1" as const;

export const WATCHER_USER_EVENT_REFERENCE_EVIDENCE_SCHEMA_VERSION =
  "midgard-watcher-user-event-reference-evidence-v1" as const;

/** Live admission is module-private; serializing this object grants nothing. */
export type WatcherUserEventReferenceAuthority = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_REFERENCE_AUTHORITY_SCHEMA_VERSION;
}>;

type ReferenceOutput = Readonly<{ outRef: string; outputCbor: string }>;

type ReferenceTransaction = Readonly<{
  txHash: string;
  transactionIndex: string;
  bodyCbor: string;
  witnessSetCbor: string;
  referenceInputs: readonly ReferenceOutput[];
}>;

type ReferenceEvidenceBase = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_REFERENCE_EVIDENCE_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  targetObservationDigest: string;
  targetBlockContentDigest: string;
  targetPointDigest: string;
  transactions: readonly ReferenceTransaction[];
}>;

/** Public evidence must be matched against fresh live authority after restart. */
export type WatcherUserEventReferenceEvidence = ReferenceEvidenceBase &
  (
    | Readonly<{
        evidenceKind: "resolved_block";
        sourceMode: "local_node";
        sourceId: string;
        confirmationDepth: string;
      }>
    | Readonly<{
        evidenceKind: "creating_bodies";
        sourceMode: "external_providers" | "local_node";
        /** Body hashes, not claimed creating-block metadata, bind outputs. */
        creatingTransactionBodies: readonly string[];
      }>
  );

type AuthorityState = Readonly<{
  evidence: WatcherUserEventReferenceEvidence;
  assertLive: () => void;
}>;

export const authorities = new WeakMap<
  WatcherUserEventReferenceAuthority,
  AuthorityState
>();

export const backfillAuthorityPairs = new WeakMap<
  WatcherUserEventReferenceAuthority,
  Readonly<{
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
  }>
>();

export const assertTarget = (
  targetBlock: WatcherNormalizedL1Block,
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
  sourceMode: "local_node" | "external_providers",
): void => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  if (
    !isWatcherL1AdapterNormalizedBlock(targetBlock) ||
    targetBlock.provider.source.sourceMode !== sourceMode ||
    targetBlock.network !== deploymentIdentity.network
  ) {
    throw new Error(
      "user-event references require a live admitted target block",
    );
  }
};

export const referenceOutRefs = (bodyCbor: string): readonly string[] => {
  const body = CML.TransactionBody.from_cbor_hex(bodyCbor);
  let inputs: CML.TransactionInputList | undefined;
  try {
    inputs = body.reference_inputs();
    const result = Array.from({ length: inputs?.len() ?? 0 }, (_, index) => {
      const input = inputs!.get(index);
      const id = input.transaction_id();
      try {
        return `${id.to_hex()}#${input.index().toString()}`;
      } finally {
        id.free();
        input.free();
      }
    });
    if (new Set(result).size !== result.length)
      throw new Error("target reference inputs are not unique");
    return result;
  } finally {
    inputs?.free();
    body.free();
  }
};

export type ReferenceBudget = { members: number; bytes: number };

export const boundedReferenceOutput = (
  outRef: string,
  outputCbor: string,
  budget: ReferenceBudget,
): ReferenceOutput => {
  budget.members += 1;
  budget.bytes += outputCbor.length / 2;
  if (
    budget.members > WATCHER_L1_ADAPTER_BOUNDS.totalCollectionMembers ||
    outputCbor.length / 2 > WATCHER_L1_ADAPTER_BOUNDS.publicBytes ||
    budget.bytes > WATCHER_L1_ADAPTER_BOUNDS.totalPublicBytes
  ) {
    throw new Error("user-event resolved reference bytes exceed bounds");
  }
  return Object.freeze({ outRef, outputCbor });
};

export const referenceTransaction = (
  targetBlock: WatcherNormalizedL1Block,
  index: number,
  inputs: readonly ReferenceOutput[],
): ReferenceTransaction => {
  const transaction = targetBlock.transactions[index]!;
  const roster = referenceOutRefs(transaction.body.bytesHex);
  const byOutRef = new Map(inputs.map((input) => [input.outRef, input]));
  if (
    inputs.length !== roster.length ||
    byOutRef.size !== inputs.length ||
    roster.some((outRef) => !byOutRef.has(outRef))
  ) {
    throw new Error(
      "user-event reference evidence differs from the exact roster",
    );
  }
  return Object.freeze({
    txHash: transaction.txHash,
    transactionIndex: index.toString(),
    bodyCbor: transaction.body.bytesHex,
    witnessSetCbor: transaction.witnessSet.bytesHex,
    referenceInputs: Object.freeze(
      roster.map((outRef) => Object.freeze({ ...byOutRef.get(outRef)! })),
    ),
  });
};

export const baseEvidence = (
  targetBlock: WatcherNormalizedL1Block,
  deploymentIdentityDigest: string,
  transactions: readonly ReferenceTransaction[],
): ReferenceEvidenceBase => ({
  schemaVersion: WATCHER_USER_EVENT_REFERENCE_EVIDENCE_SCHEMA_VERSION,
  deploymentIdentityDigest,
  targetObservationDigest: targetBlock.observationDigest,
  targetBlockContentDigest: targetBlock.blockContentDigest,
  targetPointDigest: targetBlock.chainPoint.pointDigest,
  transactions: Object.freeze([...transactions]),
});

export const admit = (
  state: AuthorityState,
): WatcherUserEventReferenceAuthority => {
  state.assertLive();
  const authority = Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_REFERENCE_AUTHORITY_SCHEMA_VERSION,
  });
  authorities.set(authority, state);
  return authority;
};

export const createWatcherLocalUserEventReferenceAuthority = async ({
  targetBlock,
  deploymentIdentity,
  resolvedBlock,
}: {
  readonly targetBlock: WatcherNormalizedL1Block;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly resolvedBlock: WatcherResolvedBlockObservation;
}): Promise<WatcherUserEventReferenceAuthority> => {
  const assertLive = () => {
    assertTarget(targetBlock, deploymentIdentity, "local_node");
    readWatcherResolvedBlockObservation(resolvedBlock);
  };
  assertLive();
  const resolved = readWatcherResolvedBlockObservation(resolvedBlock);
  const point = targetBlock.chainPoint;
  if (
    resolved.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
    resolved.rawBlock.point.blockHash !== point.blockHash ||
    resolved.rawBlock.point.blockNo.toString() !== point.blockNo ||
    resolved.rawBlock.point.slot.toString() !== point.slot ||
    resolved.rawBlock.parentBlockHash !== point.parentBlockHash ||
    BigInt(point.depth) < BigInt(resolved.minimumConfirmationDepth) ||
    resolved.rawBlock.transactions.length !== targetBlock.transactions.length ||
    resolved.rawBlock.transactions.some(
      (transaction, index) =>
        transaction.txHash !== targetBlock.transactions[index]!.txHash ||
        transaction.transactionCbor !==
          targetBlock.transactions[index]!.fullTransaction.bytesHex,
    )
  ) {
    throw new Error(
      "user-event target differs from the complete resolved block",
    );
  }
  const selected = targetBlock.transactions.flatMap((transaction, index) =>
    transaction.isValid ? [{ transaction, index }] : [],
  );
  const rawTransactions = await resolveWatcherBlockObservationTransactions(
    resolvedBlock,
    selected.map(({ transaction }) => transaction.txHash),
  );
  assertLive();
  const referenceBudget: ReferenceBudget = { members: 0, bytes: 0 };
  const transactions = selected.map(({ transaction, index }, rawIndex) => {
    const raw = rawTransactions[rawIndex]!;
    if (
      raw.txHash !== transaction.txHash ||
      raw.bodyCbor !== transaction.body.bytesHex ||
      raw.witnessSetCbor !== transaction.witnessSet.bytesHex ||
      raw.isValid !== true
    ) {
      throw new Error(
        "user-event resolved transaction bytes differ from target",
      );
    }
    return referenceTransaction(
      targetBlock,
      index,
      raw.resolvedReferenceInputs.map(({ outRef, outputCbor }) =>
        boundedReferenceOutput(outRef, outputCbor, referenceBudget),
      ),
    );
  });
  return admit({
    assertLive,
    evidence: Object.freeze({
      ...baseEvidence(targetBlock, deploymentIdentity.manifestId, transactions),
      evidenceKind: "resolved_block",
      sourceMode: "local_node",
      sourceId: resolved.sourceId,
      confirmationDepth: resolved.minimumConfirmationDepth,
    }),
  });
};
