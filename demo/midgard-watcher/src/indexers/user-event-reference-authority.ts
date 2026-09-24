import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import {
  readWatcherLocalBackfillFinalityObservation,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import {
  isWatcherL1AdapterNormalizedBlock,
  WATCHER_L1_ADAPTER_BOUNDS,
  type WatcherLocalBackfillObservationReceipt,
  type WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import {
  assertWatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservation,
} from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import {
  readWatcherResolvedBlockObservation,
  resolveWatcherBlockObservationTransactions,
  type WatcherResolvedBlockObservation,
} from "../l1/resolved-block-observation.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";

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
const authorities = new WeakMap<
  WatcherUserEventReferenceAuthority,
  AuthorityState
>();

const backfillAuthorityPairs = new WeakMap<
  WatcherUserEventReferenceAuthority,
  Readonly<{
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
  }>
>();

const assertTarget = (
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

const referenceOutRefs = (bodyCbor: string): readonly string[] => {
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

type ReferenceBudget = { members: number; bytes: number };
const boundedReferenceOutput = (
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

const referenceTransaction = (
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

const baseEvidence = (
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

const admit = (state: AuthorityState): WatcherUserEventReferenceAuthority => {
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

/**
 * Uses the target's live admission and reference-input hash commitments.
 * Creating bodies are preimages, with no invented inclusion
 * point, provider receipt or is_valid claim. Only the ordinary target/finality
 * verifier can establish that these outputs were available to a valid target.
 */
const createBodyReferenceAuthority = ({
  targetBlock,
  deploymentIdentity,
  creatingTransactionBodies,
  sourceMode,
  assertLive,
}: {
  readonly targetBlock: WatcherNormalizedL1Block;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly creatingTransactionBodies: readonly string[];
  readonly sourceMode: "external_providers" | "local_node";
  readonly assertLive: () => void;
}): WatcherUserEventReferenceAuthority => {
  assertLive();
  if (
    !Array.isArray(creatingTransactionBodies) ||
    creatingTransactionBodies.length > WATCHER_L1_ADAPTER_BOUNDS.arrayMembers
  ) {
    throw new Error("user-event creating-body collection exceeds bounds");
  }
  let totalBytes = 0;
  const bodies = new Map<string, { cbor: string; body: CML.TransactionBody }>();
  try {
    for (const cbor of creatingTransactionBodies) {
      if (
        typeof cbor !== "string" ||
        !/^(?:[0-9a-f]{2})+$/u.test(cbor) ||
        cbor.length / 2 > WATCHER_L1_ADAPTER_BOUNDS.publicBytes ||
        (totalBytes += cbor.length / 2) >
          WATCHER_L1_ADAPTER_BOUNDS.totalPublicBytes
      ) {
        throw new Error("user-event creating-body bytes exceed bounds");
      }
      const body = CML.TransactionBody.from_cbor_hex(cbor);
      if (body.to_cbor_hex() !== cbor) {
        body.free();
        throw new Error("user-event creating body encoding is not preserved");
      }
      const txHash = computeHash32(Buffer.from(cbor, "hex")).toString("hex");
      if (bodies.has(txHash)) {
        body.free();
        throw new Error("user-event creating bodies are not unique");
      }
      bodies.set(txHash, { cbor, body });
    }
    const usedBodies = new Set<string>();
    const referenceBudget: ReferenceBudget = { members: 0, bytes: totalBytes };
    const transactions = targetBlock.transactions.flatMap(
      (transaction, index) => {
        if (!transaction.isValid) return [];
        const inputs = referenceOutRefs(transaction.body.bytesHex).map(
          (outRef) => {
            const [txHash, rawIndex] = outRef.split("#");
            const creating = bodies.get(txHash!);
            if (creating === undefined) {
              throw new Error(
                "user-event reference has no creating-body preimage",
              );
            }
            const outputIndex = BigInt(rawIndex!);
            const outputs = creating.body.outputs();
            // Cardano indexes a collateral return immediately after normal outputs.
            let output: CML.TransactionOutput | undefined;
            try {
              output =
                outputIndex < BigInt(outputs.len())
                  ? outputs.get(Number(outputIndex))
                  : outputIndex === BigInt(outputs.len())
                    ? creating.body.collateral_return()
                    : undefined;
              if (output === undefined)
                throw new Error(
                  "user-event reference output index does not exist",
                );
              usedBodies.add(txHash!);
              return boundedReferenceOutput(
                outRef,
                output.to_cbor_hex(),
                referenceBudget,
              );
            } finally {
              output?.free();
              outputs.free();
            }
          },
        );
        return [referenceTransaction(targetBlock, index, inputs)];
      },
    );
    if (usedBodies.size !== bodies.size) {
      throw new Error(
        "user-event creating-body evidence includes unrelated bodies",
      );
    }
    return admit({
      assertLive,
      evidence: Object.freeze({
        ...baseEvidence(
          targetBlock,
          deploymentIdentity.manifestId,
          transactions,
        ),
        evidenceKind: "creating_bodies",
        sourceMode,
        creatingTransactionBodies: Object.freeze(
          [...bodies.entries()]
            .sort(([left], [right]) => left.localeCompare(right))
            .map(([, { cbor }]) => cbor),
        ),
      }),
    });
  } finally {
    for (const { body } of bodies.values()) body.free();
  }
};

/**
 * Explicit local preimage admission also supports pending native observations.
 * It preserves native/Kupo/Ogmios agreement and never retries through the
 * release-final raw resolver. It does not decide the target's finality status.
 */
export const createWatcherLocalUserEventReferenceAuthorityFromBodies = ({
  localObservation,
  nativeBlock,
  deploymentIdentity,
  creatingTransactionBodies,
}: {
  readonly localObservation: WatcherLocalKupmiosNativeObservation;
  readonly nativeBlock: WatcherNativeBlockAdmission;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly creatingTransactionBodies: readonly string[];
}): WatcherUserEventReferenceAuthority =>
  createBodyReferenceAuthority({
    targetBlock: localObservation.block,
    deploymentIdentity,
    creatingTransactionBodies,
    sourceMode: "local_node",
    assertLive: () => {
      assertWatcherLocalKupmiosNativeObservation(localObservation, nativeBlock);
      assertTarget(localObservation.block, deploymentIdentity, "local_node");
    },
  });

export const readWatcherUserEventReferenceEvidence = (
  authority: WatcherUserEventReferenceAuthority,
): WatcherUserEventReferenceEvidence => {
  const state = authorities.get(authority);
  if (state === undefined) {
    throw new Error("user-event reference authority was not admitted");
  }
  state.assertLive();
  return state.evidence;
};

/** Match public bytes only after the caller has admitted the target and finality. */
export const admitWatcherUserEventReferenceEvidence = ({
  evidence,
  targetBlock,
  deploymentIdentity,
  referenceAuthorities,
}: {
  readonly evidence: unknown;
  readonly targetBlock: WatcherNormalizedL1Block;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly referenceAuthorities: readonly WatcherUserEventReferenceAuthority[];
}): WatcherUserEventReferenceEvidence | null => {
  try {
    if (
      !Array.isArray(referenceAuthorities) ||
      referenceAuthorities.length > 1_024 ||
      !isWatcherL1AdapterNormalizedBlock(targetBlock)
    )
      return null;
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    const matches = referenceAuthorities.filter((authority) => {
      const state = authorities.get(authority);
      if (
        state === undefined ||
        state.evidence.deploymentIdentityDigest !==
          deploymentIdentity.manifestId ||
        state.evidence.targetObservationDigest !==
          targetBlock.observationDigest ||
        state.evidence.targetBlockContentDigest !==
          targetBlock.blockContentDigest ||
        state.evidence.targetPointDigest !==
          targetBlock.chainPoint.pointDigest ||
        state.evidence.sourceMode !== targetBlock.provider.source.sourceMode
      )
        return false;
      state.assertLive();
      return watcherSameCanonicalJson(state.evidence, evidence);
    });
    return matches.length === 1
      ? readWatcherUserEventReferenceEvidence(matches[0]!)
      : null;
  } catch {
    return null;
  }
};

export const watcherUserEventReferenceOutput = (
  evidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  outRef: string | null,
): CML.TransactionOutput | null => {
  if (outRef === null) return null;
  const transaction = evidence.transactions.find(
    ({ txHash }) => txHash === transactionHash,
  );
  const input = transaction?.referenceInputs.find(
    (candidate) => candidate.outRef === outRef,
  );
  return input === undefined
    ? null
    : CML.TransactionOutput.from_cbor_hex(input.outputCbor);
};

/** Admits captured preimages only through their identical live W12 pair. */
export const createWatcherLocalBackfillUserEventReferenceAuthority = ({
  deploymentIdentity,
  finality,
  observation,
}: Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
}>): WatcherUserEventReferenceAuthority => {
  const assertLive = () => {
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    const pair = readWatcherLocalBackfillFinalityObservation({
      finality,
      observation,
    });
    const capture = pair.observation.capture;
    if (
      capture.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
      capture.blueprintHash !== deploymentIdentity.blueprintHash ||
      capture.network !== deploymentIdentity.network
    )
      throw new Error(
        "user-event backfill references differ from the verified deployment",
      );
    return pair;
  };
  const pair = assertLive();
  const authority = createBodyReferenceAuthority({
    targetBlock: pair.observation.native,
    deploymentIdentity,
    creatingTransactionBodies:
      pair.observation.capture.creatingTransactionBodies,
    sourceMode: "local_node",
    assertLive,
  });
  assertLive();
  backfillAuthorityPairs.set(
    authority,
    Object.freeze({ finality, observation }),
  );
  return authority;
};

/** Public evidence equality is checked only after private identical pairing. */
export const admitWatcherLocalBackfillUserEventReferenceEvidence = ({
  evidence,
  deploymentIdentity,
  finality,
  observation,
  referenceAuthority,
}: Readonly<{
  evidence: unknown;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>): WatcherUserEventReferenceEvidence | null => {
  try {
    const binding = backfillAuthorityPairs.get(referenceAuthority);
    const state = authorities.get(referenceAuthority);
    if (
      binding === undefined ||
      state === undefined ||
      binding.finality !== finality ||
      binding.observation !== observation
    )
      return null;
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    const pair = readWatcherLocalBackfillFinalityObservation({
      finality,
      observation,
    });
    const capture = pair.observation.capture;
    if (
      capture.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
      capture.blueprintHash !== deploymentIdentity.blueprintHash ||
      capture.network !== deploymentIdentity.network
    )
      return null;
    state.assertLive();
    if (!watcherSameCanonicalJson(state.evidence, evidence)) return null;
    readWatcherLocalBackfillFinalityObservation({ finality, observation });
    return readWatcherUserEventReferenceEvidence(referenceAuthority);
  } catch {
    return null;
  }
};
