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
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import {
  admit,
  assertTarget,
  authorities,
  backfillAuthorityPairs,
  baseEvidence,
  boundedReferenceOutput,
  type ReferenceBudget,
  referenceOutRefs,
  referenceTransaction,
  type WatcherUserEventReferenceAuthority,
  type WatcherUserEventReferenceEvidence,
} from "./user-event-reference-authority.create-watcher-local-user-event-reference-authority.js";

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
