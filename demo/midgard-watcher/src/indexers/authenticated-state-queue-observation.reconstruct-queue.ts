import { type FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  type LockOutput,
  type QueueNode,
  type QueueOutput,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  lockOutput,
  queueOutput,
} from "./authenticated-state-queue-observation.queue-output.js";

export const reconstructQueue = ({
  previousQueue,
  transactionHash,
  spentInputOutRefs,
  resolvedInputs,
  outputs,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  previousQueue: readonly QueueNode[];
  transactionHash: string;
  spentInputOutRefs: readonly string[];
  resolvedInputs: FraudProofRawL1Transaction["resolvedInputs"];
  outputs: readonly QueueOutput[];
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): readonly QueueNode[] => {
  const spent = new Set(spentInputOutRefs);
  const consumed = resolvedInputs.flatMap((input) => {
    const decoded = queueOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  if (
    consumed.length === 0 ||
    consumed.some(
      ({ node }) =>
        !previousQueue.some(
          (prior) =>
            prior.outRef === node.outRef &&
            prior.headerHash === node.headerHash,
        ),
    )
  ) {
    throw new Error(
      "state-queue inputs do not extend the authenticated queue cursor",
    );
  }
  const previousIdentities = new Set(
    previousQueue.map(({ headerHash }) => headerHash),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.some(
      ({ node }) =>
        previousIdentities.has(node.headerHash) &&
        !previousQueue.some(
          (prior) =>
            prior.headerHash === node.headerHash && spent.has(prior.outRef),
        ),
    )
  ) {
    throw new Error(
      "state-queue continuation was not backed by its exact input",
    );
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousIdentities.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new Error(
      "state-queue transaction introduced a non-canonical identity set",
    );
  }
  const nextQueue: readonly QueueNode[] = Object.freeze(
    [...retained, ...introduced].map((node) =>
      Object.freeze({ headerHash: node.headerHash, outRef: node.outRef }),
    ),
  );
  if (nextQueue.length > 0 && nextQueue[0]!.headerHash !== null) {
    throw new Error("state-queue transaction removed or misplaced the root");
  }
  const expectedLinks = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedLinks.get(node.headerHash) !== nextHeaderHash,
    ) ||
    (nextQueue.length > 0 &&
      nextQueue.every(
        ({ outRef }) => !outRef.startsWith(`${transactionHash}#`),
      ))
  ) {
    throw new Error("state-queue output links or continuation are invalid");
  }
  return nextQueue;
};

export const decodeLockOutputs = ({
  body,
  transactionHash,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  body: CML.TransactionBody;
  transactionHash: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
}): readonly LockOutput[] => {
  const result: LockOutput[] = [];
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const decoded = lockOutput({
      output: outputs.get(index),
      outRef: `${transactionHash}#${index.toString()}`,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    if (decoded !== null) result.push(decoded);
  }
  return result;
};

export const orderedResolved = (
  labels: readonly string[],
  values: FraudProofRawL1Transaction["resolvedInputs"],
): readonly FraudProofRawL1Transaction["resolvedInputs"][number][] => {
  const byRef = new Map(values.map((value) => [value.outRef, value]));
  return labels.map((label) => {
    const value = byRef.get(label);
    if (value === undefined)
      throw new Error("resolved input order is incomplete");
    return value;
  });
};

/**
 * The values of `labels` that resolved, in label order. For a reader whose
 * unresolved inputs are known to carry no protocol unit (the follower's
 * tracked set covers every queue, lock and fraud-proof output), so a missing
 * label is a plain input rather than an incomplete read.
 */
export const resolvedInOrder = (
  labels: readonly string[],
  values: FraudProofRawL1Transaction["resolvedInputs"],
): readonly FraudProofRawL1Transaction["resolvedInputs"][number][] => {
  const byRef = new Map(values.map((value) => [value.outRef, value]));
  return labels.flatMap((label) => {
    const value = byRef.get(label);
    return value === undefined ? [] : [value];
  });
};

export const fraudProofIdentity = ({
  proof,
  fraudProofPolicyId,
  fraudProofAddress,
  targetHeaderHash,
}: {
  proof: FraudProofRawL1Transaction["resolvedReferenceInputs"][number];
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  targetHeaderHash: string;
}): SDK.CorrectionIdentity => {
  const output = CML.TransactionOutput.from_cbor_hex(proof.outputCbor);
  const core = coreToTxOutput(output);
  const matching = Object.entries(core.assets).filter(([unit, quantity]) => {
    const assetName = unit.slice(fraudProofPolicyId.length);
    return (
      unit.startsWith(fraudProofPolicyId) &&
      quantity === 1n &&
      /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash
    );
  });
  if (
    core.address !== fraudProofAddress ||
    matching.length !== 1 ||
    Object.entries(core.assets).filter(
      ([unit, quantity]) =>
        unit.startsWith(fraudProofPolicyId) && quantity !== 0n,
    ).length !== 1 ||
    output.datum()?.as_datum() === undefined
  ) {
    throw new Error(
      "fraud correction reference is not the exact permanent proof",
    );
  }
  return {
    FraudProof: {
      fraud_proof_asset_name: matching[0]![0].slice(fraudProofPolicyId.length),
    },
  };
};
