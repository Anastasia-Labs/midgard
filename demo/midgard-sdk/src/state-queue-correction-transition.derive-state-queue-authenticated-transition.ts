import {
  correctionLockWitnessMatchesTransition,
  deriveStateQueueCorrectionTransition,
} from "./state-queue-correction-transition.derive-state-queue-correction-transition.js";
import {
  canonicalNodes,
  decodeStateQueueMintRedeemer,
  outputReferenceLabel,
  parseStateQueueCorrectionLockWitness,
} from "./state-queue-correction-transition.parse-state-queue-correction-lock-witness.js";
import {
  type DeriveStateQueueAuthenticatedTransitionInput,
  digest,
  exactRecord,
  HEX_28,
  HEX_32,
  type Json,
  NATURAL,
  OUT_REF,
  STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION,
  STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION,
  type StateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransitionKind,
  type StateQueueCorrectionTransition,
  withoutDigest,
} from "./state-queue-correction-transition.state-queue-correction-lock-witness.js";

export const deriveStateQueueAuthenticatedTransition = (
  input: DeriveStateQueueAuthenticatedTransitionInput,
): StateQueueAuthenticatedTransition | null => {
  if (
    !HEX_32.test(input.deploymentIdentityDigest) ||
    !HEX_28.test(input.stateQueuePolicyId) ||
    !HEX_32.test(input.transactionHash) ||
    !HEX_32.test(input.blockHash) ||
    !HEX_32.test(input.chainPointId) ||
    !NATURAL.test(input.slot) ||
    !NATURAL.test(input.blockNo) ||
    !NATURAL.test(input.transactionIndex) ||
    !NATURAL.test(input.finalityDepth) ||
    BigInt(input.finalityDepth) === 0n ||
    !canonicalNodes(input.previousQueue) ||
    !canonicalNodes(input.nextQueue) ||
    input.spentInputOutRefs.some((reference) => !OUT_REF.test(reference)) ||
    new Set(input.spentInputOutRefs).size !== input.spentInputOutRefs.length ||
    input.referenceInputOutRefs.some((reference) => !OUT_REF.test(reference)) ||
    new Set(input.referenceInputOutRefs).size !==
      input.referenceInputOutRefs.length
  ) {
    return null;
  }
  const correctionLockWitness = parseStateQueueCorrectionLockWitness(
    input.correctionLockWitness,
  );
  const decoded = decodeStateQueueMintRedeemer(input);
  if (
    decoded === null ||
    typeof decoded !== "object" ||
    correctionLockWitness === null ||
    !correctionLockWitnessMatchesTransition({
      decoded,
      witness: correctionLockWitness,
      spentInputOutRefs: input.spentInputOutRefs,
      referenceInputOutRefs: input.referenceInputOutRefs,
      transactionHash: input.transactionHash,
    })
  ) {
    return null;
  }
  const policyIndex = input.mintPolicyIds.indexOf(input.stateQueuePolicyId);
  const redeemer = input.redeemers.find(
    (candidate) =>
      candidate.purpose === "mint" &&
      candidate.index === policyIndex.toString(),
  );
  if (policyIndex < 0 || redeemer === undefined) return null;

  const nextByHash = new Map(
    input.nextQueue.map((node) => [node.headerHash, node]),
  );
  const changed = input.previousQueue.filter(
    (node) => nextByHash.get(node.headerHash)?.outRef !== node.outRef,
  );
  const spent = new Set(input.spentInputOutRefs);
  const consumedQueueOutRefs = changed.map(({ outRef }) => outRef).sort();
  const continuedQueueOutRefs = changed
    .flatMap((node) => {
      const next = nextByHash.get(node.headerHash);
      return next === undefined
        ? []
        : [
            {
              headerHash: node.headerHash,
              consumedOutRef: node.outRef,
              producedOutRef: next.outRef,
            },
          ];
    })
    .sort((left, right) =>
      left.consumedOutRef.localeCompare(right.consumedOutRef),
    );
  const removedHeaderHashes = input.previousQueue
    .filter(
      (node): node is Readonly<{ headerHash: string; outRef: string }> =>
        node.headerHash !== null && !nextByHash.has(node.headerHash),
    )
    .map(({ headerHash }) => headerHash);
  const previousHashes = input.previousQueue.map(
    ({ headerHash }) => headerHash,
  );
  const nextHashes = input.nextQueue.map(({ headerHash }) => headerHash);
  const removalShapeIsExact =
    changed.length === 2 &&
    changed.every(({ outRef }) => spent.has(outRef)) &&
    continuedQueueOutRefs.length === 1 &&
    continuedQueueOutRefs[0]!.producedOutRef.startsWith(
      `${input.transactionHash}#`,
    ) &&
    removedHeaderHashes.length === 1 &&
    input.nextQueue.length === input.previousQueue.length - 1 &&
    nextHashes.every(
      (hash, index) =>
        hash ===
        previousHashes[
          index < previousHashes.indexOf(removedHeaderHashes[0]!)
            ? index
            : index + 1
        ],
    );
  if (!removalShapeIsExact) return null;

  let transitionKind: StateQueueAuthenticatedTransitionKind;
  let correctionTransition: StateQueueCorrectionTransition | null = null;
  if (
    "RemoveUnattestedBlockAfterTimeout" in decoded ||
    "RemoveUnavailableBlockAfterTimeout" in decoded
  ) {
    correctionTransition = deriveStateQueueCorrectionTransition(input);
    if (correctionTransition === null) return null;
    transitionKind = "timeout_correction";
  } else if ("MergeToConfirmedStateV1" in decoded) {
    const merge = decoded.MergeToConfirmedStateV1;
    const continued = continuedQueueOutRefs[0]!;
    if (
      removedHeaderHashes[0] !== input.previousQueue[1]?.headerHash ||
      merge.header_node_key !== removedHeaderHashes[0] ||
      continued.headerHash !== null ||
      outputReferenceLabel(merge.confirmed_state_input_outref) !==
        continued.consumedOutRef ||
      `${input.transactionHash}#${merge.confirmed_state_output_index.toString()}` !==
        continued.producedOutRef
    ) {
      return null;
    }
    transitionKind = "merge";
  } else if ("RemoveFraudulentBlockHeader" in decoded) {
    const removal = decoded.RemoveFraudulentBlockHeader;
    const removedIndex = previousHashes.indexOf(removedHeaderHashes[0]!);
    const continued = continuedQueueOutRefs[0]!;
    const exactApproach =
      "RemoveLastFraudulentBlock" in removal.block_removal_approach
        ? removedHeaderHashes[0] === removal.fraudulent_blocks_header_hash &&
          removedIndex === previousHashes.length - 1 &&
          continued.headerHash === previousHashes[removedIndex - 1] &&
          outputReferenceLabel(
            removal.block_removal_approach.RemoveLastFraudulentBlock
              .anchor_element_input_outref,
          ) === continued.consumedOutRef &&
          `${input.transactionHash}#${removal.block_removal_approach.RemoveLastFraudulentBlock.anchor_element_output_index.toString()}` ===
            continued.producedOutRef
        : continued.headerHash === removal.fraudulent_blocks_header_hash &&
          removedIndex > 1 &&
          previousHashes[removedIndex - 1] ===
            removal.fraudulent_blocks_header_hash &&
          outputReferenceLabel(
            removal.block_removal_approach.RemoveFraudulentBlocksLink
              .fraudulent_node_input_outref,
          ) === continued.consumedOutRef &&
          `${input.transactionHash}#${removal.block_removal_approach.RemoveFraudulentBlocksLink.fraudulent_node_output_index.toString()}` ===
            continued.producedOutRef;
    if (!exactApproach) return null;
    transitionKind = "fraud_removal";
  } else {
    return null;
  }

  const canonical = {
    schemaVersion: STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION,
    deploymentIdentityDigest: input.deploymentIdentityDigest,
    stateQueuePolicyId: input.stateQueuePolicyId,
    transactionHash: input.transactionHash,
    blockHash: input.blockHash,
    slot: input.slot,
    blockNo: input.blockNo,
    transactionIndex: input.transactionIndex,
    chainPointId: input.chainPointId,
    finalityDepth: input.finalityDepth,
    transitionKind,
    stateQueueMintRedeemer: redeemer,
    previousQueue: input.previousQueue,
    nextQueue: input.nextQueue,
    consumedQueueOutRefs,
    continuedQueueOutRefs,
    removedHeaderHashes,
    correctionLockWitness,
    correctionTransition,
  } satisfies Omit<StateQueueAuthenticatedTransition, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(canonical as unknown as Json),
  });
};

export const parseStateQueueCorrectionTransition = (
  value: unknown,
): StateQueueCorrectionTransition | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "deploymentIdentityDigest",
    "stateQueuePolicyId",
    "transactionHash",
    "blockHash",
    "slot",
    "blockNo",
    "chainPointId",
    "finalityDepth",
    "timedOutHeaderHash",
    "removalApproach",
    "consumedQueueOutRefs",
    "continuedQueueOutRefs",
    "removedHeaderHashes",
    "transitionDigest",
  ]);
  const continued = Array.isArray(record?.continuedQueueOutRefs)
    ? record.continuedQueueOutRefs.map((value) =>
        exactRecord(value, ["headerHash", "consumedOutRef", "producedOutRef"]),
      )
    : null;
  if (
    record === null ||
    record.schemaVersion !== STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION ||
    !HEX_32.test(record.deploymentIdentityDigest as string) ||
    !HEX_28.test(record.stateQueuePolicyId as string) ||
    !HEX_32.test(record.transactionHash as string) ||
    !HEX_32.test(record.blockHash as string) ||
    !HEX_32.test(record.chainPointId as string) ||
    !NATURAL.test(record.slot as string) ||
    !NATURAL.test(record.blockNo as string) ||
    !NATURAL.test(record.finalityDepth as string) ||
    BigInt(record.finalityDepth as string) === 0n ||
    !HEX_28.test(record.timedOutHeaderHash as string) ||
    (record.removalApproach !== "PruneUnattestedBlockDescendant" &&
      record.removalApproach !== "RemoveLastUnattestedBlock" &&
      record.removalApproach !== "PruneTimedOutBlockDescendant" &&
      record.removalApproach !== "RemoveTimedOutHead") ||
    !Array.isArray(record.consumedQueueOutRefs) ||
    record.consumedQueueOutRefs.some(
      (outRef) => !OUT_REF.test(outRef as string),
    ) ||
    continued === null ||
    continued.some(
      (entry) =>
        entry === null ||
        !(
          entry.headerHash === null || HEX_28.test(entry.headerHash as string)
        ) ||
        !OUT_REF.test(entry.consumedOutRef as string) ||
        !OUT_REF.test(entry.producedOutRef as string),
    ) ||
    !Array.isArray(record.removedHeaderHashes) ||
    record.removedHeaderHashes.some((hash) => !HEX_28.test(hash as string)) ||
    !HEX_32.test(record.transitionDigest as string)
  ) {
    return null;
  }
  const canonicalContinued = continued.map((entry) => ({
    headerHash: entry!.headerHash as string | null,
    consumedOutRef: entry!.consumedOutRef as string,
    producedOutRef: entry!.producedOutRef as string,
  }));
  const canonical = {
    schemaVersion: record.schemaVersion,
    deploymentIdentityDigest: record.deploymentIdentityDigest as string,
    stateQueuePolicyId: record.stateQueuePolicyId as string,
    transactionHash: record.transactionHash as string,
    blockHash: record.blockHash as string,
    slot: record.slot as string,
    blockNo: record.blockNo as string,
    chainPointId: record.chainPointId as string,
    finalityDepth: record.finalityDepth as string,
    timedOutHeaderHash: record.timedOutHeaderHash as string,
    removalApproach: record.removalApproach,
    consumedQueueOutRefs: record.consumedQueueOutRefs as string[],
    continuedQueueOutRefs: canonicalContinued,
    removedHeaderHashes: record.removedHeaderHashes as string[],
  } satisfies Omit<StateQueueCorrectionTransition, "transitionDigest">;
  return digest(withoutDigest(canonical)) === record.transitionDigest
    ? Object.freeze({
        ...canonical,
        transitionDigest: record.transitionDigest as string,
      })
    : null;
};
