import { CML, Data } from "@lucid-evolution/lucid";

import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";
import { parseStateQueueCorrectionTransition } from "./state-queue-correction-transition.derive-state-queue-authenticated-transition.js";
import { correctionLockWitnessMatchesTransition } from "./state-queue-correction-transition.derive-state-queue-correction-transition.js";
import {
  outputReferenceLabel,
  parseCanonicalNodes,
  parseStateQueueCorrectionLockWitness,
  timeoutRemoval,
} from "./state-queue-correction-transition.parse-state-queue-correction-lock-witness.js";
import {
  digest,
  exactRecord,
  HEX_28,
  HEX_32,
  type Json,
  NATURAL,
  OUT_REF,
  stableJson,
  STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION,
  type StateQueueAuthenticatedTransition,
} from "./state-queue-correction-transition.state-queue-correction-lock-witness.js";

export const parseStateQueueAuthenticatedTransition = (
  value: unknown,
): StateQueueAuthenticatedTransition | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "deploymentIdentityDigest",
    "stateQueuePolicyId",
    "transactionHash",
    "blockHash",
    "slot",
    "blockNo",
    "transactionIndex",
    "chainPointId",
    "finalityDepth",
    "transitionKind",
    "stateQueueMintRedeemer",
    "previousQueue",
    "nextQueue",
    "consumedQueueOutRefs",
    "continuedQueueOutRefs",
    "removedHeaderHashes",
    "correctionLockWitness",
    "correctionTransition",
    "transitionDigest",
  ]);
  const mintRedeemer = exactRecord(record?.stateQueueMintRedeemer, [
    "purpose",
    "index",
    "cborHex",
  ]);
  const continued = Array.isArray(record?.continuedQueueOutRefs)
    ? record.continuedQueueOutRefs.map((entry) =>
        exactRecord(entry, ["headerHash", "consumedOutRef", "producedOutRef"]),
      )
    : null;
  const correction =
    record?.correctionTransition === null
      ? null
      : parseStateQueueCorrectionTransition(record?.correctionTransition);
  const correctionLockWitness = parseStateQueueCorrectionLockWitness(
    record?.correctionLockWitness,
  );
  const previousQueue = parseCanonicalNodes(record?.previousQueue);
  const nextQueue = parseCanonicalNodes(record?.nextQueue);
  if (
    record === null ||
    record.schemaVersion !==
      STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION ||
    !HEX_32.test(record.deploymentIdentityDigest as string) ||
    !HEX_28.test(record.stateQueuePolicyId as string) ||
    !HEX_32.test(record.transactionHash as string) ||
    !HEX_32.test(record.blockHash as string) ||
    !HEX_32.test(record.chainPointId as string) ||
    !NATURAL.test(record.slot as string) ||
    !NATURAL.test(record.blockNo as string) ||
    !NATURAL.test(record.transactionIndex as string) ||
    !NATURAL.test(record.finalityDepth as string) ||
    BigInt(record.finalityDepth as string) === 0n ||
    (record.transitionKind !== "timeout_correction" &&
      record.transitionKind !== "merge" &&
      record.transitionKind !== "fraud_removal") ||
    mintRedeemer === null ||
    mintRedeemer.purpose !== "mint" ||
    !NATURAL.test(mintRedeemer.index as string) ||
    typeof mintRedeemer.cborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(mintRedeemer.cborHex) ||
    previousQueue === null ||
    nextQueue === null ||
    !Array.isArray(record.consumedQueueOutRefs) ||
    record.consumedQueueOutRefs.some(
      (entry) => !OUT_REF.test(entry as string),
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
    correctionLockWitness === null ||
    (record.transitionKind === "timeout_correction"
      ? correction === null
      : record.correctionTransition !== null) ||
    !HEX_32.test(record.transitionDigest as string)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: record.schemaVersion,
    deploymentIdentityDigest: record.deploymentIdentityDigest as string,
    stateQueuePolicyId: record.stateQueuePolicyId as string,
    transactionHash: record.transactionHash as string,
    blockHash: record.blockHash as string,
    slot: record.slot as string,
    blockNo: record.blockNo as string,
    transactionIndex: record.transactionIndex as string,
    chainPointId: record.chainPointId as string,
    finalityDepth: record.finalityDepth as string,
    transitionKind: record.transitionKind,
    stateQueueMintRedeemer: {
      purpose: "mint",
      index: mintRedeemer.index as string,
      cborHex: mintRedeemer.cborHex as string,
    },
    previousQueue,
    nextQueue,
    consumedQueueOutRefs: record.consumedQueueOutRefs as string[],
    continuedQueueOutRefs: continued.map((entry) => ({
      headerHash: entry!.headerHash as string | null,
      consumedOutRef: entry!.consumedOutRef as string,
      producedOutRef: entry!.producedOutRef as string,
    })),
    removedHeaderHashes: record.removedHeaderHashes as string[],
    correctionLockWitness,
    correctionTransition: correction,
  } satisfies Omit<StateQueueAuthenticatedTransition, "transitionDigest">;
  let decoded: StateQueueRedeemerType;
  try {
    decoded = Data.from(
      canonical.stateQueueMintRedeemer.cborHex,
      StateQueueRedeemer,
    ) as StateQueueRedeemerType;
    if (
      Data.to(decoded, StateQueueRedeemer) !==
        canonical.stateQueueMintRedeemer.cborHex &&
      CML.PlutusData.from_cbor_hex(
        canonical.stateQueueMintRedeemer.cborHex,
      ).to_canonical_cbor_hex() !== canonical.stateQueueMintRedeemer.cborHex
    ) {
      return null;
    }
  } catch {
    return null;
  }
  const sortedUnique = (values: readonly string[]): boolean =>
    new Set(values).size === values.length &&
    values.every(
      (entry, index) =>
        index === 0 || values[index - 1]!.localeCompare(entry) < 0,
    );
  const continuedConsumed = canonical.continuedQueueOutRefs.map(
    ({ consumedOutRef }) => consumedOutRef,
  );
  const nextByIdentity = new Map(
    canonical.nextQueue.map((node) => [node.headerHash, node]),
  );
  const topologyChanged = canonical.previousQueue.filter(
    (node) => nextByIdentity.get(node.headerHash)?.outRef !== node.outRef,
  );
  const topologyConsumed = topologyChanged.map(({ outRef }) => outRef).sort();
  const topologyContinued = topologyChanged
    .flatMap((node) => {
      const next = nextByIdentity.get(node.headerHash);
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
  const topologyRemoved = canonical.previousQueue
    .filter(
      (node): node is Readonly<{ headerHash: string; outRef: string }> =>
        node.headerHash !== null && !nextByIdentity.has(node.headerHash),
    )
    .map(({ headerHash }) => headerHash)
    .sort();
  const expectedSurvivorOrder = canonical.previousQueue
    .filter(({ headerHash }) => !topologyRemoved.includes(headerHash!))
    .map(({ headerHash }) => headerHash);
  const actualSurvivorOrder = canonical.nextQueue.map(
    ({ headerHash }) => headerHash,
  );
  const removalTopologyIsCanonical =
    canonical.consumedQueueOutRefs.length === 2 &&
    canonical.continuedQueueOutRefs.length === 1 &&
    canonical.removedHeaderHashes.length === 1 &&
    sortedUnique(canonical.consumedQueueOutRefs) &&
    sortedUnique(continuedConsumed) &&
    sortedUnique(canonical.removedHeaderHashes) &&
    stableJson(topologyConsumed) ===
      stableJson(canonical.consumedQueueOutRefs) &&
    stableJson(topologyContinued) ===
      stableJson(canonical.continuedQueueOutRefs) &&
    stableJson(topologyRemoved) === stableJson(canonical.removedHeaderHashes) &&
    stableJson(expectedSurvivorOrder) === stableJson(actualSurvivorOrder) &&
    canonical.continuedQueueOutRefs.every(
      ({ consumedOutRef, producedOutRef }) =>
        canonical.consumedQueueOutRefs.includes(consumedOutRef) &&
        producedOutRef.startsWith(`${canonical.transactionHash}#`),
    );
  let semanticsAreCanonical = false;
  const correctionLockSemanticsAreCanonical =
    correctionLockWitnessMatchesTransition({
      decoded,
      witness: canonical.correctionLockWitness,
      spentInputOutRefs:
        canonical.correctionLockWitness.kind === "correction_transition"
          ? [
              ...canonical.consumedQueueOutRefs,
              canonical.correctionLockWitness.consumedOutRef,
            ]
          : canonical.consumedQueueOutRefs,
      referenceInputOutRefs:
        canonical.correctionLockWitness.kind === "idle_reference"
          ? [canonical.correctionLockWitness.referenceOutRef]
          : [],
      transactionHash: canonical.transactionHash,
    });
  if (
    canonical.transitionKind === "timeout_correction" &&
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveUnavailableBlockAfterTimeout" in decoded) &&
    canonical.correctionTransition !== null
  ) {
    const nested = canonical.correctionTransition;
    const timeout = timeoutRemoval(decoded);
    if (timeout === null) return null;
    const outerNestedIdentityMatches =
      nested.deploymentIdentityDigest === canonical.deploymentIdentityDigest &&
      nested.stateQueuePolicyId === canonical.stateQueuePolicyId &&
      nested.transactionHash === canonical.transactionHash &&
      nested.blockHash === canonical.blockHash &&
      nested.slot === canonical.slot &&
      nested.blockNo === canonical.blockNo &&
      nested.chainPointId === canonical.chainPointId &&
      nested.finalityDepth === canonical.finalityDepth &&
      stableJson(nested.consumedQueueOutRefs) ===
        stableJson(canonical.consumedQueueOutRefs) &&
      stableJson(nested.continuedQueueOutRefs) ===
        stableJson(canonical.continuedQueueOutRefs) &&
      stableJson(nested.removedHeaderHashes) ===
        stableJson(canonical.removedHeaderHashes) &&
      nested.timedOutHeaderHash === timeout.target;
    const approachMatches =
      nested.removalApproach === timeout.name &&
      outputReferenceLabel(timeout.anchor) ===
        nested.continuedQueueOutRefs[0]?.consumedOutRef &&
      `${canonical.transactionHash}#${timeout.outputIndex.toString()}` ===
        nested.continuedQueueOutRefs[0]?.producedOutRef;
    const targetIndex = canonical.previousQueue.findIndex(
      ({ headerHash }) => headerHash === timeout.target,
    );
    const removedIndex = timeout.prune ? targetIndex + 1 : targetIndex;
    const anchorIndex = timeout.prune ? targetIndex : targetIndex - 1;
    semanticsAreCanonical =
      outerNestedIdentityMatches &&
      approachMatches &&
      targetIndex > 0 &&
      (!timeout.headOnly || targetIndex === 1) &&
      removedIndex < canonical.previousQueue.length &&
      (timeout.prune || targetIndex === canonical.previousQueue.length - 1) &&
      nested.removedHeaderHashes[0] ===
        canonical.previousQueue[removedIndex]?.headerHash &&
      nested.continuedQueueOutRefs[0]?.headerHash ===
        canonical.previousQueue[anchorIndex]?.headerHash;
  } else if (
    canonical.transitionKind === "merge" &&
    canonical.correctionTransition === null &&
    typeof decoded === "object" &&
    decoded !== null &&
    "MergeToConfirmedStateV1" in decoded
  ) {
    const merge = decoded.MergeToConfirmedStateV1;
    const continued = canonical.continuedQueueOutRefs[0];
    semanticsAreCanonical =
      canonical.removedHeaderHashes[0] === merge.header_node_key &&
      continued?.headerHash === null &&
      continued.consumedOutRef ===
        outputReferenceLabel(merge.confirmed_state_input_outref) &&
      continued.producedOutRef ===
        `${canonical.transactionHash}#${merge.confirmed_state_output_index.toString()}`;
  } else if (
    canonical.transitionKind === "fraud_removal" &&
    canonical.correctionTransition === null &&
    typeof decoded === "object" &&
    decoded !== null &&
    "RemoveFraudulentBlockHeader" in decoded
  ) {
    const removal = decoded.RemoveFraudulentBlockHeader;
    const continued = canonical.continuedQueueOutRefs[0];
    semanticsAreCanonical =
      continued !== undefined &&
      ("RemoveLastFraudulentBlock" in removal.block_removal_approach
        ? canonical.removedHeaderHashes[0] ===
            removal.fraudulent_blocks_header_hash &&
          continued.consumedOutRef ===
            outputReferenceLabel(
              removal.block_removal_approach.RemoveLastFraudulentBlock
                .anchor_element_input_outref,
            ) &&
          continued.producedOutRef ===
            `${canonical.transactionHash}#${removal.block_removal_approach.RemoveLastFraudulentBlock.anchor_element_output_index.toString()}`
        : continued.headerHash === removal.fraudulent_blocks_header_hash &&
          continued.consumedOutRef ===
            outputReferenceLabel(
              removal.block_removal_approach.RemoveFraudulentBlocksLink
                .fraudulent_node_input_outref,
            ) &&
          continued.producedOutRef ===
            `${canonical.transactionHash}#${removal.block_removal_approach.RemoveFraudulentBlocksLink.fraudulent_node_output_index.toString()}`);
  }
  return removalTopologyIsCanonical &&
    semanticsAreCanonical &&
    correctionLockSemanticsAreCanonical &&
    digest(canonical as unknown as Json) === record.transitionDigest
    ? Object.freeze({
        ...canonical,
        transitionDigest: record.transitionDigest as string,
      })
    : null;
};
