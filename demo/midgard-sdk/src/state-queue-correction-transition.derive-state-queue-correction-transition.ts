import type { CorrectionLockDatum } from "./correction-lock.js";
import { type StateQueueRedeemer as StateQueueRedeemerType } from "./state-queue.js";
import {
  canonicalNodes,
  decodeStateQueueMintRedeemer,
  outputReferenceLabel,
  timeoutRemoval,
} from "./state-queue-correction-transition.parse-state-queue-correction-lock-witness.js";
import {
  type DeriveStateQueueCorrectionTransitionInput,
  digest,
  HEX_28,
  HEX_32,
  type Json,
  NATURAL,
  OUT_REF,
  stableJson,
  STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION,
  type StateQueueCorrectionLockWitness,
  type StateQueueCorrectionTransition,
  withoutDigest,
} from "./state-queue-correction-transition.state-queue-correction-lock-witness.js";

export const deriveStateQueueCorrectionTransition = (
  input: DeriveStateQueueCorrectionTransitionInput,
): StateQueueCorrectionTransition | null => {
  if (
    !HEX_32.test(input.deploymentIdentityDigest) ||
    !HEX_28.test(input.stateQueuePolicyId) ||
    !HEX_32.test(input.transactionHash) ||
    !HEX_32.test(input.blockHash) ||
    !HEX_32.test(input.chainPointId) ||
    !NATURAL.test(input.slot) ||
    !NATURAL.test(input.blockNo) ||
    !NATURAL.test(input.finalityDepth) ||
    BigInt(input.finalityDepth) === 0n ||
    !canonicalNodes(input.previousQueue) ||
    !canonicalNodes(input.nextQueue) ||
    input.spentInputOutRefs.some((outRef) => !OUT_REF.test(outRef)) ||
    new Set(input.spentInputOutRefs).size !== input.spentInputOutRefs.length
  ) {
    return null;
  }
  const decoded = decodeStateQueueMintRedeemer(input);
  if (
    decoded === null ||
    typeof decoded !== "object" ||
    !(
      "RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveUnavailableBlockAfterTimeout" in decoded
    )
  ) {
    return null;
  }
  const timeout = timeoutRemoval(decoded);
  if (timeout === null) return null;
  const removalApproach = timeout.name;
  const nextByHash = new Map(
    input.nextQueue.map((node) => [node.headerHash, node]),
  );
  const removedHeaderHashes = input.previousQueue
    .filter(
      (node): node is Readonly<{ headerHash: string; outRef: string }> =>
        node.headerHash !== null && !nextByHash.has(node.headerHash),
    )
    .map(({ headerHash }) => headerHash);
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
  const timedOutHeaderHash = timeout.target;
  const previousHashes = input.previousQueue.map(
    ({ headerHash }) => headerHash,
  );
  const targetIndex = previousHashes.indexOf(timedOutHeaderHash);
  const removedIndex = timeout.prune ? targetIndex + 1 : targetIndex;
  const anchorIndex = timeout.prune ? targetIndex : targetIndex - 1;
  const continuedIdentity = continuedQueueOutRefs[0];
  const exactTopology =
    targetIndex > 0 &&
    (!timeout.headOnly || targetIndex === 1) &&
    removedIndex < input.previousQueue.length &&
    (timeout.prune || targetIndex === input.previousQueue.length - 1) &&
    changed.length === 2 &&
    changed.every(({ outRef }) => spent.has(outRef)) &&
    input.spentInputOutRefs
      .filter((outRef) =>
        input.previousQueue.some((node) => node.outRef === outRef),
      )
      .every((outRef) => consumedQueueOutRefs.includes(outRef)) &&
    continuedQueueOutRefs.length === 1 &&
    continuedIdentity?.headerHash === previousHashes[anchorIndex] &&
    outputReferenceLabel(timeout.anchor) ===
      continuedIdentity?.consumedOutRef &&
    continuedIdentity?.producedOutRef ===
      `${input.transactionHash}#${timeout.outputIndex.toString()}` &&
    removedHeaderHashes.length === 1 &&
    removedHeaderHashes[0] === previousHashes[removedIndex] &&
    input.nextQueue.length === input.previousQueue.length - 1 &&
    input.nextQueue.every(
      ({ headerHash }, index) =>
        headerHash === previousHashes[index < removedIndex ? index : index + 1],
    );
  if (!exactTopology) {
    return null;
  }
  const canonical = {
    schemaVersion: STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION,
    deploymentIdentityDigest: input.deploymentIdentityDigest,
    stateQueuePolicyId: input.stateQueuePolicyId,
    transactionHash: input.transactionHash,
    blockHash: input.blockHash,
    slot: input.slot,
    blockNo: input.blockNo,
    chainPointId: input.chainPointId,
    finalityDepth: input.finalityDepth,
    timedOutHeaderHash,
    removalApproach,
    consumedQueueOutRefs,
    continuedQueueOutRefs,
    removedHeaderHashes,
  } satisfies Omit<StateQueueCorrectionTransition, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(withoutDigest(canonical)),
  });
};

export const correctionLockWitnessMatchesTransition = ({
  decoded,
  witness,
  spentInputOutRefs,
  referenceInputOutRefs,
  transactionHash,
}: {
  readonly decoded: StateQueueRedeemerType;
  readonly witness: StateQueueCorrectionLockWitness;
  readonly spentInputOutRefs: readonly string[];
  readonly referenceInputOutRefs: readonly string[];
  readonly transactionHash: string;
}): boolean => {
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    "MergeToConfirmedStateV1" in decoded
  ) {
    return (
      witness.kind === "idle_reference" &&
      witness.datum === "Idle" &&
      referenceInputOutRefs.includes(witness.referenceOutRef) &&
      !spentInputOutRefs.includes(witness.referenceOutRef)
    );
  }
  if (
    witness.kind !== "correction_transition" ||
    !spentInputOutRefs.includes(witness.consumedOutRef) ||
    witness.continuedOutRef === witness.consumedOutRef ||
    !witness.continuedOutRef.startsWith(`${transactionHash}#`)
  ) {
    return false;
  }
  let terminal: boolean;
  let targetHeaderHash: string;
  let identityMatches: boolean;
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    "RemoveUnattestedBlockAfterTimeout" in decoded
  ) {
    const timeout = decoded.RemoveUnattestedBlockAfterTimeout;
    terminal = "RemoveLastUnattestedBlock" in timeout.removal_approach;
    targetHeaderHash = timeout.timed_out_header_hash;
    identityMatches = witness.correctionIdentity === "AttestationTimeout";
  } else if (
    typeof decoded === "object" &&
    decoded !== null &&
    "RemoveUnavailableBlockAfterTimeout" in decoded
  ) {
    const timeout = decoded.RemoveUnavailableBlockAfterTimeout;
    terminal = "RemoveTimedOutHead" in timeout.removal_approach;
    targetHeaderHash = timeout.unavailable_header_hash;
    identityMatches =
      typeof witness.correctionIdentity === "object" &&
      witness.correctionIdentity !== null &&
      "AvailabilityChallenge" in witness.correctionIdentity &&
      witness.correctionIdentity.AvailabilityChallenge.challenge_asset_name ===
        timeout.challenge_asset_name;
  } else if (
    typeof decoded === "object" &&
    decoded !== null &&
    "RemoveFraudulentBlockHeader" in decoded
  ) {
    const removal = decoded.RemoveFraudulentBlockHeader;
    terminal = "RemoveLastFraudulentBlock" in removal.block_removal_approach;
    targetHeaderHash = removal.fraudulent_blocks_header_hash;
    identityMatches =
      typeof witness.correctionIdentity === "object" &&
      witness.correctionIdentity !== null &&
      "FraudProof" in witness.correctionIdentity &&
      witness.correctionIdentity.FraudProof.fraud_proof_asset_name.slice(8) ===
        targetHeaderHash;
  } else {
    return false;
  }
  if (witness.targetHeaderHash !== targetHeaderHash || !identityMatches) {
    return false;
  }
  const expectedLocked: CorrectionLockDatum = {
    Locked: {
      target_header_hash: targetHeaderHash,
      correction_identity: witness.correctionIdentity,
    },
  };
  return (
    (witness.previousDatum === "Idle" ||
      stableJson(witness.previousDatum as unknown as Json) ===
        stableJson(expectedLocked as unknown as Json)) &&
    (terminal
      ? witness.nextDatum === "Idle"
      : stableJson(witness.nextDatum as unknown as Json) ===
        stableJson(expectedLocked as unknown as Json))
  );
};
