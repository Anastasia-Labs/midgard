import {
  canonicalQueue,
  canonicalRedeemer,
  type DeriveStateQueueAuthenticatedReplayCheckpointInput,
  digest,
  exactRecord,
  HEX_28,
  HEX_32,
  NATURAL,
  OUT_REF,
  outputIndex,
  sameIdentities,
  STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION,
  type StateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedReplayCheckpointKind,
} from "./state-queue-authenticated-replay-checkpoint.canonical-redeemer.js";
import {
  deriveStateQueueAuthenticatedTransition,
  parseStateQueueCorrectionLockWitness,
  type StateQueueTransitionNode,
  type StateQueueTransitionRedeemer,
} from "./state-queue-correction-transition.js";

export const deriveStateQueueAuthenticatedReplayCheckpoint = (
  input: DeriveStateQueueAuthenticatedReplayCheckpointInput,
): StateQueueAuthenticatedReplayCheckpoint | null => {
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
    !canonicalQueue(input.previousQueue, true) ||
    !canonicalQueue(input.nextQueue, true) ||
    input.mintPolicyIds.some((policy) => !HEX_28.test(policy)) ||
    new Set(input.mintPolicyIds).size !== input.mintPolicyIds.length ||
    input.mintPolicyIds.some(
      (policy, index) =>
        index > 0 && input.mintPolicyIds[index - 1]!.localeCompare(policy) >= 0,
    ) ||
    input.spentInputOutRefs.some((reference) => !OUT_REF.test(reference)) ||
    new Set(input.spentInputOutRefs).size !== input.spentInputOutRefs.length ||
    input.referenceInputOutRefs.some((reference) => !OUT_REF.test(reference)) ||
    new Set(input.referenceInputOutRefs).size !==
      input.referenceInputOutRefs.length ||
    parseStateQueueCorrectionLockWitness(input.correctionLockWitness) === null
  ) {
    return null;
  }
  const terminal = deriveStateQueueAuthenticatedTransition(input);
  let checkpointKind: StateQueueAuthenticatedReplayCheckpointKind;
  let stateQueueMintRedeemer: StateQueueTransitionRedeemer | null = null;
  if (terminal !== null) {
    checkpointKind = terminal.transitionKind;
    stateQueueMintRedeemer = terminal.stateQueueMintRedeemer;
  } else {
    const decoded = canonicalRedeemer(input);
    const previousByIdentity = new Map(
      input.previousQueue.map((node) => [node.headerHash, node]),
    );
    const nextByIdentity = new Map(
      input.nextQueue.map((node) => [node.headerHash, node]),
    );
    const changedPrevious = input.previousQueue.filter(
      (node) => nextByIdentity.get(node.headerHash)?.outRef !== node.outRef,
    );
    const introduced = input.nextQueue.filter(
      (node) => !previousByIdentity.has(node.headerHash),
    );
    const spent = new Set(input.spentInputOutRefs);
    const exactQueueInputs = input.previousQueue
      .filter(({ outRef }) => spent.has(outRef))
      .map(({ outRef }) => outRef)
      .sort();
    if (
      exactQueueInputs.length !== changedPrevious.length ||
      !changedPrevious.every(({ outRef }) => spent.has(outRef))
    ) {
      return null;
    }
    if (decoded === null) {
      if (
        input.mintPolicyIds.includes(input.stateQueuePolicyId) ||
        !sameIdentities(input.previousQueue, input.nextQueue) ||
        changedPrevious.length !== 1 ||
        introduced.length !== 0 ||
        !nextByIdentity
          .get(changedPrevious[0]!.headerHash)!
          .outRef.startsWith(`${input.transactionHash}#`)
      ) {
        return null;
      }
      checkpointKind = "datum_update";
    } else {
      stateQueueMintRedeemer = decoded.redeemer;
      const value = decoded.decoded;
      if (typeof value === "object" && value !== null && "InitV1" in value) {
        if (
          input.previousQueue.length !== 0 ||
          input.nextQueue.length !== 1 ||
          input.nextQueue[0]!.headerHash !== null ||
          input.nextQueue[0]!.outRef !==
            `${input.transactionHash}#${value.InitV1.output_index.toString()}`
        ) {
          return null;
        }
        checkpointKind = "init";
      } else if (value === "Deinit") {
        if (
          input.previousQueue.length !== 1 ||
          input.previousQueue[0]!.headerHash !== null ||
          input.nextQueue.length !== 0 ||
          !spent.has(input.previousQueue[0]!.outRef)
        ) {
          return null;
        }
        checkpointKind = "deinit";
      } else if (
        typeof value === "object" &&
        value !== null &&
        "CommitBlockHeader" in value
      ) {
        const commit = value.CommitBlockHeader;
        const priorTail = input.previousQueue.at(-1);
        const nextTail = input.nextQueue.at(-1);
        const continued =
          priorTail === undefined
            ? undefined
            : nextByIdentity.get(priorTail.headerHash);
        if (
          input.previousQueue.length === 0 ||
          input.nextQueue.length !== input.previousQueue.length + 1 ||
          !input.previousQueue.every(
            (node, index) =>
              node.headerHash === input.nextQueue[index]?.headerHash,
          ) ||
          changedPrevious.length !== 1 ||
          changedPrevious[0]!.headerHash !== priorTail!.headerHash ||
          introduced.length !== 1 ||
          introduced[0]!.headerHash !== nextTail!.headerHash ||
          continued?.outRef !==
            `${input.transactionHash}#${commit.continued_latest_block_output_index.toString()}` ||
          introduced[0]!.outRef !==
            `${input.transactionHash}#${commit.new_block_output_index.toString()}` ||
          outputIndex(continued.outRef) === outputIndex(introduced[0]!.outRef)
        ) {
          return null;
        }
        checkpointKind = "append";
      } else {
        return null;
      }
    }
  }
  const lock = parseStateQueueCorrectionLockWitness(
    input.correctionLockWitness,
  )!;
  const lockTopologyIsExact =
    checkpointKind === "init"
      ? lock.kind === "genesis" &&
        lock.nextDatum === "Idle" &&
        lock.producedOutRef.startsWith(`${input.transactionHash}#`)
      : checkpointKind === "deinit"
        ? lock.kind === "deinit" &&
          lock.previousDatum === "Idle" &&
          input.spentInputOutRefs.includes(lock.consumedOutRef)
        : checkpointKind === "append"
          ? lock.kind === "idle_reference" &&
            lock.datum === "Idle" &&
            input.referenceInputOutRefs.includes(lock.referenceOutRef) &&
            !input.spentInputOutRefs.includes(lock.referenceOutRef)
          : checkpointKind === "datum_update"
            ? lock.kind === "none"
            : terminal !== null;
  if (!lockTopologyIsExact) return null;
  const canonical = {
    schemaVersion: STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION,
    deploymentIdentityDigest: input.deploymentIdentityDigest,
    stateQueuePolicyId: input.stateQueuePolicyId,
    transactionHash: input.transactionHash,
    blockHash: input.blockHash,
    slot: input.slot,
    blockNo: input.blockNo,
    transactionIndex: input.transactionIndex,
    chainPointId: input.chainPointId,
    finalityDepth: input.finalityDepth,
    checkpointKind,
    mintPolicyIds: input.mintPolicyIds,
    stateQueueMintRedeemer,
    spentInputOutRefs: input.spentInputOutRefs,
    referenceInputOutRefs: input.referenceInputOutRefs,
    correctionLockWitness: lock,
    previousQueue: input.previousQueue,
    nextQueue: input.nextQueue,
    terminalTransition: terminal,
  } satisfies Omit<StateQueueAuthenticatedReplayCheckpoint, "checkpointDigest">;
  return Object.freeze({ ...canonical, checkpointDigest: digest(canonical) });
};

export const parseNodes = (
  input: unknown,
): readonly StateQueueTransitionNode[] | null =>
  Array.isArray(input)
    ? input.map((value) => {
        const node = exactRecord(value, ["headerHash", "outRef"]);
        return node === null
          ? ({ headerHash: "invalid", outRef: "invalid" } as const)
          : {
              headerHash: node.headerHash as string | null,
              outRef: node.outRef as string,
            };
      })
    : null;
