import { CML, Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";
import {
  deriveStateQueueAuthenticatedTransition,
  parseStateQueueAuthenticatedTransition,
  parseStateQueueCorrectionLockWitness,
  type StateQueueAuthenticatedTransition,
  type StateQueueCorrectionLockWitness,
  type StateQueueTransitionNode,
  type StateQueueTransitionRedeemer,
} from "./state-queue-correction-transition-v1.js";

export const STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION =
  "midgard-state-queue-authenticated-replay-checkpoint-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type StateQueueAuthenticatedReplayCheckpointKind =
  | "init"
  | "deinit"
  | "append"
  | "datum_update"
  | "merge"
  | "fraud_removal"
  | "timeout_correction";

export type StateQueueAuthenticatedReplayCheckpoint = Readonly<{
  schemaVersion: typeof STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  checkpointKind: StateQueueAuthenticatedReplayCheckpointKind;
  mintPolicyIds: readonly string[];
  stateQueueMintRedeemer: StateQueueTransitionRedeemer | null;
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitness;
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
  terminalTransition: StateQueueAuthenticatedTransition | null;
  checkpointDigest: string;
}>;

export type DeriveStateQueueAuthenticatedReplayCheckpointInput = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  mintPolicyIds: readonly string[];
  redeemers: readonly StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitness;
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
}>;

type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

const stableJson = (value: Json): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};
const digest = (value: unknown): string =>
  toHex(sha256(new TextEncoder().encode(stableJson(value as Json))));

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  return Object.getPrototypeOf(value) === Object.prototype &&
    actual.length === keys.length &&
    actual.every((key) => typeof key === "string" && expected.has(key))
    ? (value as Record<string, unknown>)
    : null;
};

const canonicalQueue = (
  queue: readonly StateQueueTransitionNode[],
  allowEmpty: boolean,
): boolean =>
  (allowEmpty || queue.length > 0) &&
  (queue.length === 0 || queue[0]?.headerHash === null) &&
  queue.every(
    (node, index) =>
      Object.getPrototypeOf(node) === Object.prototype &&
      Reflect.ownKeys(node).length === 2 &&
      Object.prototype.hasOwnProperty.call(node, "headerHash") &&
      Object.prototype.hasOwnProperty.call(node, "outRef") &&
      OUT_REF.test(node.outRef) &&
      (index === 0 ? node.headerHash === null : HEX_28.test(node.headerHash!)),
  ) &&
  new Set(queue.map(({ headerHash }) => headerHash)).size === queue.length &&
  new Set(queue.map(({ outRef }) => outRef)).size === queue.length;

const outputIndex = (outRef: string): bigint => BigInt(outRef.split("#")[1]!);
const sameIdentities = (
  left: readonly StateQueueTransitionNode[],
  right: readonly StateQueueTransitionNode[],
): boolean =>
  left.length === right.length &&
  left.every((node, index) => node.headerHash === right[index]?.headerHash);

const canonicalRedeemer = (
  input: DeriveStateQueueAuthenticatedReplayCheckpointInput,
): {
  redeemer: StateQueueTransitionRedeemer;
  decoded: StateQueueRedeemerType;
} | null => {
  const policyIndex = input.mintPolicyIds.indexOf(input.stateQueuePolicyId);
  const matches = input.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (policyIndex < 0 || matches.length !== 1) return null;
  const redeemer = matches[0]!;
  try {
    const decoded = Data.from(
      redeemer.cborHex,
      StateQueueRedeemer,
    ) as StateQueueRedeemerType;
    if (
      Data.to(decoded, StateQueueRedeemer) !== redeemer.cborHex &&
      CML.PlutusData.from_cbor_hex(redeemer.cborHex).to_canonical_cbor_hex() !==
        redeemer.cborHex
    ) {
      return null;
    }
    return { redeemer, decoded };
  } catch {
    return null;
  }
};

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

const parseNodes = (
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

export const parseStateQueueAuthenticatedReplayCheckpoint = (
  input: unknown,
): StateQueueAuthenticatedReplayCheckpoint | null => {
  const record = exactRecord(input, [
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
    "checkpointKind",
    "mintPolicyIds",
    "stateQueueMintRedeemer",
    "spentInputOutRefs",
    "referenceInputOutRefs",
    "correctionLockWitness",
    "previousQueue",
    "nextQueue",
    "terminalTransition",
    "checkpointDigest",
  ]);
  const mint =
    record?.stateQueueMintRedeemer === null
      ? null
      : exactRecord(record?.stateQueueMintRedeemer, [
          "purpose",
          "index",
          "cborHex",
        ]);
  const previousQueue = parseNodes(record?.previousQueue);
  const nextQueue = parseNodes(record?.nextQueue);
  const terminal =
    record?.terminalTransition === null
      ? null
      : parseStateQueueAuthenticatedTransition(record?.terminalTransition);
  const correctionLockWitness = parseStateQueueCorrectionLockWitness(
    record?.correctionLockWitness,
  );
  if (
    record === null ||
    record.schemaVersion !==
      STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION ||
    typeof record.deploymentIdentityDigest !== "string" ||
    typeof record.stateQueuePolicyId !== "string" ||
    typeof record.transactionHash !== "string" ||
    typeof record.blockHash !== "string" ||
    typeof record.slot !== "string" ||
    typeof record.blockNo !== "string" ||
    typeof record.transactionIndex !== "string" ||
    typeof record.chainPointId !== "string" ||
    typeof record.finalityDepth !== "string" ||
    !Array.isArray(record.mintPolicyIds) ||
    record.mintPolicyIds.some((value) => typeof value !== "string") ||
    !Array.isArray(record.spentInputOutRefs) ||
    record.spentInputOutRefs.some((value) => typeof value !== "string") ||
    !Array.isArray(record.referenceInputOutRefs) ||
    record.referenceInputOutRefs.some((value) => typeof value !== "string") ||
    correctionLockWitness === null ||
    previousQueue === null ||
    nextQueue === null ||
    (mint !== null &&
      (typeof mint.purpose !== "string" ||
        typeof mint.index !== "string" ||
        typeof mint.cborHex !== "string")) ||
    typeof record.checkpointDigest !== "string" ||
    !HEX_32.test(record.checkpointDigest)
  ) {
    return null;
  }
  const derived = deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: record.deploymentIdentityDigest,
    stateQueuePolicyId: record.stateQueuePolicyId,
    transactionHash: record.transactionHash,
    blockHash: record.blockHash,
    slot: record.slot,
    blockNo: record.blockNo,
    transactionIndex: record.transactionIndex,
    chainPointId: record.chainPointId,
    finalityDepth: record.finalityDepth,
    mintPolicyIds: record.mintPolicyIds as string[],
    redeemers:
      mint === null
        ? []
        : [
            {
              purpose: mint.purpose as string,
              index: mint.index as string,
              cborHex: mint.cborHex as string,
            },
          ],
    spentInputOutRefs: record.spentInputOutRefs as string[],
    referenceInputOutRefs: record.referenceInputOutRefs as string[],
    correctionLockWitness,
    previousQueue,
    nextQueue,
  });
  return derived !== null &&
    derived.checkpointKind === record.checkpointKind &&
    derived.checkpointDigest === record.checkpointDigest &&
    stableJson(derived.terminalTransition as unknown as Json) ===
      stableJson(terminal as unknown as Json)
    ? derived
    : null;
};

export const replayStateQueueAuthenticatedCheckpoints = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  minimumFinalityDepth,
  anchor,
  checkpoints: checkpointInputs,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly minimumFinalityDepth: bigint;
  readonly anchor: Readonly<{
    queue: readonly StateQueueTransitionNode[];
    blockNo: string;
    transactionIndex: string;
  }>;
  readonly checkpoints: readonly unknown[];
}): Readonly<{
  queue: readonly StateQueueTransitionNode[];
  lastBlockNo: string;
  lastTransactionIndex: string;
  terminals: readonly StateQueueAuthenticatedTransition[];
}> | null => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    minimumFinalityDepth <= 0n ||
    !canonicalQueue(anchor.queue, true) ||
    !NATURAL.test(anchor.blockNo) ||
    !NATURAL.test(anchor.transactionIndex)
  ) {
    return null;
  }
  let queue = anchor.queue;
  let blockNo = anchor.blockNo;
  let transactionIndex = anchor.transactionIndex;
  const terminals: StateQueueAuthenticatedTransition[] = [];
  for (const input of checkpointInputs) {
    const checkpoint = parseStateQueueAuthenticatedReplayCheckpoint(input);
    const ordered =
      checkpoint !== null &&
      (BigInt(checkpoint.blockNo) > BigInt(blockNo) ||
        (checkpoint.blockNo === blockNo &&
          BigInt(checkpoint.transactionIndex) > BigInt(transactionIndex)));
    if (
      checkpoint === null ||
      !ordered ||
      checkpoint.deploymentIdentityDigest !== deploymentIdentityDigest ||
      checkpoint.stateQueuePolicyId !== stateQueuePolicyId ||
      BigInt(checkpoint.finalityDepth) < minimumFinalityDepth ||
      stableJson(checkpoint.previousQueue as unknown as Json) !==
        stableJson(queue as unknown as Json)
    ) {
      return null;
    }
    queue = checkpoint.nextQueue;
    blockNo = checkpoint.blockNo;
    transactionIndex = checkpoint.transactionIndex;
    if (checkpoint.terminalTransition !== null) {
      terminals.push(checkpoint.terminalTransition);
    }
  }
  return Object.freeze({
    queue,
    lastBlockNo: blockNo,
    lastTransactionIndex: transactionIndex,
    terminals: Object.freeze(terminals),
  });
};
