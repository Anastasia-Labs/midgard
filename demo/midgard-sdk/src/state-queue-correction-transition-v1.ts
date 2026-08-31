import { CML, Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import type {
  CorrectionIdentity,
  CorrectionLockDatum,
} from "./correction-lock.js";
import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";

export const STATE_QUEUE_CORRECTION_TRANSITION_V1_SCHEMA_VERSION =
  "midgard-state-queue-correction-transition-v1" as const;
export const STATE_QUEUE_AUTHENTICATED_TRANSITION_V1_SCHEMA_VERSION =
  "midgard-state-queue-authenticated-transition-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NON_EMPTY_BYTES = /^(?:[0-9a-f]{2})+$/u;

export type StateQueueTransitionNodeV1 = Readonly<{
  headerHash: string | null;
  outRef: string;
}>;

export type StateQueueTransitionRedeemerV1 = Readonly<{
  purpose: string;
  index: string;
  cborHex: string;
}>;

/**
 * Exact CorrectionLock evidence carried by an authenticated state-queue
 * checkpoint.  The variants mirror the only legal relationships with the
 * singleton: genesis creates it, deinit burns it, append/merge reference Idle,
 * and a correction consumes and continues it.
 */
export type StateQueueCorrectionLockWitnessV1 =
  | Readonly<{
      kind: "none";
    }>
  | Readonly<{
      kind: "genesis";
      producedOutRef: string;
      nextDatum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "deinit";
      consumedOutRef: string;
      previousDatum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "idle_reference";
      referenceOutRef: string;
      datum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "correction_transition";
      consumedOutRef: string;
      continuedOutRef: string;
      targetHeaderHash: string;
      correctionIdentity: CorrectionIdentity;
      previousDatum: CorrectionLockDatum;
      nextDatum: CorrectionLockDatum;
    }>;

export type StateQueueCorrectionTransitionV1 = Readonly<{
  schemaVersion: typeof STATE_QUEUE_CORRECTION_TRANSITION_V1_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  chainPointId: string;
  finalityDepth: string;
  timedOutHeaderHash: string;
  removalApproach: "PruneTimedOutBlockDescendant" | "RemoveTimedOutHead";
  consumedQueueOutRefs: readonly string[];
  continuedQueueOutRefs: readonly Readonly<{
    headerHash: string | null;
    consumedOutRef: string;
    producedOutRef: string;
  }>[];
  removedHeaderHashes: readonly string[];
  transitionDigest: string;
}>;

export type DeriveStateQueueCorrectionTransitionV1Input = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  chainPointId: string;
  finalityDepth: string;
  mintPolicyIds: readonly string[];
  redeemers: readonly StateQueueTransitionRedeemerV1[];
  spentInputOutRefs: readonly string[];
  previousQueue: readonly StateQueueTransitionNodeV1[];
  nextQueue: readonly StateQueueTransitionNodeV1[];
}>;

export type StateQueueAuthenticatedTransitionKindV1 =
  | "timeout_correction"
  | "merge"
  | "fraud_removal";

/**
 * Pure, service-independent provenance for an accepted state-queue removal.
 * Admission/authentication remains the responsibility of each service's own
 * Kupo/Ogmios chain follower; this exact, digest-bound shape prevents the node,
 * watcher and committee scanner from giving the same L1 transition different
 * meanings after admission.
 */
export type StateQueueAuthenticatedTransitionV1 = Readonly<{
  schemaVersion: typeof STATE_QUEUE_AUTHENTICATED_TRANSITION_V1_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  transitionKind: StateQueueAuthenticatedTransitionKindV1;
  stateQueueMintRedeemer: StateQueueTransitionRedeemerV1;
  previousQueue: readonly StateQueueTransitionNodeV1[];
  nextQueue: readonly StateQueueTransitionNodeV1[];
  consumedQueueOutRefs: readonly string[];
  continuedQueueOutRefs: readonly Readonly<{
    headerHash: string | null;
    consumedOutRef: string;
    producedOutRef: string;
  }>[];
  removedHeaderHashes: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitnessV1;
  correctionTransition: StateQueueCorrectionTransitionV1 | null;
  transitionDigest: string;
}>;

export type DeriveStateQueueAuthenticatedTransitionV1Input =
  DeriveStateQueueCorrectionTransitionV1Input &
    Readonly<{
      transactionIndex: string;
      referenceInputOutRefs: readonly string[];
      correctionLockWitness: StateQueueCorrectionLockWitnessV1;
    }>;

type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

const stableJson = (value: Json): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};

const digest = (value: Json): string =>
  toHex(sha256(new TextEncoder().encode(stableJson(value))));

const withoutDigest = (
  value: Omit<StateQueueCorrectionTransitionV1, "transitionDigest">,
): Json => value as Json;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  if (
    Object.getPrototypeOf(value) !== Object.prototype ||
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !expected.has(key))
  ) {
    return null;
  }
  return value as Record<string, unknown>;
};

const parseCorrectionIdentity = (value: unknown): CorrectionIdentity | null => {
  if (value === "AttestationTimeout") return value;
  const fraud = exactRecord(value, ["FraudProof"]);
  if (fraud !== null) {
    const fields = exactRecord(fraud.FraudProof, ["fraud_proof_asset_name"]);
    return fields !== null &&
      typeof fields.fraud_proof_asset_name === "string" &&
      HEX_32.test(fields.fraud_proof_asset_name)
      ? {
          FraudProof: {
            fraud_proof_asset_name: fields.fraud_proof_asset_name,
          },
        }
      : null;
  }
  const availability = exactRecord(value, ["AvailabilityChallenge"]);
  if (availability !== null) {
    const fields = exactRecord(availability.AvailabilityChallenge, [
      "challenge_asset_name",
    ]);
    return fields !== null &&
      typeof fields.challenge_asset_name === "string" &&
      NON_EMPTY_BYTES.test(fields.challenge_asset_name)
      ? {
          AvailabilityChallenge: {
            challenge_asset_name: fields.challenge_asset_name,
          },
        }
      : null;
  }
  return null;
};

export const parseStateQueueCorrectionLockDatumV1 = (
  value: unknown,
): CorrectionLockDatum | null => {
  if (value === "Idle") return value;
  const locked = exactRecord(value, ["Locked"]);
  const fields = exactRecord(locked?.Locked, [
    "target_header_hash",
    "correction_identity",
  ]);
  const identity = parseCorrectionIdentity(fields?.correction_identity);
  return fields !== null &&
    typeof fields.target_header_hash === "string" &&
    HEX_28.test(fields.target_header_hash) &&
    identity !== null
    ? ({
        Locked: {
          target_header_hash: fields.target_header_hash,
          correction_identity: identity,
        },
      } as CorrectionLockDatum)
    : null;
};

export const parseStateQueueCorrectionLockWitnessV1 = (
  value: unknown,
): StateQueueCorrectionLockWitnessV1 | null => {
  const kind =
    typeof value === "object" && value !== null && !Array.isArray(value)
      ? (value as { kind?: unknown }).kind
      : undefined;
  if (kind === "none") {
    return exactRecord(value, ["kind"]) === null ? null : { kind };
  }
  if (kind === "genesis") {
    const record = exactRecord(value, ["kind", "producedOutRef", "nextDatum"]);
    const nextDatum = parseStateQueueCorrectionLockDatumV1(record?.nextDatum);
    return record !== null &&
      typeof record.producedOutRef === "string" &&
      OUT_REF.test(record.producedOutRef) &&
      nextDatum !== null
      ? { kind, producedOutRef: record.producedOutRef, nextDatum }
      : null;
  }
  if (kind === "deinit") {
    const record = exactRecord(value, [
      "kind",
      "consumedOutRef",
      "previousDatum",
    ]);
    const previousDatum = parseStateQueueCorrectionLockDatumV1(
      record?.previousDatum,
    );
    return record !== null &&
      typeof record.consumedOutRef === "string" &&
      OUT_REF.test(record.consumedOutRef) &&
      previousDatum !== null
      ? { kind, consumedOutRef: record.consumedOutRef, previousDatum }
      : null;
  }
  if (kind === "idle_reference") {
    const record = exactRecord(value, ["kind", "referenceOutRef", "datum"]);
    const datum = parseStateQueueCorrectionLockDatumV1(record?.datum);
    return record !== null &&
      typeof record.referenceOutRef === "string" &&
      OUT_REF.test(record.referenceOutRef) &&
      datum !== null
      ? { kind, referenceOutRef: record.referenceOutRef, datum }
      : null;
  }
  if (kind === "correction_transition") {
    const record = exactRecord(value, [
      "kind",
      "consumedOutRef",
      "continuedOutRef",
      "targetHeaderHash",
      "correctionIdentity",
      "previousDatum",
      "nextDatum",
    ]);
    const correctionIdentity = parseCorrectionIdentity(
      record?.correctionIdentity,
    );
    const previousDatum = parseStateQueueCorrectionLockDatumV1(
      record?.previousDatum,
    );
    const nextDatum = parseStateQueueCorrectionLockDatumV1(record?.nextDatum);
    return record !== null &&
      typeof record.consumedOutRef === "string" &&
      OUT_REF.test(record.consumedOutRef) &&
      typeof record.continuedOutRef === "string" &&
      OUT_REF.test(record.continuedOutRef) &&
      typeof record.targetHeaderHash === "string" &&
      HEX_28.test(record.targetHeaderHash) &&
      correctionIdentity !== null &&
      previousDatum !== null &&
      nextDatum !== null
      ? {
          kind,
          consumedOutRef: record.consumedOutRef,
          continuedOutRef: record.continuedOutRef,
          targetHeaderHash: record.targetHeaderHash,
          correctionIdentity,
          previousDatum,
          nextDatum,
        }
      : null;
  }
  return null;
};

const canonicalNodes = (
  nodes: readonly StateQueueTransitionNodeV1[],
): boolean => {
  const exactNodes = nodes.map((candidate) =>
    exactRecord(candidate, ["headerHash", "outRef"]),
  );
  return (
    nodes.length > 0 &&
    exactNodes.every((node) => node !== null) &&
    exactNodes[0]?.headerHash === null &&
    exactNodes.every(
      (node, index) =>
        typeof node!.outRef === "string" &&
        OUT_REF.test(node!.outRef) &&
        (index === 0
          ? node!.headerHash === null
          : typeof node!.headerHash === "string" &&
            HEX_28.test(node!.headerHash)),
    ) &&
    new Set(exactNodes.map((node) => node!.outRef)).size === nodes.length &&
    new Set(exactNodes.map((node) => node!.headerHash)).size === nodes.length
  );
};

const parseCanonicalNodes = (
  value: unknown,
): readonly StateQueueTransitionNodeV1[] | null => {
  if (!Array.isArray(value) || !canonicalNodes(value)) return null;
  return value.map((node) => ({
    headerHash: (node as StateQueueTransitionNodeV1).headerHash,
    outRef: (node as StateQueueTransitionNodeV1).outRef,
  }));
};

const decodeStateQueueMintRedeemer = (
  input: DeriveStateQueueCorrectionTransitionV1Input,
): StateQueueRedeemerType | null => {
  const canonicalPolicies = [...input.mintPolicyIds].sort();
  if (
    canonicalPolicies.length !== input.mintPolicyIds.length ||
    !canonicalPolicies.every(
      (policyId, index) =>
        HEX_28.test(policyId) && policyId === input.mintPolicyIds[index],
    ) ||
    new Set(canonicalPolicies).size !== canonicalPolicies.length
  ) {
    return null;
  }
  const policyIndex = canonicalPolicies.indexOf(input.stateQueuePolicyId);
  const matches = input.redeemers.filter(
    (redeemer) =>
      redeemer.purpose === "mint" && redeemer.index === policyIndex.toString(),
  );
  if (policyIndex < 0 || matches.length !== 1) {
    return null;
  }
  try {
    const decoded = Data.from(
      matches[0]!.cborHex,
      StateQueueRedeemer,
    ) as StateQueueRedeemerType;
    const lucidCbor = Data.to(decoded, StateQueueRedeemer);
    const cardanoCanonicalCbor = CML.PlutusData.from_cbor_hex(
      matches[0]!.cborHex,
    ).to_canonical_cbor_hex();
    return lucidCbor === matches[0]!.cborHex ||
      cardanoCanonicalCbor === matches[0]!.cborHex
      ? decoded
      : null;
  } catch {
    return null;
  }
};

export const deriveStateQueueCorrectionTransitionV1 = (
  input: DeriveStateQueueCorrectionTransitionV1Input,
): StateQueueCorrectionTransitionV1 | null => {
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
    !("RemoveUnattestedBlockAfterTimeout" in decoded)
  ) {
    return null;
  }
  const timeout = decoded.RemoveUnattestedBlockAfterTimeout;
  const approach = timeout.removal_approach;
  const [removalApproach, pruneApproach, headApproach] =
    "PruneTimedOutBlockDescendant" in approach
      ? ([
          "PruneTimedOutBlockDescendant",
          approach.PruneTimedOutBlockDescendant,
          null,
        ] as const)
      : (["RemoveTimedOutHead", null, approach.RemoveTimedOutHead] as const);
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
  const timedOutHeaderHash = timeout.timed_out_header_hash;
  const nextHashes = input.nextQueue.map(({ headerHash }) => headerHash);
  const previousHashes = input.previousQueue.map(
    ({ headerHash }) => headerHash,
  );
  const priorHead = input.previousQueue[1]?.headerHash;
  const redeemerInputOutRef =
    pruneApproach?.timed_out_node_input_outref ??
    headApproach!.confirmed_state_input_outref;
  const redeemerInputOutRefLabel = `${redeemerInputOutRef.transactionId}#${redeemerInputOutRef.outputIndex.toString()}`;
  const continuedIdentity =
    removalApproach === "PruneTimedOutBlockDescendant"
      ? continuedQueueOutRefs.find(
          ({ headerHash }) => headerHash === timedOutHeaderHash,
        )
      : continuedQueueOutRefs.find(({ headerHash }) => headerHash === null);
  const redeemerOutputIndex =
    pruneApproach?.timed_out_node_output_index ??
    headApproach!.confirmed_state_output_index;
  const exactTopology =
    priorHead === timedOutHeaderHash &&
    changed.length === 2 &&
    changed.every(({ outRef }) => spent.has(outRef)) &&
    input.spentInputOutRefs
      .filter((outRef) =>
        input.previousQueue.some((node) => node.outRef === outRef),
      )
      .every((outRef) => consumedQueueOutRefs.includes(outRef)) &&
    continuedQueueOutRefs.every(({ producedOutRef }) =>
      producedOutRef.startsWith(`${input.transactionHash}#`),
    ) &&
    continuedIdentity !== undefined &&
    redeemerInputOutRefLabel === continuedIdentity.consumedOutRef &&
    continuedIdentity.producedOutRef ===
      `${input.transactionHash}#${redeemerOutputIndex.toString()}` &&
    (removalApproach === "PruneTimedOutBlockDescendant"
      ? removedHeaderHashes.length === 1 &&
        removedHeaderHashes[0] === input.previousQueue[2]?.headerHash &&
        input.previousQueue.length >= 3 &&
        nextHashes.length === previousHashes.length - 1 &&
        nextHashes.every(
          (hash, index) =>
            hash === previousHashes[index < 2 ? index : index + 1],
        )
      : removedHeaderHashes.length === 1 &&
        removedHeaderHashes[0] === timedOutHeaderHash &&
        input.previousQueue.length === 2 &&
        input.nextQueue.length === 1);
  if (!exactTopology) {
    return null;
  }
  const canonical = {
    schemaVersion: STATE_QUEUE_CORRECTION_TRANSITION_V1_SCHEMA_VERSION,
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
  } satisfies Omit<StateQueueCorrectionTransitionV1, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(withoutDigest(canonical)),
  });
};

const outputReferenceLabel = (reference: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string => `${reference.transactionId}#${reference.outputIndex.toString()}`;

const correctionLockWitnessMatchesTransition = ({
  decoded,
  witness,
  spentInputOutRefs,
  referenceInputOutRefs,
  transactionHash,
}: {
  readonly decoded: StateQueueRedeemerType;
  readonly witness: StateQueueCorrectionLockWitnessV1;
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
    terminal = "RemoveTimedOutHead" in timeout.removal_approach;
    targetHeaderHash = timeout.timed_out_header_hash;
    identityMatches = witness.correctionIdentity === "AttestationTimeout";
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

export const deriveStateQueueAuthenticatedTransitionV1 = (
  input: DeriveStateQueueAuthenticatedTransitionV1Input,
): StateQueueAuthenticatedTransitionV1 | null => {
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
  const correctionLockWitness = parseStateQueueCorrectionLockWitnessV1(
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

  let transitionKind: StateQueueAuthenticatedTransitionKindV1;
  let correctionTransition: StateQueueCorrectionTransitionV1 | null = null;
  if ("RemoveUnattestedBlockAfterTimeout" in decoded) {
    correctionTransition = deriveStateQueueCorrectionTransitionV1(input);
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
    schemaVersion: STATE_QUEUE_AUTHENTICATED_TRANSITION_V1_SCHEMA_VERSION,
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
  } satisfies Omit<StateQueueAuthenticatedTransitionV1, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(canonical as unknown as Json),
  });
};

export const parseStateQueueCorrectionTransitionV1 = (
  value: unknown,
): StateQueueCorrectionTransitionV1 | null => {
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
    record.schemaVersion !==
      STATE_QUEUE_CORRECTION_TRANSITION_V1_SCHEMA_VERSION ||
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
    (record.removalApproach !== "PruneTimedOutBlockDescendant" &&
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
  } satisfies Omit<StateQueueCorrectionTransitionV1, "transitionDigest">;
  return digest(withoutDigest(canonical)) === record.transitionDigest
    ? Object.freeze({
        ...canonical,
        transitionDigest: record.transitionDigest as string,
      })
    : null;
};

export const parseStateQueueAuthenticatedTransitionV1 = (
  value: unknown,
): StateQueueAuthenticatedTransitionV1 | null => {
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
      : parseStateQueueCorrectionTransitionV1(record?.correctionTransition);
  const correctionLockWitness = parseStateQueueCorrectionLockWitnessV1(
    record?.correctionLockWitness,
  );
  const previousQueue = parseCanonicalNodes(record?.previousQueue);
  const nextQueue = parseCanonicalNodes(record?.nextQueue);
  if (
    record === null ||
    record.schemaVersion !==
      STATE_QUEUE_AUTHENTICATED_TRANSITION_V1_SCHEMA_VERSION ||
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
  } satisfies Omit<StateQueueAuthenticatedTransitionV1, "transitionDigest">;
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
    "RemoveUnattestedBlockAfterTimeout" in decoded &&
    canonical.correctionTransition !== null
  ) {
    const nested = canonical.correctionTransition;
    const timeout = decoded.RemoveUnattestedBlockAfterTimeout;
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
      nested.timedOutHeaderHash === timeout.timed_out_header_hash;
    const approachMatches =
      "PruneTimedOutBlockDescendant" in timeout.removal_approach
        ? nested.removalApproach === "PruneTimedOutBlockDescendant" &&
          outputReferenceLabel(
            timeout.removal_approach.PruneTimedOutBlockDescendant
              .timed_out_node_input_outref,
          ) === nested.continuedQueueOutRefs[0]?.consumedOutRef &&
          `${canonical.transactionHash}#${timeout.removal_approach.PruneTimedOutBlockDescendant.timed_out_node_output_index.toString()}` ===
            nested.continuedQueueOutRefs[0]?.producedOutRef
        : nested.removalApproach === "RemoveTimedOutHead" &&
          outputReferenceLabel(
            timeout.removal_approach.RemoveTimedOutHead
              .confirmed_state_input_outref,
          ) === nested.continuedQueueOutRefs[0]?.consumedOutRef &&
          `${canonical.transactionHash}#${timeout.removal_approach.RemoveTimedOutHead.confirmed_state_output_index.toString()}` ===
            nested.continuedQueueOutRefs[0]?.producedOutRef;
    semanticsAreCanonical = outerNestedIdentityMatches && approachMatches;
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

export const withStateQueueAuthenticatedTransitionFinalityDepthV1 = (
  transitionInput: unknown,
  finalityDepth: string,
): StateQueueAuthenticatedTransitionV1 | null => {
  const transition = parseStateQueueAuthenticatedTransitionV1(transitionInput);
  if (
    transition === null ||
    !NATURAL.test(finalityDepth) ||
    BigInt(finalityDepth) === 0n ||
    BigInt(finalityDepth) < BigInt(transition.finalityDepth)
  ) {
    return null;
  }
  const correctionTransition =
    transition.correctionTransition === null
      ? null
      : withStateQueueCorrectionTransitionFinalityDepthV1(
          transition.correctionTransition,
          finalityDepth,
        );
  if (
    transition.correctionTransition !== null &&
    correctionTransition === null
  ) {
    return null;
  }
  const { transitionDigest: _priorDigest, ...withoutPriorDigest } = transition;
  const canonical = {
    ...withoutPriorDigest,
    finalityDepth,
    correctionTransition,
  };
  const rebound = {
    ...canonical,
    transitionDigest: digest(canonical as unknown as Json),
  };
  return parseStateQueueAuthenticatedTransitionV1(rebound);
};

/**
 * Advances only the finality attestation of an already canonical transition.
 * The L1 observer must independently prove that the same block remains on its
 * selected chain; this helper merely rebinds that newly observed depth into the
 * transition digest without replaying topology from an unauthenticated shape.
 */
export const withStateQueueCorrectionTransitionFinalityDepthV1 = (
  transitionInput: unknown,
  finalityDepth: string,
): StateQueueCorrectionTransitionV1 | null => {
  const transition = parseStateQueueCorrectionTransitionV1(transitionInput);
  if (
    transition === null ||
    !NATURAL.test(finalityDepth) ||
    BigInt(finalityDepth) === 0n ||
    BigInt(finalityDepth) < BigInt(transition.finalityDepth)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: transition.schemaVersion,
    deploymentIdentityDigest: transition.deploymentIdentityDigest,
    stateQueuePolicyId: transition.stateQueuePolicyId,
    transactionHash: transition.transactionHash,
    blockHash: transition.blockHash,
    slot: transition.slot,
    blockNo: transition.blockNo,
    chainPointId: transition.chainPointId,
    finalityDepth,
    timedOutHeaderHash: transition.timedOutHeaderHash,
    removalApproach: transition.removalApproach,
    consumedQueueOutRefs: transition.consumedQueueOutRefs,
    continuedQueueOutRefs: transition.continuedQueueOutRefs,
    removedHeaderHashes: transition.removedHeaderHashes,
  } satisfies Omit<StateQueueCorrectionTransitionV1, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(withoutDigest(canonical)),
  });
};
