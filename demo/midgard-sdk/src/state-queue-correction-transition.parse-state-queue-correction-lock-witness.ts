import { CML, Data } from "@lucid-evolution/lucid";

import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";
import {
  type DeriveStateQueueCorrectionTransitionInput,
  exactRecord,
  HEX_28,
  OUT_REF,
  parseCorrectionIdentity,
  parseStateQueueCorrectionLockDatum,
  type StateQueueCorrectionLockWitness,
  type StateQueueTransitionNode,
} from "./state-queue-correction-transition.state-queue-correction-lock-witness.js";

export const parseStateQueueCorrectionLockWitness = (
  value: unknown,
): StateQueueCorrectionLockWitness | null => {
  const kind =
    typeof value === "object" && value !== null && !Array.isArray(value)
      ? (value as { kind?: unknown }).kind
      : undefined;
  if (kind === "none") {
    return exactRecord(value, ["kind"]) === null ? null : { kind };
  }
  if (kind === "genesis") {
    const record = exactRecord(value, ["kind", "producedOutRef", "nextDatum"]);
    const nextDatum = parseStateQueueCorrectionLockDatum(record?.nextDatum);
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
    const previousDatum = parseStateQueueCorrectionLockDatum(
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
    const datum = parseStateQueueCorrectionLockDatum(record?.datum);
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
    const previousDatum = parseStateQueueCorrectionLockDatum(
      record?.previousDatum,
    );
    const nextDatum = parseStateQueueCorrectionLockDatum(record?.nextDatum);
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

export const canonicalNodes = (
  nodes: readonly StateQueueTransitionNode[],
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

export const parseCanonicalNodes = (
  value: unknown,
): readonly StateQueueTransitionNode[] | null => {
  if (!Array.isArray(value) || !canonicalNodes(value)) return null;
  return value.map((node) => ({
    headerHash: (node as StateQueueTransitionNode).headerHash,
    outRef: (node as StateQueueTransitionNode).outRef,
  }));
};

export const decodeStateQueueMintRedeemer = (
  input: DeriveStateQueueCorrectionTransitionInput,
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

/** Normalizes the distinct unattested and unavailable timeout wires without
 * widening availability-challenge removal beyond the queue head. */
export const timeoutRemoval = (decoded: StateQueueRedeemerType) => {
  if (typeof decoded !== "object" || decoded === null) return null;
  if ("RemoveUnattestedBlockAfterTimeout" in decoded) {
    const timeout = decoded.RemoveUnattestedBlockAfterTimeout;
    const approach = timeout.removal_approach;
    return "PruneUnattestedBlockDescendant" in approach
      ? {
          target: timeout.timed_out_header_hash,
          name: "PruneUnattestedBlockDescendant" as const,
          prune: true,
          headOnly: false,
          anchor:
            approach.PruneUnattestedBlockDescendant.timed_out_node_input_outref,
          outputIndex:
            approach.PruneUnattestedBlockDescendant.timed_out_node_output_index,
        }
      : {
          target: timeout.timed_out_header_hash,
          name: "RemoveLastUnattestedBlock" as const,
          prune: false,
          headOnly: false,
          anchor: approach.RemoveLastUnattestedBlock.predecessor_input_outref,
          outputIndex:
            approach.RemoveLastUnattestedBlock.predecessor_output_index,
        };
  }
  if ("RemoveUnavailableBlockAfterTimeout" in decoded) {
    const timeout = decoded.RemoveUnavailableBlockAfterTimeout;
    const approach = timeout.removal_approach;
    return "PruneTimedOutBlockDescendant" in approach
      ? {
          target: timeout.unavailable_header_hash,
          name: "PruneTimedOutBlockDescendant" as const,
          prune: true,
          headOnly: true,
          anchor:
            approach.PruneTimedOutBlockDescendant.timed_out_node_input_outref,
          outputIndex:
            approach.PruneTimedOutBlockDescendant.timed_out_node_output_index,
        }
      : {
          target: timeout.unavailable_header_hash,
          name: "RemoveTimedOutHead" as const,
          prune: false,
          headOnly: true,
          anchor: approach.RemoveTimedOutHead.confirmed_state_input_outref,
          outputIndex: approach.RemoveTimedOutHead.confirmed_state_output_index,
        };
  }
  return null;
};

export const outputReferenceLabel = (reference: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string => `${reference.transactionId}#${reference.outputIndex.toString()}`;
