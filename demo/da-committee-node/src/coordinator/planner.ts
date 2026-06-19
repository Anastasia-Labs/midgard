import type { DaAttestationCandidateRecord } from "../domain.js";
import {
  isSignerBitSet,
  packSortedSignatureWitnesses,
  parseSignatureWitness,
} from "./witnesses.js";

export type CoordinatorAction =
  | { readonly kind: "init"; readonly headerHash: string }
  | {
      readonly kind: "add_signatures";
      readonly headerHash: string;
      readonly candidateOutRef: string;
      readonly packedWitnessesHex: string;
      readonly signerIndexes: readonly number[];
    }
  | {
      readonly kind: "apply";
      readonly headerHash: string;
      readonly candidateOutRef: string;
    }
  | { readonly kind: "wait"; readonly headerHash: string; readonly reason: string };

export type CoordinatorPlanInput = {
  readonly headerHash: string;
  readonly threshold: number;
  readonly committeeSignersHash: string;
  readonly candidates: readonly DaAttestationCandidateRecord[];
  readonly ownWitnessHex?: string;
  readonly witnessHexes?: readonly string[];
  readonly peerWitnessHexes?: readonly string[];
  readonly requireThresholdWitnesses?: boolean;
};

export const planDaAttestationLifecycle = (
  input: CoordinatorPlanInput,
): CoordinatorAction => {
  const usableCandidates = [...usableCandidatesFor(input)].sort(
    (left, right) => right.attestationCount - left.attestationCount,
  );
  if (usableCandidates.length === 0) {
    if (
      input.requireThresholdWitnesses === true &&
      uniqueWitnessCount(input) < input.threshold
    ) {
      return {
        kind: "wait",
        headerHash: input.headerHash,
        reason: "insufficient witnesses for threshold",
      };
    }
    return { kind: "init", headerHash: input.headerHash };
  }
  const candidate = usableCandidates[0]!;
  if (candidate.attestationCount >= input.threshold) {
    return {
      kind: "apply",
      headerHash: input.headerHash,
      candidateOutRef: candidate.outRef,
    };
  }
  const witnessesToAdd = allWitnessHexes(input).filter((witnessHex) => {
    const witness = parseSignatureWitness(witnessHex);
    return !isSignerBitSet(candidate.bitmap, witness.signerIndex);
  });
  if (witnessesToAdd.length === 0) {
    return {
      kind: "wait",
      headerHash: input.headerHash,
      reason: "no new witnesses for selected candidate",
    };
  }
  if (
    input.requireThresholdWitnesses === true &&
    candidate.attestationCount + uniqueWitnessCount({ witnessHexes: witnessesToAdd }) <
      input.threshold
  ) {
    return {
      kind: "wait",
      headerHash: input.headerHash,
      reason: "insufficient witnesses for threshold",
    };
  }
  const parsedWitnesses = witnessesToAdd.map(parseSignatureWitness);
  return {
    kind: "add_signatures",
    headerHash: input.headerHash,
    candidateOutRef: candidate.outRef,
    packedWitnessesHex: packSortedSignatureWitnesses(witnessesToAdd),
    signerIndexes: parsedWitnesses
      .map((witness) => witness.signerIndex)
      .sort((left, right) => left - right),
  };
};

const allWitnessHexes = (
  input: Pick<
    CoordinatorPlanInput,
    "ownWitnessHex" | "witnessHexes" | "peerWitnessHexes"
  >,
): readonly string[] => [
  ...(input.ownWitnessHex === undefined ? [] : [input.ownWitnessHex]),
  ...(input.witnessHexes ?? []),
  ...(input.peerWitnessHexes ?? []),
];

const uniqueWitnessCount = (
  input: Pick<
    CoordinatorPlanInput,
    "ownWitnessHex" | "witnessHexes" | "peerWitnessHexes"
  >,
): number =>
  new Set(
    allWitnessHexes(input).map(
      (witnessHex) => parseSignatureWitness(witnessHex).signerIndex,
    ),
  ).size;

export const usableCandidatesFor = (
  input: Pick<
    CoordinatorPlanInput,
    "headerHash" | "threshold" | "committeeSignersHash" | "candidates"
  >,
): readonly DaAttestationCandidateRecord[] =>
  input.candidates.filter(
    (candidate) =>
      candidate.headerHash === input.headerHash &&
      candidate.threshold === input.threshold &&
      candidate.committeeSignersHash === input.committeeSignersHash &&
      candidate.status !== "burned" &&
      candidate.status !== "stale",
  );
