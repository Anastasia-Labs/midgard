import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_DISPUTE_VERSION,
} from "./consensus-profile.js";
import {
  encodeMidgardValidationTraceDescriptor,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  verifyMidgardValidationTraceProof,
} from "./validation-trace.js";

export const MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS =
  MIDGARD_CONSENSUS_LIMITS.validationDisputeResponseWindowMs;
export const MIDGARD_VALIDATION_DISPUTE_MAX_BISECTION_ROUNDS =
  MIDGARD_CONSENSUS_LIMITS.maxValidationBisectionRounds;
export const MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS =
  (2 * MIDGARD_VALIDATION_DISPUTE_MAX_BISECTION_ROUNDS + 2) *
  MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS;

export type MidgardValidationDisputeTurn =
  | {
      readonly type: "awaitingOperator";
      readonly midpoint: number;
    }
  | {
      readonly type: "awaitingChallenger";
      readonly midpoint: number;
      readonly operatorMidpointHash: Buffer;
    }
  | {
      readonly type: "readyForOneStep";
    };

export type MidgardValidationDispute = {
  readonly version: typeof MIDGARD_VALIDATION_DISPUTE_VERSION;
  readonly operatorDescriptor: MidgardValidationTraceDescriptor;
  readonly challengerDescriptor: MidgardValidationTraceDescriptor;
  readonly lowIndex: number;
  readonly highIndex: number;
  readonly agreedLowHash: Buffer;
  readonly operatorHighHash: Buffer;
  readonly challengerHighHash: Buffer;
  readonly round: number;
  readonly responseDeadline: number;
  readonly turn: MidgardValidationDisputeTurn;
};

export type MidgardValidationDisputeWinner =
  | "operator"
  | "challenger"
  | "neither";

const requireSafeUnsigned = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return value;
};

const requireDescriptor = (
  descriptor: MidgardValidationTraceDescriptor,
  field: string,
): void => {
  try {
    encodeMidgardValidationTraceDescriptor(descriptor);
  } catch (cause) {
    throw new Error(
      `${field} is not a valid V1 descriptor: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
};

const hashesEqual = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const midpoint = (low: number, high: number): number =>
  low + Math.floor((high - low) / 2);

const nextTurn = (low: number, high: number): MidgardValidationDisputeTurn =>
  high === low + 1
    ? { type: "readyForOneStep" }
    : { type: "awaitingOperator", midpoint: midpoint(low, high) };

const requireLiveDispute = (
  dispute: MidgardValidationDispute,
  currentTime: number,
): void => {
  if (dispute.version !== MIDGARD_VALIDATION_DISPUTE_VERSION) {
    throw new Error("Unsupported validation dispute version");
  }
  requireSafeUnsigned(currentTime, "currentTime");
  requireSafeUnsigned(dispute.responseDeadline, "responseDeadline");
  if (currentTime > dispute.responseDeadline) {
    throw new Error("Validation dispute response deadline has passed");
  }
};

export const midgardValidationDescriptorsCanDispute = (
  operator: MidgardValidationTraceDescriptor,
  challenger: MidgardValidationTraceDescriptor,
): boolean => {
  try {
    requireDescriptor(operator, "operatorDescriptor");
    requireDescriptor(challenger, "challengerDescriptor");
    return (
      operator.machineVersion === challenger.machineVersion &&
      operator.stepCount === challenger.stepCount &&
      hashesEqual(operator.initialStateHash, challenger.initialStateHash) &&
      (!hashesEqual(operator.terminalStateHash, challenger.terminalStateHash) ||
        operator.verdict !== challenger.verdict ||
        !hashesEqual(operator.rejectionCodeHash, challenger.rejectionCodeHash))
    );
  } catch {
    return false;
  }
};

export const canOpenMidgardValidationDisputeBeforeMaturity = ({
  currentTimeUpper,
  challengedBlockEndTime,
  maturityDuration,
}: {
  readonly currentTimeUpper: number;
  readonly challengedBlockEndTime: number;
  readonly maturityDuration: number;
}): boolean => {
  try {
    requireSafeUnsigned(currentTimeUpper, "currentTimeUpper");
    requireSafeUnsigned(challengedBlockEndTime, "challengedBlockEndTime");
    requireSafeUnsigned(maturityDuration, "maturityDuration");
    return (
      maturityDuration >= MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS &&
      currentTimeUpper + MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS <=
        challengedBlockEndTime + maturityDuration
    );
  } catch {
    return false;
  }
};

export const openMidgardValidationDispute = ({
  operatorDescriptor,
  challengerDescriptor,
  currentTime,
}: {
  readonly operatorDescriptor: MidgardValidationTraceDescriptor;
  readonly challengerDescriptor: MidgardValidationTraceDescriptor;
  readonly currentTime: number;
}): MidgardValidationDispute => {
  requireSafeUnsigned(currentTime, "currentTime");
  if (
    !midgardValidationDescriptorsCanDispute(
      operatorDescriptor,
      challengerDescriptor,
    )
  ) {
    throw new Error("Validation trace descriptors cannot be disputed");
  }
  if (operatorDescriptor.stepCount <= 0) {
    throw new Error("Validation dispute requires at least one transition");
  }
  const responseDeadline =
    currentTime + MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS;
  requireSafeUnsigned(responseDeadline, "responseDeadline");
  return {
    version: MIDGARD_VALIDATION_DISPUTE_VERSION,
    operatorDescriptor,
    challengerDescriptor,
    lowIndex: 0,
    highIndex: operatorDescriptor.stepCount,
    agreedLowHash: operatorDescriptor.initialStateHash,
    operatorHighHash: operatorDescriptor.terminalStateHash,
    challengerHighHash: challengerDescriptor.terminalStateHash,
    round: 0,
    responseDeadline,
    turn: nextTurn(0, operatorDescriptor.stepCount),
  };
};

export const revealMidgardValidationOperatorMidpoint = ({
  dispute,
  proof,
  currentTime,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly proof: MidgardValidationTraceProof;
  readonly currentTime: number;
}): MidgardValidationDispute => {
  requireLiveDispute(dispute, currentTime);
  if (dispute.turn.type !== "awaitingOperator") {
    throw new Error("Validation dispute is not awaiting the operator");
  }
  if (
    proof.stateIndex !== dispute.turn.midpoint ||
    !verifyMidgardValidationTraceProof({
      descriptor: dispute.operatorDescriptor,
      proof,
    })
  ) {
    throw new Error("Invalid operator midpoint proof");
  }
  return {
    ...dispute,
    responseDeadline:
      currentTime + MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
    turn: {
      type: "awaitingChallenger",
      midpoint: dispute.turn.midpoint,
      operatorMidpointHash: proof.stateHash,
    },
  };
};

export const revealMidgardValidationChallengerMidpoint = ({
  dispute,
  proof,
  currentTime,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly proof: MidgardValidationTraceProof;
  readonly currentTime: number;
}): MidgardValidationDispute => {
  requireLiveDispute(dispute, currentTime);
  if (dispute.turn.type !== "awaitingChallenger") {
    throw new Error("Validation dispute is not awaiting the challenger");
  }
  if (
    proof.stateIndex !== dispute.turn.midpoint ||
    !verifyMidgardValidationTraceProof({
      descriptor: dispute.challengerDescriptor,
      proof,
    })
  ) {
    throw new Error("Invalid challenger midpoint proof");
  }
  const midpointAgrees = hashesEqual(
    dispute.turn.operatorMidpointHash,
    proof.stateHash,
  );
  const nextLow = midpointAgrees ? dispute.turn.midpoint : dispute.lowIndex;
  const nextHigh = midpointAgrees ? dispute.highIndex : dispute.turn.midpoint;
  if (nextHigh <= nextLow) {
    throw new Error("Validation dispute bisection did not make progress");
  }
  const round = dispute.round + 1;
  if (round > MIDGARD_VALIDATION_DISPUTE_MAX_BISECTION_ROUNDS) {
    throw new Error("Validation dispute exceeded the bisection-round bound");
  }
  return {
    ...dispute,
    lowIndex: nextLow,
    highIndex: nextHigh,
    agreedLowHash: midpointAgrees ? proof.stateHash : dispute.agreedLowHash,
    operatorHighHash: midpointAgrees
      ? dispute.operatorHighHash
      : dispute.turn.operatorMidpointHash,
    challengerHighHash: midpointAgrees
      ? dispute.challengerHighHash
      : proof.stateHash,
    round,
    responseDeadline:
      currentTime + MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
    turn: nextTurn(nextLow, nextHigh),
  };
};

export const timeoutMidgardValidationDispute = ({
  dispute,
  currentTime,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly currentTime: number;
}): MidgardValidationDisputeWinner => {
  if (dispute.version !== MIDGARD_VALIDATION_DISPUTE_VERSION) {
    throw new Error("Unsupported validation dispute version");
  }
  requireSafeUnsigned(currentTime, "currentTime");
  if (currentTime <= dispute.responseDeadline) {
    throw new Error("Validation dispute response deadline has not passed");
  }
  switch (dispute.turn.type) {
    case "awaitingOperator":
      return "challenger";
    case "awaitingChallenger":
      return "operator";
    case "readyForOneStep":
      return "neither";
  }
};

/**
 * Selects the authenticated proof for the only legal bisection move owned by
 * `role`. Transaction construction can consume this result without
 * reimplementing midpoint or turn semantics.
 */
export const selectMidgardValidationDisputeReveal = ({
  dispute,
  role,
  proofs,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly role: "operator" | "challenger";
  readonly proofs: readonly MidgardValidationTraceProof[];
}):
  | {
      readonly type: "revealOperator";
      readonly proof: MidgardValidationTraceProof;
    }
  | {
      readonly type: "revealChallenger";
      readonly proof: MidgardValidationTraceProof;
    }
  | {
      readonly type: "readyForOneStep";
    } => {
  if (dispute.turn.type === "readyForOneStep") {
    return { type: "readyForOneStep" };
  }
  if (
    (dispute.turn.type === "awaitingOperator" && role !== "operator") ||
    (dispute.turn.type === "awaitingChallenger" && role !== "challenger")
  ) {
    throw new Error(`Validation dispute is not awaiting the ${role}`);
  }
  const proof = proofs[dispute.turn.midpoint];
  if (proof === undefined || proof.stateIndex !== dispute.turn.midpoint) {
    throw new Error("Local validation trace is missing the required midpoint");
  }
  return dispute.turn.type === "awaitingOperator"
    ? { type: "revealOperator", proof }
    : { type: "revealChallenger", proof };
};
