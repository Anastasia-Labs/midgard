import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_DISPUTE_VERSION,
} from "./consensus-profile.js";
import { SELECTED_DEPLOYMENT_PROFILE } from "./deployment-profile.js";
import {
  encodeMidgardValidationTraceDescriptor,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  verifyMidgardValidationTraceProof,
} from "./validation-trace.js";

/** C: each side's cumulative move budget (the chess clock). */
export const MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_side_clock_ms;
/** L_d: the longest validity range any dispute move may carry. */
export const MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;
/** R: the allowance for everything after the game leaves ReadyForOneStep. */
export const MIDGARD_VALIDATION_DISPUTE_RESOLUTION_ALLOWANCE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_resolution_allowance_ms;
export const MIDGARD_VALIDATION_DISPUTE_MAX_BISECTION_ROUNDS =
  MIDGARD_CONSENSUS_LIMITS.maxValidationBisectionRounds;
/** `2C + 2(2·rounds + 2)·L_d + R`, mirroring the validator's bound. */
export const MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS =
  2 * MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS +
  2 *
    (2 * MIDGARD_VALIDATION_DISPUTE_MAX_BISECTION_ROUNDS + 2) *
    MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS +
  MIDGARD_VALIDATION_DISPUTE_RESOLUTION_ALLOWANCE_MS;

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
  readonly turnStartedAt: number;
  readonly operatorClockMs: number;
  readonly challengerClockMs: number;
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

/** A move's validity range: lower and upper bounds, at most L_d apart. */
export type MidgardValidationDisputeMoveTime = {
  readonly currentTimeLower: number;
  readonly currentTimeUpper: number;
};

const requireVersion = (dispute: MidgardValidationDispute): void => {
  if (dispute.version !== MIDGARD_VALIDATION_DISPUTE_VERSION) {
    throw new Error("Unsupported validation dispute version");
  }
};

const requireMoveTime = ({
  currentTimeLower,
  currentTimeUpper,
}: MidgardValidationDisputeMoveTime): void => {
  requireSafeUnsigned(currentTimeLower, "currentTimeLower");
  requireSafeUnsigned(currentTimeUpper, "currentTimeUpper");
  if (currentTimeUpper < currentTimeLower) {
    throw new Error("Validation dispute move range is inverted");
  }
  if (
    currentTimeUpper - currentTimeLower >
    MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS
  ) {
    throw new Error("Validation dispute move range exceeds L_d");
  }
};

/**
 * The mover's clock after a move: the move is charged the time since the turn
 * started, read from its lower bound, and never credited.
 */
const charge = (
  clock: number,
  turnStartedAt: number,
  currentTimeLower: number,
): number => {
  const elapsed = Math.max(0, currentTimeLower - turnStartedAt);
  if (elapsed > clock) {
    throw new Error("Validation dispute mover's clock has run out");
  }
  return clock - elapsed;
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
  agreedLowHash = operatorDescriptor.initialStateHash,
  sourceTimeUpper,
}: {
  readonly operatorDescriptor: MidgardValidationTraceDescriptor;
  readonly challengerDescriptor: MidgardValidationTraceDescriptor;
  /** Defaults to the operator descriptor's initial state hash. */
  readonly agreedLowHash?: Buffer;
  /** The upper bound of the VerifySource transaction that starts the game. */
  readonly sourceTimeUpper: number;
}): MidgardValidationDispute => {
  requireSafeUnsigned(sourceTimeUpper, "sourceTimeUpper");
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
  return {
    version: MIDGARD_VALIDATION_DISPUTE_VERSION,
    operatorDescriptor,
    challengerDescriptor,
    lowIndex: 0,
    highIndex: operatorDescriptor.stepCount,
    agreedLowHash,
    operatorHighHash: operatorDescriptor.terminalStateHash,
    challengerHighHash: challengerDescriptor.terminalStateHash,
    round: 0,
    turnStartedAt: sourceTimeUpper,
    operatorClockMs: MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
    challengerClockMs: MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
    turn: nextTurn(0, operatorDescriptor.stepCount),
  };
};

export const revealMidgardValidationOperatorMidpoint = ({
  dispute,
  proof,
  ...time
}: {
  readonly dispute: MidgardValidationDispute;
  readonly proof: MidgardValidationTraceProof;
} & MidgardValidationDisputeMoveTime): MidgardValidationDispute => {
  requireVersion(dispute);
  requireMoveTime(time);
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
    turnStartedAt: time.currentTimeUpper,
    operatorClockMs: charge(
      dispute.operatorClockMs,
      dispute.turnStartedAt,
      time.currentTimeLower,
    ),
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
  ...time
}: {
  readonly dispute: MidgardValidationDispute;
  readonly proof: MidgardValidationTraceProof;
} & MidgardValidationDisputeMoveTime): MidgardValidationDispute => {
  requireVersion(dispute);
  requireMoveTime(time);
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
    turnStartedAt: time.currentTimeUpper,
    challengerClockMs: charge(
      dispute.challengerClockMs,
      dispute.turnStartedAt,
      time.currentTimeLower,
    ),
    turn: nextTurn(nextLow, nextHigh),
  };
};

/**
 * The only exit from ReadyForOneStep: the challenger's move, charged to the
 * challenger's clock. The game state is otherwise unchanged.
 */
export const enterMidgardValidationDisputeResolution = ({
  dispute,
  ...time
}: {
  readonly dispute: MidgardValidationDispute;
} & MidgardValidationDisputeMoveTime): MidgardValidationDispute => {
  requireVersion(dispute);
  requireMoveTime(time);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error("Validation dispute is not ready for one-step resolution");
  }
  return {
    ...dispute,
    turnStartedAt: time.currentTimeUpper,
    challengerClockMs: charge(
      dispute.challengerClockMs,
      dispute.turnStartedAt,
      time.currentTimeLower,
    ),
  };
};

/**
 * The side to move loses once its whole remaining clock has passed since the
 * turn started, read from the timeout transaction's lower bound.
 */
export const timeoutMidgardValidationDispute = ({
  dispute,
  currentTimeLower,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly currentTimeLower: number;
}): MidgardValidationDisputeWinner => {
  requireVersion(dispute);
  requireSafeUnsigned(currentTimeLower, "currentTimeLower");
  const moverClock =
    dispute.turn.type === "awaitingOperator"
      ? dispute.operatorClockMs
      : dispute.challengerClockMs;
  if (currentTimeLower <= dispute.turnStartedAt + moverClock) {
    throw new Error("Validation dispute mover's clock has not run out");
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
