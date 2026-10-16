import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationDispute,
  type MidgardValidationMachineState,
  type MidgardValidationPhaseName,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  type MidgardValidationTraceTree,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";

/**
 * Two three-step traces over real machine states, for driving the validation
 * game through the deployed validators. Both start from the same state, so the
 * game opens from it; they agree at index 1 and part at index 2. Two full
 * bisection rounds leave the game at ReadyForOneStep on the agreed state at
 * index 1.
 */
/** One side's committed trace: its descriptor and a proof for any index. */
export type TraceSide = {
  readonly descriptor: MidgardValidationTraceDescriptor;
  readonly proofAt: (stateIndex: number) => MidgardValidationTraceProof;
};

export type GameTraces = {
  readonly operator: TraceSide;
  readonly challenger: TraceSide;
  /** The machine state both sides agree on once the bisection ends. */
  readonly agreedLowState: MidgardValidationMachineState;
};

const hash32 = (byte: number) => Buffer.alloc(32, byte);

export const machineState = (
  programCounter: number,
  phase: MidgardValidationPhaseName,
): MidgardValidationMachineState => ({
  machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
  eventKeyHash: hash32(0x11),
  transactionId: hash32(0x12),
  transactionCommitment: hash32(0x13),
  validationContextHash: hash32(0x14),
  sourceKind: "normal",
  priorLedgerRoot: hash32(0x15),
  phase,
  programCounter,
  workRoot: hash32(0x16),
  executionCpu: 0n,
  executionMemory: 0n,
  verdict: "pending",
  rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  ledgerDeltaRoot: hash32(0x17),
});

export const sideOf = (tree: MidgardValidationTraceTree): TraceSide => ({
  descriptor: tree.descriptor,
  proofAt: (stateIndex) => {
    const proof = tree.proofs[stateIndex];
    if (proof === undefined) throw new Error(`no state ${stateIndex}`);
    return proof;
  },
});

const traces = (
  operatorStates: readonly MidgardValidationMachineState[],
  challengerStates: readonly MidgardValidationMachineState[],
  agreedLowState: MidgardValidationMachineState,
): GameTraces => ({
  operator: sideOf(
    buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineState),
      "accepted",
    ),
  ),
  challenger: sideOf(
    buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineState),
      "accepted",
    ),
  ),
  agreedLowState,
});

/** The sides part at a one-step boundary inside CanonicalDecode. */
export const divergentTraces = (): GameTraces => {
  const agreed = machineState(1, "canonicalDecode");
  return traces(
    [machineState(0, "canonicalDecode"), agreed, machineState(2, "cek")],
    [machineState(0, "canonicalDecode"), agreed, machineState(3, "cek")],
    agreed,
  );
};

export type MoveTime = {
  readonly currentTimeLower: number;
  readonly currentTimeUpper: number;
};

export const openGame = (game: GameTraces, sourceTimeUpper: number) =>
  openMidgardValidationDispute({
    operatorDescriptor: game.operator.descriptor,
    challengerDescriptor: game.challenger.descriptor,
    sourceTimeUpper,
  });

/** The operator's proof for the state the dispute is waiting on. */
export const operatorProof = (
  game: GameTraces,
  dispute: MidgardValidationDispute,
) => {
  if (dispute.turn.type !== "awaitingOperator") {
    throw new Error("the game is not awaiting the operator");
  }
  return game.operator.proofAt(dispute.turn.midpoint);
};

export const challengerProof = (
  game: GameTraces,
  dispute: MidgardValidationDispute,
) => {
  if (dispute.turn.type !== "awaitingChallenger") {
    throw new Error("the game is not awaiting the challenger");
  }
  return game.challenger.proofAt(dispute.turn.midpoint);
};

export const revealOperator = (
  game: GameTraces,
  dispute: MidgardValidationDispute,
  time: MoveTime,
) =>
  revealMidgardValidationOperatorMidpoint({
    dispute,
    proof: operatorProof(game, dispute),
    ...time,
  });

export const revealChallenger = (
  game: GameTraces,
  dispute: MidgardValidationDispute,
  time: MoveTime,
) =>
  revealMidgardValidationChallengerMidpoint({
    dispute,
    proof: challengerProof(game, dispute),
    ...time,
  });

/**
 * Plays both bisection rounds at `time`, leaving the game at
 * ReadyForOneStep. Every move is charged from the same turn start, so the
 * resulting state is a plain function of `time`.
 */
export const playToOneStep = (
  game: GameTraces,
  dispute: MidgardValidationDispute,
  time: MoveTime,
): MidgardValidationDispute => {
  let next = dispute;
  while (next.turn.type !== "readyForOneStep") {
    next =
      next.turn.type === "awaitingOperator"
        ? revealOperator(game, next, time)
        : revealChallenger(game, next, time);
  }
  return next;
};
