import {
  deriveMidgardValidationInitialStateV1,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  encodeMidgardRejectionReasonV1Cbor,
  hashMidgardValidationRejectionReasonV1,
  type MidgardRejectionReasonV1,
  type MidgardRejectionReasonV1ArmName,
} from "@al-ft/midgard-core/validation-trace";
import { Effect } from "effect";

import type { MidgardRawEnvelopePhaseAProjection } from "../ledger-tx.js";
import type { MidgardLedgerTx } from "../ledger-tx/types.js";
import {
  REJECTION_REASON_ARMS_BY_CODE,
  rejectionReasonArmName,
  rejectionReasonFitsCode,
  resolveRejectionReason,
} from "../rejection-reason.js";
import type { LocalScriptEvaluation } from "../types.js";
import { encodeValidationTerminalWitnessCbor } from "./terminal-witness.js";
import { rejectionPhase, type ValidationReplay } from "./trace-replay.js";
import {
  validationMachineStateV1,
  type ValidationTraceRecording,
  type ValidationTraceStateFrame,
} from "./trace-sink.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineReplayInput,
  ValidationMachineWorkWitness,
} from "./types.js";

/**
 * What a producer derived for its step: the walk goes on, or the machine
 * stops on `reason` because of `fault`.
 */
export type PhaseOutcome =
  | { readonly kind: "continue" }
  | {
      readonly kind: "stopped";
      readonly reason: MidgardRejectionReasonV1;
      /** What the producer found, for a disagreement's message. */
      readonly fault: string;
    };

export const phaseContinues: PhaseOutcome = { kind: "continue" };

export const phaseStops = (
  reason: MidgardRejectionReasonV1,
  fault: string,
): PhaseOutcome => ({ kind: "stopped", reason, fault });

/**
 * A producer and replay derived different outcomes for the same transaction.
 * The mismatch is deterministic, so no retry resolves it.
 */
export class ValidationTraceDisagreement extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ValidationTraceDisagreement";
  }
}

/** The replay input a producer reads: the transaction and its context, never its classification. */
export type ValidationTraceTransactionInput = Pick<
  ValidationMachineReplayInput,
  | "consensusProfile"
  | "transactionId"
  | "canonicalTransactionCbor"
  | "sourceKind"
  | "ledgerWitnessEntries"
  | "expectedNetworkId"
>;

/** The material replay hands the producers. */
export type ReplayedValidationTransaction = {
  readonly input: ValidationTraceTransactionInput;
  readonly contextCbor: Buffer;
  /** Phase A's ledger transaction, or the raw envelope's projection of it. */
  readonly phaseALedgerTx:
    | MidgardLedgerTx
    | MidgardRawEnvelopePhaseAProjection["ledgerTx"]
    | null;
  /** Phase A's script witnesses: none when Phase A rejected. */
  readonly scriptWitnesses: MidgardLedgerTx["scriptWitnesses"];
  readonly ledgerState: ReadonlyMap<string, Buffer>;
  readonly ledgerDescriptorState: ReadonlyMap<string, Buffer>;
  /** The script evaluations replay ran or reused, in run order. */
  readonly scriptEvaluations: readonly LocalScriptEvaluation[];
};

type TraceWalkEntry = {
  readonly step: string;
  readonly phase: MidgardValidationPhaseName;
  /**
   * The replay arms that steer this step: where replay rejected in the phase
   * on one of them and the producer derived no stop, the walk stops on
   * replay's reason. Every other arm replay rejects on must be one the
   * producer derives, so a walk that goes on past it disagrees. `every` is
   * every arm of the phase.
   */
  readonly steeredBy: "every" | readonly MidgardRejectionReasonV1ArmName[];
};

/** The walk's steps, in order. */
export const TRACE_WALK_STEPS = [
  { step: "canonicalDecode", phase: "canonicalDecode", steeredBy: [] },
  { step: "compactBinding", phase: "compactBinding", steeredBy: "every" },
  { step: "staticLedgerRules", phase: "staticLedgerRules", steeredBy: "every" },
  {
    step: "inputSets",
    phase: "inputSets",
    steeredBy: ["ValidityIntervalMalformed"],
  },
  { step: "signatures", phase: "signatures", steeredBy: [] },
  { step: "phaseANativeScripts", phase: "phaseANativeScripts", steeredBy: [] },
  {
    step: "phaseAScriptPreconditions",
    phase: "phaseAScriptPreconditions",
    steeredBy: [
      "ScriptIntegrityHashMissing",
      "ObserversForbiddenOnUntaggedNetwork",
      "ObserverOrderInvalid",
    ],
  },
  {
    step: "validityInterval",
    phase: "resolveInputs",
    steeredBy: ["ValidityIntervalExcludesBlockSlot"],
  },
  { step: "resolveInputs", phase: "resolveInputs", steeredBy: [] },
  { step: "scriptSources", phase: "scriptSources", steeredBy: [] },
  { step: "nativeScripts", phase: "nativeScripts", steeredBy: [] },
  {
    step: "scriptIntegrity",
    phase: "scriptIntegrity",
    steeredBy: ["ScriptIntegrityHashMismatch"],
  },
  { step: "cek", phase: "cek", steeredBy: [] },
  { step: "valueAndMint", phase: "valueAndMint", steeredBy: [] },
] as const satisfies readonly TraceWalkEntry[];

export type TraceWalkStep = (typeof TRACE_WALK_STEPS)[number]["step"];

/** What a producer reaches of the walk: no rejection, verdict or code. */
export type TraceWalk = {
  readonly mode: ValidationTraceRecording["mode"];
  readonly frame: ValidationTraceStateFrame;
  /** The executed budget every pushed state records; CEK steps raise it. */
  readonly budget: { cpu: bigint; memory: bigint };
  readonly push: (
    phase: MidgardValidationPhaseName,
    cbor: Buffer,
    auxiliary?: ValidationMachineWorkWitness["auxiliary"],
  ) => void;
  /**
   * Runs `step`'s producer, then compares its outcome with replay. Steps run
   * in table order; once the walk stops, later steps do not run.
   */
  readonly phase: (
    step: TraceWalkStep,
    produce: () => Effect.Effect<PhaseOutcome, Error>,
  ) => Effect.Effect<void, Error>;
};

/**
 * What a completed replay leaves besides the states it pushed to its sink:
 * the trace's verdict and the bytes a full trace carries.
 */
export type CompletedValidationTrace = Pick<
  DeterministicValidationMachineTrace,
  | "validationContextCbor"
  | "verdict"
  | "rejectionCode"
  | "rejectionReason"
  | "ledgerOps"
> & { readonly rejectionCodeHash: Buffer };

const sameReason = (
  left: MidgardRejectionReasonV1,
  right: MidgardRejectionReasonV1,
): boolean =>
  encodeMidgardRejectionReasonV1Cbor(left).equals(
    encodeMidgardRejectionReasonV1Cbor(right),
  );

/**
 * Opens the walk of `replay`'s transaction: the frame of state 0, the sink,
 * the budget and the step order. At each step's end the walk compares the
 * producer's outcome with replay, both ways, coordinates included.
 * `finish` pushes the terminal state.
 */
export const openTraceWalk = (
  input: ValidationMachineReplayInput,
  replay: ValidationReplay,
  recording: ValidationTraceRecording,
) =>
  Effect.gen(function* () {
    const { rejection } = replay;
    const terminalPhase =
      rejection === null ? "valueAndMint" : rejectionPhase(rejection);
    if (!TRACE_WALK_STEPS.some((entry) => entry.phase === terminalPhase)) {
      return yield* Effect.fail(
        new Error(`unknown validation terminal phase ${terminalPhase}`),
      );
    }
    // State 0 is the claim's derivation, not a builder construction.
    const { canonicalTransactionCbor } = input;
    const initialState = deriveMidgardValidationInitialStateV1({
      ...replay.contextHeader,
      eventKeyCbor: input.eventKeyCbor,
      source:
        input.sourceKind === "forced"
          ? { forced: { canonicalTransactionCbor } }
          : { normal: { canonicalTransactionCbor } },
      preUtxosRoot: replay.priorLedgerRoot,
    });
    if (!initialState.transactionId.equals(input.transactionId)) {
      return yield* Effect.fail(
        new Error("transactionId is not the canonical transaction's id"),
      );
    }
    // Every state shares state 0's frame, and state 0 is the claim's
    // derivation: the first push must reproduce it exactly.
    const frame: ValidationTraceStateFrame = {
      eventKeyHash: initialState.eventKeyHash,
      transactionId: initialState.transactionId,
      transactionCommitment: initialState.transactionCommitment,
      validationContextHash: initialState.validationContextHash,
      sourceKind: initialState.sourceKind,
      priorLedgerRoot: initialState.priorLedgerRoot,
    };
    const initialStateHash = hashMidgardValidationMachineState(initialState);
    const sink = recording.open(frame);
    const budget = { cpu: 0n, memory: 0n };
    const push: TraceWalk["push"] = (phase, cbor, auxiliary = null) => {
      const witness = { phase, programCounter: sink.length, cbor, auxiliary };
      if (
        witness.programCounter === 0 &&
        !hashMidgardValidationMachineState(
          validationMachineStateV1(frame, witness, budget, null),
        ).equals(initialStateHash)
      ) {
        throw new Error(
          "validation trace does not start from the derived initial state",
        );
      }
      sink.push(witness, budget, null);
    };

    const replayOutcome =
      rejection === null
        ? "accepted"
        : `rejected at ${terminalPhase}/${rejection.code}`;
    const resolved =
      rejection === null ? null : resolveRejectionReason(rejection, "machine");

    // The reason the machine stops on: the producer's, or replay's where
    // replay steers the step.
    const compare = (
      index: number,
      outcome: PhaseOutcome,
    ): Effect.Effect<MidgardRejectionReasonV1 | null, Error> => {
      const entry: TraceWalkEntry = TRACE_WALK_STEPS[index]!;
      const rejectedHere =
        rejection !== null && resolved !== null && terminalPhase === entry.phase
          ? { rejection, resolved }
          : null;
      if (outcome.kind === "stopped") {
        const arm = rejectionReasonArmName(outcome.reason);
        if (
          rejectedHere === null ||
          !rejectionReasonFitsCode(rejectedHere.rejection.code, outcome.reason)
        ) {
          return Effect.fail(
            new ValidationTraceDisagreement(
              `${outcome.fault}: the walk stops on ${arm} in ${entry.phase} but replay ${replayOutcome}`,
            ),
          );
        }
        if (
          rejectedHere.resolved.kind === "reason" &&
          !sameReason(rejectedHere.resolved.reason, outcome.reason)
        ) {
          return Effect.fail(
            new ValidationTraceDisagreement(
              `${outcome.fault}: the walk stops on ${arm} but replay recorded ${rejectionReasonArmName(rejectedHere.resolved.reason)} with different coordinates`,
            ),
          );
        }
        return Effect.succeed(outcome.reason);
      }
      if (rejectedHere === null) return Effect.succeed(null);
      const { code } = rejectedHere.rejection;
      // A phase's steps are adjacent: its last step is the one the next
      // phase follows.
      const last = TRACE_WALK_STEPS[index + 1]?.phase !== entry.phase;
      const codeArms = REJECTION_REASON_ARMS_BY_CODE.get(code);
      if (codeArms === undefined) {
        return entry.steeredBy === "every" || last
          ? Effect.fail(
              new Error(
                `V1 trace stopped on ${code}, which no RejectionReasonV1 arm proves`,
              ),
            )
          : Effect.succeed(null);
      }
      const steeredArms =
        entry.steeredBy === "every"
          ? codeArms
          : entry.steeredBy.filter((arm) => codeArms.includes(arm));
      if (steeredArms.length > 0) {
        if (rejectedHere.resolved.kind !== "reason") {
          return Effect.fail(
            new Error(
              `V1 trace stopped on ${code} without the subject that names its RejectionReasonV1 coordinates`,
            ),
          );
        }
        const { reason } = rejectedHere.resolved;
        if (steeredArms.includes(rejectionReasonArmName(reason))) {
          return Effect.succeed(reason);
        }
      }
      return last
        ? Effect.fail(
            new ValidationTraceDisagreement(
              `replay ${replayOutcome} but the walk found no fault in ${entry.phase}`,
            ),
          )
        : Effect.succeed(null);
    };

    let cursor = 0;
    let stopReason: MidgardRejectionReasonV1 | null = null;
    const walk: TraceWalk = {
      mode: recording.mode,
      frame,
      budget,
      push,
      phase: (step, produce) =>
        Effect.gen(function* () {
          if (stopReason !== null) return;
          const index = cursor;
          if (TRACE_WALK_STEPS[index]?.step !== step) {
            return yield* Effect.fail(
              new Error(
                `trace walk step ${step} is out of order: the next step is ${TRACE_WALK_STEPS[index]?.step ?? "the terminal"}`,
              ),
            );
          }
          cursor += 1;
          stopReason = yield* compare(index, yield* produce());
        }),
    };

    const finish = () =>
      Effect.gen(function* () {
        if (stopReason === null && cursor !== TRACE_WALK_STEPS.length) {
          return yield* Effect.fail(
            new Error(
              `V1 trace walk ended before step ${TRACE_WALK_STEPS[cursor]!.step}`,
            ),
          );
        }
        const rejectionReason: MidgardRejectionReasonV1 | null = stopReason;
        if ((rejectionReason === null) !== (rejection === null)) {
          return yield* Effect.fail(
            new Error(
              `V1 trace did not reach rejection phase ${terminalPhase}`,
            ),
          );
        }
        const verdict = rejection === null ? "accepted" : "rejected";
        const rejectionCodeHash =
          rejectionReason === null
            ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
            : hashMidgardValidationRejectionReasonV1(rejectionReason);
        sink.push(
          {
            phase: "terminal",
            programCounter: sink.length,
            cbor: encodeValidationTerminalWitnessCbor(
              rejectionReason === null
                ? { verdict: "accepted" }
                : {
                    verdict: "rejected",
                    reason: rejectionReason,
                    priorLedgerRoot: replay.priorLedgerRoot,
                  },
            ),
            auxiliary: null,
          },
          budget,
          { verdict, rejectionCodeHash },
        );
        return {
          validationContextCbor: replay.transaction.contextCbor,
          verdict,
          rejectionCode: rejection?.code ?? null,
          rejectionCodeHash,
          rejectionReason,
          ledgerOps: replay.ledgerOps,
        } satisfies CompletedValidationTrace;
      });

    return { walk, finish };
  });
