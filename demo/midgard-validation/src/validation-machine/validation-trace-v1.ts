/**
 * The validation-trace commitment of one transaction event, as the node
 * commits it and as a challenger re-derives it.
 *
 * - `requiresRealValidationDescriptorV1` decides which events commit a real
 *   descriptor (rule R-DESC); every other event keeps the placeholder.
 * - `commitValidationTraceV1` replays the transaction once in streaming
 *   commit mode and returns its descriptor: no CEK step is verified or
 *   retained, and the trace is folded into an O(log n) frontier.
 * - `openValidationTraceV1` replays it the same way and keeps every state
 *   hash (32 bytes per state), plus the full states and work witnesses
 *   asked for, so a challenger can build proofs and reveal states without
 *   a proof build. Its `steps` replays it again on the script evaluations
 *   the opening ran and keeps the states and work witnesses it is asked
 *   for, as the proof build holds them, so a one-step resolution is built
 *   without the proof build.
 */
import {
  decodeMidgardValidationTraceDescriptor,
  encodeMidgardValidationTraceDescriptor,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  type MidgardValidationMachineState,
  type MidgardValidationTraceDescriptor,
  MidgardValidationTraceFrontier,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Effect } from "effect";

import { verifyMidgardCekCoreStep } from "../cek-machine.js";
import { type RejectCode } from "../types.js";
import { type ValidationMachineLedgerOp } from "./ledger-mutation.js";
import {
  classifyValidationTraceFailure,
  replayValidationTrace,
} from "./trace-builder.js";
import {
  HashingValidationTraceSink,
  type ValidationTraceStateObserver,
} from "./trace-sink.js";
import {
  type ValidationMachineReplayInput,
  type ValidationMachineWorkWitness,
  type ValidationTraceSteps,
} from "./types.js";

const ZERO_32 = Buffer.alloc(32);

/**
 * Rule R-DESC: an event commits a real validation descriptor iff it is a
 * normal transaction whose witness-set field 8 (redeemers) is non-empty,
 * read from the compact witness set the dispute authenticates. Forced
 * transactions and normal transactions with no redeemers keep the
 * placeholder. Bytes that do not derive a normal proof source cannot pass
 * the on-chain gate either, so they keep the placeholder too.
 */
export const requiresRealValidationDescriptorV1 = (
  canonicalTransactionCbor: Uint8Array,
  sourceKind: "normal" | "forced",
): boolean => {
  if (sourceKind !== "normal") return false;
  let redeemerTxWitsHash: Uint8Array;
  try {
    redeemerTxWitsHash = decodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(
        canonicalTransactionCbor,
      ).witnessSetCompactCbor,
    ).redeemerTxWitsHash;
  } catch {
    return false;
  }
  return !Buffer.from(redeemerTxWitsHash).equals(
    MIDGARD_EMPTY_FIELD_COMMITMENT,
  );
};

/** A transaction's committed trace: its descriptor and its replay result. */
export type ValidationTraceCommitmentV1 = {
  readonly descriptor: MidgardValidationTraceDescriptor;
  readonly rejectionCode: RejectCode | null;
  readonly ledgerOps: readonly ValidationMachineLedgerOp[];
};

/** Replays `input` in commit mode, handing each state to `observe`; returns
 * its commitment and the script evaluations it ran or reused, in run order. */
const commitWith = (
  input: ValidationMachineReplayInput,
  observe: ValidationTraceStateObserver,
) =>
  Effect.gen(function* () {
    const frontier = new MidgardValidationTraceFrontier();
    const completed = yield* replayValidationTrace(input, {
      mode: "commit",
      open: (frame) =>
        new HashingValidationTraceSink(
          frame,
          (index, stateHash, state, witness) => {
            frontier.appendState(stateHash);
            observe(index, stateHash, state, witness);
          },
        ),
    });
    const stepCount = frontier.stateCount - 1;
    if (stepCount > MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount) {
      return yield* Effect.fail(
        new Error("validation trace exceeds the maximum step count"),
      );
    }
    if (
      frontier.initialStateHash.equals(ZERO_32) ||
      frontier.lastStateHash.equals(ZERO_32)
    ) {
      return yield* Effect.fail(
        new Error("validation trace endpoint hash must not be zero"),
      );
    }
    // Round-trip through the descriptor codec, so a commitment is always a
    // well-formed descriptor.
    const descriptor = decodeMidgardValidationTraceDescriptor(
      encodeMidgardValidationTraceDescriptor({
        schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
        machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
        traceRoot: frontier.root(),
        stepCount,
        initialStateHash: frontier.initialStateHash,
        terminalStateHash: frontier.lastStateHash,
        verdict: completed.verdict,
        rejectionCodeHash: completed.rejectionCodeHash,
      }),
    );
    return {
      commitment: {
        descriptor,
        rejectionCode: completed.rejectionCode,
        ledgerOps: completed.ledgerOps,
      } satisfies ValidationTraceCommitmentV1,
      scriptEvaluations: completed.scriptEvaluations,
    };
  });

/**
 * The descriptor of `input`'s validation trace, computed in streaming commit
 * mode. It is bit-identical to the descriptor of
 * `buildDeterministicValidationMachineTrace` for the same input; the build
 * keeps O(log n) trace state. A failure is a process-local
 * `ValidationTraceStopped`, classified as the proof build classifies it.
 */
export const commitValidationTraceV1 = (
  input: ValidationMachineReplayInput,
): Effect.Effect<ValidationTraceCommitmentV1, Error> =>
  commitWith(input, () => undefined).pipe(
    Effect.map(({ commitment }) => commitment),
    classifyValidationTraceFailure(input),
  );

/** Picks the steps a replay keeps by program counter and work witness. */
type ValidationTraceStepPick = (
  programCounter: number,
  witness: ValidationMachineWorkWitness,
) => boolean;

/** `witness` as a replay keeps it: a CEK step verified as the proof build
 * verifies every step. */
const keptWitness = (
  witness: ValidationMachineWorkWitness,
): ValidationMachineWorkWitness => {
  const { auxiliary } = witness;
  if (
    auxiliary?.kind === "cekCoreStep" &&
    !verifyMidgardCekCoreStep(
      auxiliary.step.pre,
      auxiliary.step.post,
      auxiliary.step.witness,
    )
  )
    throw new Error(
      `generated CEK ${auxiliary.step.witness.kind} transition does not pass the consensus verifier`,
    );
  return witness;
};

/** Every state hash of one honest trace, as a challenger replays it, and a
 * handle that replays it again for the steps a one-step resolution reads. */
export type ValidationTraceLeavesV1 = ValidationTraceCommitmentV1 & {
  /** `T`: the trace has `T + 1` states. */
  readonly stepCount: number;
  /** State hash `i` is bytes `32i .. 32i + 32`. */
  readonly stateHashes: Buffer;
  /** The terminal state and every state asked for, by program counter. */
  readonly states: ReadonlyMap<number, MidgardValidationMachineState>;
  readonly sourceKind: "normal" | "forced";
  /** Rule R-DESC for the replayed transaction. */
  readonly requiresRealDescriptor: boolean;
  /** Every work witness asked for, by program counter in trace order. */
  readonly witnesses: ReadonlyMap<number, ValidationMachineWorkWitness>;
  /**
   * Replays the transaction again and keeps the states and work witnesses
   * `keep` picks: each exactly as `buildDeterministicValidationMachineTrace`
   * holds it at that program counter, a kept CEK step verified as the proof
   * build verifies every step. What a one-step resolution reads of the
   * trace, without the proof build's O(T) witnesses. The replay reuses the
   * opening's script evaluations instead of running the evaluator again; it
   * refuses an evaluation its own run does not match exactly.
   */
  readonly steps: (
    keep: ValidationTraceStepPick,
  ) => Effect.Effect<ValidationTraceSteps, Error>;
};

/** State hash `index` of an opened trace. */
export const validationTraceStateHashV1 = (
  leaves: Pick<ValidationTraceLeavesV1, "stepCount" | "stateHashes">,
  index: number,
): Buffer => {
  if (!Number.isSafeInteger(index) || index < 0 || index > leaves.stepCount) {
    throw new Error("validation trace state index is out of range");
  }
  return leaves.stateHashes.subarray(index * 32, index * 32 + 32);
};

/**
 * Replays `input`, reusing its `scriptEvaluations`, and keeps the states and
 * work witnesses `keep` picks.
 */
const replaySteps = (
  input: ValidationMachineReplayInput,
  keep: ValidationTraceStepPick,
): Effect.Effect<ValidationTraceSteps, Error> =>
  Effect.gen(function* () {
    const states: Record<number, MidgardValidationMachineState> = {};
    const witnesses: Record<number, ValidationMachineWorkWitness> = {};
    yield* commitWith(input, (index, _stateHash, state, witness) => {
      if (!keep(index, witness)) return;
      witnesses[index] = keptWitness(witness);
      states[index] = state();
    });
    return { states, witnesses } satisfies ValidationTraceSteps;
  }).pipe(classifyValidationTraceFailure(input));

/**
 * Replays `input` in commit mode and keeps its state hashes: one replay per
 * (block, transaction), shared by every game on it. `revealStates` names
 * the program counters whose full states are kept as well; the terminal
 * state is always kept. A counter past the end is kept as the terminal
 * state, the state the honest trace holds there at any longer step count.
 * `keepWitnesses` picks the work witnesses kept, each as `steps` keeps it.
 */
export const openValidationTraceV1 = (
  input: ValidationMachineReplayInput,
  options: {
    readonly revealStates?: readonly number[];
    readonly keepWitnesses?: ValidationTraceStepPick;
  } = {},
): Effect.Effect<ValidationTraceLeavesV1, Error> =>
  Effect.gen(function* () {
    const reveal = new Set(options.revealStates ?? []);
    let hashes = Buffer.alloc(32 * 1024);
    let count = 0;
    const kept = new Map<number, MidgardValidationMachineState>();
    const witnesses = new Map<number, ValidationMachineWorkWitness>();
    let last: MidgardValidationMachineState | null = null;
    const { commitment, scriptEvaluations } = yield* commitWith(
      input,
      (index, stateHash, state, witness) => {
        if ((count + 1) * 32 > hashes.length) {
          const grown = Buffer.alloc(hashes.length * 2);
          hashes.copy(grown);
          hashes = grown;
        }
        stateHash.copy(hashes, count * 32);
        count += 1;
        last = state();
        if (reveal.has(index)) kept.set(index, last);
        if (options.keepWitnesses?.(index, witness) === true)
          witnesses.set(index, keptWitness(witness));
      },
    );
    const stepCount = count - 1;
    kept.set(stepCount, last!);
    for (const index of reveal) {
      if (Number.isSafeInteger(index) && index > stepCount)
        kept.set(index, last!);
      if (!kept.has(index)) {
        return yield* Effect.fail(
          new Error("validation trace has no state at a revealed index"),
        );
      }
    }
    return {
      ...commitment,
      stepCount,
      stateHashes: Buffer.from(hashes.subarray(0, count * 32)),
      states: kept,
      sourceKind: input.sourceKind,
      requiresRealDescriptor: requiresRealValidationDescriptorV1(
        input.canonicalTransactionCbor,
        input.sourceKind,
      ),
      witnesses,
      steps: (keep) => replaySteps({ ...input, scriptEvaluations }, keep),
    } satisfies ValidationTraceLeavesV1;
  }).pipe(classifyValidationTraceFailure(input));
