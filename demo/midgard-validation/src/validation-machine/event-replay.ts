import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekProgramMaterialSidecar,
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type L1SlotConfig,
  parseL1SlotConfig,
} from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";

import { validatePhaseASingle } from "../phase-a.js";
import {
  runPhaseBValidationWithPatch,
  type UTxOStatePatch,
} from "../phase-b.js";
import {
  type LocalScriptEvaluation,
  type PhaseBConfig,
  type QueuedTx,
  type RejectCode,
  type RejectedTx,
} from "../types.js";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerOp,
  validationMachineLedgerRoot,
} from "./ledger-mutation.js";
import { buildDeterministicValidationMachineTrace } from "./trace-builder.js";
import {
  type DeterministicValidationMachineTrace,
  type ValidationMachineReplayInput,
} from "./types.js";

export type ValidationMachineEventReplayInput = Readonly<{
  consensusProfile: MidgardConsensusProfile;
  eventKeyCbor: Buffer;
  canonicalTransactionCbor: Buffer;
  programMaterialSidecarCbor?: Buffer;
  /** The complete prior ledger, whose descriptor root must equal priorUtxosRoot. */
  ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  priorUtxosRoot: string;
  blockEndTimeMs: number;
  expectedNetworkId: bigint;
  minFeeA: bigint;
  minFeeB: bigint;
  blockSlot: bigint;
  /** The deployment manifest's slot mapping (see `PhaseBConfig`). */
  l1SlotConfig: L1SlotConfig;
  sourceKind: "normal" | "forced";
}>;

export type ValidationMachineEventReplay = Readonly<{
  replayInput: ValidationMachineReplayInput;
  trace: DeterministicValidationMachineTrace;
  statePatch: UTxOStatePatch;
  rejection?: RejectedTx;
}>;

export type ValidationVerdictEventInput = Readonly<{
  consensusProfile: MidgardConsensusProfile;
  canonicalTransactionCbor: Buffer;
  programMaterialSidecarCbor?: Buffer;
  /** Every ledger entry the transaction may resolve, keyed by out-ref. */
  ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  blockEndTimeMs: number;
  expectedNetworkId: bigint;
  minFeeA: bigint;
  minFeeB: bigint;
  blockSlot: bigint;
  /** The deployment manifest's slot mapping (see `PhaseBConfig`). */
  l1SlotConfig: L1SlotConfig;
  sourceKind: "normal" | "forced";
  /** "proof" captures every CEK step for a validation-machine trace;
   * verdict-only callers leave the default. */
  cekExecutionMode?: PhaseBConfig["cekExecutionMode"];
}>;

export type ValidationVerdictEvent = Readonly<{
  transactionId: Buffer;
  verdict: "accepted" | "rejected";
  rejection?: RejectedTx;
  statePatch: UTxOStatePatch;
  scriptEvaluations: readonly LocalScriptEvaluation[];
}>;

/**
 * Derives one event's canonical verdict and ledger patch with the shared
 * Phase A/B pipeline, without building a validation-machine trace.
 */
export const replayValidationVerdictEvent = (
  input: ValidationVerdictEventInput,
): Effect.Effect<ValidationVerdictEvent, Error> =>
  Effect.gen(function* () {
    const snapshot = yield* Effect.try(() => {
      if (!isMidgardConsensusProfile(input.consensusProfile)) {
        throw new Error("event replay requires the compiled consensus profile");
      }
      if (input.sourceKind !== "normal" && input.sourceKind !== "forced") {
        throw new Error("event replay requires an exact source commitment");
      }
      const canonicalTransactionCbor = Buffer.from(
        input.canonicalTransactionCbor,
      );
      return {
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        canonicalTransactionCbor,
        transactionId: Buffer.from(
          input.sourceKind === "forced"
            ? deriveMidgardForcedTxFaultEvidenceMaterial(
                canonicalTransactionCbor,
              ).transactionId
            : computeMidgardNativeTxId(
                decodeMidgardNativeTxFullFromCanonicalCbor(
                  canonicalTransactionCbor,
                ).compact,
              ),
        ),
        programMaterialSidecarCbor: Buffer.from(
          input.programMaterialSidecarCbor ??
            encodeMidgardCekProgramMaterialSidecar([]),
        ),
        l1SlotConfig: parseL1SlotConfig(input.l1SlotConfig),
      };
    });
    const queued: QueuedTx = {
      sourceKind: input.sourceKind,
      txId: snapshot.transactionId,
      txCbor: snapshot.canonicalTransactionCbor,
      programMaterialSidecarCbor: snapshot.programMaterialSidecarCbor,
      arrivalSeq: 0n,
      createdAt: new Date(input.blockEndTimeMs),
    };
    const phaseA = yield* Effect.try(() =>
      validatePhaseASingle(queued, {
        consensusProfile: snapshot.consensusProfile,
        expectedNetworkId: input.expectedNetworkId,
        minFeeA: input.minFeeA,
        minFeeB: input.minFeeB,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    const scriptEvaluations: LocalScriptEvaluation[] = [];
    if ("code" in phaseA) {
      return {
        transactionId: snapshot.transactionId,
        verdict: "rejected" as const,
        rejection: phaseA,
        statePatch: { deletedOutRefs: [], upsertedOutRefs: [] },
        scriptEvaluations,
      };
    }
    const phaseB = yield* runPhaseBValidationWithPatch(
      [phaseA],
      new Map(
        input.ledgerWitnessEntries.map(({ outRef, output }) => [
          Buffer.from(outRef).toString("hex"),
          Buffer.from(output),
        ]),
      ),
      {
        nowCardanoSlotNo: input.blockSlot,
        l1SlotConfig: snapshot.l1SlotConfig,
        bucketConcurrency: 1,
        enforceScriptBudget: true,
        ...(input.cekExecutionMode === undefined
          ? {}
          : { cekExecutionMode: input.cekExecutionMode }),
        maxScriptExecutionSteps:
          snapshot.consensusProfile.limits.maxValidationMachineStepCount,
        onScriptEvaluated: (_txId, evaluation) =>
          scriptEvaluations.push(evaluation),
      },
    );
    if (phaseB.accepted.length + phaseB.rejected.length !== 1) {
      return yield* Effect.fail(
        new Error("event replay must produce exactly one canonical verdict"),
      );
    }
    const rejection = phaseB.rejected[0];
    if (
      rejection !== undefined &&
      (phaseB.statePatch.deletedOutRefs.length !== 0 ||
        phaseB.statePatch.upsertedOutRefs.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("rejected event replay produced a ledger mutation"),
      );
    }
    return {
      transactionId: snapshot.transactionId,
      verdict:
        rejection === undefined ? ("accepted" as const) : ("rejected" as const),
      rejection,
      statePatch: phaseB.statePatch,
      scriptEvaluations,
    };
  });

/**
 * Replays one event with the canonical validation pipeline, then constructs its
 * trace from the independently derived verdict and exact ledger mutations.
 * It grants no L1 or history authority: callers must authenticate the source,
 * event order and context before using the result in a proof workflow.
 */
export const replayValidationMachineEvent = (
  input: ValidationMachineEventReplayInput,
): Effect.Effect<ValidationMachineEventReplay, Error> =>
  Effect.gen(function* () {
    const ledgerWitnessEntries = yield* Effect.try(() => {
      const sorted = input.ledgerWitnessEntries
        .map(({ outRef, output }) => ({
          outRef: Buffer.from(outRef),
          output: Buffer.from(output),
        }))
        .sort((left, right) => Buffer.compare(left.outRef, right.outRef));
      for (let index = 1; index < sorted.length; index++) {
        if (sorted[index - 1]!.outRef.equals(sorted[index]!.outRef)) {
          throw new Error("event replay prior ledger repeats an out-ref");
        }
      }
      return sorted;
    });
    const priorRoot = yield* Effect.tryPromise(() =>
      validationMachineLedgerRoot(ledgerWitnessEntries),
    );
    if (priorRoot.toString("hex") !== input.priorUtxosRoot) {
      return yield* Effect.fail(
        new Error(
          "event replay prior ledger root differs from authenticated context",
        ),
      );
    }
    const verdictEvent = yield* replayValidationVerdictEvent({
      ...input,
      ledgerWitnessEntries,
      // The validation trace replays every retained CEK step.
      cekExecutionMode: "proof",
    });
    const { statePatch, rejection } = verdictEvent;
    const rejectionCode: RejectCode | null = rejection?.code ?? null;
    const expectedLedgerOps: readonly ValidationMachineLedgerOp[] = [
      ...statePatch.deletedOutRefs.map((outRef) => ({
        type: "delete" as const,
        key: Buffer.from(outRef, "hex"),
      })),
      ...statePatch.upsertedOutRefs.map(([outRef, output]) =>
        buildValidationMachineLedgerInsertOp({
          key: Buffer.from(outRef, "hex"),
          outputCbor: output,
        }),
      ),
    ];
    const ledgerMutationSteps = yield* Effect.tryPromise(() =>
      buildValidationMachineLedgerMutationSteps({
        initialEntries: ledgerWitnessEntries,
        operations: expectedLedgerOps,
      }),
    );
    const postUtxosRoot = (
      ledgerMutationSteps.at(-1)?.postRoot ?? priorRoot
    ).toString("hex");
    const replayInput: ValidationMachineReplayInput = {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(input.eventKeyCbor),
      canonicalTransactionCbor: Buffer.from(input.canonicalTransactionCbor),
      transactionId: verdictEvent.transactionId,
      programMaterialSidecarCbor: Buffer.from(
        input.programMaterialSidecarCbor ??
          encodeMidgardCekProgramMaterialSidecar([]),
      ),
      ledgerWitnessEntries,
      priorUtxosRoot: input.priorUtxosRoot,
      blockEndTimeMs: input.blockEndTimeMs,
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      blockSlot: input.blockSlot,
      l1SlotConfig: parseL1SlotConfig(input.l1SlotConfig),
      sourceKind: input.sourceKind,
      expectedVerdict: verdictEvent.verdict,
      scriptEvaluations: [...verdictEvent.scriptEvaluations],
      expectedRejectionCode: rejectionCode,
      expectedLedgerOps,
      ledgerMutationSteps,
      postUtxosRoot,
    };
    const trace = yield* buildDeterministicValidationMachineTrace(replayInput);
    return { replayInput, trace, statePatch, rejection };
  });
