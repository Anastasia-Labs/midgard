import {
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core";
import {
  type L1SlotConfig,
  parseL1SlotConfig,
} from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";

import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  type RejectCode,
  type RejectedTx,
  replayValidationVerdictEvent,
  type UTxOStatePatch,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerOp,
  validationMachineLedgerRoot,
  type ValidationMachineReplayInput,
} from "../src/index.js";
import { buildDeterministicValidationMachineTrace } from "./helpers/trace-commit-golden.js";

export type ValidationMachineEventReplayInput = Readonly<{
  consensusProfile: MidgardConsensusProfile;
  eventKeyCbor: Buffer;
  canonicalTransactionCbor: Buffer;
  /** The complete prior ledger, whose descriptor root must equal priorUtxosRoot. */
  ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  priorUtxosRoot: string;
  blockTimeMs: number;
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

/**
 * Replays one event with the canonical validation pipeline, then constructs its
 * trace from the independently derived verdict and exact ledger mutations.
 * Test support: no production path builds a per-transaction trace in P1, so
 * the tests that exercise the trace builder drive it through this helper.
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
      ledgerWitnessEntries,
      priorUtxosRoot: input.priorUtxosRoot,
      blockTimeMs: input.blockTimeMs,
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
