import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekProgramMaterialSidecar,
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import { Effect } from "effect";

import { validatePhaseASingle } from "../phase-a.js";
import {
  runPhaseBValidationWithPatch,
  type UTxOStatePatch,
} from "../phase-b.js";
import { type QueuedTx, type RejectCode } from "../types.js";
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
  sourceKind: "normal" | "forced";
}>;

export type ValidationMachineEventReplay = Readonly<{
  replayInput: ValidationMachineReplayInput;
  trace: DeterministicValidationMachineTrace;
  statePatch: UTxOStatePatch;
}>;

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
    const snapshot = yield* Effect.try(() => {
      if (!isMidgardConsensusProfile(input.consensusProfile)) {
        throw new Error("event replay requires the compiled consensus profile");
      }
      if (input.sourceKind !== "normal" && input.sourceKind !== "forced") {
        throw new Error("event replay requires an exact source commitment");
      }
      const ledgerWitnessEntries = input.ledgerWitnessEntries
        .map(({ outRef, output }) => ({
          outRef: Buffer.from(outRef),
          output: Buffer.from(output),
        }))
        .sort((left, right) => Buffer.compare(left.outRef, right.outRef));
      for (let index = 1; index < ledgerWitnessEntries.length; index++) {
        if (
          ledgerWitnessEntries[index - 1]!.outRef.equals(
            ledgerWitnessEntries[index]!.outRef,
          )
        ) {
          throw new Error("event replay prior ledger repeats an out-ref");
        }
      }
      const canonicalTransactionCbor = Buffer.from(
        input.canonicalTransactionCbor,
      );
      const transactionId = Buffer.from(
        computeMidgardNativeTxId(
          (input.sourceKind === "forced"
            ? decodeMidgardForcedTxFullFromCanonicalCbor
            : decodeMidgardNativeTxFullFromCanonicalCbor)(
            canonicalTransactionCbor,
          ).compact,
        ),
      );
      return {
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: Buffer.from(input.eventKeyCbor),
        canonicalTransactionCbor,
        transactionId,
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
        sourceKind: input.sourceKind,
      };
    });
    const priorRoot = yield* Effect.tryPromise(() =>
      validationMachineLedgerRoot(snapshot.ledgerWitnessEntries),
    );
    if (priorRoot.toString("hex") !== snapshot.priorUtxosRoot) {
      return yield* Effect.fail(
        new Error(
          "event replay prior ledger root differs from authenticated context",
        ),
      );
    }
    const queued: QueuedTx = {
      sourceKind: snapshot.sourceKind,
      txId: snapshot.transactionId,
      txCbor: snapshot.canonicalTransactionCbor,
      programMaterialSidecarCbor: snapshot.programMaterialSidecarCbor,
      arrivalSeq: 0n,
      createdAt: new Date(snapshot.blockEndTimeMs),
    };
    const phaseA = yield* Effect.try(() =>
      validatePhaseASingle(queued, {
        consensusProfile: snapshot.consensusProfile,
        expectedNetworkId: snapshot.expectedNetworkId,
        minFeeA: snapshot.minFeeA,
        minFeeB: snapshot.minFeeB,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    let rejectionCode: RejectCode | null;
    let statePatch: UTxOStatePatch = {
      deletedOutRefs: [],
      upsertedOutRefs: [],
    };
    if ("code" in phaseA) {
      rejectionCode = phaseA.code;
    } else {
      const phaseB = yield* runPhaseBValidationWithPatch(
        [phaseA],
        new Map(
          snapshot.ledgerWitnessEntries.map(({ outRef, output }) => [
            outRef.toString("hex"),
            Buffer.from(output),
          ]),
        ),
        {
          nowCardanoSlotNo: snapshot.blockSlot,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      );
      if (phaseB.accepted.length + phaseB.rejected.length !== 1) {
        return yield* Effect.fail(
          new Error("event replay must produce exactly one canonical verdict"),
        );
      }
      rejectionCode = phaseB.rejected[0]?.code ?? null;
      statePatch = phaseB.statePatch;
    }
    const expectedVerdict = rejectionCode === null ? "accepted" : "rejected";
    if (
      expectedVerdict === "rejected" &&
      (statePatch.deletedOutRefs.length !== 0 ||
        statePatch.upsertedOutRefs.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("rejected event replay produced a ledger mutation"),
      );
    }
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
        initialEntries: snapshot.ledgerWitnessEntries,
        operations: expectedLedgerOps,
      }),
    );
    const postUtxosRoot = (
      ledgerMutationSteps.at(-1)?.postRoot ?? priorRoot
    ).toString("hex");
    const replayInput: ValidationMachineReplayInput = {
      ...snapshot,
      expectedVerdict,
      expectedRejectionCode: rejectionCode,
      expectedLedgerOps,
      ledgerMutationSteps,
      postUtxosRoot,
    };
    const trace = yield* buildDeterministicValidationMachineTrace(replayInput);
    return { replayInput, trace, statePatch };
  });
