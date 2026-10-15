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
  type MidgardCekVerdictRequest,
  runPhaseBValidationWithPatch,
  type UTxOStatePatch,
} from "../phase-b.js";
import {
  type LocalScriptEvaluation,
  type PhaseBConfig,
  type QueuedTx,
  type RejectedTx,
} from "../types.js";
import { type ValidationMachineLedgerEntry } from "./ledger-mutation.js";

export type ValidationVerdictEventInput = Readonly<{
  consensusProfile: MidgardConsensusProfile;
  canonicalTransactionCbor: Buffer;
  programMaterialSidecarCbor?: Buffer;
  /** Every ledger entry the transaction may resolve, keyed by out-ref. */
  ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  /**
   * The block's clock for validity: its state-queue node's authenticated
   * `block_time`. The trace context's third element also carries this value,
   * but the on-chain validation claim (`validation_context_is_exact`) requires
   * that element to equal `header.end_time`. The validation-trace family
   * (00000006) is excluded in P1; P2 reconciles the context element.
   */
  blockTimeMs: number;
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
  /**
   * Replays the transaction by its committed Accepted verdict: no script is
   * run. Each script run is handed here instead (for a later check), and
   * counts as accepted within its declared budget. Every other check runs as
   * in a full replay. Only for verdict mode.
   */
  committedScriptRun?: (request: MidgardCekVerdictRequest) => void;
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
      createdAt: new Date(input.blockTimeMs),
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
    const committedScriptRun = input.committedScriptRun;
    if (
      committedScriptRun !== undefined &&
      (input.cekExecutionMode ?? "verdict") !== "verdict"
    ) {
      return yield* Effect.fail(
        new Error("a committed-verdict replay runs in verdict mode only"),
      );
    }
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
        ...(committedScriptRun === undefined
          ? {
              onScriptEvaluated: (_txId, evaluation) =>
                scriptEvaluations.push(evaluation),
            }
          : {
              evaluateCekVerdict: (request) =>
                Effect.sync(() => {
                  committedScriptRun(request);
                  return {
                    kind: "accepted" as const,
                    budget: { cpu: 0n, memory: 0n },
                  };
                }),
            }),
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
