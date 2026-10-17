import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekProgramMaterialSidecar,
  findLedgerSignatureVerifierUnavailable,
  isMidgardConsensusProfile,
  ledgerEd25519SignatureReady,
  ledgerSignatureVerifierUnavailable,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type L1SlotConfig,
  parseL1SlotConfig,
} from "@al-ft/midgard-core/l1-slot-config";
import { Cause, Effect } from "effect";

import { validatePhaseASingle } from "../phase-a.js";
import {
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
 *
 * It waits for the ledger's Ed25519 verifier first. A fault of the verifier
 * fails with `LedgerSignatureVerifierUnavailableError` (the original error
 * its cause), a fault of this process and never a verdict on the event.
 */
export const replayValidationVerdictEvent = (
  input: ValidationVerdictEventInput,
): Effect.Effect<ValidationVerdictEvent, Error> =>
  Effect.gen(function* () {
    yield* Effect.tryPromise({
      try: () => ledgerEd25519SignatureReady(),
      catch: ledgerSignatureVerifierUnavailable,
    });
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
    const phaseA = yield* Effect.try({
      try: () =>
        validatePhaseASingle(queued, {
          consensusProfile: snapshot.consensusProfile,
          expectedNetworkId: input.expectedNetworkId,
          minFeeA: input.minFeeA,
          minFeeB: input.minFeeB,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
        }),
      catch: (cause) =>
        findLedgerSignatureVerifierUnavailable(cause) ??
        new Cause.UnknownException(cause),
    });
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
