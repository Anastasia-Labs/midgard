import {
  deriveMidgardValidationContextCborV1,
  encodeCbor,
} from "@al-ft/midgard-core";
import { Effect } from "effect";

import { awaitLedgerEd25519SignatureReady } from "../ledger-ed25519-ready.js";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../ledger-output-descriptor.js";
import { projectMidgardMalformedNativeWitnessEnvelopeV1 } from "../ledger-tx.js";
import { validatePhaseASingle } from "../phase-a.js";
import { runPhaseBValidationWithPatch } from "../phase-b.js";
import type { LocalScriptEvaluation, QueuedTx, RejectedTx } from "../types.js";
import { RejectCodes } from "../types.js";
import { canonicalDecodeItemRefusal } from "./canonical-field-order.js";
import { noExactLedgerDescriptor } from "./input-resolution.js";
import {
  buildValidationMachineLedgerInsertOp,
  type ValidationMachineLedgerOp,
} from "./ledger-mutation.js";
import { ValidationTraceStopped } from "./trace-builder-stop.js";
import type { ValidationTraceRecording } from "./trace-sink.js";
import type { ReplayedValidationTransaction } from "./trace-walk.js";
import type { ValidationMachineReplayInput } from "./types.js";

/**
 * A rejection with a direct proof but no validation-machine trace: it is not
 * provable by trace. A canonical-decode refusal, and an undecodable Plutus
 * item (arm 48, a witness or an output reference script), which the machine
 * has no successor for and the script-decode family proves.
 */
export class DirectValidationTraceUnavailable extends Error {
  constructor(
    readonly rejectionCode:
      | typeof RejectCodes.InvalidFieldType
      | typeof RejectCodes.IsValidFalseForbidden
      | typeof RejectCodes.ScriptProgramEncoding,
  ) {
    super(`Canonical rejection ${rejectionCode} requires its direct proof`);
    this.name = "DirectValidationTraceUnavailable";
  }
}

const exactHash32 = (hex: string, field: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(hex)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return Buffer.from(hex, "hex");
};

const canonicalLedgerOps = (
  operations: readonly ValidationMachineLedgerOp[],
): Buffer =>
  encodeCbor(
    operations.map((operation) =>
      operation.type === "delete"
        ? [0n, operation.key]
        : [1n, operation.key, operation.value],
    ),
  );

const sameLedgerOps = (
  left: readonly ValidationMachineLedgerOp[],
  right: readonly ValidationMachineLedgerOp[],
): boolean => canonicalLedgerOps(left).equals(canonicalLedgerOps(right));

const safeBlockTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};

/** The phase a V1 rejection stops the machine in. */
export const rejectionPhase = (rejection: RejectedTx) => {
  if (rejection.consensusPhase === undefined) {
    throw new Error(
      `V1 rejection ${rejection.code} is missing its exact consensus phase`,
    );
  }
  return rejection.consensusPhase;
};

/**
 * Replays `input` through Phase A and Phase B once, checks the outcome against
 * the operator's classification and the block transition, and returns it with
 * the transaction material the trace walk reads. The walk's producers see
 * that material only: never the rejection, the verdict or the classification.
 */
export const replayValidationTransaction = (
  input: ValidationMachineReplayInput,
  mode: ValidationTraceRecording["mode"],
) =>
  Effect.gen(function* () {
    yield* awaitLedgerEd25519SignatureReady;
    // The claim derives this context from the header; its time element is
    // `header.end_time` (see `ValidationMachineReplayInput.blockTimeMs`).
    const contextHeader = {
      endTimeMs: safeBlockTime(input.blockTimeMs),
      blockSlot: input.blockSlot,
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
    };
    const contextCbor = deriveMidgardValidationContextCborV1(contextHeader);
    const priorLedgerRoot = exactHash32(input.priorUtxosRoot, "priorUtxosRoot");
    const postLedgerRoot = exactHash32(input.postUtxosRoot, "postUtxosRoot");
    if (input.transactionId.length !== 32) {
      return yield* Effect.fail(
        new Error("transactionId must contain exactly 32 bytes"),
      );
    }

    const queued: QueuedTx = {
      sourceKind: input.sourceKind,
      txId: Buffer.from(input.transactionId),
      txCbor: Buffer.from(input.canonicalTransactionCbor),
      arrivalSeq: 0n,
      createdAt: new Date(input.blockTimeMs),
    };
    const phaseA = validatePhaseASingle(queued, {
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
      consensusProfile: input.consensusProfile,
    });

    const ledgerState = new Map<string, Buffer>();
    const ledgerDescriptorState = new Map<string, Buffer>();
    for (const entry of input.ledgerWitnessEntries) {
      const outRefHex = entry.outRef.toString("hex");
      if (ledgerState.has(outRefHex)) {
        return yield* Effect.fail(
          new Error(`duplicate ledger witness entry for out-ref ${outRefHex}`),
        );
      }
      ledgerState.set(outRefHex, Buffer.from(entry.output));
      let descriptorCbor: Buffer;
      try {
        descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: entry.outRef,
          outputCbor: entry.output,
        }).descriptorCbor;
      } catch {
        // An entry the trace never resolves needs only its trie leaf; input
        // resolution refuses one without a canonical descriptor.
        if (entry.descriptor === undefined) {
          return yield* Effect.fail(noExactLedgerDescriptor(outRefHex));
        }
        descriptorCbor = Buffer.from(entry.descriptor);
      }
      ledgerDescriptorState.set(outRefHex, descriptorCbor);
    }
    const phaseALedgerTx =
      "ledgerTx" in phaseA
        ? phaseA.ledgerTx
        : (projectMidgardMalformedNativeWitnessEnvelopeV1(
            queued.txCbor,
            queued.sourceKind,
          )?.projection.ledgerTx ?? null);
    const scriptEvaluations: LocalScriptEvaluation[] = [];
    let reusedEvaluationIndex = 0;
    let rejection: RejectedTx | null = null;
    let ledgerOps: readonly ValidationMachineLedgerOp[] = [];
    if (!("ledgerTx" in phaseA)) {
      rejection = phaseA;
    } else {
      const phaseB = yield* runPhaseBValidationWithPatch(
        [phaseA],
        ledgerState,
        {
          nowCardanoSlotNo: input.blockSlot,
          l1SlotConfig: input.l1SlotConfig,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          maxScriptExecutionSteps:
            input.consensusProfile.limits.maxValidationMachineStepCount,
          onScriptEvaluated: (_txId, evaluation) => {
            if (input.scriptEvaluations === undefined)
              scriptEvaluations.push(evaluation);
          },
          ...(input.scriptEvaluations === undefined
            ? {
                cekExecutionMode:
                  mode === "proof" ? ("proof" as const) : ("verdict" as const),
              }
            : {
                evaluateProofScript: (
                  scriptBytes: Uint8Array,
                  scriptContextCbor: Uint8Array,
                  executionBudget:
                    | { readonly cpu: bigint; readonly memory: bigint }
                    | undefined,
                  executionIndex?: bigint,
                ) =>
                  Effect.try({
                    try: () => {
                      const captured =
                        input.scriptEvaluations![reusedEvaluationIndex++];
                      if (
                        captured === undefined ||
                        !captured.scriptBytes.equals(
                          Buffer.from(scriptBytes),
                        ) ||
                        !captured.contextCbor.equals(
                          Buffer.from(scriptContextCbor),
                        ) ||
                        captured.executionIndex !== executionIndex ||
                        captured.executionBudget?.cpu !==
                          executionBudget?.cpu ||
                        captured.executionBudget?.memory !==
                          executionBudget?.memory
                      ) {
                        throw new ValidationTraceStopped(
                          "unavailable",
                          input,
                          "Trace cannot reuse the evaluator capture for this script/context/budget/ordinal",
                        );
                      }
                      scriptEvaluations.push(captured);
                      return captured.result;
                    },
                    catch: (cause) =>
                      cause instanceof Error ? cause : new Error(String(cause)),
                  }),
              }),
        },
      );
      rejection = phaseB.rejected[0] ?? null;
      if (rejection === null) {
        ledgerOps = [
          ...phaseB.statePatch.deletedOutRefs.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...phaseB.statePatch.upsertedOutRefs.map(([outRef, output]) =>
            buildValidationMachineLedgerInsertOp({
              key: Buffer.from(outRef, "hex"),
              outputCbor: output,
            }),
          ),
        ];
      }
    }

    if (
      input.scriptEvaluations !== undefined &&
      reusedEvaluationIndex !== input.scriptEvaluations.length
    ) {
      return yield* Effect.fail(
        new ValidationTraceStopped(
          "disagreement",
          input,
          "Trace replay did not consume every evaluator capture",
        ),
      );
    }
    const verdict: "accepted" | "rejected" =
      rejection === null ? "accepted" : "rejected";
    const rejectionCode = rejection?.code ?? null;
    if (
      verdict !== input.expectedVerdict ||
      rejectionCode !== input.expectedRejectionCode
    ) {
      return yield* Effect.fail(
        new ValidationTraceStopped(
          "disagreement",
          input,
          `validation replay disagrees with operator classification: expected=${input.expectedVerdict}/${input.expectedRejectionCode ?? "none"},actual=${verdict}/${rejectionCode ?? "none"},detail=${rejection?.detail ?? "none"}`,
        ),
      );
    }
    if (!sameLedgerOps(ledgerOps, input.expectedLedgerOps)) {
      return yield* Effect.fail(
        new ValidationTraceStopped(
          "disagreement",
          input,
          "validation replay ledger delta differs from block transition",
        ),
      );
    }
    if (
      input.ledgerMutationSteps.length !== ledgerOps.length ||
      input.ledgerMutationSteps.some(
        (step, index) => !sameLedgerOps([step.operation], [ledgerOps[index]!]),
      )
    ) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation steps differ from the exact ledger delta",
        ),
      );
    }
    let mutationRoot = priorLedgerRoot;
    for (const step of input.ledgerMutationSteps) {
      if (!step.preRoot.equals(mutationRoot)) {
        return yield* Effect.fail(
          new Error(
            "validation replay ledger-mutation roots are not contiguous",
          ),
        );
      }
      mutationRoot = step.postRoot;
    }
    if (!mutationRoot.equals(postLedgerRoot)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation terminal root differs from the block transition",
        ),
      );
    }
    if (
      verdict === "rejected" &&
      (!priorLedgerRoot.equals(postLedgerRoot) || ledgerOps.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("a rejected transaction must commit an exact ledger no-op"),
      );
    }
    if (
      rejection !== null &&
      rejectionPhase(rejection) === "canonicalDecode" &&
      !canonicalDecodeItemRefusal(
        input.canonicalTransactionCbor,
        input.sourceKind,
      )
    ) {
      if (
        rejection.code === RejectCodes.InvalidFieldType ||
        rejection.code === RejectCodes.IsValidFalseForbidden
      )
        return yield* Effect.fail(
          new DirectValidationTraceUnavailable(rejection.code),
        );
      return yield* Effect.fail(
        new Error(
          `V1 canonical rejection ${rejection.code} is not representable by the bounded canonical source`,
        ),
      );
    }
    // The machine has no successor for an undecodable Plutus item, of either
    // item kind: the rejection is not provable by trace, and no step of it is
    // recorded. The script-decode family proves it.
    if (rejection?.subject?.arm === "PlutusScriptUndecodable")
      return yield* Effect.fail(
        new DirectValidationTraceUnavailable(RejectCodes.ScriptProgramEncoding),
      );

    const transaction: ReplayedValidationTransaction = {
      input: {
        consensusProfile: input.consensusProfile,
        transactionId: input.transactionId,
        canonicalTransactionCbor: input.canonicalTransactionCbor,
        sourceKind: input.sourceKind,
        ledgerWitnessEntries: input.ledgerWitnessEntries,
        expectedNetworkId: input.expectedNetworkId,
      },
      contextCbor,
      phaseALedgerTx,
      scriptWitnesses:
        "ledgerTx" in phaseA ? phaseA.ledgerTx.scriptWitnesses : [],
      ledgerState,
      ledgerDescriptorState,
      scriptEvaluations,
    };
    return {
      // `phaseB.rejected[0]` reads as never absent; an accepted replay has none.
      rejection: rejection as RejectedTx | null,
      ledgerOps,
      contextHeader,
      priorLedgerRoot,
      transaction,
    };
  });

/** One replay's outcome: what the walk compares against, and what it reads. */
export type ValidationReplay = Effect.Effect.Success<
  ReturnType<typeof replayValidationTransaction>
>;
