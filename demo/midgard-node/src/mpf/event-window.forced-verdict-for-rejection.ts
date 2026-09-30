import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyValidationMachineLedgerMutationStep,
  type CanonicalTransitionEffect,
  type RejectCode,
  RejectCodes,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/index.js";
import { type MpfBatchOp } from "./types.js";

/**
 * The operator's recorded verdict for a Phase A/B rejection, as the #640
 * forced leaf carries it. The node's classifier resolves faults only to the
 * 19 descriptor-level `RejectCode`s, so each bucket names the corresponding
 * `RejectionReasonV1` arm at subject ordinal 0 — the coordinates are refined
 * where the classifier learns to report them.
 *
 * `E_NATIVE_SCRIPT_INVALID` is the one code whose arm depends on the phase:
 * Phase A emits it only from the witness-set native scan
 * (`WitnessNativeScriptFalse`), Phase B only from execution natives
 * (`ExecutionNativeScriptFalse`) — both arms bridge back to exactly this
 * code, so the split loses nothing and never claims a CEK failure
 * (`PlutusExecutionFailed`) for a native-script refusal.
 */
export const forcedVerdictForRejection = (
  code: RejectCode,
  phase: "phaseA" | "phaseB",
): SDK.OperatorVerdict => {
  // Rejection codes are grouped into the protocol reason families below.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (code) {
    case RejectCodes.InputNotFound:
      return {
        ForcedTxInvalid: {
          reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
        },
      };
    case RejectCodes.InvalidSignature:
    case RejectCodes.MissingRequiredWitness:
      return {
        ForcedTxInvalid: {
          reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
        },
      };
    case RejectCodes.NativeScriptInvalid:
      return phase === "phaseA"
        ? {
            ForcedTxInvalid: {
              reason: { WitnessNativeScriptFalse: { script_index: 0n } },
            },
          }
        : {
            ForcedTxInvalid: {
              reason: { ExecutionNativeScriptFalse: { execution_index: 0n } },
            },
          };
    case RejectCodes.PlutusScriptInvalid:
    case RejectCodes.PlutusEvaluationUnavailable:
      return {
        ForcedTxInvalid: {
          reason: { PlutusExecutionFailed: { execution_index: 0n } },
        },
      };
    case RejectCodes.MinFee:
      return { ForcedTxInvalid: { reason: "FeeBelowMinimum" } };
    default:
      return { ForcedTxInvalid: { reason: "ValueNotPreserved" } };
  }
};

export type ClassifiedForcedTransaction = {
  readonly entry: ForcedTransactionsDB.Entry;
  /** Byte-exact source effect shared with independent replay consumers. */
  readonly transitionEffect: CanonicalTransitionEffect;
  /** Consensus MPF operations; insert values are canonical descriptors. */
  readonly ledgerOps: readonly MpfBatchOp[];
  /** Full-output operations used only for Phase B state and DA material. */
  readonly rawLedgerOps: readonly MpfBatchOp[];
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly rejectionCode: RejectCode | null;
  readonly programMaterialSidecarCbor: Buffer;
};

export type ForcedProgramMaterialSidecarResolver<R> = (
  envelopes: readonly MidgardCekProgramEnvelope[],
) => Effect.Effect<Buffer, DatabaseError, R>;

export const applyValidationLedgerMutations = async (
  trie: Trie,
  operations: readonly MpfBatchOp[],
): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of operations) {
    steps.push(await applyValidationMachineLedgerMutationStep(trie, operation));
  }
  return steps;
};

export const validationLedgerWitnesses = (
  state: ReadonlyMap<string, Buffer>,
  outRefHexes: readonly string[],
): readonly ValidationMachineLedgerEntry[] =>
  [...new Set(outRefHexes)].sort().flatMap((outRefHex) => {
    const output = state.get(outRefHex);
    return output === undefined
      ? []
      : [
          {
            outRef: Buffer.from(outRefHex, "hex"),
            output: Buffer.from(output),
          },
        ];
  });

export const programMaterialSidecarForEnvelopes = (
  envelopes: readonly MidgardCekProgramEnvelope[],
): Effect.Effect<Buffer, DatabaseError, Database> =>
  CekProgramMaterialDB.retrieveVerifiedBundles(envelopes).pipe(
    Effect.map((entries) => encodeMidgardCekProgramMaterialSidecar(entries)),
  );
