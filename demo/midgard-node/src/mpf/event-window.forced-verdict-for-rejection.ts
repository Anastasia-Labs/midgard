import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  applyValidationMachineLedgerMutationStep,
  type CanonicalTransitionEffect,
  type RejectCode,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/index.js";
import { type MpfBatchOp } from "./types.js";

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
