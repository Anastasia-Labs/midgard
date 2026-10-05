import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  ForcedRejectionStopped,
  forcedVerdictForRejection,
} from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  applyValidationMachineLedgerMutationStep,
  type CanonicalTransitionEffect,
  type LocalScriptEvaluation,
  type RejectCode,
  type RejectedTx,
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
  readonly scriptEvaluations: readonly LocalScriptEvaluation[];
  readonly rejectionCode: RejectCode | null;
  readonly programMaterialSidecarCbor: Buffer;
};

/**
 * The forced leaf's verdict for a rejection. A rejection the writer cannot
 * cite exactly fails the block build instead of committing a guessed reason.
 */
export const forcedRejectionVerdict = (
  rejection: RejectedTx,
): Effect.Effect<SDK.OperatorVerdict, DatabaseError | ForcedRejectionStopped> =>
  Effect.try({
    try: () => forcedVerdictForRejection(rejection),
    catch: (cause) =>
      cause instanceof ForcedRejectionStopped
        ? cause
        : new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message: "Forced transaction rejection has no exact verdict",
            cause,
          }),
  }).pipe(
    Effect.tapError((error) =>
      error instanceof ForcedRejectionStopped
        ? Effect.logError(
            "Forced transaction block stopped: no exact machine verdict",
          ).pipe(
            Effect.annotateLogs({
              rejectCode: error.code,
              consensusPhase: error.consensusPhase ?? "unknown",
              retryable: error.retryable,
            }),
          )
        : Effect.void,
    ),
  );

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

const ledgerWitnessEntry = (
  outRefHex: string,
  output: Buffer,
): ValidationMachineLedgerEntry => ({
  outRef: Buffer.from(outRefHex, "hex"),
  output: Buffer.from(output),
});

/**
 * The ledger witnesses of a transaction the block applies: every spent and
 * reference input resolves against the state immediately before the
 * transaction, as on Cardano, so each one must be present there. A missing
 * input is an invariant failure, never an omitted witness.
 */
export const acceptedTransactionLedgerWitnesses = (
  state: ReadonlyMap<string, Buffer>,
  subject: { readonly table: string; readonly txIdHex: string },
  outRefHexes: readonly string[],
): Effect.Effect<readonly ValidationMachineLedgerEntry[], DatabaseError> =>
  Effect.gen(function* () {
    const witnesses: ValidationMachineLedgerEntry[] = [];
    for (const outRefHex of [...new Set(outRefHexes)].sort()) {
      const output = state.get(outRefHex);
      if (output === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: subject.table,
            message:
              "An applied transaction has an input that is absent from the state immediately before it",
            cause: `tx_id=${subject.txIdHex},outref=${outRefHex}`,
          }),
        );
      }
      witnesses.push(ledgerWitnessEntry(outRefHex, output));
    }
    return witnesses;
  });

/**
 * The ledger witnesses of a rejected forced transaction: the inputs present
 * in the state immediately before it. A rejected forced transaction may name
 * inputs that do not exist there (that is often why it was rejected), and
 * those have no witness.
 */
export const rejectedForcedTransactionLedgerWitnesses = (
  state: ReadonlyMap<string, Buffer>,
  outRefHexes: readonly string[],
): readonly ValidationMachineLedgerEntry[] =>
  [...new Set(outRefHexes)].sort().flatMap((outRefHex) => {
    const output = state.get(outRefHex);
    return output === undefined ? [] : [ledgerWitnessEntry(outRefHex, output)];
  });

export const programMaterialSidecarForEnvelopes = (
  envelopes: readonly MidgardCekProgramEnvelope[],
): Effect.Effect<Buffer, DatabaseError, Database> =>
  CekProgramMaterialDB.retrieveVerifiedBundles(envelopes).pipe(
    Effect.map((entries) => encodeMidgardCekProgramMaterialSidecar(entries)),
  );
