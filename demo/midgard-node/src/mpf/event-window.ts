/**
 * Resolving the deposit, withdrawal, and forced-transaction entries included in a commit window.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec";
import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  collectMidgardAttachedProgramEnvelopes,
  collectMidgardReferencedProgramEnvelopes,
} from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  applyValidationMachineLedgerMutationStep,
  buildCanonicalTransitionEffect,
  type CanonicalTransitionEffect,
  canonicalTransitionEffectFromStatePatch,
  type RejectCode,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import * as DepositsDB from "../database/deposits.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as WithdrawalsDB from "../database/withdrawals.js";
import { Database } from "../services/index.js";
import { sha256 } from "../sha256.js";
import {
  ledgerOutputToInsertBatchOp,
  transitionEffectToLedgerOps,
  transitionEffectToRawLedgerOps,
} from "./ledger-delta.js";
import { type ProcessMpfsConfig } from "./process-config.js";
import { type MpfBatchOp } from "./types.js";

export const resolveIncludedDepositEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
  persistProjection = true,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
  readonly persistProjection?: boolean;
}): Effect.Effect<readonly DepositsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime);
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: DepositsDB.tableName,
              message:
                "Refusing to build a block because one or more deposits due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] !== DepositsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected deposit UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime(),
        );
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

        const awaitingEntries = currentWindowEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (persistProjection && awaitingEntries.length > 0) {
          const mempoolEntries = yield* Effect.forEach(
            awaitingEntries,
            DepositsDB.toMempoolLedgerEntry,
          );
          yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
          yield* DepositsDB.markAwaitingAsProjected(
            awaitingEntries.map((entry) => entry[DepositsDB.Columns.ID]),
          );
        }

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting
              ? {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
                }
              : entry,
        );
        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      DepositsDB.tableName,
      "Failed to resolve deposits for the current block window",
    ),
  );

export const resolveIncludedWithdrawalEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<readonly WithdrawalsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* WithdrawalsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] ===
            WithdrawalsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because one or more withdrawals due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[WithdrawalsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] !==
            WithdrawalsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected withdrawal UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime(),
        );

        return [...replayableOverdueEntries, ...currentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      WithdrawalsDB.tableName,
      "Failed to resolve withdrawals for the current block window",
    ),
  );

export const resolveIncludedForcedTransactionEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
  persistProjection = true,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
  readonly persistProjection?: boolean;
}): Effect.Effect<
  readonly ForcedTransactionsDB.Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message:
                "Refusing to build a block because one or more tx-order events due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) =>
                  entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
                    "hex",
                  ),
                )
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] !==
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected tx-order event(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
        );
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

        const awaitingEntries = currentWindowEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (persistProjection && awaitingEntries.length > 0) {
          yield* ForcedTransactionsDB.markAwaitingAsProjected(
            awaitingEntries.map(
              (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
            ),
          );
        }

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting
              ? {
                  ...entry,
                  [ForcedTransactionsDB.Columns.STATUS]:
                    ForcedTransactionsDB.Status.Projected,
                }
              : entry,
        );
        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      ForcedTransactionsDB.tableName,
      "Failed to resolve forced transactions for the current block window",
    ),
  );

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
const forcedVerdictForRejection = (
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

export const classifyForcedTransactions = <R>({
  entries,
  initialState,
  effectiveEndTime,
  consensusProfile,
  validation,
  resolveProgramMaterialSidecar,
}: {
  readonly entries: readonly ForcedTransactionsDB.Entry[];
  readonly initialState: Map<string, Buffer>;
  readonly effectiveEndTime: Date;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly validation: NonNullable<ProcessMpfsConfig["forcedValidation"]>;
  readonly resolveProgramMaterialSidecar: ForcedProgramMaterialSidecarResolver<R>;
}): Effect.Effect<readonly ClassifiedForcedTransaction[], DatabaseError, R> =>
  Effect.gen(function* () {
    const state = new Map(
      [...initialState.entries()].map(([key, value]) => [
        key,
        Buffer.from(value),
      ]),
    );
    const mutationTrie = yield* Effect.tryPromise({
      try: () =>
        Trie.fromList(
          [...state.entries()].map(([key, value]) =>
            ledgerOutputToInsertBatchOp({
              outRef: Buffer.from(key, "hex"),
              outputCbor: value,
            }),
          ),
          new Store(undefined),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Failed to construct the forced-transaction validation mutation trie",
          cause,
        }),
    });
    const classified: ClassifiedForcedTransaction[] = [];
    let arrivalSeq = 0n;
    for (const entry of entries) {
      const nativeTxCbor = entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
      const transactionCommitment =
        entry[ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT];
      const profileId =
        entry[ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID];
      if (
        profileId !== consensusProfile.profileId ||
        nativeTxCbor == null ||
        transactionCommitment == null
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "V1 block contains a forced transaction without exact V1 source material",
            cause: `tx_order_id=${entry[
              ForcedTransactionsDB.Columns.TX_ORDER_ID
            ].toString("hex")},profile=${profileId ?? "missing"}`,
          }),
        );
      }
      const txId = entry[ForcedTransactionsDB.Columns.TX_ID];
      const canonicalTx = yield* Effect.try({
        try: () => decodeMidgardNativeTxFullFromCanonicalCbor(nativeTxCbor),
        catch: (cause) =>
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction canonical bytes cannot be decoded while resolving CEK program material",
            cause,
          }),
      });
      // Malformed attached script/output bytes remain a deterministic Phase A
      // rejection. No material can be authenticated for a non-envelope.
      const attachedEnvelopes = (() => {
        try {
          return collectMidgardAttachedProgramEnvelopes(canonicalTx);
        } catch {
          return Object.freeze([]) as readonly MidgardCekProgramEnvelope[];
        }
      })();
      let programMaterialSidecarCbor =
        yield* resolveProgramMaterialSidecar(attachedEnvelopes);
      const phaseA = yield* runPhaseAValidation(
        [
          {
            txId,
            txCbor: nativeTxCbor,
            arrivalSeq,
            createdAt: entry[ForcedTransactionsDB.Columns.INCLUSION_TIME],
            programMaterialSidecarCbor,
          },
        ],
        {
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
          consensusProfile,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message: "Forced transaction Phase A evaluation failed",
              cause,
            }),
        ),
      );
      arrivalSeq += 1n;

      let verdict: SDK.OperatorVerdict;
      let ledgerOps: readonly MpfBatchOp[] = [];
      let rawLedgerOps: readonly MpfBatchOp[] = [];
      let transitionEffect = buildCanonicalTransitionEffect([]);
      let ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[] = [];
      let ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[] =
        [];
      let rejectionCode: RejectCode | null = null;
      if (phaseA.rejected.length > 0) {
        rejectionCode = phaseA.rejected[0]!.code;
        verdict = forcedVerdictForRejection(rejectionCode, "phaseA");
      } else {
        let acceptedCandidate = phaseA.accepted[0]!;
        if (
          acceptedCandidate.graph.referenceOutRefHexes.every((outRef) =>
            state.has(outRef),
          )
        ) {
          // A malformed referenced ledger output is classified by Phase B.
          const referencedEnvelopes = (() => {
            try {
              return collectMidgardReferencedProgramEnvelopes(
                canonicalTx,
                state,
              );
            } catch {
              return Object.freeze([]) as readonly MidgardCekProgramEnvelope[];
            }
          })();
          if (referencedEnvelopes.length > 0) {
            programMaterialSidecarCbor = yield* resolveProgramMaterialSidecar([
              ...attachedEnvelopes,
              ...referencedEnvelopes,
            ]);
            acceptedCandidate = {
              ...acceptedCandidate,
              submission: {
                ...acceptedCandidate.submission,
                programMaterialSidecarCbor,
              },
            };
          }
        }
        ledgerWitnessEntries = validationLedgerWitnesses(state, [
          ...acceptedCandidate.graph.spentOutRefHexes,
          ...acceptedCandidate.graph.referenceOutRefHexes,
        ]);
        const phaseB = yield* runPhaseBValidationWithPatch(
          [acceptedCandidate],
          state,
          {
            nowCardanoSlotNo: validation.slotForUnixTime(
              effectiveEndTime.getTime(),
            ),
            bucketConcurrency: validation.bucketConcurrency,
            enforceScriptBudget: true,
          },
        ).pipe(
          Effect.mapError(
            (cause) =>
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message: "Forced transaction Phase B evaluation failed",
                cause,
              }),
          ),
        );
        if (phaseB.rejected.length > 0) {
          rejectionCode = phaseB.rejected[0]!.code;
          verdict = forcedVerdictForRejection(rejectionCode, "phaseB");
        } else {
          verdict = "ForcedTxValid";
          transitionEffect = canonicalTransitionEffectFromStatePatch(
            phaseB.statePatch,
          );
          rawLedgerOps = transitionEffectToRawLedgerOps(transitionEffect);
          ledgerOps = transitionEffectToLedgerOps(transitionEffect);
          ledgerMutationSteps = yield* Effect.tryPromise({
            try: () => applyValidationLedgerMutations(mutationTrie, ledgerOps),
            catch: (cause) =>
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Failed to derive forced-transaction ledger mutation roots",
                cause,
              }),
          });
          applyUTxOStatePatch(state, phaseB.statePatch);
        }
      }
      const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValue({
        nativeTxCbor,
        verdict,
        consensusProfile,
      });
      // The row's tx_compact / transaction_commitment columns are the
      // SUBMITTED identity written at ingest (admission requires TxIsValid),
      // while `encoded` carries the operator-adjudicated leaf whose validity
      // scalar is stamped from the verdict. Identity is therefore checked
      // against a fresh derivation from the canonical bytes; `tx_id` hashes
      // the body only and is invariant under adjudication.
      const submittedSource = yield* Effect.try({
        try: () => deriveMidgardNativeTxProofSource(canonicalTx),
        catch: (cause) =>
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction canonical bytes cannot derive a submitted proof source",
            cause,
          }),
      });
      if (
        !encoded.txId.equals(txId) ||
        !computeMidgardNativeTxProofCommitment(submittedSource).equals(
          transactionCommitment,
        ) ||
        !submittedSource.compactCbor.equals(
          entry[ForcedTransactionsDB.Columns.TX_COMPACT],
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction persisted identity does not match its exact canonical V1 bytes",
            cause: `tx_order_id=${entry[
              ForcedTransactionsDB.Columns.TX_ORDER_ID
            ].toString("hex")}`,
          }),
        );
      }
      classified.push({
        entry: {
          ...entry,
          [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]:
            ForcedTransactionsDB.midgardTxValidityOfVerdict(verdict),
          [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
          [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
            programMaterialSidecarCbor,
          [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
            sha256(programMaterialSidecarCbor),
        },
        transitionEffect,
        ledgerOps,
        rawLedgerOps,
        ledgerWitnessEntries,
        ledgerMutationSteps,
        rejectionCode,
        programMaterialSidecarCbor,
      });
    }
    return classified;
  });
