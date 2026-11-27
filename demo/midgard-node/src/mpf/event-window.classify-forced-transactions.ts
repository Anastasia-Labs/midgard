import {
  computeMidgardForcedTxProofCommitment,
  deriveMidgardForcedTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec";
import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import {
  type Ledger,
  ledgerTrie,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { ForcedRejectionStopped } from "@al-ft/midgard-fault-proofs/workflow/forced-rejection-reason";
import type * as SDK from "@al-ft/midgard-sdk/core";
import {
  applyValidationLedgerMutations,
  buildCanonicalTransitionEffect,
  canonicalTransitionEffectFromStatePatch,
  midgardOutRefToCborHex,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  transitionEffectToLedgerOps,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { projectMidgardMalformedNativeWitnessEnvelopeV1 } from "@al-ft/midgard-validation/ledger-tx/codec";
import type { RejectCode } from "@al-ft/midgard-validation/types";
import { Effect } from "effect";

import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  acceptedTransactionLedgerWitnesses,
  type ClassifiedForcedTransaction,
  forcedRejectionVerdict,
  rejectedForcedTransactionLedgerWitnesses,
} from "./event-window.forced-verdict-for-rejection.js";
import { transitionEffectToRawLedgerOps } from "./ledger-delta.js";
import { ledgerPointState } from "./ledger-point-state.js";
import { type ProcessMpfsConfig } from "./process-config.js";
import { type MpfBatchOp } from "./types.js";

/**
 * Classifies a block's forced transactions in order over `ledger`, the
 * block's ledger before them (its base after the withdrawal phase). Each
 * transaction's inputs are read by key from that ledger as the earlier
 * forced transactions leave it, and an accepted one's ledger mutation steps
 * come from a trie over the ledger's nodes, so nothing reads the whole
 * ledger.
 */
export const classifyForcedTransactions = ({
  entries,
  ledger,
  blockTime,
  consensusProfile,
  validation,
}: {
  readonly entries: readonly ForcedTransactionsDB.Entry[];
  readonly ledger: Ledger;
  readonly blockTime: Date;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly validation: NonNullable<ProcessMpfsConfig["forcedValidation"]>;
}): Effect.Effect<
  readonly ClassifiedForcedTransaction[],
  DatabaseError | ForcedRejectionStopped
> =>
  Effect.gen(function* () {
    const state = ledgerPointState(ledger, ForcedTransactionsDB.tableName);
    const mutationTrie = yield* Effect.tryPromise({
      try: () => ledgerTrie(ledger),
      catch: (cause) =>
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Failed to open the forced-transaction validation mutation trie",
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
      const phaseA = yield* runPhaseAValidation(
        [
          {
            txId,
            txCbor: nativeTxCbor,
            sourceKind: "forced",
            arrivalSeq,
            createdAt: entry[ForcedTransactionsDB.Columns.INCLUSION_TIME],
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
        verdict = yield* forcedRejectionVerdict(phaseA.rejected[0]!);
        if (
          phaseA.rejected[0]!.subject?.arm === "WitnessNativeScriptMalformed"
        ) {
          // The machine consumes a malformed native payload after resolving
          // inputs. Retain the authentic pre-state members needed to reach it;
          // an earlier machine fault still stops trace construction.
          const raw = projectMidgardMalformedNativeWitnessEnvelopeV1(
            nativeTxCbor,
            "forced",
          );
          if (raw !== null) {
            const inputOutRefHexes = [
              ...raw.projection.ledgerTx.spendInputs,
              ...raw.projection.ledgerTx.referenceInputs,
            ].map(midgardOutRefToCborHex);
            ledgerWitnessEntries = rejectedForcedTransactionLedgerWitnesses(
              yield* state.read(inputOutRefHexes),
              inputOutRefHexes,
            );
          }
        }
      } else {
        const acceptedCandidate = phaseA.accepted[0]!;
        const inputOutRefHexes = [
          ...acceptedCandidate.graph.spentOutRefHexes,
          ...acceptedCandidate.graph.referenceOutRefHexes,
        ];
        // Phase B resolves exactly the transaction's spent and reference
        // inputs.
        const preState = yield* state.read(inputOutRefHexes);
        const phaseB = yield* runPhaseBValidationWithPatch(
          [acceptedCandidate],
          preState,
          {
            nowCardanoSlotNo: validation.slotForUnixTime(blockTime.getTime()),
            bucketConcurrency: validation.bucketConcurrency,
            l1SlotConfig: validation.l1SlotConfig,
            enforceScriptBudget: true,
            maxScriptExecutionSteps:
              consensusProfile.limits.maxValidationMachineStepCount,
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
          verdict = yield* forcedRejectionVerdict(phaseB.rejected[0]!);
          ledgerWitnessEntries = rejectedForcedTransactionLedgerWitnesses(
            preState,
            inputOutRefHexes,
          );
        } else {
          verdict = "ForcedTxValid";
          ledgerWitnessEntries = yield* acceptedTransactionLedgerWitnesses(
            preState,
            {
              table: ForcedTransactionsDB.tableName,
              txIdHex: txId.toString("hex"),
            },
            inputOutRefHexes,
          );
          transitionEffect = yield* Effect.try({
            try: () =>
              canonicalTransitionEffectFromStatePatch(phaseB.statePatch),
            catch: (cause) =>
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Forced transaction state patch has no canonical transition effect",
                cause,
              }),
          });
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
          state.apply(phaseB.statePatch);
        }
      }
      const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        verdict,
        consensusProfile,
      });
      // Classification must preserve the submission identity authenticated at ingest.
      const submittedSource = yield* Effect.try({
        try: () =>
          deriveMidgardForcedTxFaultEvidenceMaterial(nativeTxCbor).proofSource,
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
        !computeMidgardForcedTxProofCommitment(submittedSource).equals(
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
          [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
        },
        transitionEffect,
        ledgerOps,
        rawLedgerOps,
        ledgerWitnessEntries,
        ledgerMutationSteps,
        rejectionCode,
        rejectionReason:
          verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
      });
    }
    return classified;
  });
