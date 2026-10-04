import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { type MidgardCekProgramEnvelope } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { type MidgardForcedTxAdmissionStopped } from "@al-ft/midgard-core/consensus-validation";
import {
  collectMidgardAttachedProgramEnvelopes,
  collectMidgardEventProgramEnvelopes,
} from "@al-ft/midgard-core/script-proof";
import { type ForcedRejectionStopped } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  buildCanonicalTransitionEffect,
  canonicalTransitionEffectFromStatePatch,
  type LocalScriptEvaluation,
  midgardOutRefToCborHex,
  type RejectCode,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { projectMidgardMalformedNativeWitnessEnvelopeV1 } from "@al-ft/midgard-validation/ledger-tx/codec";
import { Effect } from "effect";

import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import { DatabaseError } from "../database/utils/common.js";
import { sha256 } from "../sha256.js";
import {
  acceptedTransactionLedgerWitnesses,
  applyValidationLedgerMutations,
  type ClassifiedForcedTransaction,
  type ForcedProgramMaterialSidecarResolver,
  forcedRejectionVerdict,
  rejectedForcedTransactionLedgerWitnesses,
} from "./event-window.forced-verdict-for-rejection.js";
import {
  ledgerOutputToInsertBatchOp,
  transitionEffectToLedgerOps,
  transitionEffectToRawLedgerOps,
} from "./ledger-delta.js";
import { type ProcessMpfsConfig } from "./process-config.js";
import { type MpfBatchOp } from "./types.js";

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
}): Effect.Effect<
  readonly ClassifiedForcedTransaction[],
  DatabaseError | ForcedRejectionStopped | MidgardForcedTxAdmissionStopped,
  R
> =>
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
        try: () => decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor),
        catch: (cause) =>
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction canonical bytes cannot be decoded while resolving CEK program material",
            cause,
          }),
      });
      // The program material of a forced transaction is the shared per-event
      // set: its attached programs plus the script_refs of its reference
      // inputs present in the state immediately before it, whatever its
      // verdict. Malformed attached script/output bytes remain a deterministic
      // Phase A rejection: no material can be authenticated for a
      // non-envelope.
      const eventEnvelopes = yield* (() => {
        try {
          collectMidgardAttachedProgramEnvelopes(canonicalTx, "forced");
        } catch {
          return Effect.succeed(
            Object.freeze([]) as readonly MidgardCekProgramEnvelope[],
          );
        }
        return Effect.try({
          try: () =>
            collectMidgardEventProgramEnvelopes(
              canonicalTx,
              (outRefHex) => state.get(outRefHex),
              "forced",
            ),
          catch: (cause) =>
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message:
                "A ledger output referenced by a forced transaction has a malformed script_ref",
              cause,
            }),
        });
      })();
      const programMaterialSidecarCbor =
        yield* resolveProgramMaterialSidecar(eventEnvelopes);
      const phaseA = yield* runPhaseAValidation(
        [
          {
            txId,
            txCbor: nativeTxCbor,
            sourceKind: "forced",
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

      const scriptEvaluations: LocalScriptEvaluation[] = [];
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
            ledgerWitnessEntries = rejectedForcedTransactionLedgerWitnesses(
              state,
              [
                ...raw.projection.ledgerTx.spendInputs,
                ...raw.projection.ledgerTx.referenceInputs,
              ].map(midgardOutRefToCborHex),
            );
          }
        }
      } else {
        const acceptedCandidate = phaseA.accepted[0]!;
        const phaseB = yield* runPhaseBValidationWithPatch(
          [acceptedCandidate],
          state,
          {
            nowCardanoSlotNo: validation.slotForUnixTime(
              effectiveEndTime.getTime(),
            ),
            bucketConcurrency: validation.bucketConcurrency,
            enforceScriptBudget: true,
            maxScriptExecutionSteps:
              consensusProfile.limits.maxValidationMachineStepCount,
            onScriptEvaluated: (_txId, evaluation) =>
              scriptEvaluations.push(evaluation),
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
        const inputOutRefHexes = [
          ...acceptedCandidate.graph.spentOutRefHexes,
          ...acceptedCandidate.graph.referenceOutRefHexes,
        ];
        if (phaseB.rejected.length > 0) {
          rejectionCode = phaseB.rejected[0]!.code;
          verdict = yield* forcedRejectionVerdict(phaseB.rejected[0]!);
          ledgerWitnessEntries = rejectedForcedTransactionLedgerWitnesses(
            state,
            inputOutRefHexes,
          );
        } else {
          verdict = "ForcedTxValid";
          ledgerWitnessEntries = yield* acceptedTransactionLedgerWitnesses(
            state,
            {
              table: ForcedTransactionsDB.tableName,
              txIdHex: txId.toString("hex"),
            },
            inputOutRefHexes,
          );
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
      const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        verdict,
        consensusProfile,
      });
      // Classification must preserve the submission identity authenticated at ingest.
      const submittedSource = yield* Effect.try({
        try: () => deriveMidgardForcedTxProofSource(canonicalTx),
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
        scriptEvaluations,
        programMaterialSidecarCbor,
      });
    }
    return classified;
  });
