import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type FetchLike,
  type ObservedL1TransactionAtPoint,
} from "../l1-kupmios.js";
import {
  decodeKupoCorrectionLockMatch,
  fetchKupoResolvedOutput,
  fraudProofAssetNameFromResolvedMatch,
  type HistoricalCorrectionLockOutput,
} from "./state-queue-correction-observer.decode-kupo-correction-lock-match.js";

export const deriveCorrectionLockWitnessFromRaw = async ({
  transaction,
  transactionOutputs,
  stateQueuePolicyId,
  correctionLockAddress,
  hubOraclePolicyId,
  fraudProofAddress,
  fraudProofPolicyId,
  kupoUrl,
  fetchImpl,
}: {
  readonly transaction: ObservedL1TransactionAtPoint;
  readonly transactionOutputs: readonly HistoricalCorrectionLockOutput[];
  readonly stateQueuePolicyId: string;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly kupoUrl: string;
  readonly fetchImpl: FetchLike;
}): Promise<SDK.StateQueueCorrectionLockWitness> => {
  const spentReferences = transaction.spentInputs ?? [];
  const spentResolved = await Promise.all(
    spentReferences.map(async (reference) => ({
      reference,
      match: await fetchKupoResolvedOutput({
        kupoUrl,
        reference,
        fetchImpl,
      }),
    })),
  );
  const referenceResolved = await Promise.all(
    transaction.referenceInputs.map(async (reference) => ({
      reference,
      match: await fetchKupoResolvedOutput({
        kupoUrl,
        reference,
        fetchImpl,
      }),
    })),
  );
  const locksIn = spentResolved.flatMap(({ reference, match }) => {
    const lock = decodeKupoCorrectionLockMatch({
      candidate: match,
      expectedTransactionHash: reference.txHash,
      expectedOutputIndex: reference.outputIndex,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
  const locksReferenced = referenceResolved.flatMap(({ reference, match }) => {
    const lock = decodeKupoCorrectionLockMatch({
      candidate: match,
      expectedTransactionHash: reference.txHash,
      expectedOutputIndex: reference.outputIndex,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
  const stateQueuePolicyIndex =
    transaction.mintPolicyIds.indexOf(stateQueuePolicyId);
  const mintRedeemers = transaction.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === stateQueuePolicyIndex,
  );
  if (stateQueuePolicyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 0
    ) {
      throw new Error(
        "Non-mint state-queue transition unexpectedly mutates or references CorrectionLock",
      );
    }
    return { kind: "none" };
  }
  if (mintRedeemers.length !== 1) {
    throw new Error(
      "State-queue transaction has no unique canonical mint redeemer",
    );
  }
  let decoded: SDK.StateQueueRedeemer;
  try {
    decoded = Data.from(mintRedeemers[0]!.redeemer, SDK.StateQueueRedeemer);
  } catch (cause) {
    throw new Error("State-queue mint redeemer is not canonical data", {
      cause,
    });
  }
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 1 ||
      transactionOutputs[0]!.datum !== "Idle"
    ) {
      throw new Error("State-queue Init has invalid CorrectionLock topology");
    }
    return {
      kind: "genesis",
      producedOutRef: transactionOutputs[0]!.outRef,
      nextDatum: transactionOutputs[0]!.datum,
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 0
    ) {
      throw new Error("State-queue Deinit has invalid CorrectionLock topology");
    }
    return {
      kind: "deinit",
      consumedOutRef: locksIn[0]!.outRef,
      previousDatum: locksIn[0]!.datum,
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("CommitBlockHeader" in decoded || "MergeToConfirmedStateV1" in decoded)
  ) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 1 ||
      locksReferenced[0]!.datum !== "Idle" ||
      transactionOutputs.length !== 0
    ) {
      throw new Error(
        "State-queue append/merge has invalid CorrectionLock topology",
      );
    }
    return {
      kind: "idle_reference",
      referenceOutRef: locksReferenced[0]!.outRef,
      datum: locksReferenced[0]!.datum,
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveUnavailableBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      transactionOutputs.length !== 1 ||
      locksReferenced.length !== 0
    ) {
      throw new Error("Correction has invalid CorrectionLock topology");
    }
    const targetHeaderHash =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
        : "RemoveUnavailableBlockAfterTimeout" in decoded
          ? decoded.RemoveUnavailableBlockAfterTimeout.unavailable_header_hash
          : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const correctionIdentity: SDK.CorrectionIdentity =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? "AttestationTimeout"
        : "RemoveUnavailableBlockAfterTimeout" in decoded
          ? {
              AvailabilityChallenge: {
                challenge_asset_name:
                  decoded.RemoveUnavailableBlockAfterTimeout
                    .challenge_asset_name,
              },
            }
          : (() => {
              const proofIndex = Number(
                decoded.RemoveFraudulentBlockHeader.fraud_proof_ref_input_index,
              );
              const proof = referenceResolved[proofIndex];
              if (proof === undefined) {
                throw new Error(
                  "Fraud correction proof reference index is out of bounds",
                );
              }
              const assetName = fraudProofAssetNameFromResolvedMatch({
                candidate: proof.match,
                fraudProofAddress,
                fraudProofPolicyId,
                targetHeaderHash,
              });
              if (assetName === null) {
                throw new Error(
                  "Fraud correction proof reference is not the exact permanent proof identity",
                );
              }
              return {
                FraudProof: { fraud_proof_asset_name: assetName },
              };
            })();
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: transactionOutputs[0]!.outRef,
      targetHeaderHash,
      correctionIdentity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: transactionOutputs[0]!.datum,
    };
  }
  throw new Error("State-queue mint redeemer has no CorrectionLock topology");
};
