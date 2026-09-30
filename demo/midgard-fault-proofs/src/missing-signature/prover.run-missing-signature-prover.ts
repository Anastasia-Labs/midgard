import {
  FraudProofComputationThreadStepDatum,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
  missingSignatureThreadTokenAssetName,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";

import { outRefLabel } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import {
  assertMissingSignatureFindingProvable,
  type MissingSignatureFinding,
} from "./finding.js";
import {
  assertEvidenceCoherent,
  locateMissingSignatureThread,
  type MissingSignatureProofOutcome,
  type MissingSignatureProverDeps,
  type MissingSignatureProverEvent,
  type MissingSignatureSubjectEvidence,
  type MissingSignatureThreadPosition,
  requireDatum,
  toError,
} from "./prover.assert-evidence-coherent.js";
import { missingSignatureSubmitError } from "./submit-common.js";
import { submitMissingSignatureInit } from "./submit-missing-signature-init.js";
import { submitMissingSignatureStep01 } from "./submit-missing-signature-step-01.js";
import { submitMissingSignatureStep02 } from "./submit-missing-signature-step-02.js";
import { submitMissingSignatureStep03 } from "./submit-missing-signature-step-03.js";
import { submitMissingSignatureStep04 } from "./submit-missing-signature-step-04.js";

export const runMissingSignatureProver = async (
  finding: MissingSignatureFinding,
  deps: MissingSignatureProverDeps,
): Promise<MissingSignatureProofOutcome> => {
  const { lucid, contracts, policy, signer } = deps;
  const headerHash = finding.headerHash;
  const journal = async (
    event: Omit<MissingSignatureProverEvent, "headerHash">,
  ) => deps.journal({ ...event, headerHash });
  const refused = async (
    refusal: "classification" | "policy" | "duplicate" | "alreadyProven",
    reason: string,
  ): Promise<MissingSignatureProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `refused (${refusal}): ${reason}`,
    });
    return { kind: "refused", refusal, reason };
  };
  const stalled = async (
    reason: string,
    threadOutRef: string | null,
    cause: unknown,
  ): Promise<MissingSignatureProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `STALLED: ${reason}`,
      ...(threadOutRef === null ? {} : { threadOutRef }),
    });
    return { kind: "stalled", reason, threadOutRef, cause };
  };

  try {
    assertMissingSignatureFindingProvable(finding);
  } catch (cause) {
    return refused("classification", toError(cause).message);
  }

  const assetName = missingSignatureThreadTokenAssetName(
    deps.category.categoryId,
    headerHash,
  );
  const threadUnit = toUnit(contracts.computationThread.policyId, assetName);
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, assetName);
  const existingProofs = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    fraudProofUnit,
  );
  if (existingProofs.length > 0) {
    return refused(
      "alreadyProven",
      `fraud-proof token ${fraudProofUnit} already exists at ${outRefLabel(existingProofs[0]!)}.`,
    );
  }

  let position: MissingSignatureThreadPosition;
  try {
    position = await locateMissingSignatureThread({
      lucid,
      contracts,
      threadUnit,
    });
  } catch (cause) {
    return stalled(
      `thread discovery failed: ${toError(cause).message}`,
      null,
      cause,
    );
  }
  if (position.step !== "none") {
    try {
      const datum = Data.from(
        requireDatum(position.threadUtxo),
        FraudProofComputationThreadStepDatum,
      );
      if (datum.fraud_prover !== signer.paymentKeyHash) {
        return refused(
          "duplicate",
          `live thread at ${outRefLabel(position.threadUtxo)} names fraud prover ${datum.fraud_prover}, not this wallet.`,
        );
      }
    } catch (cause) {
      return stalled(
        `live thread datum is invalid: ${toError(cause).message}`,
        outRefLabel(position.threadUtxo),
        cause,
      );
    }
  }

  let txInclusion: SubmitStep01TxInclusion;
  let subject: MissingSignatureSubjectEvidence;
  try {
    [txInclusion, subject] = await Promise.all([
      deps.evidence.txInclusion(finding),
      deps.evidence.subjectTx(finding),
    ]);
    assertEvidenceCoherent({ finding, txInclusion, subject });
  } catch (cause) {
    return position.step === "none"
      ? refused("classification", toError(cause).message)
      : stalled(
          `evidence reconstruction failed on a live thread: ${toError(cause).message}`,
          outRefLabel(position.threadUtxo),
          cause,
        );
  }

  if (position.step === "none") {
    if (policy.minSettlementDepth > 0n) {
      if (deps.observations.settlementDepthOf === undefined) {
        return refused(
          "policy",
          "settlement-depth policy is enabled but no observer is configured.",
        );
      }
      const depth = await deps.observations.settlementDepthOf(
        finding.fraudulentBlockOutRef,
      );
      if (depth < policy.minSettlementDepth) {
        return refused(
          "policy",
          `faulted block depth ${depth.toString()} is below ${policy.minSettlementDepth.toString()}.`,
        );
      }
    }
    if (policy.maxThreadBudgetLovelace !== null) {
      const extraScans = Math.floor(
        Math.max(0, subject.addrTxWits.length - 1) /
          MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
      );
      const projected = BigInt(6 + extraScans) * policy.assumedFeePerTxLovelace;
      if (projected > policy.maxThreadBudgetLovelace) {
        return refused(
          "policy",
          `projected thread cost ${projected.toString()} lovelace exceeds cap ${policy.maxThreadBudgetLovelace.toString()}.`,
        );
      }
    }
  }

  type Cursor = "init" | "step01" | "step02" | "step03" | "step04";
  let cursor: Cursor = position.step === "none" ? "init" : position.step;
  let currentOutRef =
    position.step === "none" ? null : outRefLabel(position.threadUtxo);
  const txHashes: string[] = [];
  let step04Transactions = 0;
  const maximumStep04Transactions =
    Math.floor(
      Math.max(0, subject.addrTxWits.length - 1) /
        MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
    ) + 1;

  try {
    while (true) {
      switch (cursor) {
        case "init": {
          const result = await submitMissingSignatureInit({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            category: deps.category,
            catalogue: deps.catalogue,
            signer,
            fraudulentBlockOutRef: finding.fraudulentBlockOutRef,
            fraudulentHeaderHash: finding.headerHash,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "init",
            message: "computation thread initialized",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step01";
          break;
        }
        case "step01": {
          const result = await submitMissingSignatureStep01({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            stateQueueBlockOutRef: finding.fraudulentBlockOutRef,
            txInclusion,
            referenceScriptUtxo: deps.referenceScriptUtxos.step01,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step01",
            message: "transaction bound",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step02";
          break;
        }
        case "step02": {
          const result = await submitMissingSignatureStep02({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            requiredSignerHashes: subject.requiredSignerHashes,
            nativeTxCompactCbor: subject.nativeTxCompactCbor,
            badRequiredSignerHashIndex: finding.accusedRequiredSignerIndex,
            publishCarriage: deps.publishCarriage,
            certificateUtxo: deps.fieldCertificates?.step02,
            referenceScriptUtxo: deps.referenceScriptUtxos.step02,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step02",
            message: "required signer selected",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step03";
          break;
        }
        case "step03": {
          const result = await submitMissingSignatureStep03({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            missingRequiredSignerVkey: finding.resolvedVkey!,
            referenceScriptUtxo: deps.referenceScriptUtxos.step03,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step03",
            message: "verification-key preimage lifted",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = "step04";
          break;
        }
        case "step04": {
          step04Transactions += 1;
          if (step04Transactions > maximumStep04Transactions) {
            throw missingSignatureSubmitError(
              `step-04 exceeded its deterministic ${maximumStep04Transactions.toString()}-transaction scan schedule.`,
            );
          }
          const result = await submitMissingSignatureStep04({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: currentOutRef!,
            addrTxWits: subject.addrTxWits,
            nativeTxCompactCbor: subject.nativeTxCompactCbor,
            witnessSetCompact: subject.witnessSetCompact,
            publishCarriage: deps.publishCarriage,
            certificateUtxo: deps.fieldCertificates?.step04,
            referenceScriptUtxo: deps.referenceScriptUtxos.step04,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          if (result.kind === "advanced") {
            currentOutRef = result.nextThreadOutRef;
            await journal({
              phase: "step04",
              message: `absence scan advanced to witness ${result.nextItemIndex.toString()}`,
              txHash: result.txHash,
              threadOutRef: currentOutRef,
            });
            break;
          }
          await journal({
            phase: "step04",
            message: "fraud-proof token minted",
            txHash: result.txHash,
            threadOutRef: result.fraudProofOutRef,
          });
          await journal({ phase: "outcome", message: "proven" });
          return {
            kind: "proven",
            fraudProofUnit: result.fraudProofUnit,
            fraudProofOutRef: result.fraudProofOutRef,
            txHashes,
          };
        }
      }
    }
  } catch (cause) {
    return stalled(
      `unexpected abort at ${cursor}: ${toError(cause).message}`,
      currentOutRef,
      cause,
    );
  }
};
