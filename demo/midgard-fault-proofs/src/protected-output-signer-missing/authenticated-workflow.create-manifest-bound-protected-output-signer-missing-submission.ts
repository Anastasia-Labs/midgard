import {
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { buildCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import { buildSignerWitnessScan } from "../signer-witness-scan/submit-scan.js";
import { buildSignerWitnessTerminal } from "../signer-witness-scan/submit-terminal.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import {
  type ManifestBoundProtectedOutputSignerMissingConfig,
  type ProtectedOutputSignerMissingRuntimeLoader,
  required,
} from "./authenticated-workflow.create-protected-output-signer-missing-bound-config.js";
import { type ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";
import { buildProtectedOutputSignerMissingCancel } from "./submit-cancel.js";
import { buildProtectedOutputSignerMissingStep01Accepted } from "./submit-step-01-accepted.js";
import { buildProtectedOutputSignerMissingStep01Forced } from "./submit-step-01-forced.js";
import { buildProtectedOutputSignerMissingStep02 } from "./submit-step-02.js";
import { type ProtectedOutputSignerStage } from "./workflow.js";

export const createManifestBoundProtectedOutputSignerMissingSubmission = ({
  config,
  observe,
  resolveStage,
  stateQueueMutationLeaseCoordinator,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly config: ManifestBoundProtectedOutputSignerMissingConfig;
  readonly observe: (identity: string) => Promise<ProtectedOutputSignerStage>;
  readonly resolveStage: ProtectedOutputSignerMissingRuntimeLoader["resolveStage"];
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitScan"
      | "submitTerminal"
      | "removeDescendants",
    evidence: ProtectedOutputSignerMissingEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "protectedOutputSignerMissing evidence transaction id is not canonical",
      );
    const stage = await resolveStage({ action, evidence });
    if (action === "submitInit") {
      const result = await buildCommittedFieldShapeInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts as never,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      });
      return {
        transaction: result.transaction,
        stage: "step01" as const,
        txHash: result.transaction.txHash,
        outputReference: `${result.transaction.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await buildProtectedOutputSignerMissingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          evidence,
          threadUtxo: required(stage.threadUtxo, "step01 thread UTxO"),
          threadToken: required(stage.threadToken, "step01 thread token"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
        });
        return {
          transaction: result.transaction,
          stage: "step02" as const,
          txHash: result.transaction.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "protectedOutputSignerMissing evidence source kind is invalid",
        );
      const result = await buildProtectedOutputSignerMissingStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        evidence,
        forcedSource: {
          header: required(stage.forcedHeader, "forced header"),
          membership: required(stage.forcedMembership, "forced membership"),
        },
        referenceScriptUtxo: config.referenceScripts.step01,
      });
      return {
        transaction: result.transaction,
        stage: "step02" as const,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const result = await buildProtectedOutputSignerMissingStep02({
        ownOutputs,
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step02 thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        referenceScriptUtxo: config.referenceScripts.step02,
        fieldGrammar: config.fieldGrammar,
      });
      if (isPrerequisiteTurn(result)) return result;
      return {
        transaction: result.transaction,
        stage: result.stage,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitScan") {
      const result = await buildSignerWitnessScan({
        ownOutputs,
        lucid: config.lucid,
        family: "protected-output-signer-missing",
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "scan thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        referenceScriptUtxo: config.referenceScripts.step03,
      });
      if (isPrerequisiteTurn(result)) return result;
      return {
        transaction: result.transaction,
        stage: result.stage,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitTerminal") {
      const result = await buildSignerWitnessTerminal({
        lucid: config.lucid,
        family: "protected-output-signer-missing",
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "terminal thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      });
      return {
        transaction: result.transaction,
        stage: "proven" as const,
        txHash: result.transaction.txHash,
        outputReference: null,
      };
    }
    const result = await buildRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory: "protectedOutputSignerMissing",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "protectedOutputSignerMissing production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      validFrom: stage.validFrom,
      validTo: stage.validTo,
    });
    return {
      transaction: result.transaction,
      stateQueueMutationLease: result.stateQueueMutationLease,
      // A removal of a successor leaves the fraud proof in place.
      stage:
        result.kind === "remove-target"
          ? ("removed" as const)
          : ("proven" as const),
      txHash: result.transaction.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "scanning" | "terminal",
    evidence: ProtectedOutputSignerMissingEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index = { step01: 0, step02: 1, scanning: 2, terminal: 3 }[current];
    const result = await buildProtectedOutputSignerMissingCancel({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
        config.referenceScripts.step04,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
    });
    return {
      transaction: result.transaction,
      stage: "cancelled" as const,
      txHash: result.transaction.txHash,
      outputReference: null,
    };
  },
});
