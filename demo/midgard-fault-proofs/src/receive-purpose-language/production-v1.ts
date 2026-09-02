import type {
  ForcedInclusionTxV1,
  HeaderV1,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { ReceivePurposeLanguageContractsV1 } from "./contracts-v1.js";
import type { ReceivePurposeLanguageEvidenceV1 } from "./family-v1.js";
import { submitReceivePurposeLanguageCancelV1 } from "./submit-cancel-v1.js";
import { submitReceivePurposeLanguageInitV1 } from "./submit-init-v1.js";
import {
  submitReceivePurposeLanguageStep01AcceptedV1,
  submitReceivePurposeLanguageStep01ForcedV1,
} from "./submit-step-01-v1.js";
import type { ReceivePurposeLanguageAuthenticationV1 } from "./submit-step-02-v1.js";
import { submitReceivePurposeLanguageStep02V1 } from "./submit-step-02-v1.js";
import { submitReceivePurposeLanguageStep03V1 } from "./submit-step-03-v1.js";

export const RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS_V1 = Object.freeze([
  "blueprint",
  "network",
  "lucid",
  "signer",
  "contracts",
  "categoryId",
  "catalogue",
  "category",
  "referenceScripts",
] as const);
export type ReceivePurposeLanguageProductionReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;
export type ReceivePurposeLanguageProductionConfigV1 = Readonly<{
  blueprint: unknown;
  network: Network;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ReceivePurposeLanguageContractsV1;
  categoryId: string;
  catalogue: Parameters<
    typeof submitReceivePurposeLanguageInitV1
  >[0]["catalogue"];
  category: Parameters<
    typeof submitReceivePurposeLanguageInitV1
  >[0]["category"];
  referenceScripts: ReceivePurposeLanguageProductionReferencesV1;
}>;
export type ReceivePurposeLanguageDirectProductionArtifactV1 = Readonly<{
  evidence: ReceivePurposeLanguageEvidenceV1;
  authentication: ReceivePurposeLanguageAuthenticationV1;
  header: HeaderV1;
  source:
    | Readonly<{
        kind: "accepted";
        stateQueueBlockOutRef: string;
        inclusion: SubmitStep01TxInclusion;
      }>
    | Readonly<{
        kind: "forced";
        membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
      }>;
}>;
export type ReceivePurposeLanguageProductionActionV1 =
  | Readonly<{
      stage: "init";
      stateQueueBlockOutRef: string;
      fraudulentHeaderHash: string;
    }>
  | Readonly<{
      stage: "step01";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectProductionArtifactV1;
    }>
  | Readonly<{
      stage: "step02";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectProductionArtifactV1;
    }>
  | Readonly<{
      stage: "step03";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectProductionArtifactV1;
    }>
  | Readonly<{ stage: "cancel"; threadOutRef: string }>;

/**
 * Package-owned production actuator. The config contains no submission or
 * verdict callback: every transaction is built, locally evaluated, signed and
 * submitted by the family-owned Lucid lifecycle.
 */
export const createReceivePurposeLanguageProductionWorkflowV1 = (
  config: ReceivePurposeLanguageProductionConfigV1,
) =>
  Object.freeze({
    run: async (action: ReceivePurposeLanguageProductionActionV1) => {
      const common = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.categoryId,
        signer: config.signer,
      } as const;
      if (action.stage === "init")
        return await submitReceivePurposeLanguageInitV1({
          lucid: config.lucid,
          blueprint: config.blueprint,
          network: config.network,
          signer: config.signer,
          contracts: config.contracts,
          catalogue: config.catalogue,
          category: config.category,
          fraudulentBlockOutRef: action.stateQueueBlockOutRef,
          fraudulentHeaderHash: action.fraudulentHeaderHash,
          witnessReferenceScripts: config.referenceScripts.witnesses,
        });
      if (action.stage === "step01") {
        const index = BigInt(action.artifact.evidence.finding.executionIndex);
        return action.artifact.source.kind === "accepted"
          ? await submitReceivePurposeLanguageStep01AcceptedV1({
              ...common,
              blueprint: config.blueprint,
              network: config.network,
              threadOutRef: action.threadOutRef,
              stateQueueBlockOutRef:
                action.artifact.source.stateQueueBlockOutRef,
              txInclusion: action.artifact.source.inclusion,
              header: action.artifact.header,
              executionIndex: index,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
            })
          : await submitReceivePurposeLanguageStep01ForcedV1({
              ...common,
              threadOutRef: action.threadOutRef,
              header: action.artifact.header,
              membership: action.artifact.source.membership,
              executionIndex: index,
              referenceScriptUtxo: config.referenceScripts.steps[0],
            });
      }
      if (action.stage === "step02")
        return await submitReceivePurposeLanguageStep02V1({
          ...common,
          threadOutRef: action.threadOutRef,
          evidence: action.artifact.evidence,
          authentication: action.artifact.authentication,
          referenceScriptUtxo: config.referenceScripts.steps[1],
        });
      if (action.stage === "step03")
        return await submitReceivePurposeLanguageStep03V1({
          ...common,
          threadOutRef: action.threadOutRef,
          evidence: action.artifact.evidence,
          referenceScriptUtxo: config.referenceScripts.steps[2],
          witnessReferenceScripts: config.referenceScripts.witnesses,
        });
      const thread = await config.lucid.utxosByOutRef([
        {
          txHash: action.threadOutRef.slice(0, 64),
          outputIndex: Number(action.threadOutRef.slice(65)),
        },
      ]);
      const stepIndex = config.contracts.steps.findIndex(
        (step) => step.spendingScriptAddress === thread[0]?.address,
      );
      if (stepIndex < 0)
        throw new Error(
          "receivePurposeLanguage cancel thread is not at a family step",
        );
      return await submitReceivePurposeLanguageCancelV1({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.categoryId,
        signer: config.signer,
        threadOutRef: action.threadOutRef,
        referenceScriptUtxo: config.referenceScripts.steps[stepIndex]!,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      });
    },
  });
