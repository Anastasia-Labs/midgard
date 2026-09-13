import type {
  ForcedInclusionTxV1,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ReceivePurposeLanguageContracts } from "./contracts.js";
import type { ReceivePurposeLanguageEvidence } from "./family.js";
import { submitReceivePurposeLanguageCancel } from "./submit-cancel.js";
import { submitReceivePurposeLanguageInit } from "./submit-init.js";
import {
  submitReceivePurposeLanguageStep01Accepted,
  submitReceivePurposeLanguageStep01Forced,
} from "./submit-step-01.js";
import type { ReceivePurposeLanguageAuthentication } from "./submit-step-02.js";
import { submitReceivePurposeLanguageStep02 } from "./submit-step-02.js";
import { submitReceivePurposeLanguageStep03 } from "./submit-step-03.js";

export const RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS = Object.freeze([
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
export type ReceivePurposeLanguageReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;
export type ReceivePurposeLanguageConfig = Readonly<{
  blueprint: unknown;
  network: Network;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ReceivePurposeLanguageContracts;
  categoryId: string;
  catalogue: Parameters<
    typeof submitReceivePurposeLanguageInit
  >[0]["catalogue"];
  category: Parameters<typeof submitReceivePurposeLanguageInit>[0]["category"];
  referenceScripts: ReceivePurposeLanguageReferences;
}>;
export type ReceivePurposeLanguageDirectArtifact = Readonly<{
  evidence: ReceivePurposeLanguageEvidence;
  authentication: ReceivePurposeLanguageAuthentication;
  header: Header;
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
export type ReceivePurposeLanguageAction =
  | Readonly<{
      stage: "init";
      stateQueueBlockOutRef: string;
      fraudulentHeaderHash: string;
    }>
  | Readonly<{
      stage: "step01";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectArtifact;
    }>
  | Readonly<{
      stage: "step02";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectArtifact;
    }>
  | Readonly<{
      stage: "step03";
      threadOutRef: string;
      artifact: ReceivePurposeLanguageDirectArtifact;
    }>
  | Readonly<{ stage: "cancel"; threadOutRef: string }>;

/**
 * Package-owned production actuator. The config contains no submission or
 * verdict callback: every transaction is built, locally evaluated, signed and
 * submitted by the family-owned Lucid lifecycle.
 */
export const createReceivePurposeLanguageWorkflow = (
  config: ReceivePurposeLanguageConfig,
) =>
  Object.freeze({
    run: async (action: ReceivePurposeLanguageAction) => {
      const common = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.categoryId,
        signer: config.signer,
      } as const;
      if (action.stage === "init")
        return await submitReceivePurposeLanguageInit({
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
          ? await submitReceivePurposeLanguageStep01Accepted({
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
          : await submitReceivePurposeLanguageStep01Forced({
              ...common,
              threadOutRef: action.threadOutRef,
              header: action.artifact.header,
              membership: action.artifact.source.membership,
              executionIndex: index,
              referenceScriptUtxo: config.referenceScripts.steps[0],
            });
      }
      if (action.stage === "step02")
        return await submitReceivePurposeLanguageStep02({
          ...common,
          threadOutRef: action.threadOutRef,
          evidence: action.artifact.evidence,
          authentication: action.artifact.authentication,
          referenceScriptUtxo: config.referenceScripts.steps[1],
        });
      if (action.stage === "step03")
        return await submitReceivePurposeLanguageStep03({
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
      return await submitReceivePurposeLanguageCancel({
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
