import {
  type ForcedInclusionTxV1,
  type Header,
  type OutputReference,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { PLUTUS_SCRIPT_REFUSED_RESULT_BASE } from "../script-decode/item-record.js";
import type { ScriptDecodeDeployment } from "../script-decode/submit.js";
import { type SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { WitnessScriptDecodingContracts } from "./contracts.js";
import {
  type WitnessScriptDecodingJournalEntry,
  WitnessScriptDecodingResultClasses,
} from "./witness-script-decoding.js";

export type WitnessScriptDecodingJournal = Readonly<{
  load: (
    identity: string,
  ) => Promise<readonly WitnessScriptDecodingJournalEntry[]>;
  append: (entry: WitnessScriptDecodingJournalEntry) => Promise<void>;
}>;

export const WITNESS_SCRIPT_DECODING_WORKFLOW =
  "midgard-witness-script-decoding-production-workflow-v1" as const;

export const WITNESS_SCRIPT_DECODING_VIOLATION_IDS = Object.freeze({
  HeaderMalformed: "witness-script-header-malformed",
  NativeMalformed: "witness-native-script-malformed",
  NodeLimit: "witness-native-script-node-limit",
  DepthLimit: "witness-native-script-depth-limit",
  PlutusScriptUndecodable: "witness-plutus-script-undecodable",
} as const);

export const witnessScriptDecodingViolationId = (
  resultClass: number,
): (typeof WITNESS_SCRIPT_DECODING_VIOLATION_IDS)[keyof typeof WITNESS_SCRIPT_DECODING_VIOLATION_IDS] => {
  if (resultClass === WitnessScriptDecodingResultClasses.HeaderMalformed)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.HeaderMalformed;
  if (resultClass === WitnessScriptDecodingResultClasses.NativeMalformed)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.NativeMalformed;
  if (resultClass === WitnessScriptDecodingResultClasses.NodeLimit)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.NodeLimit;
  if (resultClass === WitnessScriptDecodingResultClasses.DepthLimit)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.DepthLimit;
  if (resultClass > PLUTUS_SCRIPT_REFUSED_RESULT_BASE)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.PlutusScriptUndecodable;
  throw new Error(
    "witnessScriptDecoding result has no exact classifier violation ID",
  );
};

export const WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofWitnessScriptDecoding",
  step02: "fraudProofWitnessScriptDecodingStep02",
  step03: "fraudProofWitnessScriptDecodingStep03",
  step04: "fraudProofWitnessScriptDecodingStep04",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type WitnessScriptDecodingReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundWitnessScriptDecodingConfig = Readonly<{
  schemaVersion: typeof WITNESS_SCRIPT_DECODING_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  contracts: WitnessScriptDecodingContracts;
  referenceScripts: WitnessScriptDecodingReferenceScripts;
  /**
   * The script-decode deployment step 02 finds a Plutus item's record in;
   * without it a Plutus item cannot be proven.
   */
  scriptDecode?: ScriptDecodeDeployment;
}>;

export type LoadManifestBoundWitnessScriptDecodingConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: WitnessScriptDecodingReferenceScripts;
}>;

export const witnessScriptDecodingConfigFromBinding = (input: {
  binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WitnessScriptDecodingReferenceScripts;
  scriptDecode?: ScriptDecodeDeployment;
}): ManifestBoundWitnessScriptDecodingConfig => {
  const { binding, referenceScripts } = input;
  const chain = binding.resolvedContracts.contracts.witnessScriptDecoding;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 4) {
    throw new Error(
      "witnessScriptDecoding deployment changed its four-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "witnessScriptDecoding deployment omitted field-preimage certificate",
    );
  }
  return Object.freeze({
    schemaVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/witness_script_decoding/step_01.main.spend",
          "fraud_proofs/witness_script_decoding/step_02.main.spend",
          "fraud_proofs/witness_script_decoding/step_03.main.spend",
          "fraud_proofs/witness_script_decoding/step_04.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as WitnessScriptDecodingContracts["steps"],
      computationThread: binding.resolvedContracts.contracts.computationThread,
      fraudProof: binding.resolvedContracts.contracts.fraudProof,
      hubOraclePolicyId: binding.contractEntries.hubOracleMint!.scriptHash,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    referenceScripts,
    ...(input.scriptDecode === undefined
      ? {}
      : { scriptDecode: input.scriptDecode }),
  });
};

export type WitnessScriptDecodingAuthenticatedStage = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  threadUtxo?: UTxO;
  threadToken?: Readonly<{ unit: string; fraudulentHeaderHash: string }>;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
  nativeTxCompactCbor?: string;
  witnessSetCompactCbor?: string;
  publishedCarriageUtxos?: readonly UTxO[];
  certificateUtxo?: UTxO;
  validFrom?: bigint;
  validTo?: bigint;
}>;
