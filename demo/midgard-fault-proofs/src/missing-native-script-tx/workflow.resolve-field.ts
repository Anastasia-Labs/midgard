import {
  encodeMidgardTxInputCanonical,
  MIDGARD_FIELD_INDEX,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { FraudProofRawL1Point } from "../workflow/raw-l1-snapshot.js";
import { type AdmittedMissingNativeScriptTxArtifact } from "./artifact.js";
import type { MissingNativeScriptTxContracts } from "./contracts.js";
import { type HistoricalNativeScriptSourceRoster } from "./historical-script.js";

export type MissingNativeScriptTxWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptTx">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptTxContracts;
  references: MissingNativeScriptTxWorkflowReferenceScripts;
  historicalCorpus: () => HistoricalNativeScriptCorpus;
  historicalSourceRoster: HistoricalNativeScriptSourceRoster;
  historicalThroughPoint: () => FraudProofRawL1Point;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export const direct = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
): boolean =>
  admitted.evidence.badTxScriptWitnessItemCbors.length <=
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT;

export const spendFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: admitted.evidence.badTxInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.evidence.badTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.badTxSpendInputs.map(
      encodeMidgardTxInputCanonical,
    ),
    owner,
    publish: true,
    label: "missing-native-script-tx field 0",
  });

export const outputFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: admitted.evidence.producingTxInclusion.nativeTxId,
    nativeTxCompactCbor:
      admitted.evidence.producingTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.producingOutputItemCbors,
    owner,
    publish: true,
    label: "missing-native-script-tx field 1",
  });

export const scriptFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.evidence.badTxInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.evidence.badTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.badTxScriptWitnessItemCbors,
    owner,
    publish: true,
    witnessSet: admitted.evidence.badTxWitnessSet,
    anchorWitnessSetHash:
      admitted.evidence.badTxInclusion.nativeTx.witness_set_hash,
    label: "missing-native-script-tx field 6",
  });

export const resolveField = async ({
  config,
  planned,
}: {
  readonly config: BoundConfig;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("missing-native-script-tx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("missing-native-script-tx certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

export const expectedScriptHashFromDetection = (
  detectionId: string,
): string => {
  const fields = detectionId.split(":");
  const value = fields.length === 8 ? fields[7] : undefined;
  if (value === undefined || !/^[0-9a-f]{56}$/u.test(value)) {
    throw new Error(
      "missing-native-script-tx detection omitted its exact script hash",
    );
  }
  return value;
};
