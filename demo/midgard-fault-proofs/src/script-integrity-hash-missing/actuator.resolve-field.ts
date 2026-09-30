import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
} from "../workflow/transaction-boundary.js";
import { type AdmittedScriptIntegrityHashMissingArtifact } from "./artifact.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts.js";
import { ScriptIntegrityStepDatums } from "./schemas.js";
import { scriptIntegrityHashMissingUsesDirectRoute } from "./staged-plan.js";

export type ScriptIntegrityHashMissingWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundScriptIntegrityHashMissingActuatorConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ScriptIntegrityHashMissingContracts;
  references: ScriptIntegrityHashMissingWorkflowReferenceScripts;
  lease: StateQueueMutationLeaseCoordinator;
}>;

export const txWitnessSetHash = (
  compactCbor: string,
  source: "normal" | "forced",
): string =>
  Buffer.from(
    (source === "forced"
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact)(Buffer.from(compactCbor, "hex"))
      .transactionWitnessSetHash,
  ).toString("hex");

export const resolveField = async (
  config: BoundScriptIntegrityHashMissingActuatorConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined)
    throw new Error(
      "scriptIntegrityHashMissing field publications disappeared",
    );
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined)
    throw new Error("scriptIntegrityHashMissing certificate disappeared");
  return Object.freeze({
    carriage: Object.freeze([
      ...publications,
      ...(certificate === undefined ? [] : [certificate]),
    ]),
  });
};

export const threadDatum = async (
  config: BoundScriptIntegrityHashMissingActuatorConfig,
  outRef: string,
  ordinal: 4 | 5 | 6,
) => {
  const [txHash, output] = outRef.split("#");
  const [utxo] = await config.lucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(output) },
  ]);
  if (utxo?.datum === undefined || utxo.datum === null)
    throw new Error("scriptIntegrityHashMissing cursor datum disappeared");
  return Data.from(
    utxo.datum,
    ScriptIntegrityStepDatums[ordinal - 1] as never,
  ) as unknown as {
    fraud_prover: string;
    data: Record<string, unknown>;
  };
};

export const phaseHash = (
  state: { data: Record<string, unknown> },
  phase: "ScriptGrammar" | "ScriptScan" | "RedeemerGrammar",
): string => {
  const selected = (state.data.phase as Record<string, unknown> | undefined)?.[
    phase
  ] as Record<string, unknown> | undefined;
  if (typeof selected?.checkpoint_hash !== "string")
    throw new Error(`scriptIntegrityHashMissing expected ${phase} cursor`);
  return selected.checkpoint_hash;
};

export const captured = async (
  submit: (boundary: FraudProofPreSubmitBoundary) => Promise<void>,
) =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

export const direct = (
  admitted: Pick<
    AdmittedScriptIntegrityHashMissingArtifact,
    "evidence" | "staged"
  >,
): boolean =>
  scriptIntegrityHashMissingUsesDirectRoute({
    scriptItemCount: admitted.staged.scriptItems.length,
    redeemerItemCount: admitted.staged.redeemerItems.length,
    fieldBytes:
      Buffer.from(admitted.evidence.scriptWitnessesPreimageCbor, "hex").length +
      Buffer.from(admitted.evidence.redeemersPreimageCbor, "hex").length,
  });
