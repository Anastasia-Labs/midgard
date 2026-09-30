import {
  CEK_MATERIAL_TASK_YIELD_ROLES,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
} from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../../linear-fault-cancel.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import { requireComputationThreadToken } from "../../step-support.js";
import { type FaultProofWitnessReferenceScripts } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { submitCekMaterialTraversal } from ".././cek-material-traversal.js";
import { requireValidationDisputeReferenceScript } from "./reference-scripts.js";
import {
  refreshExpiredValidationDisputeValidityRange,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const cancelValidationCekMaterialTraversal = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const {
    deploymentInfo: deployed,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const stage = contracts.validationTraceDispute.cekMaterialTraversal;
  const entry = deployed.validationTraceDisputeCekMaterialTraversal;
  if (entry?.refScriptUTxO == null)
    throw new Error("Missing CEK material traversal publication");
  const reference = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: "CEK material traversal",
  });
  requireValidationDisputeReferenceScript({
    utxo: reference,
    deployedScriptHash: entry.scriptHash,
    expectedScriptHash: stage.spendingScriptHash,
    authPolicyId: referenceScriptAuthPolicyId,
    role: "V1 validation-trace CEK material traversal",
  });
  return await submitLinearFaultCancel({
    lucid,
    family: "validation-trace CEK material traversal",
    steps: [stage],
    computationThread: contracts.computationThread,
    categoryId: validationTraceDisputeCategory.categoryId,
    signer,
    threadOutRef,
    referenceScriptUtxo: reference,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

/** Resume only an authenticated live material checkpoint from retained canonical DA. */
export const resumeValidationCekMaterialTraversal = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  material,
  maxTransactions,
  validityRange,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly material: {
    readonly envelopeCborHex: string;
    readonly programMaterialSidecarCborHex: string;
  };
  readonly maxTransactions?: number;
  readonly validityRange?: ValidationDisputeValidityRange;
}) => {
  if (
    ![material.envelopeCborHex, material.programMaterialSidecarCborHex].every(
      (hex) => /^(?:[0-9a-f]{2})+$/u.test(hex),
    )
  )
    throw new Error(
      "CEK retained material must be non-empty lowercase hex bytes",
    );
  const {
    deploymentInfo: deployed,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "CEK material checkpoint"),
    label: "CEK material checkpoint",
  });
  if (
    threadUtxo.address !==
    contracts.validationTraceDispute.cekMaterialTraversal.spendingScriptAddress
  )
    throw new Error(
      "CEK checkpoint is not at the deployed traversal validator",
    );
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const specs = [
    {
      deployment: "validationTraceDisputeCekMaterialTraversal" as const,
      role: "V1 validation-trace CEK material traversal" as const,
      hash: contracts.validationTraceDispute.cekMaterialTraversal
        .spendingScriptHash,
    },
    ...CEK_MATERIAL_TASK_YIELD_ROLES.map((spec) => ({
      deployment: spec.deployment,
      role: spec.role,
      hash: contracts.validationTraceDispute.yields[spec.contract]
        .withdrawalScriptHash,
    })),
  ];
  const references = await Promise.all(
    specs.map(async (spec) => {
      const entry = deployed[spec.deployment];
      if (entry?.refScriptUTxO == null)
        throw new Error(`Missing ${spec.role} publication`);
      const utxo = await fetchUtxoByOutRef({
        lucid,
        outRef: entry.refScriptUTxO,
        label: spec.role,
      });
      requireValidationDisputeReferenceScript({
        utxo,
        deployedScriptHash: entry.scriptHash,
        expectedScriptHash: spec.hash,
        authPolicyId: referenceScriptAuthPolicyId,
        role: spec.role,
      });
      return utxo;
    }),
  );
  const range = requireValidityRange(
    validityRange ??
      validationDisputeValidityRange(lucid.slotToUnixTime(lucid.currentSlot())),
  );
  return await submitCekMaterialTraversal({
    lucid,
    network,
    signer,
    contracts: contracts.validationTraceDispute,
    threadUtxo,
    threadUnit: token.unit,
    material: {
      envelopeCbor: Buffer.from(material.envelopeCborHex, "hex"),
      programMaterialSidecarCbor: Buffer.from(
        material.programMaterialSidecarCborHex,
        "hex",
      ),
    },
    traversalReference: references[0]!,
    taskReferences: [references[1]!, references[2]!],
    awardDatum: Data.to(
      { fraud_prover: signer.paymentKeyHash, data: { version: 1n } },
      WinningValidationResolutionDatum,
    ),
    getValidityRange: () =>
      refreshExpiredValidationDisputeValidityRange({
        range,
        currentLedgerTime: lucid.slotToUnixTime(lucid.currentSlot()),
      }),
    maxTransactions,
  });
};
