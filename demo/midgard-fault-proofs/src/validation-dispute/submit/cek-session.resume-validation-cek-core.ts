import {
  CEK_CORE_STAGE_REFERENCES,
  cekContextItemReferenceScripts,
  CekCoreDatum,
  type CekCoreStages,
  PreparedValidationResolutionState,
  type SharedRedeemerItemStages,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
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
import { deriveCekCorePlan, submitCekCoreChain } from ".././cek-core.js";
import { type ValidationOneStepSubmissionArgument } from "./evidence.js";
import {
  requireStagedOneStepArgument,
  requireValidationDisputeReferenceScript,
} from "./reference-scripts.js";
import {
  refreshExpiredValidationDisputeValidityRange,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const resumeValidationCekCore = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  validityRange,
  maxTransactions,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly maxTransactions?: number;
  /** Production workflow seam: each stage reaches it before submission. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
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
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "CEK core checkpoint"),
    label: "CEK core checkpoint",
  });
  if (
    !Object.values(contracts.validationTraceDispute.cekCoreStages).some(
      (stage) => stage.spendingScriptAddress === threadUtxo.address,
    )
  )
    throw new Error("CEK core checkpoint is not at a deployed core stage");
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  if (threadUtxo.datum == null)
    throw new Error("CEK core checkpoint has no datum");
  const datum = Data.from(threadUtxo.datum, CekCoreDatum);
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data === null)
    throw new Error("CEK core checkpoint has no state or the wrong prover");
  const prepared = Data.from(
    Data.to(datum.data.prepared),
    PreparedValidationResolutionState,
  );
  const staged = requireStagedOneStepArgument(oneStepArgument);
  if (
    oneStepArgument.resolverIndex !== 11 ||
    staged.semanticResolverIndex !== 3 ||
    staged.evidenceHash !== prepared.evidence_hash
  )
    throw new Error(
      "Retained evidence does not match the authenticated CEK core checkpoint",
    );
  const step = staged.auxiliary.fields[0]!;
  const plan = deriveCekCorePlan(datum.data.prepared, step);
  const stageReferences: Partial<Record<keyof CekCoreStages, UTxO>> = {};
  for (const key of [
    ...plan.route.slice(Number(datum.data.progress)),
    "settle",
  ] as const) {
    const spec = CEK_CORE_STAGE_REFERENCES[key];
    const contract = contracts.validationTraceDispute.cekCoreStages[key];
    const entry = deployed[spec.deployment];
    if (entry?.refScriptUTxO == null)
      throw new Error(`Missing CEK core publication: ${spec.deployment}`);
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: spec.role,
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: contract.spendingScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: spec.role,
    });
    stageReferences[key] = utxo;
  }
  const binder =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ]!;
  const range = requireValidityRange(
    validityRange ??
      validationDisputeValidityRange(lucid.slotToUnixTime(lucid.currentSlot())),
  );
  return await submitCekCoreChain({
    lucid,
    signer,
    contracts: contracts.validationTraceDispute,
    binder,
    stageReferences,
    threadUtxo,
    threadUnit: token.unit,
    prepared: datum.data.prepared,
    transition: staged.transitionData,
    step,
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
    preSubmitBoundary,
  });
};

export const cancelValidationCekCore = async ({
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
  const thread = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "CEK core checkpoint"),
    label: "CEK core checkpoint",
  });
  const key = (
    Object.keys(CEK_CORE_STAGE_REFERENCES) as (keyof CekCoreStages)[]
  ).find(
    (key) =>
      contracts.validationTraceDispute.cekCoreStages[key]
        .spendingScriptAddress === thread.address,
  );
  if (key === undefined)
    throw new Error("CEK core cancellation is not at a deployed core stage");
  const spec = CEK_CORE_STAGE_REFERENCES[key];
  const stage = contracts.validationTraceDispute.cekCoreStages[key];
  const entry = deployed[spec.deployment];
  if (entry?.refScriptUTxO == null)
    throw new Error(`Missing ${spec.role} publication`);
  const reference = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: spec.role,
  });
  requireValidationDisputeReferenceScript({
    utxo: reference,
    deployedScriptHash: entry.scriptHash,
    expectedScriptHash: stage.spendingScriptHash,
    authPolicyId: referenceScriptAuthPolicyId,
    role: spec.role,
  });
  return await submitLinearFaultCancel({
    lucid,
    signer,
    threadOutRef,
    steps: [stage],
    categoryId: validationTraceDisputeCategory.categoryId,
    computationThread: contracts.computationThread,
    referenceScriptUtxo: reference,
    witnessReferenceScripts,
    family: "validationTraceDispute",
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const resolveCekContextItemReferences = async ({
  lucid,
  deployed,
  authPolicyId,
  stages,
}: {
  readonly lucid: LucidEvolution;
  readonly deployed: Awaited<
    ReturnType<typeof resolveValidationTraceDisputeDeploymentContracts>
  >["deploymentInfo"];
  readonly authPolicyId: string;
  readonly stages: SharedRedeemerItemStages;
}) => {
  const references = new Map<string, UTxO>();
  for (const {
    deploymentEntry,
    role,
    validator,
  } of cekContextItemReferenceScripts(stages)) {
    const entry = deployed[deploymentEntry];
    if (entry?.refScriptUTxO == null)
      throw new Error(`Missing CEK item publication: ${deploymentEntry}`);
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: role,
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: validator.spendingScriptHash,
      authPolicyId,
      role,
    });
    references.set(validator.spendingScriptHash, utxo);
  }
  return references;
};
