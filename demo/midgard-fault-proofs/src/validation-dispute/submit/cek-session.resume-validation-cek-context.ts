import {
  CEK_CONTEXT_STAGE_REFERENCES,
  CekContextDatum,
  cekContextItemReferenceScripts,
  cekContextReferenceScripts,
  type CekContextStages,
  deriveValidationTraceDeploymentId,
  PreparedValidationResolutionState,
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
import {
  deriveCekContextPlan,
  submitCekContextChain,
} from ".././cek-context.js";
import { resolveCekContextItemReferences } from "./cek-session.resume-validation-cek-core.js";
import {
  exactPlutusDataFromCbor,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import {
  requireStagedOneStepArgument,
  requireValidationDisputeReferenceScript,
} from "./reference-scripts.js";
import { type ValidationFinalizationResult } from "./semantic-redeemers.js";
import {
  refreshExpiredValidationDisputeValidityRange,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const resumeValidationCekContext = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  preparedCbor,
  validityRange,
  maxTransactions,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  readonly preparedCbor: Uint8Array;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly maxTransactions?: number;
}) => {
  const {
    deploymentInfo: deployed,
    fraudProofCataloguePolicyId,
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
    outRef: parseOutRef(threadOutRef, "CEK context checkpoint"),
    label: "CEK context checkpoint",
  });
  if (
    ![
      ...cekContextReferenceScripts(
        contracts.validationTraceDispute.cekContextStages,
        contracts.validationTraceDispute.cekContextItemStages,
      ),
      ...cekContextItemReferenceScripts(
        contracts.validationTraceDispute.cekContextItemStages,
      ),
    ].some(
      ({ validator }) => validator.spendingScriptAddress === threadUtxo.address,
    )
  )
    throw new Error(
      "CEK context checkpoint is not at a deployed context stage",
    );
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  if (threadUtxo.datum == null)
    throw new Error("CEK context checkpoint has no datum");
  const datum = Data.from(threadUtxo.datum, CekContextDatum);
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data === null)
    throw new Error("CEK context checkpoint has no state or the wrong prover");
  const preparedData = exactPlutusDataFromCbor(
    preparedCbor,
    "retained CEK context prepared state",
  );
  const prepared = Data.from(
    Data.to(preparedData),
    PreparedValidationResolutionState,
  );
  const staged = requireStagedOneStepArgument(oneStepArgument);
  if (
    oneStepArgument.resolverIndex !== 11 ||
    staged.semanticResolverIndex !== 2 ||
    staged.evidenceHash !== prepared.evidence_hash
  )
    throw new Error(
      "Retained evidence does not match the authenticated CEK context checkpoint",
    );
  if (staged.cekContextSuccessorWorkWitnessCbor === undefined)
    throw new Error("CEK context requires retained canonical successor bytes");
  const successorWorkWitnessCbor = Buffer.from(
    staged.cekContextSuccessorWorkWitnessCbor,
  ).toString("hex");
  const plan = deriveCekContextPlan({
    prepared: preparedData,
    transition: staged.transitionData,
    auxiliary: staged.auxiliaryData,
    successorWorkWitnessCbor,
  });
  const stageReferences: Partial<Record<keyof CekContextStages, UTxO>> = {};
  for (const key of plan.route) {
    const spec = CEK_CONTEXT_STAGE_REFERENCES[key];
    const contract = contracts.validationTraceDispute.cekContextStages[key];
    const entry = deployed[spec.deployment];
    if (entry?.refScriptUTxO == null)
      throw new Error(`Missing CEK context publication: ${spec.deployment}`);
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
  return await submitCekContextChain({
    lucid,
    signer,
    contracts: contracts.validationTraceDispute,
    binder,
    stageReferences,
    sharedItem:
      plan.item === undefined
        ? undefined
        : {
            stages: contracts.validationTraceDispute.cekContextItemStages,
            deploymentId: deriveValidationTraceDeploymentId(
              fraudProofCataloguePolicyId,
            ),
            references: await resolveCekContextItemReferences({
              lucid,
              deployed: deployed,
              authPolicyId: referenceScriptAuthPolicyId,
              stages: contracts.validationTraceDispute.cekContextItemStages,
            }),
          },
    threadUtxo,
    threadUnit: token.unit,
    prepared: preparedData,
    transition: staged.transitionData,
    auxiliary: staged.auxiliaryData,
    successorWorkWitnessCbor,
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

export const cancelValidationCekContext = async ({
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
    outRef: parseOutRef(threadOutRef, "CEK context checkpoint"),
    label: "CEK context checkpoint",
  });
  const spec = [
    ...cekContextReferenceScripts(
      contracts.validationTraceDispute.cekContextStages,
      contracts.validationTraceDispute.cekContextItemStages,
    ),
    ...cekContextItemReferenceScripts(
      contracts.validationTraceDispute.cekContextItemStages,
    ),
  ].find(({ validator }) => validator.spendingScriptAddress === thread.address);
  if (spec === undefined)
    throw new Error(
      "CEK context cancellation is not at a deployed context stage",
    );
  const stage = spec.validator;
  const entry = deployed[spec.deploymentEntry];
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

export type SubmitValidationDisputeAwardResult = ValidationFinalizationResult;
