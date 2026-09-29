import {
  CEK_CONTEXT_STAGE_REFERENCES,
  CEK_CORE_STAGE_REFERENCES,
  CEK_MATERIAL_TASK_YIELD_ROLES,
  CekContextDatum,
  cekContextItemReferenceScripts,
  cekContextReferenceScripts,
  type CekContextStages,
  CekCoreDatum,
  type CekCoreStages,
  deriveValidationTraceDeploymentId,
  PreparedValidationResolutionState,
  sharedRedeemerItemReferenceScripts,
  type SharedRedeemerItemStages,
  ValidationAwardSpendRedeemer,
  validationTraceDescriptorDataFromCore,
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
  outRefLabel,
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
import { deriveCekCorePlan, submitCekCoreChain } from ".././cek-core.js";
import { submitCekMaterialTraversal } from ".././cek-material-traversal.js";
import {
  exactPlutusDataFromCbor,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import {
  requireStagedOneStepArgument,
  requireValidationDisputeReferenceScript,
} from "./reference-scripts.js";
import { requireWinningResolutionDatum } from "./resolution.js";
import {
  submitValidationFinalizationTransaction,
  type ValidationFinalizationResult,
} from "./semantic-redeemers.js";
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

export const submitValidationDisputeAward = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  awardReferenceScriptUtxo,
  witnessReferenceScripts,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The mandatory published V1 validation-trace award script. */
  readonly awardReferenceScriptUtxo?: UTxO;
  /** Required published shared minting witnesses for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeAwardResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "winning validation award UTxO",
  });
  const awardContract = contracts.validationTraceDispute.award;
  if (threadUtxo.address !== awardContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation award validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireWinningResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation award requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  return await submitValidationFinalizationTransaction({
    lucid,
    blueprint,
    deploymentInfo,
    network,
    contracts,
    signer,
    threadUtxo,
    threadOutRef,
    token,
    spendingScript: awardContract,
    spendingScriptReferenceUtxo: awardReferenceScriptUtxo,
    witnessReferenceScripts,
    spendLabel: "Validation-dispute award",
    encodeSpendRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            },
          ],
        },
        ValidationAwardSpendRedeemer,
      ),
    validityRange: range,
    awaitConfirmation,
    preSubmitBoundary,
  });
};

export const validationDisputeDescriptorData =
  validationTraceDescriptorDataFromCore;

/** Cancel an owned prepared semantic thread by its live out-ref. */
export const cancelValidationSemanticResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceScriptUtxo,
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
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  return await submitLinearFaultCancel({
    lucid,
    family: "validation semantic resolution",
    steps: [
      ...contracts.validationTraceDispute.semanticResolvers,
      ...sharedRedeemerItemReferenceScripts(
        contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages,
      ).map(({ validator }) => validator),
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages
        .settlement,
    ],
    computationThread: contracts.computationThread,
    categoryId: validationTraceDisputeCategory.categoryId,
    signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
