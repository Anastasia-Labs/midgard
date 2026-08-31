import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytesV1,
  encodeMidgardTxInputCanonicalV1,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1,
  WithdrawnReferenceInputStep02Datum,
  WithdrawnReferenceInputStep03Datum,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { WithdrawnReferenceInputContractsV1 } from "../withdrawn-reference-input/contracts-v1.js";
import {
  prepareWithdrawnReferenceInputV1,
  verifyWithdrawnReferenceInputMembershipV1,
} from "../withdrawn-reference-input/prepare-withdrawn-reference-input-v1.js";
import { submitWithdrawnReferenceInputInit } from "../withdrawn-reference-input/submit-withdrawn-reference-input-init.js";
import { submitWithdrawnReferenceInputStep01 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-01.js";
import { submitWithdrawnReferenceInputStep02 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-02.js";
import { submitWithdrawnReferenceInputStep03 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-03.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  admitProductionTxInputListV1,
  canonicalHexV1,
  EVEN_HEX_V1,
  exactJournalRecordV1,
  HEX_28_V1,
  HEX_32_V1,
  NATURAL_DECIMAL_V1,
  type ProductionNativeInclusionArtifactV1,
  safeNaturalNumberV1,
} from "./production-native-index-artifact-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_WITHDRAWN_REFERENCE_INPUT_ARTIFACT_V1 =
  "midgard-production-withdrawn-reference-input-artifact-v1" as const;

type InputJsonV1 = Readonly<{ tx_id: string; output_index: string }>;

export type ProductionWithdrawnReferenceInputArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_WITHDRAWN_REFERENCE_INPUT_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    referenceInputs: readonly InputJsonV1[];
    badReferenceInputIndex: number;
    withdrawalIndex: number;
    withdrawalMembershipCbor: string;
  }>;

type AdmittedArtifactV1 = Readonly<{
  artifact: ProductionWithdrawnReferenceInputArtifactV1;
  inclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  referenceInputs: ReturnType<typeof admitProductionTxInputListV1>["inputs"];
  withdrawalMembership: WithdrawalSourceMembershipProofV1;
  referencePlan: FaultProofFieldOpeningPlanV1;
}>;

const verifyMembership = async (
  membership: WithdrawalSourceMembershipProofV1,
): Promise<void> => {
  if (
    JSON.stringify(membership.domain) !==
      JSON.stringify(ROOT_DOMAINS.withdrawals) ||
    membership.count <= 0n ||
    !HEX_32_V1.test(membership.root) ||
    !HEX_32_V1.test(membership.phas_root)
  ) {
    throw new Error("withdrawn-reference-input membership is malformed");
  }
  const counted = await Effect.runPromise(
    commitCountedRootProgram({
      domain: membership.domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    }),
  );
  if (counted !== membership.root) {
    throw new Error(
      "withdrawn-reference-input membership count does not open its root",
    );
  }
  verifyWithdrawnReferenceInputMembershipV1(membership);
};

export const admitProductionWithdrawnReferenceInputArtifactV1 = async (
  value: unknown,
  carriageOwner = "00".repeat(28),
): Promise<AdmittedArtifactV1> => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("withdrawn-reference-input carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "referenceInputs",
      "badReferenceInputIndex",
      "withdrawalIndex",
      "withdrawalMembershipCbor",
    ],
    "withdrawn-reference-input artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_WITHDRAWN_REFERENCE_INPUT_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("withdrawn-reference-input artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "withdrawn-reference-input header",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "withdrawn-reference-input position",
  );
  const badReferenceInputIndex = safeNaturalNumberV1(
    parsed.badReferenceInputIndex,
    "withdrawn-reference-input bad reference-input index",
  );
  const withdrawalIndex = safeNaturalNumberV1(
    parsed.withdrawalIndex,
    "withdrawn-reference-input withdrawal index",
  );
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "withdrawn-reference-input transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("withdrawn-reference-input transaction is not accepted");
  }
  const referenceInputs = admitProductionTxInputListV1(
    parsed.referenceInputs,
    "withdrawn-reference-input reference inputs",
  );
  const selectedInput = referenceInputs.inputs[badReferenceInputIndex];
  if (selectedInput === undefined) {
    throw new Error("withdrawn-reference-input selection is out of range");
  }
  const withdrawalMembershipCbor = canonicalHexV1(
    parsed.withdrawalMembershipCbor,
    EVEN_HEX_V1,
    "withdrawn-reference-input membership",
  );
  let withdrawalMembership: WithdrawalSourceMembershipProofV1;
  try {
    withdrawalMembership = Data.from(
      withdrawalMembershipCbor,
      WithdrawalSourceMembershipProof,
    );
  } catch {
    throw new Error("withdrawn-reference-input membership is malformed");
  }
  if (
    Data.to(withdrawalMembership, WithdrawalSourceMembershipProof) !==
    withdrawalMembershipCbor
  ) {
    throw new Error("withdrawn-reference-input membership is non-canonical");
  }
  await verifyMembership(withdrawalMembership);
  const withdrawn = withdrawalMembership.value.body.l2_outref;
  if (
    withdrawalMembership.value.validity !== "WithdrawalIsValid" ||
    selectedInput.tx_id !== withdrawn.transactionId ||
    selectedInput.output_index !== withdrawn.outputIndex
  ) {
    throw new Error(
      "withdrawn-reference-input artifact does not prove its violation",
    );
  }
  const expectedDetection = `${WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1}:${position.toString()}:${badReferenceInputIndex.toString()}:${withdrawalIndex.toString()}:${tx.artifact.nativeTxId}:${committedWithdrawalKeyBytesV1(withdrawalMembership.key)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("withdrawn-reference-input detection identity changed");
  }
  const referencePlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: tx.artifact.nativeTxId,
    nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputs.inputs.map(encodeMidgardTxInputCanonicalV1),
    owner: carriageOwner,
    label: "withdrawn-reference-input artifact reference inputs",
  });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_WITHDRAWN_REFERENCE_INPUT_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    referenceInputs: referenceInputs.json,
    badReferenceInputIndex,
    withdrawalIndex,
    withdrawalMembershipCbor,
  }) satisfies ProductionWithdrawnReferenceInputArtifactV1;
  return Object.freeze({
    artifact,
    inclusion: tx.inclusion,
    referenceInputs: referenceInputs.inputs,
    withdrawalMembership,
    referencePlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
) => {
  const [
    violationId,
    positionValue,
    inputValue,
    withdrawalValue,
    txId,
    withdrawalKey,
    ...rest
  ] = classification.selected.detectionId.split(":");
  if (
    classification.category !== "withdrawnReferenceInput" ||
    classification.selected.violationId !==
      WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1 ||
    violationId !== WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1 ||
    rest.length !== 0 ||
    !NATURAL_DECIMAL_V1.test(positionValue ?? "") ||
    !NATURAL_DECIMAL_V1.test(inputValue ?? "") ||
    !NATURAL_DECIMAL_V1.test(withdrawalValue ?? "") ||
    !HEX_32_V1.test(txId ?? "") ||
    !EVEN_HEX_V1.test(withdrawalKey ?? "")
  ) {
    throw new Error("withdrawn-reference-input classification is malformed");
  }
  const position = Number(positionValue);
  const badReferenceInputIndex = Number(inputValue);
  const withdrawalIndex = Number(withdrawalValue);
  if (
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badReferenceInputIndex) ||
    !Number.isSafeInteger(withdrawalIndex) ||
    classification.selected.position !== BigInt(positionValue!)
  ) {
    throw new Error("withdrawn-reference-input classification index is unsafe");
  }
  return Object.freeze({
    position,
    badReferenceInputIndex,
    withdrawalIndex,
    txId: txId!,
    withdrawalKey: withdrawalKey!,
  });
};

export const prepareProductionWithdrawnReferenceInputArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionWithdrawnReferenceInputArtifactV1> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-reference-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnReferenceInputV1({
    header: evidence.header,
    blockTxs: evidence.transactions,
    withdrawals: evidence.reconstruction.withdrawals.map(({ key, value }) => ({
      id: key,
      info: value,
    })),
    accusedTxId: selected.txId,
  });
  const selectedWithdrawal =
    evidence.reconstruction.withdrawals[selected.withdrawalIndex];
  if (
    selectedWithdrawal === undefined ||
    prepared.badReferenceInputIndex !== selected.badReferenceInputIndex ||
    committedWithdrawalKeyBytesV1(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    committedWithdrawalKeyBytesV1(prepared.withdrawal.id) !==
      selected.withdrawalKey
  ) {
    throw new Error("withdrawn-reference-input public evidence changed");
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_WITHDRAWN_REFERENCE_INPUT_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: prepared.txInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.txInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
    },
    referenceInputs: prepared.referenceInputs.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badReferenceInputIndex: prepared.badReferenceInputIndex,
    withdrawalIndex: selected.withdrawalIndex,
    withdrawalMembershipCbor: Data.to(
      prepared.withdrawalMembership,
      WithdrawalSourceMembershipProof,
    ),
  }) as ProductionWithdrawnReferenceInputArtifactV1;
  await admitProductionWithdrawnReferenceInputArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type WithdrawnReferenceInputWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"withdrawnReferenceInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnReferenceInputContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"withdrawnReferenceInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"withdrawnReferenceInput">["catalogue"];
  referenceScripts: WithdrawnReferenceInputWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"withdrawnReferenceInput">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (action: FraudProofWorkflowActionV1) => {
  const input = exactJournalRecordV1(
    action.input,
    Object.keys(action.input),
    "withdrawn-reference-input action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "withdrawnReferenceInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("withdrawn-reference-input action identity changed");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`withdrawn-reference-input action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfigV1,
  plan: FaultProofFieldOpeningPlanV1,
) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("withdrawn-reference-input publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("withdrawn-reference-input certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const captureRemoval = async (
  config: BoundConfigV1,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransactionV1(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "withdrawnReferenceInput",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "withdrawn-reference-input removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"withdrawnReferenceInput"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "withdrawnReferenceInput",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionWithdrawnReferenceInputArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitProductionWithdrawnReferenceInputArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-reference-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnReferenceInputInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnReferenceInputStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const field = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnReferenceInputStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceInputs: admitted.referenceInputs,
              nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              badReferenceInputIndex: BigInt(
                admitted.artifact.badReferenceInputIndex,
              ),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              publishedCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : { certificateUtxo: field.certificate }),
              publishMissingCarriage: false,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnReferenceInputStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              withdrawalMembership: admitted.withdrawalMembership,
              referenceScriptUtxo: config.referenceScripts.steps[2],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") return await captureRemoval(config, input);
    throw new Error(
      `unsupported withdrawn-reference-input stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundWithdrawnReferenceInputWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WithdrawnReferenceInputWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundWithdrawnReferenceInputWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"withdrawnReferenceInput">;
  l1: FraudProofFamilyL1ObservationPortV1<"withdrawnReferenceInput">;
  transactions: ProductionLinearFamilyTransactionPortV1<"withdrawnReferenceInput">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundWithdrawnReferenceInputWorkflowV1 = async (
  config: ManifestBoundWithdrawnReferenceInputWorkflowConfigV1,
): Promise<ManifestBoundWithdrawnReferenceInputWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "withdrawnReferenceInput",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      WithdrawnReferenceInputStep02Datum,
      WithdrawnReferenceInputStep03Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "withdrawn-reference-input manifest omitted certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const chain = binding.resolvedContracts.contracts.withdrawnReferenceInput;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    stateQueuePolicyId === undefined
  ) {
    throw new Error("withdrawn-reference-input deployment chain is incomplete");
  }
  const contracts: WithdrawnReferenceInputContractsV1 = Object.freeze({
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!] as const,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    claimRegistry: binding.claimRegistry,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const ref = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  const referenceScripts: WithdrawnReferenceInputWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        ref(
          "fraudProofWithdrawnReferenceInput",
          config.referenceScripts.steps[0],
        ),
        ref(
          "fraudProofWithdrawnReferenceInputStep02",
          config.referenceScripts.steps[1],
        ),
        ref(
          "fraudProofWithdrawnReferenceInputStep03",
          config.referenceScripts.steps[2],
        ),
      ] as const),
      witnesses: Object.freeze({
        computationThreadMint: ref(
          "computationThreadMint",
          config.referenceScripts.witnesses.computationThreadMint,
        ),
        fraudProofMint: ref(
          "fraudProofMint",
          config.referenceScripts.witnesses.fraudProofMint,
        ),
        phasMembershipWithdraw: ref(
          "phasMembershipWithdraw",
          config.referenceScripts.witnesses.phasMembershipWithdraw,
        ),
      }),
      fieldPreimageCertificateMint: ref(
        "fieldPreimageCertificateMint",
        config.referenceScripts.fieldPreimageCertificateMint,
      ),
    });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error("withdrawn-reference-input raw-L1 omitted publications");
  }
  const transactions = createTransactionPort({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    contracts,
    category: binding.resolvedContracts.category,
    catalogue: binding.catalogue,
    referenceScripts,
    certificate,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "withdrawnReferenceInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "withdrawnReferenceInput",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "withdrawnReferenceInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitProductionWithdrawnReferenceInputArtifactV1(
          artifact,
          config.signer.paymentKeyHash,
        );
        return {
          planned: admitted.referencePlan,
          compactCbor: admitted.referencePlan.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: referenceScripts.fieldPreimageCertificateMint,
          },
        } satisfies ProductionFieldCarriageRequirementV1;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundWithdrawnReferenceInputWorkflowV1 =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundWithdrawnReferenceInputWorkflowV1;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStoreV1;
  }): Promise<FraudProofWorkflowRunResultV1> => {
    const observation = await workflow.l1.observeHeader({
      headerHash: workflow.binding.definition.headerHash,
    });
    return await runFraudProofWorkflowFromRetainedDaV1({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      observation,
      sources,
      replayer: WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
      registry: createFraudProofWorkflowRegistryV1({
        adapters: [workflow.adapter],
        launchScope: ["withdrawnReferenceInput"],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  };
