import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytesV1,
  committedWithdrawalValueBytesV1,
  encodeMidgardTxInputCanonicalV1,
  FraudProofComputationThreadStepDatum,
  isWithdrawnInputViolationV1,
  type Proof,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_INPUT_VIOLATION_ID_V1,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep03Datum,
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
import { prepareWithdrawnInputFromCanonicalEvidenceV1 } from "../prepare-withdrawn-input.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { WithdrawnInputContractsV1 } from "../withdrawn-input/contracts-v1.js";
import { submitWithdrawnInputInit } from "../withdrawn-input/submit-withdrawn-input-init.js";
import { submitWithdrawnInputStep01 } from "../withdrawn-input/submit-withdrawn-input-step-01.js";
import { submitWithdrawnInputStep02 } from "../withdrawn-input/submit-withdrawn-input-step-02.js";
import { submitWithdrawnInputStep03 } from "../withdrawn-input/submit-withdrawn-input-step-03.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_WITHDRAWN_INPUT_ARTIFACT_V1 =
  "midgard-production-withdrawn-input-artifact-v1" as const;

type InputJsonV1 = Readonly<{ tx_id: string; output_index: string }>;

export type ProductionWithdrawnInputArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_WITHDRAWN_INPUT_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    spendInputs: readonly InputJsonV1[];
    badInputIndex: number;
    withdrawalIndex: number;
    withdrawalMembershipCbor: string;
  }>;

type AdmittedWithdrawnInputArtifactV1 = Readonly<{
  artifact: ProductionWithdrawnInputArtifactV1;
  inclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  spendInputs: ReturnType<typeof admitProductionTxInputListV1>["inputs"];
  withdrawalMembership: WithdrawalSourceMembershipProofV1;
  spendPlan: FaultProofFieldOpeningPlanV1;
}>;

const proofSteps = (proof: Proof) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const verifyWithdrawalMembership = async (
  membership: WithdrawalSourceMembershipProofV1,
): Promise<void> => {
  if (
    JSON.stringify(membership.domain) !==
      JSON.stringify(ROOT_DOMAINS.withdrawals) ||
    membership.count <= 0n ||
    !HEX_32_V1.test(membership.root) ||
    !HEX_32_V1.test(membership.phas_root)
  ) {
    throw new Error("withdrawn-input membership identity is malformed");
  }
  const countedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: membership.domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    }),
  );
  if (countedRoot !== membership.root) {
    throw new Error("withdrawn-input membership count does not open its root");
  }
  let opened: Buffer | null;
  try {
    opened = MpfProof.fromJSON(
      Buffer.from(committedWithdrawalKeyBytesV1(membership.key), "hex"),
      Buffer.from(committedWithdrawalValueBytesV1(membership.value), "hex"),
      proofSteps(membership.proof),
    ).verify(true);
  } catch {
    throw new Error("withdrawn-input membership proof cannot be replayed");
  }
  if (opened === null || opened.toString("hex") !== membership.phas_root) {
    throw new Error("withdrawn-input membership does not open its PHAS root");
  }
};

export const admitProductionWithdrawnInputArtifactV1 = async (
  value: unknown,
  carriageOwner = "00".repeat(28),
): Promise<AdmittedWithdrawnInputArtifactV1> => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("withdrawn-input carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "spendInputs",
      "badInputIndex",
      "withdrawalIndex",
      "withdrawalMembershipCbor",
    ],
    "withdrawn-input artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_WITHDRAWN_INPUT_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("withdrawn-input artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "withdrawn-input header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "withdrawn-input position",
  );
  const badInputIndex = safeNaturalNumberV1(
    parsed.badInputIndex,
    "withdrawn-input bad input index",
  );
  const withdrawalIndex = safeNaturalNumberV1(
    parsed.withdrawalIndex,
    "withdrawn-input withdrawal index",
  );
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "withdrawn-input transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("withdrawn-input transaction is not accepted");
  }
  const spendInputs = admitProductionTxInputListV1(
    parsed.spendInputs,
    "withdrawn-input spend inputs",
  );
  const selectedInput = spendInputs.inputs[badInputIndex];
  if (selectedInput === undefined) {
    throw new Error("withdrawn-input selected input is out of range");
  }
  const withdrawalMembershipCbor = canonicalHexV1(
    parsed.withdrawalMembershipCbor,
    EVEN_HEX_V1,
    "withdrawn-input withdrawal membership",
  );
  let withdrawalMembership: WithdrawalSourceMembershipProofV1;
  try {
    withdrawalMembership = Data.from(
      withdrawalMembershipCbor,
      WithdrawalSourceMembershipProof,
    );
  } catch {
    throw new Error("withdrawn-input withdrawal membership is malformed");
  }
  if (
    Data.to(withdrawalMembership, WithdrawalSourceMembershipProof) !==
    withdrawalMembershipCbor
  ) {
    throw new Error("withdrawn-input withdrawal membership is non-canonical");
  }
  await verifyWithdrawalMembership(withdrawalMembership);
  if (
    !isWithdrawnInputViolationV1({
      input: selectedInput,
      withdrawal: withdrawalMembership.value,
    })
  ) {
    throw new Error("withdrawn-input artifact does not prove its violation");
  }
  const expectedDetection = `${WITHDRAWN_INPUT_VIOLATION_ID_V1}:${position.toString()}:${badInputIndex.toString()}:${withdrawalIndex.toString()}:${tx.artifact.nativeTxId}:${committedWithdrawalKeyBytesV1(withdrawalMembership.key)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("withdrawn-input detection identity changed");
  }
  const spendPlan = planFaultProofFieldOpeningV1({
    fieldIndex: 0,
    anchorTxId: tx.artifact.nativeTxId,
    nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
    itemCbors: spendInputs.inputs.map(encodeMidgardTxInputCanonicalV1),
    owner: carriageOwner,
    label: "withdrawn-input artifact spend inputs",
  });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_WITHDRAWN_INPUT_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputs: spendInputs.json,
    badInputIndex,
    withdrawalIndex,
    withdrawalMembershipCbor,
  }) satisfies ProductionWithdrawnInputArtifactV1;
  return Object.freeze({
    artifact,
    inclusion: tx.inclusion,
    spendInputs: spendInputs.inputs,
    withdrawalMembership,
    spendPlan,
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
    withdrawalIndexValue,
    txId,
    withdrawalKey,
    ...rest
  ] = classification.selected.detectionId.split(":");
  if (
    classification.category !== "withdrawnInput" ||
    classification.selected.violationId !== WITHDRAWN_INPUT_VIOLATION_ID_V1 ||
    violationId !== WITHDRAWN_INPUT_VIOLATION_ID_V1 ||
    rest.length !== 0 ||
    !NATURAL_DECIMAL_V1.test(positionValue ?? "") ||
    !NATURAL_DECIMAL_V1.test(inputValue ?? "") ||
    !NATURAL_DECIMAL_V1.test(withdrawalIndexValue ?? "") ||
    !HEX_32_V1.test(txId ?? "") ||
    !EVEN_HEX_V1.test(withdrawalKey ?? "")
  ) {
    throw new Error(
      `withdrawn-input classification is malformed: ${classification.selected.detectionId}`,
    );
  }
  const position = Number(positionValue);
  const badInputIndex = Number(inputValue);
  const withdrawalIndex = Number(withdrawalIndexValue);
  if (
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badInputIndex) ||
    !Number.isSafeInteger(withdrawalIndex) ||
    classification.selected.position !== BigInt(positionValue!)
  ) {
    throw new Error("withdrawn-input classification index is unsafe");
  }
  return Object.freeze({
    position,
    badInputIndex,
    withdrawalIndex,
    txId: txId!,
    withdrawalKey: withdrawalKey!,
  });
};

export const prepareProductionWithdrawnInputArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionWithdrawnInputArtifactV1> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnInputFromCanonicalEvidenceV1({
    evidence,
    badTxId: selected.txId,
    badInputIndex: selected.badInputIndex,
  });
  const selectedKey = committedWithdrawalKeyBytesV1(prepared.withdrawalId);
  const selectedWithdrawal =
    evidence.reconstruction.withdrawals[selected.withdrawalIndex];
  if (
    selectedWithdrawal === undefined ||
    committedWithdrawalKeyBytesV1(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    selectedKey !== selected.withdrawalKey ||
    prepared.badTxInclusion.nativeTxId !== selected.txId
  ) {
    throw new Error("withdrawn-input selected public evidence changed");
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_WITHDRAWN_INPUT_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: prepared.badTxInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.badTxInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.badTxInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.badTxInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.badTxInclusion.txMembershipProofCbor,
    },
    spendInputs: prepared.spendInputs.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badInputIndex: prepared.badInputIndex,
    withdrawalIndex: selected.withdrawalIndex,
    withdrawalMembershipCbor: Data.to(
      prepared.withdrawalMembership,
      WithdrawalSourceMembershipProof,
    ),
  }) as ProductionWithdrawnInputArtifactV1;
  await admitProductionWithdrawnInputArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type WithdrawnInputWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"withdrawnInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnInputContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"withdrawnInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"withdrawnInput">["catalogue"];
  referenceScripts: WithdrawnInputWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"withdrawnInput">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (action: FraudProofWorkflowActionV1) => {
  const input = exactJournalRecordV1(
    action.input,
    Object.keys(action.input),
    "withdrawn-input action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "withdrawnInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("withdrawn-input action identity changed");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`withdrawn-input action omitted ${field}`);
  }
  return value;
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
        fraudCategory: "withdrawnInput",
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
              "withdrawn-input removal changed authenticated inputs",
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
    throw new Error("withdrawn-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("withdrawn-input field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"withdrawnInput"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "withdrawnInput",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionWithdrawnInputArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitProductionWithdrawnInputArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputInit({
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
      const chunks = await resolveDirectFirstProofChunksV1({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputStep01({
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
              publishedProofChunks: chunks,
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
      const field = await resolveField(config, admitted.spendPlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              evidence: {
                inputs: admitted.spendInputs,
                badInputIndex: admitted.artifact.badInputIndex,
                nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              },
              referenceScriptUtxo: config.referenceScripts.steps[1],
              publishedCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : { certificateUtxo: field.certificate }),
              publishMissingCarriage: false,
              publishCarriage: false,
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
            await submitWithdrawnInputStep03({
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
    throw new Error(`unsupported withdrawn-input stage ${String(input.stage)}`);
  },
});

export type ManifestBoundWithdrawnInputWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WithdrawnInputWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundWithdrawnInputWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"withdrawnInput">;
  l1: FraudProofFamilyL1ObservationPortV1<"withdrawnInput">;
  transactions: ProductionLinearFamilyTransactionPortV1<"withdrawnInput">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundWithdrawnInputWorkflowV1 = async (
  config: ManifestBoundWithdrawnInputWorkflowConfigV1,
): Promise<ManifestBoundWithdrawnInputWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "withdrawnInput",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      WithdrawnInputStep02Datum,
      WithdrawnInputStep03Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error("withdrawn-input manifest omitted certificate policy");
  }
  const certificate = binding.fieldPreimageCertificate;
  const chain = binding.resolvedContracts.contracts.withdrawnInput;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    stateQueuePolicyId === undefined
  ) {
    throw new Error("withdrawn-input deployment chain is incomplete");
  }
  const contracts: WithdrawnInputContractsV1 = Object.freeze({
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!] as const,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptHash:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptHash,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const ref = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  const referenceScripts: WithdrawnInputWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        ref("fraudProofWithdrawnInput", config.referenceScripts.steps[0]),
        ref("fraudProofWithdrawnInputStep02", config.referenceScripts.steps[1]),
        ref("fraudProofWithdrawnInputStep03", config.referenceScripts.steps[2]),
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
        chunkedVerifyWithdraw: ref(
          "chunkedVerifyWithdraw",
          config.referenceScripts.witnesses.chunkedVerifyWithdraw,
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
    throw new Error("withdrawn-input raw-L1 omitted publications");
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
    category: "withdrawnInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "withdrawnInput",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "withdrawnInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitProductionWithdrawnInputArtifactV1(
          artifact,
          config.signer.paymentKeyHash,
        );
        return {
          planned: admitted.spendPlan,
          compactCbor: admitted.spendPlan.nativeTxCompactCbor,
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
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "withdrawnInput",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePortV1({
      category: "withdrawnInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      proofCborForAction: ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_01") return null;
        const record = exactJournalRecordV1(
          artifact,
          [
            "schemaVersion",
            "headerHash",
            "detectionId",
            "position",
            "tx",
            "spendInputs",
            "badInputIndex",
            "withdrawalIndex",
            "withdrawalMembershipCbor",
          ],
          "withdrawn-input proof-chunk artifact",
        );
        return admitProductionNativeInclusionArtifactV1(
          record.tx,
          "withdrawn-input proof-chunk transaction",
        ).artifact.txMembershipProofCbor;
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

export const runOrResumeManifestBoundWithdrawnInputWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWithdrawnInputWorkflowV1;
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
    replayer: WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["withdrawnInput"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
