import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  encodeMidgardTxInputCanonical,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
  WithdrawnReferenceInputStep02Datum,
  WithdrawnReferenceInputStep03Datum,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { WithdrawnReferenceInputContracts } from "../withdrawn-reference-input/contracts.js";
import {
  prepareWithdrawnReferenceInput,
  verifyWithdrawnReferenceInputMembership,
} from "../withdrawn-reference-input/prepare-withdrawn-reference-input.js";
import { submitWithdrawnReferenceInputInit } from "../withdrawn-reference-input/submit-withdrawn-reference-input-init.js";
import { submitWithdrawnReferenceInputStep01 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-01.js";
import { submitWithdrawnReferenceInputStep02 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-02.js";
import { submitWithdrawnReferenceInputStep03 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-03.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  admitNativeInclusionArtifact,
  admitTxInputList,
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./native-index-artifact.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const WITHDRAWN_REFERENCE_INPUT_ARTIFACT =
  "midgard-production-withdrawn-reference-input-artifact-v1" as const;

type InputJson = Readonly<{ tx_id: string; output_index: string }>;

export type WithdrawnReferenceInputArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof WITHDRAWN_REFERENCE_INPUT_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    referenceInputs: readonly InputJson[];
    badReferenceInputIndex: number;
    withdrawalIndex: number;
    withdrawalMembershipCbor: string;
  }>;

type AdmittedArtifact = Readonly<{
  artifact: WithdrawnReferenceInputArtifact;
  inclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  referenceInputs: ReturnType<typeof admitTxInputList>["inputs"];
  withdrawalMembership: WithdrawalSourceMembershipProofV1;
  referencePlan: FaultProofFieldOpeningPlan;
}>;

const verifyMembership = async (
  membership: WithdrawalSourceMembershipProofV1,
): Promise<void> => {
  if (
    JSON.stringify(membership.domain) !==
      JSON.stringify(ROOT_DOMAINS.withdrawals) ||
    membership.count <= 0n ||
    !HEX_32.test(membership.root) ||
    !HEX_32.test(membership.phas_root)
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
  verifyWithdrawnReferenceInputMembership(membership);
};

export const admitWithdrawnReferenceInputArtifact = async (
  value: unknown,
  carriageOwner = "00".repeat(28),
): Promise<AdmittedArtifact> => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("withdrawn-reference-input carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
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
    parsed.schemaVersion !== WITHDRAWN_REFERENCE_INPUT_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("withdrawn-reference-input artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "withdrawn-reference-input header",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "withdrawn-reference-input position",
  );
  const badReferenceInputIndex = safeNaturalNumber(
    parsed.badReferenceInputIndex,
    "withdrawn-reference-input bad reference-input index",
  );
  const withdrawalIndex = safeNaturalNumber(
    parsed.withdrawalIndex,
    "withdrawn-reference-input withdrawal index",
  );
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "withdrawn-reference-input transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("withdrawn-reference-input transaction is not accepted");
  }
  const referenceInputs = admitTxInputList(
    parsed.referenceInputs,
    "withdrawn-reference-input reference inputs",
  );
  const selectedInput = referenceInputs.inputs[badReferenceInputIndex];
  if (selectedInput === undefined) {
    throw new Error("withdrawn-reference-input selection is out of range");
  }
  const withdrawalMembershipCbor = canonicalHex(
    parsed.withdrawalMembershipCbor,
    EVEN_HEX,
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
  const expectedDetection = `${WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID}:${position.toString()}:${badReferenceInputIndex.toString()}:${withdrawalIndex.toString()}:${tx.artifact.nativeTxId}:${committedWithdrawalKeyBytes(withdrawalMembership.key)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("withdrawn-reference-input detection identity changed");
  }
  const referencePlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: tx.artifact.nativeTxId,
    nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputs.inputs.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "withdrawn-reference-input artifact reference inputs",
  });
  const artifact = Object.freeze({
    schemaVersion: WITHDRAWN_REFERENCE_INPUT_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    referenceInputs: referenceInputs.json,
    badReferenceInputIndex,
    withdrawalIndex,
    withdrawalMembershipCbor,
  }) satisfies WithdrawnReferenceInputArtifact;
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
    CanonicalBlockClassification,
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
      WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID ||
    violationId !== WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID ||
    rest.length !== 0 ||
    !NATURAL_DECIMAL.test(positionValue ?? "") ||
    !NATURAL_DECIMAL.test(inputValue ?? "") ||
    !NATURAL_DECIMAL.test(withdrawalValue ?? "") ||
    !HEX_32.test(txId ?? "") ||
    !EVEN_HEX.test(withdrawalKey ?? "")
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

export const prepareWithdrawnReferenceInputArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<WithdrawnReferenceInputArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-reference-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnReferenceInput({
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
    committedWithdrawalKeyBytes(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    committedWithdrawalKeyBytes(prepared.withdrawal.id) !==
      selected.withdrawalKey
  ) {
    throw new Error("withdrawn-reference-input public evidence changed");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: WITHDRAWN_REFERENCE_INPUT_ARTIFACT,
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
  }) as WithdrawnReferenceInputArtifact;
  await admitWithdrawnReferenceInputArtifact(artifact);
  return Object.freeze(artifact);
};

export type WithdrawnReferenceInputWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnReferenceInputContracts;
  category: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["catalogue"];
  referenceScripts: WithdrawnReferenceInputWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (action: FraudProofWorkflowAction) => {
  const input = exactJournalRecord(
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
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("withdrawn-reference-input publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
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
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"withdrawnReferenceInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "withdrawnReferenceInput",
  prepare: async ({ evidence, classification }) =>
    await prepareWithdrawnReferenceInputArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitWithdrawnReferenceInputArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-reference-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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

export type ManifestBoundWithdrawnReferenceInputWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WithdrawnReferenceInputWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundWithdrawnReferenceInputWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">;
  l1: FraudProofFamilyL1ObservationPort<"withdrawnReferenceInput">;
  transactions: LinearFamilyTransactionPort<"withdrawnReferenceInput">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundWithdrawnReferenceInputWorkflow = async (
  config: ManifestBoundWithdrawnReferenceInputWorkflowConfig,
): Promise<ManifestBoundWithdrawnReferenceInputWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const contracts: WithdrawnReferenceInputContracts = Object.freeze({
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
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const ref = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo,
    });
  const referenceScripts: WithdrawnReferenceInputWorkflowReferenceScripts =
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "withdrawnReferenceInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withFieldCarriagePrerequisite({
    category: "withdrawnReferenceInput",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: "withdrawnReferenceInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitWithdrawnReferenceInputArtifact(
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
        } satisfies FieldCarriageRequirement;
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
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundWithdrawnReferenceInputWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWithdrawnReferenceInputWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["withdrawnReferenceInput"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
