import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  encodeMidgardTxInputCanonical,
  FraudProofComputationThreadStepDatum,
  isWithdrawnInputViolation,
  type Proof,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_INPUT_VIOLATION_ID,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep03Datum,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import { prepareWithdrawnInputFromCanonicalEvidence } from "../prepare-withdrawn-input.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { WithdrawnInputContracts } from "../withdrawn-input/contracts-v1.js";
import { submitWithdrawnInputInit } from "../withdrawn-input/submit-withdrawn-input-init.js";
import { submitWithdrawnInputStep01 } from "../withdrawn-input/submit-withdrawn-input-step-01.js";
import { submitWithdrawnInputStep02 } from "../withdrawn-input/submit-withdrawn-input-step-02.js";
import { submitWithdrawnInputStep03 } from "../withdrawn-input/submit-withdrawn-input-step-03.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassification } from "./classification-v1.js";
import { WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./production-linear-family-adapter-v1.js";
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
} from "./production-native-index-artifact-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export const WITHDRAWN_INPUT_ARTIFACT =
  "midgard-production-withdrawn-input-artifact-v1" as const;

type InputJson = Readonly<{ tx_id: string; output_index: string }>;

export type WithdrawnInputArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof WITHDRAWN_INPUT_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    spendInputs: readonly InputJson[];
    badInputIndex: number;
    withdrawalIndex: number;
    withdrawalMembershipCbor: string;
  }>;

type AdmittedWithdrawnInputArtifact = Readonly<{
  artifact: WithdrawnInputArtifact;
  inclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  spendInputs: ReturnType<typeof admitTxInputList>["inputs"];
  withdrawalMembership: WithdrawalSourceMembershipProofV1;
  spendPlan: FaultProofFieldOpeningPlan;
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
    !HEX_32.test(membership.root) ||
    !HEX_32.test(membership.phas_root)
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
      Buffer.from(committedWithdrawalKeyBytes(membership.key), "hex"),
      Buffer.from(committedWithdrawalValueBytes(membership.value), "hex"),
      proofSteps(membership.proof),
    ).verify(true);
  } catch {
    throw new Error("withdrawn-input membership proof cannot be replayed");
  }
  if (opened === null || opened.toString("hex") !== membership.phas_root) {
    throw new Error("withdrawn-input membership does not open its PHAS root");
  }
};

export const admitWithdrawnInputArtifact = async (
  value: unknown,
  carriageOwner = "00".repeat(28),
): Promise<AdmittedWithdrawnInputArtifact> => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("withdrawn-input carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
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
    parsed.schemaVersion !== WITHDRAWN_INPUT_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("withdrawn-input artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "withdrawn-input header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "withdrawn-input position",
  );
  const badInputIndex = safeNaturalNumber(
    parsed.badInputIndex,
    "withdrawn-input bad input index",
  );
  const withdrawalIndex = safeNaturalNumber(
    parsed.withdrawalIndex,
    "withdrawn-input withdrawal index",
  );
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "withdrawn-input transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("withdrawn-input transaction is not accepted");
  }
  const spendInputs = admitTxInputList(
    parsed.spendInputs,
    "withdrawn-input spend inputs",
  );
  const selectedInput = spendInputs.inputs[badInputIndex];
  if (selectedInput === undefined) {
    throw new Error("withdrawn-input selected input is out of range");
  }
  const withdrawalMembershipCbor = canonicalHex(
    parsed.withdrawalMembershipCbor,
    EVEN_HEX,
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
    !isWithdrawnInputViolation({
      input: selectedInput,
      withdrawal: withdrawalMembership.value,
    })
  ) {
    throw new Error("withdrawn-input artifact does not prove its violation");
  }
  const expectedDetection = `${WITHDRAWN_INPUT_VIOLATION_ID}:${position.toString()}:${badInputIndex.toString()}:${withdrawalIndex.toString()}:${tx.artifact.nativeTxId}:${committedWithdrawalKeyBytes(withdrawalMembership.key)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("withdrawn-input detection identity changed");
  }
  const spendPlan = planFaultProofFieldOpening({
    fieldIndex: 0,
    anchorTxId: tx.artifact.nativeTxId,
    nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
    itemCbors: spendInputs.inputs.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "withdrawn-input artifact spend inputs",
  });
  const artifact = Object.freeze({
    schemaVersion: WITHDRAWN_INPUT_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputs: spendInputs.json,
    badInputIndex,
    withdrawalIndex,
    withdrawalMembershipCbor,
  }) satisfies WithdrawnInputArtifact;
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
    CanonicalBlockClassification,
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
    classification.selected.violationId !== WITHDRAWN_INPUT_VIOLATION_ID ||
    violationId !== WITHDRAWN_INPUT_VIOLATION_ID ||
    rest.length !== 0 ||
    !NATURAL_DECIMAL.test(positionValue ?? "") ||
    !NATURAL_DECIMAL.test(inputValue ?? "") ||
    !NATURAL_DECIMAL.test(withdrawalIndexValue ?? "") ||
    !HEX_32.test(txId ?? "") ||
    !EVEN_HEX.test(withdrawalKey ?? "")
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

export const prepareWithdrawnInputArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<WithdrawnInputArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnInputFromCanonicalEvidence({
    evidence,
    badTxId: selected.txId,
    badInputIndex: selected.badInputIndex,
  });
  const selectedKey = committedWithdrawalKeyBytes(prepared.withdrawalId);
  const selectedWithdrawal =
    evidence.reconstruction.withdrawals[selected.withdrawalIndex];
  if (
    selectedWithdrawal === undefined ||
    committedWithdrawalKeyBytes(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    selectedKey !== selected.withdrawalKey ||
    prepared.badTxInclusion.nativeTxId !== selected.txId
  ) {
    throw new Error("withdrawn-input selected public evidence changed");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: WITHDRAWN_INPUT_ARTIFACT,
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
  }) as WithdrawnInputArtifact;
  await admitWithdrawnInputArtifact(artifact);
  return Object.freeze(artifact);
};

export type WithdrawnInputWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnInputContracts;
  category: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["catalogue"];
  referenceScripts: WithdrawnInputWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"withdrawnInput">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (action: FraudProofWorkflowAction) => {
  const input = exactJournalRecord(
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
        fraudCategory: "withdrawnInput",
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
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("withdrawn-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"withdrawnInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "withdrawnInput",
  prepare: async ({ evidence, classification }) =>
    await prepareWithdrawnInputArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitWithdrawnInputArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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

export type ManifestBoundWithdrawnInputWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WithdrawnInputWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundWithdrawnInputWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"withdrawnInput">;
  l1: FraudProofFamilyL1ObservationPort<"withdrawnInput">;
  transactions: LinearFamilyTransactionPort<"withdrawnInput">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundWithdrawnInputWorkflow = async (
  config: ManifestBoundWithdrawnInputWorkflowConfig,
): Promise<ManifestBoundWithdrawnInputWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const contracts: WithdrawnInputContracts = Object.freeze({
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
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo,
    });
  const referenceScripts: WithdrawnInputWorkflowReferenceScripts =
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "withdrawnInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withFieldCarriagePrerequisite({
    category: "withdrawnInput",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: "withdrawnInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitWithdrawnInputArtifact(
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
        } satisfies FieldCarriageRequirement;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  adapter = withProofChunkPrerequisite({
    category: "withdrawnInput",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePort({
      category: "withdrawnInput",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      proofCborForAction: ({ action, artifact }) => {
        if (actionInput(action).stage !== "step_01") return null;
        const record = exactJournalRecord(
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
        return admitNativeInclusionArtifact(
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
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundWithdrawnInputWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWithdrawnInputWorkflow;
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
    replayer: WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["withdrawnInput"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
