import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  InputSetUniquenessStep02Datum,
  MIDGARD_FIELD_INDEX_V1,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type { InputSetUniquenessContractsV1 } from "../input-set-uniqueness/contracts-v1.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID_V1,
  type InputSetUniquenessClaimV1,
  scanInputSetUniquenessV1,
} from "../input-set-uniqueness/scan-v1.js";
import { submitInputSetUniquenessInit } from "../input-set-uniqueness/submit-input-set-uniqueness-init.js";
import { submitInputSetUniquenessStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-01.js";
import { submitInputSetUniquenessStep02 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-02.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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

export const PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1 =
  "midgard-production-input-set-uniqueness-artifact-v1" as const;

type ClaimJsonV1 =
  | Readonly<{
      kind: "duplicateSpendInputs" | "duplicateReferenceInputs";
      firstIndex: string;
      secondIndex: string;
    }>
  | Readonly<{
      kind: "spendReferenceOverlap";
      spendIndex: string;
      referenceIndex: string;
    }>;

export type ProductionInputSetUniquenessArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    claim: ClaimJsonV1;
  }>;

type AdmittedArtifactV1 = Readonly<{
  artifact: ProductionInputSetUniquenessArtifactV1;
  inclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  claim: InputSetUniquenessClaimV1;
  spendPlan: FaultProofFieldOpeningPlanV1 | null;
  referencePlan: FaultProofFieldOpeningPlanV1 | null;
}>;

const inputItems = (
  tx: MidgardNativeTxFullV1,
  field: "spend" | "reference",
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    field === "spend"
      ? tx.body.spendInputsPreimageCbor
      : tx.body.referenceInputsPreimageCbor,
    `${field} inputs`,
  ).map((item) => Buffer.from(item).toString("hex"));

const claimJson = (claim: InputSetUniquenessClaimV1): ClaimJsonV1 =>
  claim.kind === "spendReferenceOverlap"
    ? Object.freeze({
        kind: claim.kind,
        spendIndex: claim.spendIndex.toString(),
        referenceIndex: claim.referenceIndex.toString(),
      })
    : Object.freeze({
        kind: claim.kind,
        firstIndex: claim.firstIndex.toString(),
        secondIndex: claim.secondIndex.toString(),
      });

const claimIdentity = (claim: InputSetUniquenessClaimV1): string =>
  claim.kind === "spendReferenceOverlap"
    ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
    : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;

const parseClaim = (value: unknown): InputSetUniquenessClaimV1 => {
  const record = exactJournalRecordV1(
    value,
    typeof value === "object" && value !== null && "kind" in value
      ? (value as { readonly kind?: unknown }).kind === "spendReferenceOverlap"
        ? ["kind", "spendIndex", "referenceIndex"]
        : ["kind", "firstIndex", "secondIndex"]
      : [],
    "input-set-uniqueness claim",
  );
  const natural = (field: string): bigint => {
    const item = record[field];
    if (typeof item !== "string" || !NATURAL_DECIMAL_V1.test(item)) {
      throw new Error(`input-set-uniqueness claim ${field} is malformed`);
    }
    return BigInt(item);
  };
  if (record.kind === "duplicateSpendInputs") {
    return {
      kind: "duplicateSpendInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "duplicateReferenceInputs") {
    return {
      kind: "duplicateReferenceInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "spendReferenceOverlap") {
    return {
      kind: "spendReferenceOverlap",
      spendIndex: natural("spendIndex"),
      referenceIndex: natural("referenceIndex"),
    };
  }
  throw new Error("input-set-uniqueness claim kind is unsupported");
};

const parseItemList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) => {
      const parsed = canonicalHexV1(
        item,
        EVEN_HEX_V1,
        `${label}[${index.toString()}]`,
      );
      if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(parsed)) {
        throw new Error(`${label}[${index.toString()}] is not an out-ref item`);
      }
      return parsed;
    }),
  );
};

export const admitProductionInputSetUniquenessArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifactV1 => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "spendInputItemCbors",
      "referenceInputItemCbors",
      "claim",
    ],
    "input-set-uniqueness artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "input-set-uniqueness header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "input-set-uniqueness position",
  );
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "input-set-uniqueness transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("input-set-uniqueness transaction is not accepted");
  }
  const spends = parseItemList(
    parsed.spendInputItemCbors,
    "input-set-uniqueness spend inputs",
  );
  const references = parseItemList(
    parsed.referenceInputItemCbors,
    "input-set-uniqueness reference inputs",
  );
  const claim = parseClaim(parsed.claim);
  const rederived = scanInputSetUniquenessV1({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  });
  if (
    !rederived.some(
      (candidate) => claimIdentity(candidate) === claimIdentity(claim),
    )
  ) {
    throw new Error(
      "input-set-uniqueness artifact does not re-derive its claim",
    );
  }
  const expectedDetection = `${INPUT_SET_UNIQUENESS_VIOLATION_ID_V1}:${position.toString()}:${tx.artifact.nativeTxId}:${claimIdentity(claim)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness detection identity changed");
  }
  const plan = (
    fieldIndex: number,
    items: readonly string[],
  ): FaultProofFieldOpeningPlanV1 =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: tx.artifact.nativeTxId,
      nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness artifact",
    });
  const spendPlan =
    claim.kind === "duplicateReferenceInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX_V1.spendInputs, spends);
  const referencePlan =
    claim.kind === "duplicateSpendInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX_V1.referenceInputs, references);
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) satisfies ProductionInputSetUniquenessArtifactV1;
  return Object.freeze({
    artifact,
    inclusion: tx.inclusion,
    claim,
    spendPlan,
    referencePlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  if (
    classification.category !== "inputSetUniqueness" ||
    classification.selected.violationId !==
      INPUT_SET_UNIQUENESS_VIOLATION_ID_V1 ||
    fields.length !== 6 ||
    fields[0] !== INPUT_SET_UNIQUENESS_VIOLATION_ID_V1 ||
    !NATURAL_DECIMAL_V1.test(fields[1] ?? "") ||
    !HEX_32_V1.test(fields[2] ?? "") ||
    ![
      "duplicateSpendInputs",
      "duplicateReferenceInputs",
      "spendReferenceOverlap",
    ].includes(fields[3] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[5] ?? "") ||
    !Number.isSafeInteger(position) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-set-uniqueness classification is malformed");
  }
  return Object.freeze({ position, txId: fields[2]! });
};

export const prepareProductionInputSetUniquenessArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionInputSetUniquenessArtifactV1> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("input-set-uniqueness classification changed header");
  }
  const selected = selectedIdentity(classification);
  const decoded = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const tx = decoded.find((candidate) => candidate.nodeTxId === selected.txId);
  if (tx === undefined) {
    throw new Error("input-set-uniqueness selected transaction disappeared");
  }
  const spends = inputItems(tx.nativeTx, "spend");
  const references = inputItems(tx.nativeTx, "reference");
  const claim = scanInputSetUniquenessV1({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  }).find((candidate) =>
    classification.selected.detectionId.endsWith(claimIdentity(candidate)),
  );
  if (claim === undefined) {
    throw new Error("input-set-uniqueness selected claim disappeared");
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: tx.nodeTxId,
      nativeTxCompactCbor: tx.nativeCompactCbor,
      l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: requireProof(
        trie,
        transactionSourceTrieItemV1(tx).key,
        "input-set-uniqueness transaction",
      ),
    },
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) as ProductionInputSetUniquenessArtifactV1;
  admitProductionInputSetUniquenessArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type InputSetUniquenessWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
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
  network: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: InputSetUniquenessContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["catalogue"];
  referenceScripts: InputSetUniquenessWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const record = (value: unknown, label: string) =>
  exactJournalRecordV1(
    value,
    typeof value === "object" && value !== null ? Object.keys(value) : [],
    label,
  );

const actionInput = (action: FraudProofWorkflowActionV1) => {
  const input = record(action.input, "input-set-uniqueness action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "inputSetUniqueness" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("input-set-uniqueness action identity changed");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-set-uniqueness action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfigV1,
  plan: FaultProofFieldOpeningPlanV1 | null,
) => {
  if (plan === null)
    return Object.freeze({ publications: [], certificate: undefined });
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("input-set-uniqueness field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("input-set-uniqueness field certificate disappeared");
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
        fraudCategory: "inputSetUniqueness",
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
              "input-set-uniqueness removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease ? { mutationLease } : {}),
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"inputSetUniqueness"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "inputSetUniqueness",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionInputSetUniquenessArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionInputSetUniquenessArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-set-uniqueness artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessInit({
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
            await submitInputSetUniquenessStep01({
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
      const spend = await resolveField(config, admitted.spendPlan);
      const reference = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              claim: admitted.claim,
              nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedSpendCarriageUtxos: spend.publications,
              ...(spend.certificate === undefined
                ? {}
                : { spendCertificateUtxo: spend.certificate }),
              publishedReferenceCarriageUtxos: reference.publications,
              ...(reference.certificate === undefined
                ? {}
                : { referenceCertificateUtxo: reference.certificate }),
              publishMissingCarriage: false,
              referenceScriptUtxo: config.referenceScripts.steps[1],
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
      `unsupported input-set-uniqueness stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputSetUniquenessWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InputSetUniquenessWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInputSetUniquenessWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">;
  l1: FraudProofFamilyL1ObservationPortV1<"inputSetUniqueness">;
  transactions: ProductionLinearFamilyTransactionPortV1<"inputSetUniqueness">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundInputSetUniquenessWorkflowV1 = async (
  config: ManifestBoundInputSetUniquenessWorkflowConfigV1,
): Promise<ManifestBoundInputSetUniquenessWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "inputSetUniqueness",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      InputSetUniquenessStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error("input-set-uniqueness manifest omitted certificate policy");
  }
  const certificate = binding.fieldPreimageCertificate;
  const chain = binding.resolvedContracts.contracts.inputSetUniqueness;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    stateQueuePolicyId === undefined ||
    chain === undefined ||
    chain.steps.length !== 2
  ) {
    throw new Error("input-set-uniqueness deployment chain is incomplete");
  }
  const contracts: InputSetUniquenessContractsV1 = {
    steps: [chain.steps[0]!, chain.steps[1]!],
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
  };
  const ref = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  const references: InputSetUniquenessWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        ref("fraudProofInputSetUniqueness", config.referenceScripts.steps[0]),
        ref(
          "fraudProofInputSetUniquenessStep02",
          config.referenceScripts.steps[1],
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
    throw new Error("input-set-uniqueness raw-L1 omitted publications");
  }
  const publications = l1.publications;
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
    referenceScripts: references,
    certificate,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "inputSetUniqueness",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = (
    selector: (
      artifact: AdmittedArtifactV1,
    ) => FaultProofFieldOpeningPlanV1 | null,
  ) =>
    createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      requirementForAction: ({ action, artifact }) => {
        const input = actionInput(action);
        if (input.stage !== "step_02") return null;
        const plan = selector(
          admitProductionInputSetUniquenessArtifactV1(
            artifact,
            config.signer.paymentKeyHash,
          ),
        );
        return plan === null
          ? null
          : ({
              planned: plan,
              compactCbor: plan.nativeTxCompactCbor,
              certificate: {
                policyId: certificate.policyId,
                mintingScript: certificate.mintingScript,
                referenceScriptUtxo: references.fieldPreimageCertificateMint,
              },
            } satisfies ProductionFieldCarriageRequirementV1);
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.spendPlan),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.referencePlan),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePortV1({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      proofCborForAction: ({ action, artifact }) =>
        actionInput(action).stage === "step_01"
          ? admitProductionInputSetUniquenessArtifactV1(
              artifact,
              config.signer.paymentKeyHash,
            ).artifact.tx.txMembershipProofCbor
          : null,
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

export const runOrResumeManifestBoundInputSetUniquenessWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInputSetUniquenessWorkflowV1;
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
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["inputSetUniqueness"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
