import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIN_FEE_VIOLATION_ID_V1,
  MinFeeStep02Datum,
  minimumFeeFromProofSourceV1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalEvidenceBuilderInputV1,
  prepareMinFeeFromCanonicalEvidenceV1,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type { MinFeeContractsV1 } from "../min-fee-contracts-v1.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitMinFeeInit } from "../submit-min-fee-init.js";
import { submitMinFeeStep01 } from "../submit-min-fee-step-01.js";
import {
  type MinFeeFieldItemCborsV1,
  submitMinFeeStep02,
} from "../submit-min-fee-step-02.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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

export const PRODUCTION_MIN_FEE_ARTIFACT_V1 =
  "midgard-production-min-fee-artifact-v1" as const;

export type ProductionMinFeeArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_MIN_FEE_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProofCbor: string;
    witnessSet: Readonly<{
      addr_tx_wits_hash: string;
      script_tx_wits_hash: string;
      redeemer_tx_wits_hash: string;
    }>;
    fieldItemCbors: readonly (readonly string[])[];
    minFeeA: string;
    minFeeB: string;
    fee: string;
    canonicalTxSize: string;
    minimumFee: string;
    shortfall: string;
  }>;

type AdmittedProductionMinFeeArtifactV1 = Readonly<{
  artifact: ProductionMinFeeArtifactV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  fieldItemCbors: MinFeeFieldItemCborsV1;
  fieldPlans: readonly FaultProofFieldOpeningPlanV1[];
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const FIELD_COUNT = 9;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const naturalString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

const witnessSetCore = (witnessSet: NativeTxWitnessSetCompact) => ({
  addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
  scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
  redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
});

const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
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

const parseWitnessSet = (value: unknown): NativeTxWitnessSetCompact => {
  const parsed = exact(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "min-fee witness set",
  );
  return {
    addr_tx_wits_hash: canonicalHex(
      parsed.addr_tx_wits_hash,
      HEX_32,
      "address-witness hash",
    ),
    script_tx_wits_hash: canonicalHex(
      parsed.script_tx_wits_hash,
      HEX_32,
      "script-witness hash",
    ),
    redeemer_tx_wits_hash: canonicalHex(
      parsed.redeemer_tx_wits_hash,
      HEX_32,
      "redeemer-witness hash",
    ),
  };
};

const parseFieldItems = (value: unknown): MinFeeFieldItemCborsV1 => {
  if (!Array.isArray(value) || value.length !== FIELD_COUNT) {
    throw new Error("min-fee artifact requires exactly nine field item lists");
  }
  return value.map((items, fieldIndex) => {
    if (!Array.isArray(items)) {
      throw new Error(
        `min-fee field ${fieldIndex.toString()} items must be an array`,
      );
    }
    return items.map((item, itemIndex) =>
      Buffer.from(
        canonicalHex(
          item,
          EVEN_HEX,
          `min-fee field ${fieldIndex.toString()} item ${itemIndex.toString()}`,
        ),
        "hex",
      ),
    );
  }) as unknown as MinFeeFieldItemCborsV1;
};

const parseArtifact = (
  value: unknown,
): Omit<AdmittedProductionMinFeeArtifactV1, "fieldPlans"> => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
      "witnessSet",
      "fieldItemCbors",
      "minFeeA",
      "minFeeB",
      "fee",
      "canonicalTxSize",
      "minimumFee",
      "shortfall",
    ],
    "min-fee production artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_MIN_FEE_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("min-fee production artifact identity changed");
  }
  const artifact: ProductionMinFeeArtifactV1 = Object.freeze({
    schemaVersion: PRODUCTION_MIN_FEE_ARTIFACT_V1,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "min-fee header"),
    detectionId: parsed.detectionId,
    position: naturalNumber(parsed.position, "min-fee position"),
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, "min-fee tx id"),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "min-fee compact tx",
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "min-fee transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "min-fee transactions PHAS root",
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "min-fee transaction proof",
    ),
    witnessSet: parseWitnessSet(parsed.witnessSet),
    fieldItemCbors: Array.isArray(parsed.fieldItemCbors)
      ? parsed.fieldItemCbors.map((items) =>
          Array.isArray(items) ? Object.freeze([...items] as string[]) : [],
        )
      : [],
    minFeeA: naturalString(parsed.minFeeA, "minFeeA"),
    minFeeB: naturalString(parsed.minFeeB, "minFeeB"),
    fee: naturalString(parsed.fee, "fee"),
    canonicalTxSize: naturalString(parsed.canonicalTxSize, "canonicalTxSize"),
    minimumFee: naturalString(parsed.minimumFee, "minimumFee"),
    shortfall: naturalString(parsed.shortfall, "shortfall"),
  });
  const fieldItemCbors = parseFieldItems(parsed.fieldItemCbors);
  const compact = decodeMidgardNativeTxCompactV1(
    Buffer.from(artifact.nativeTxCompactCbor, "hex"),
  );
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error("min-fee transaction proof cannot be replayed");
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error("min-fee transaction proof does not open its PHAS root");
  }
  return Object.freeze({
    artifact,
    inclusion,
    witnessSet: artifact.witnessSet,
    fieldItemCbors,
  });
};

export const admitProductionMinFeeArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedProductionMinFeeArtifactV1 => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("min-fee carriage owner must be a 28-byte key hash");
  }
  const parsed = parseArtifact(value);
  const fieldPlans = parsed.fieldItemCbors.map((items, fieldIndex) =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: parsed.artifact.nativeTxId,
      nativeTxCompactCbor: parsed.artifact.nativeTxCompactCbor,
      itemCbors: items,
      owner: carriageOwner,
      publish: false,
      ...(fieldIndex < 6
        ? {}
        : {
            witnessSet: parsed.witnessSet,
            anchorWitnessSetHash: parsed.inclusion.nativeTx.witness_set_hash,
          }),
      label: `min-fee artifact field ${fieldIndex.toString()}`,
    }),
  );
  const boundary = minimumFeeFromProofSourceV1({
    source: {
      compactCbor: Buffer.from(parsed.artifact.nativeTxCompactCbor, "hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
        witnessSetCore(parsed.witnessSet),
      ),
      fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengthsV1(
        fieldPlans.map((plan) => plan.preimage.length),
      ),
    },
    minFeeA: BigInt(parsed.artifact.minFeeA),
    minFeeB: BigInt(parsed.artifact.minFeeB),
  });
  const fee = parsed.inclusion.nativeTx.body.fee;
  const expectedDetection = `${MIN_FEE_VIOLATION_ID_V1}:${parsed.artifact.position.toString()}:${parsed.artifact.nativeTxId}:${fee.toString()}:${boundary.minimumFee.toString()}`;
  if (
    fee >= boundary.minimumFee ||
    parsed.artifact.fee !== fee.toString() ||
    parsed.artifact.canonicalTxSize !== boundary.canonicalTxSize.toString() ||
    parsed.artifact.minimumFee !== boundary.minimumFee.toString() ||
    parsed.artifact.shortfall !== (boundary.minimumFee - fee).toString() ||
    parsed.artifact.detectionId !== expectedDetection
  ) {
    throw new Error("min-fee artifact does not re-derive its exact violation");
  }
  return Object.freeze({ ...parsed, fieldPlans: Object.freeze(fieldPlans) });
};

const selectedTxId = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
): string => {
  if (
    classification.category !== "minFee" ||
    classification.selected.violationId !== MIN_FEE_VIOLATION_ID_V1
  ) {
    throw new Error("min-fee workflow received another classification");
  }
  const fields = classification.selected.detectionId.split(":");
  if (
    fields.length !== 5 ||
    fields[0] !== MIN_FEE_VIOLATION_ID_V1 ||
    !NATURAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    !NATURAL.test(fields[3] ?? "") ||
    !NATURAL.test(fields[4] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("min-fee classification identity is malformed");
  }
  return fields[2]!;
};

export const prepareProductionMinFeeArtifactV1 = async ({
  evidence,
  classification,
  categoryId,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
  readonly categoryId: string;
}): Promise<ProductionMinFeeArtifactV1> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("min-fee classification differs from canonical evidence");
  }
  const prepared = await prepareMinFeeFromCanonicalEvidenceV1({
    evidence,
    txId: selectedTxId(classification),
    categoryId,
  });
  const detectionId = `${MIN_FEE_VIOLATION_ID_V1}:${classification.selected.position.toString()}:${prepared.tx.nodeTxId}:${prepared.tx.fee.toString()}:${prepared.tx.minimumFee.toString()}`;
  if (classification.selected.detectionId !== detectionId) {
    throw new Error("min-fee prepared evidence changed classification");
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_MIN_FEE_ARTIFACT_V1,
    headerHash: prepared.headerHash,
    detectionId,
    position: Number(classification.selected.position),
    nativeTxId: prepared.tx.nodeTxId,
    nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
    l2TransactionSourceCbor: prepared.tx.txInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: prepared.transactionsPhasRoot,
    txMembershipProofCbor: prepared.tx.txInclusion.txMembershipProofCbor,
    witnessSet: prepared.tx.witnessSet,
    fieldItemCbors: prepared.tx.fieldItemCbors,
    minFeeA: prepared.minFeeA.toString(),
    minFeeB: prepared.minFeeB.toString(),
    fee: prepared.tx.fee.toString(),
    canonicalTxSize: prepared.tx.canonicalTxSize.toString(),
    minimumFee: prepared.tx.minimumFee.toString(),
    shortfall: prepared.tx.shortfall.toString(),
  }) as ProductionMinFeeArtifactV1;
  admitProductionMinFeeArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type MinFeeWorkflowReferenceScriptsV1 = Readonly<{
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
  network: FraudProofWorkflowDeploymentBindingV1<"minFee">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MinFeeContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"minFee">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"minFee">["catalogue"];
  referenceScripts: MinFeeWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "min-fee workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "minFee" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("min-fee workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`min-fee workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async ({
  config,
  input,
}: {
  readonly config: BoundConfigV1;
  readonly input: Readonly<Record<string, unknown>>;
}) => {
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
        fraudCategory: "minFee",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            )
          ) {
            throw new Error(
              "min-fee removal changed its authenticated queue input",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "min-fee removal omitted its authenticated proof token",
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

const resolveFieldCarriagesV1 = async (
  config: BoundConfigV1,
  admitted: AdmittedProductionMinFeeArtifactV1,
): Promise<
  Readonly<{ publications: readonly UTxO[]; certificates: readonly UTxO[] }>
> => {
  const publications: UTxO[] = [];
  const certificates: UTxO[] = [];
  for (const plan of admitted.fieldPlans) {
    const resolvedPublications =
      await resolveFaultProofFieldCarriagePublicationsV1({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
    if (resolvedPublications === undefined) {
      throw new Error(
        `min-fee field ${plan.fieldIndex.toString()} publication disappeared after authenticated prerequisite`,
      );
    }
    publications.push(...resolvedPublications);
    const certificate = await resolveFaultProofFieldPreimageCertificateV1({
      lucid: config.lucid,
      network: config.network,
      planned: plan,
      certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
    });
    if (plan.plan.tier === "Certified" && certificate === undefined) {
      throw new Error(
        `min-fee field ${plan.fieldIndex.toString()} certificate disappeared after authenticated prerequisite`,
      );
    }
    if (certificate !== undefined) certificates.push(certificate);
  }
  return Object.freeze({
    publications: Object.freeze(publications),
    certificates: Object.freeze(certificates),
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"minFee"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "minFee",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionMinFeeArtifactV1({
      evidence,
      classification,
      categoryId: config.category.categoryId,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionMinFeeArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("min-fee artifact changed its manifest-bound header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinFeeInit({
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
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinFeeStep01({
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
      const carriages = await resolveFieldCarriagesV1(config, admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinFeeStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              witnessSet: admitted.witnessSet,
              fieldItemCbors: admitted.fieldItemCbors,
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              certificateUtxos: carriages.certificates,
              existingPublicationUtxos: carriages.publications,
              publishMissingCarriages: false,
              publishCarriages: false,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval({ config, input });
    }
    throw new Error(
      `min-fee workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundMinFeeWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MinFeeWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinFeeWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"minFee">;
  l1: FraudProofFamilyL1ObservationPortV1<"minFee">;
  transactions: ProductionLinearFamilyTransactionPortV1<"minFee">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundMinFeeWorkflowV1 = async (
  config: ManifestBoundMinFeeWorkflowConfigV1,
): Promise<ManifestBoundMinFeeWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "minFee",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [FraudProofComputationThreadStepDatum, MinFeeStep02Datum],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.minFee;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error("min-fee manifest binding omitted required contracts");
  }
  const references: MinFeeWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMinFee",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMinFeeStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const contracts: MinFeeContractsV1 = Object.freeze({
    steps: chain.steps,
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error("min-fee raw-L1 authority omitted publication observer");
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
    referenceScripts: references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "minFee",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  // Compose one prerequisite per field. Wrapping from field 0 upward makes
  // field 0 the innermost and therefore the first action observed, followed
  // deterministically by fields 1..8 before the proof step can execute.
  for (let fieldIndex = 0; fieldIndex < FIELD_COUNT; fieldIndex += 1) {
    const index = fieldIndex;
    const prerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "minFee",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: ({ action, artifact }) => {
        const input = record(action.input, "min-fee prerequisite action");
        if (input.stage !== "step_02") return null;
        const admitted = admitProductionMinFeeArtifactV1(
          artifact,
          config.signer.paymentKeyHash,
        );
        const planned = admitted.fieldPlans[index];
        if (planned === undefined) {
          throw new Error(`min-fee artifact omitted field ${index.toString()}`);
        }
        return {
          planned,
          compactCbor: admitted.artifact.nativeTxCompactCbor,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
            witnessSetCore(admitted.witnessSet),
          ).toString("hex"),
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies ProductionFieldCarriageRequirementV1;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    });
    adapter = withProductionFieldCarriagePrerequisiteV1({
      category: "minFee",
      base: adapter,
      prerequisite,
    });
  }
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "minFee",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = record(action.input, "min-fee proof prerequisite action");
      return input.stage === "step_01"
        ? admitProductionMinFeeArtifactV1(
            artifact,
            config.signer.paymentKeyHash,
          ).artifact.txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "minFee",
    base: adapter,
    prerequisite: proofPrerequisite,
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

export const runOrResumeManifestBoundMinFeeWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinFeeWorkflowV1;
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
    replayer: MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["minFee"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
