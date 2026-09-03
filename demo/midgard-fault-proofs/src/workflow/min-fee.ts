import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIN_FEE_VIOLATION_ID,
  MinFeeStep02Datum,
  minimumFeeFromProofSource,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalEvidenceBuilderInput,
  prepareMinFeeFromCanonicalEvidence,
} from "../evidence/prepare-from-evidence.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { MinFeeContracts } from "../min-fee-contracts.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitMinFeeInit } from "../submit-min-fee-init.js";
import { submitMinFeeStep01 } from "../submit-min-fee-step-01.js";
import {
  type MinFeeFieldItemCbors,
  submitMinFeeStep02,
} from "../submit-min-fee-step-02.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
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
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const MIN_FEE_ARTIFACT =
  "midgard-production-min-fee-artifact-v1" as const;

export type MinFeeArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MIN_FEE_ARTIFACT;
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

type AdmittedMinFeeArtifact = Readonly<{
  artifact: MinFeeArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  fieldItemCbors: MinFeeFieldItemCbors;
  fieldPlans: readonly FaultProofFieldOpeningPlan[];
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

const parseFieldItems = (value: unknown): MinFeeFieldItemCbors => {
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
  }) as unknown as MinFeeFieldItemCbors;
};

const parseArtifact = (
  value: unknown,
): Omit<AdmittedMinFeeArtifact, "fieldPlans"> => {
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
    parsed.schemaVersion !== MIN_FEE_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("min-fee production artifact identity changed");
  }
  const artifact: MinFeeArtifact = Object.freeze({
    schemaVersion: MIN_FEE_ARTIFACT,
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
  const compact = decodeMidgardNativeTxCompact(
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

export const admitMinFeeArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedMinFeeArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("min-fee carriage owner must be a 28-byte key hash");
  }
  const parsed = parseArtifact(value);
  const fieldPlans = parsed.fieldItemCbors.map((items, fieldIndex) =>
    planFaultProofFieldOpening({
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
  const boundary = minimumFeeFromProofSource({
    source: {
      compactCbor: Buffer.from(parsed.artifact.nativeTxCompactCbor, "hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        witnessSetCore(parsed.witnessSet),
      ),
      fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
        fieldPlans.map((plan) => plan.preimage.length),
      ),
    },
    minFeeA: BigInt(parsed.artifact.minFeeA),
    minFeeB: BigInt(parsed.artifact.minFeeB),
  });
  const fee = parsed.inclusion.nativeTx.body.fee;
  const expectedDetection = `${MIN_FEE_VIOLATION_ID}:${parsed.artifact.position.toString()}:${parsed.artifact.nativeTxId}:${fee.toString()}:${boundary.minimumFee.toString()}`;
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
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
): string => {
  if (
    classification.category !== "minFee" ||
    classification.selected.violationId !== MIN_FEE_VIOLATION_ID
  ) {
    throw new Error("min-fee workflow received another classification");
  }
  const fields = classification.selected.detectionId.split(":");
  if (
    fields.length !== 5 ||
    fields[0] !== MIN_FEE_VIOLATION_ID ||
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

export const prepareMinFeeArtifact = async ({
  evidence,
  classification,
  categoryId,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
  readonly categoryId: string;
}): Promise<MinFeeArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("min-fee classification differs from canonical evidence");
  }
  const prepared = await prepareMinFeeFromCanonicalEvidence({
    evidence,
    txId: selectedTxId(classification),
    categoryId,
  });
  const detectionId = `${MIN_FEE_VIOLATION_ID}:${classification.selected.position.toString()}:${prepared.tx.nodeTxId}:${prepared.tx.fee.toString()}:${prepared.tx.minimumFee.toString()}`;
  if (classification.selected.detectionId !== detectionId) {
    throw new Error("min-fee prepared evidence changed classification");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: MIN_FEE_ARTIFACT,
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
  }) as MinFeeArtifact;
  admitMinFeeArtifact(artifact);
  return Object.freeze(artifact);
};

export type MinFeeWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
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
  network: FraudProofWorkflowDeploymentBinding<"minFee">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MinFeeContracts;
  category: FraudProofWorkflowDeploymentBinding<"minFee">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"minFee">["catalogue"];
  referenceScripts: MinFeeWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
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
  readonly config: BoundConfig;
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
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            )
          ) {
            throw new Error(
              "min-fee removal changed its authenticated queue input",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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

const resolveFieldCarriages = async (
  config: BoundConfig,
  admitted: AdmittedMinFeeArtifact,
): Promise<
  Readonly<{ publications: readonly UTxO[]; certificates: readonly UTxO[] }>
> => {
  const publications: UTxO[] = [];
  const certificates: UTxO[] = [];
  for (const plan of admitted.fieldPlans) {
    const resolvedPublications =
      await resolveFaultProofFieldCarriagePublications({
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
    const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"minFee"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "minFee",
  prepare: async ({ evidence, classification }) =>
    await prepareMinFeeArtifact({
      evidence,
      classification,
      categoryId: config.category.categoryId,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitMinFeeArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("min-fee artifact changed its manifest-bound header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
      const carriages = await resolveFieldCarriages(config, admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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

export type ManifestBoundMinFeeWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MinFeeWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinFeeWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"minFee">;
  l1: FraudProofFamilyL1ObservationPort<"minFee">;
  transactions: LinearFamilyTransactionPort<"minFee">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundMinFeeWorkflow = async (
  config: ManifestBoundMinFeeWorkflowConfig,
): Promise<ManifestBoundMinFeeWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "minFee",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [FraudProofComputationThreadStepDatum, MinFeeStep02Datum],
  });
  assertManifestBoundWorkflowSigner({
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
  const references: MinFeeWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMinFee",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMinFeeStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const contracts: MinFeeContracts = Object.freeze({
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
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
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
    const prerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "minFee",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: ({ action, artifact }) => {
        const input = record(action.input, "min-fee prerequisite action");
        if (input.stage !== "step_02") return null;
        const admitted = admitMinFeeArtifact(
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
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
            witnessSetCore(admitted.witnessSet),
          ).toString("hex"),
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    });
    adapter = withFieldCarriagePrerequisite({
      category: "minFee",
      base: adapter,
      prerequisite,
    });
  }
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "minFee",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = record(action.input, "min-fee proof prerequisite action");
      return input.stage === "step_01"
        ? admitMinFeeArtifact(artifact, config.signer.paymentKeyHash).artifact
            .txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "minFee",
    base: adapter,
    prerequisite: proofPrerequisite,
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

export const runOrResumeManifestBoundMinFeeWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinFeeWorkflow;
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
    replayer: MIN_FEE_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["minFee"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
