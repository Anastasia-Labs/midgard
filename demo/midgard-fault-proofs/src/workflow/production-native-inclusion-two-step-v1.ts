import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompactV1 } from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  InvalidRangeStep02Datum,
  invalidRangeViolationReason,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  ZeroInputStep02Datum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  prepareInvalidRangeFromCanonicalEvidenceV1,
  prepareZeroInputFromCanonicalEvidenceV1,
} from "../evidence/prepare-from-evidence-v1.js";
import type { PreparedTxInclusionJson } from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInvalidRangeStep01 } from "../submit-invalid-range-step-01.js";
import { submitInvalidRangeStep02 } from "../submit-invalid-range-step-02.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { submitZeroInputStep01 } from "../submit-zero-input-step-01.js";
import { submitZeroInputStep02 } from "../submit-zero-input-step-02.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import {
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "./complete-replay-v1.js";
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

export const PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1 =
  "midgard-production-native-inclusion-two-step-artifact-v1" as const;

export type ProductionNativeInclusionTwoStepCategoryV1 =
  | "invalidRange"
  | "zeroInput";

export type ProductionNativeInclusionTwoStepArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1;
    category: ProductionNativeInclusionTwoStepCategoryV1;
    headerHash: string;
    detectionId: string;
    position: number;
    blockSlot: string | null;
    violationReason: string | null;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProofCbor: string;
  }>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

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

const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

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

const parseArtifact = (
  value: unknown,
): ProductionNativeInclusionTwoStepArtifactV1 => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "category",
      "headerHash",
      "detectionId",
      "position",
      "blockSlot",
      "violationReason",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
    ],
    "native-inclusion two-step artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1 ||
    (parsed.category !== "invalidRange" && parsed.category !== "zeroInput") ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("native-inclusion two-step artifact identity changed");
  }
  let blockSlot: string | null;
  let violationReason: string | null;
  if (parsed.category === "invalidRange") {
    if (
      typeof parsed.blockSlot !== "string" ||
      !NATURAL.test(parsed.blockSlot) ||
      typeof parsed.violationReason !== "string"
    ) {
      throw new Error(
        "native-inclusion two-step artifact family fields changed",
      );
    }
    blockSlot = parsed.blockSlot;
    violationReason = parsed.violationReason;
  } else {
    if (parsed.blockSlot !== null || parsed.violationReason !== null) {
      throw new Error(
        "native-inclusion two-step artifact family fields changed",
      );
    }
    blockSlot = null;
    violationReason = null;
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1,
    category: parsed.category,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "artifact header"),
    detectionId: parsed.detectionId,
    position: naturalNumber(parsed.position, "artifact position"),
    blockSlot,
    violationReason,
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, "artifact tx id"),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "artifact compact tx",
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "artifact transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "artifact transaction PHAS root",
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "artifact membership proof",
    ),
  });
};

export const admitProductionNativeInclusionTwoStepArtifactV1 = (
  value: unknown,
): Readonly<{
  artifact: ProductionNativeInclusionTwoStepArtifactV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const artifact = parseArtifact(value);
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
    throw new Error(
      "native-inclusion artifact membership proof cannot be replayed",
    );
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "native-inclusion artifact membership proof does not open its PHAS root",
    );
  }
  if (artifact.category === "invalidRange") {
    const reason = invalidRangeViolationReason({
      blockSlot: BigInt(artifact.blockSlot!),
      normalizedRange: normalizeNativeTxValidityRange(inclusion.nativeTx.body),
    });
    const expectedDetection = `invalid-range:${artifact.position.toString()}:${artifact.nativeTxId}:${reason ?? "none"}`;
    if (
      reason === null ||
      reason !== artifact.violationReason ||
      artifact.detectionId !== expectedDetection
    ) {
      throw new Error(
        "invalid-range artifact does not re-derive its selected violation",
      );
    }
  } else {
    if (
      !nativeTxBodyHasZeroInputViolation({ txBody: inclusion.nativeTx.body }) ||
      artifact.detectionId !==
        `zero-input:${artifact.position.toString()}:${artifact.nativeTxId}`
    ) {
      throw new Error(
        "zero-input artifact does not re-derive its selected violation",
      );
    }
  }
  return Object.freeze({ artifact, inclusion });
};

const selectedTxId = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
): string => {
  const prefix =
    classification.category === "invalidRange" ? "invalid-range" : "zero-input";
  const fields = classification.selected.detectionId.split(":");
  const expectedLength = classification.category === "invalidRange" ? 4 : 3;
  if (
    fields.length !== expectedLength ||
    fields[0] !== prefix ||
    !NATURAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error(`${classification.category} classification is malformed`);
  }
  return fields[2]!;
};

export const prepareProductionNativeInclusionTwoStepArtifactV1 = async <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>({
  category,
  evidence,
  classification,
}: {
  readonly category: Category;
  readonly evidence: Parameters<
    typeof prepareInvalidRangeFromCanonicalEvidenceV1
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionNativeInclusionTwoStepArtifactV1> => {
  if (
    classification.category !== category ||
    classification.headerHash !== evidence.headerHash
  ) {
    throw new Error(
      `${category} classification differs from canonical evidence`,
    );
  }
  const txId = selectedTxId(classification);
  let preparedHeaderHash: string;
  let preparedNodeTxId: string;
  let preparedInclusion: PreparedTxInclusionJson;
  let violationReason: string | null;
  let blockSlot: string | null;
  if (category === "invalidRange") {
    const prepared = await prepareInvalidRangeFromCanonicalEvidenceV1({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = prepared.tx.violationReason;
    blockSlot = prepared.blockSlot.toString();
  } else {
    const prepared = await prepareZeroInputFromCanonicalEvidenceV1({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = null;
    blockSlot = null;
  }
  if (
    classification.selected.detectionId !==
    (category === "invalidRange"
      ? `invalid-range:${classification.selected.position.toString()}:${preparedNodeTxId}:${violationReason}`
      : `zero-input:${classification.selected.position.toString()}:${preparedNodeTxId}`)
  ) {
    throw new Error(`${category} prepared transaction changed classification`);
  }
  if (classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${category} detection position exceeds journal encoding`);
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1,
    category,
    headerHash: preparedHeaderHash,
    detectionId: classification.selected.detectionId,
    position: Number(classification.selected.position),
    blockSlot,
    violationReason,
    nativeTxId: preparedInclusion.nativeTxId,
    nativeTxCompactCbor: preparedInclusion.nativeTxCompactCbor,
    l2TransactionSourceCbor: preparedInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: preparedInclusion.transactionsPhasRoot,
    txMembershipProofCbor: preparedInclusion.txMembershipProofCbor,
  }) as ProductionNativeInclusionTwoStepArtifactV1;
  admitProductionNativeInclusionTwoStepArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type NativeInclusionTwoStepWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
}>;

type BoundConfigV1<
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
> = Readonly<{
  category: Category;
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<Category>["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: NativeInclusionTwoStepWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = ({
  category,
  action,
}: {
  readonly category: ProductionNativeInclusionTwoStepCategoryV1;
  readonly action: FraudProofWorkflowActionV1;
}): Readonly<Record<string, unknown>> => {
  const input = record(action.input, `${category} workflow action`);
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${category} workflow action changed identity`);
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string")
    throw new Error(`workflow action omitted ${field}`);
  return value;
};

const captureRemoval = async <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>({
  config,
  input,
}: {
  readonly config: BoundConfigV1<Category>;
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
        fraudCategory: config.category,
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
              `${config.category} removal changed its authenticated queue input`,
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              `${config.category} removal did not reference the retained proof token`,
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

const createTransactionPort = <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>(
  config: BoundConfigV1<Category>,
): ProductionLinearFamilyTransactionPortV1<Category> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: config.category,
  prepare: async ({ evidence, classification }) =>
    await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: config.category,
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionNativeInclusionTwoStepArtifactV1(artifact);
    if (
      admitted.artifact.category !== config.category ||
      admitted.artifact.headerHash !== config.headerHash
    ) {
      throw new Error(`${config.category} artifact changed workflow identity`);
    }
    const input = actionInput({ category: config.category, action });
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: config.category,
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
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
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
            } as const;
            if (config.category === "invalidRange") {
              await submitInvalidRangeStep01(common);
            } else {
              await submitZeroInputStep01(common);
            }
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (config.category === "invalidRange") {
              await submitInvalidRangeStep02(common);
            } else {
              await submitZeroInputStep02({
                ...common,
                nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval({ config, input });
    }
    throw new Error(
      `${config.category} workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

type ManifestConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeInclusionTwoStepWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInvalidRangeWorkflowConfigV1 = ManifestConfigV1;
export type ManifestBoundZeroInputWorkflowConfigV1 = ManifestConfigV1;

export type ManifestBoundNativeInclusionTwoStepWorkflowV1<
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
> = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<Category>;
  l1: FraudProofFamilyL1ObservationPortV1<Category>;
  transactions: ProductionLinearFamilyTransactionPortV1<Category>;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export type ManifestBoundInvalidRangeWorkflowV1 =
  ManifestBoundNativeInclusionTwoStepWorkflowV1<"invalidRange">;
export type ManifestBoundZeroInputWorkflowV1 =
  ManifestBoundNativeInclusionTwoStepWorkflowV1<"zeroInput">;

const bindReferences = <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>({
  binding,
  supplied,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<Category>;
  readonly supplied: NativeInclusionTwoStepWorkflowReferenceScriptsV1;
}): NativeInclusionTwoStepWorkflowReferenceScriptsV1 => {
  const prefix =
    binding.definition.category === "invalidRange"
      ? "fraudProofInvalidRange"
      : "fraudProofZeroInput";
  return Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: prefix,
        utxo: supplied.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: `${prefix}Step02`,
        utxo: supplied.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "computationThreadMint",
        utxo: supplied.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMint",
        utxo: supplied.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: supplied.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: supplied.witnesses.chunkedVerifyWithdraw,
      }),
    }),
  });
};

const createWorkflow = async <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>({
  category,
  config,
}: {
  readonly category: Category;
  readonly config: ManifestConfigV1;
}): Promise<ManifestBoundNativeInclusionTwoStepWorkflowV1<Category>> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas:
      category === "invalidRange"
        ? [FraudProofComputationThreadStepDatum, InvalidRangeStep02Datum]
        : [FraudProofComputationThreadStepDatum, ZeroInputStep02Datum],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const references = bindReferences({
    binding,
    supplied: config.referenceScripts,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error(
      `${category} raw-L1 authority omitted publication observer`,
    );
  }
  const transactions = createTransactionPort({
    category,
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    referenceScripts: references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const linear = createProductionLinearFamilyWorkflowAdapterV1({
    category,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
    proofCborForAction: ({ action, artifact }) => {
      const admitted =
        admitProductionNativeInclusionTwoStepArtifactV1(artifact);
      return action.input.stage === "step_01"
        ? admitted.artifact.txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: withProductionProofChunkPrerequisiteV1({
      category,
      base: linear,
      prerequisite,
    }),
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const createManifestBoundInvalidRangeWorkflowV1 = async (
  config: ManifestBoundInvalidRangeWorkflowConfigV1,
): Promise<ManifestBoundInvalidRangeWorkflowV1> =>
  await createWorkflow({ category: "invalidRange", config });

export const createManifestBoundZeroInputWorkflowV1 = async (
  config: ManifestBoundZeroInputWorkflowConfigV1,
): Promise<ManifestBoundZeroInputWorkflowV1> =>
  await createWorkflow({ category: "zeroInput", config });

const runWorkflow = async <
  Category extends ProductionNativeInclusionTwoStepCategoryV1,
>({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeInclusionTwoStepWorkflowV1<Category>;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const category = workflow.binding.definition.category;
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer:
      category === "invalidRange"
        ? INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1
        : ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: [category],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const runOrResumeManifestBoundInvalidRangeWorkflowV1 = async (input: {
  readonly workflow: ManifestBoundInvalidRangeWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => await runWorkflow(input);

export const runOrResumeManifestBoundZeroInputWorkflowV1 = async (input: {
  readonly workflow: ManifestBoundZeroInputWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => await runWorkflow(input);
