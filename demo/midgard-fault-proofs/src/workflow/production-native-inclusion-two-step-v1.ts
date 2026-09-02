import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  decodeMidgardNativeTxCompactV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadStepDatum,
  InvalidRangeStep02Datum,
  invalidRangeViolationReason,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  prepareInvalidRangeFromCanonicalEvidenceV1,
  prepareZeroInputFromCanonicalEvidenceV1,
} from "../evidence/prepare-from-evidence-v1.js";
import type { InvalidRangeContractsV1 } from "../invalid-range/contracts-v1.js";
import {
  invalidRangeEvidenceClosesV1,
  type InvalidRangeEvidenceV1,
  prepareInvalidRangeEvidenceV1,
} from "../invalid-range/family-v1.js";
import { prepareInvalidRangeForcedProductionPlanV1 } from "../invalid-range/production-v1.js";
import {
  submitInvalidRangeStep01ForcedV1,
  submitInvalidRangeStep02V1,
} from "../invalid-range/submit-v1.js";
import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
import type { PreparedTxInclusionJson } from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInvalidRangeStep01 } from "../submit-invalid-range-step-01.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { ZeroInputContractsV1 } from "../zero-input/contracts-v1.js";
import {
  prepareZeroInputEvidenceV1,
  type ZeroInputEvidenceV1,
  ZeroInputVerdictSubjectV1Schema,
} from "../zero-input/family-v1.js";
import { prepareZeroInputForcedProductionPlanV1 } from "../zero-input/production-v1.js";
import {
  ZeroInputForcedSourcePayloadV1Schema,
  ZeroInputStep02DatumV1Schema,
} from "../zero-input/schemas-v1.js";
import {
  submitZeroInputStep01AcceptedV1,
  submitZeroInputStep01ForcedV1,
} from "../zero-input/submit-step-01-v1.js";
import { submitZeroInputStep02V1 } from "../zero-input/submit-step-02-v1.js";
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
    sourceKind: "accepted" | "forced";
    subjectCbor: string;
    inputFieldPreimageCbor: string;
    inputFieldCommitment: string;
    forcedSourceCbor: string;
  }>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const OPTIONAL_HEX = /^(?:[0-9a-f]{2})*$/u;
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
      "sourceKind",
      "subjectCbor",
      "inputFieldPreimageCbor",
      "inputFieldCommitment",
      "forcedSourceCbor",
    ],
    "native-inclusion two-step artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1 ||
    (parsed.category !== "invalidRange" && parsed.category !== "zeroInput") ||
    (parsed.sourceKind !== "accepted" && parsed.sourceKind !== "forced") ||
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
      OPTIONAL_HEX,
      "artifact membership proof",
    ),
    sourceKind: parsed.sourceKind,
    subjectCbor: canonicalHex(
      parsed.subjectCbor,
      OPTIONAL_HEX,
      "artifact subject",
    ),
    inputFieldPreimageCbor: canonicalHex(
      parsed.inputFieldPreimageCbor,
      OPTIONAL_HEX,
      "artifact input field preimage",
    ),
    inputFieldCommitment: canonicalHex(
      parsed.inputFieldCommitment,
      HEX_32,
      "artifact input field commitment",
    ),
    forcedSourceCbor: canonicalHex(
      parsed.forcedSourceCbor,
      OPTIONAL_HEX,
      "artifact forced source",
    ),
  });
};

export const admitProductionNativeInclusionTwoStepArtifactV1 = (
  value: unknown,
): Readonly<{
  artifact: ProductionNativeInclusionTwoStepArtifactV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion> | null;
  zeroInputEvidence: ZeroInputEvidenceV1 | null;
  invalidRangeEvidence: InvalidRangeEvidenceV1 | null;
  forcedSource: Readonly<Record<string, unknown>> | null;
}> => {
  const artifact = parseArtifact(value);
  const compact = decodeMidgardNativeTxCompactV1(
    Buffer.from(artifact.nativeTxCompactCbor, "hex"),
  );
  const inclusion =
    artifact.sourceKind === "accepted"
      ? parseSubmitStep01TxInclusion({
          nativeTxId: artifact.nativeTxId,
          nativeTx: nativeTxFromCoreCompact(compact),
          nativeTxCompactCbor: artifact.nativeTxCompactCbor,
          l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
          transactionsPhasRoot: artifact.transactionsPhasRoot,
          txMembershipProofCbor: artifact.txMembershipProofCbor,
        })
      : null;
  let openedRoot: Buffer | null;
  try {
    if (inclusion === null) throw new Error("forced source");
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    if (artifact.sourceKind === "forced") openedRoot = null;
    else
      throw new Error(
        "native-inclusion artifact membership proof cannot be replayed",
      );
  }
  if (
    artifact.sourceKind === "accepted" &&
    (openedRoot === null ||
      openedRoot.toString("hex") !== artifact.transactionsPhasRoot)
  ) {
    throw new Error(
      "native-inclusion artifact membership proof does not open its PHAS root",
    );
  }
  let invalidRangeEvidence: InvalidRangeEvidenceV1 | null = null;
  if (
    artifact.category === "invalidRange" &&
    artifact.sourceKind === "accepted"
  ) {
    const reason = invalidRangeViolationReason({
      blockSlot: BigInt(artifact.blockSlot!),
      normalizedRange: normalizeNativeTxValidityRange(inclusion!.nativeTx.body),
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
    const subject = Data.from(
      artifact.subjectCbor,
      SDK.InvalidRangeVerdictSubjectV1Schema as never,
    ) as SDK.VerdictSubjectV1;
    invalidRangeEvidence = prepareInvalidRangeEvidenceV1({
      subject,
      blockSlot: BigInt(artifact.blockSlot!),
      txBody: inclusion!.nativeTx.body,
    });
    if (
      subject.direction !== 0n ||
      !invalidRangeEvidenceClosesV1(invalidRangeEvidence) ||
      artifact.forcedSourceCbor !== ""
    )
      throw new Error("invalid-range accepted artifact source changed");
  } else if (artifact.category === "zeroInput") {
    if (
      artifact.sourceKind === "accepted" &&
      (!nativeTxBodyHasZeroInputViolation({
        txBody: inclusion!.nativeTx.body,
      }) ||
        artifact.detectionId !==
          `zero-input:${artifact.position.toString()}:${artifact.nativeTxId}`)
    ) {
      throw new Error(
        "zero-input artifact does not re-derive its selected violation",
      );
    }
  }
  let zeroInputEvidence: ZeroInputEvidenceV1 | null = null;
  let forcedSource: Readonly<Record<string, unknown>> | null = null;
  if (artifact.category === "zeroInput") {
    const subject = Data.from(
      artifact.subjectCbor,
      ZeroInputVerdictSubjectV1Schema as never,
    ) as SDK.VerdictSubjectV1;
    zeroInputEvidence = prepareZeroInputEvidenceV1({
      finding: { subject },
      inputFieldPreimage: Buffer.from(artifact.inputFieldPreimageCbor, "hex"),
      committedFieldHashHex: artifact.inputFieldCommitment,
    });
    if (
      compact.transactionBody.spendInputsHash.toString("hex") !==
        artifact.inputFieldCommitment ||
      zeroInputEvidence.subject.transaction_id !== artifact.nativeTxId
    ) {
      throw new Error("zero-input artifact field evidence changed transaction");
    }
    if (artifact.sourceKind === "accepted") {
      if (artifact.forcedSourceCbor !== "" || subject.direction !== 0n)
        throw new Error("zero-input accepted artifact source changed");
    } else {
      if (
        artifact.txMembershipProofCbor !== "" ||
        artifact.transactionsPhasRoot !== "00".repeat(32)
      )
        throw new Error("zero-input forced artifact carried accepted evidence");
      forcedSource = Data.from(
        artifact.forcedSourceCbor,
        ZeroInputForcedSourcePayloadV1Schema as never,
      ) as Readonly<Record<string, unknown>>;
      const source = forcedSource as {
        readonly header: SDK.HeaderV1;
        readonly membership: SDK.ForcedTransactionSourceMembershipProof;
        readonly direction: bigint;
      };
      const leaf = source.membership.value;
      if (
        computeHash28(SDK.encodeHeaderV1Cbor(source.header)).toString("hex") !==
          artifact.headerHash ||
        source.direction !== 1n ||
        source.membership.root !== source.header.forcedTransactionsRoot ||
        source.membership.count !== source.header.forcedTransactionCount ||
        leaf.tx_id !== artifact.nativeTxId ||
        leaf.source.compact_cbor !== artifact.nativeTxCompactCbor ||
        Data.to(
          { tx_id: leaf.tx_id, source: leaf.source } as never,
          SDK.L2TransactionSourceV1 as never,
        ) !== artifact.l2TransactionSourceCbor ||
        leaf.verdict === "ForcedTxValid" ||
        leaf.verdict.ForcedTxInvalid.reason !== "EmptyInputs"
      )
        throw new Error(
          "zero-input forced artifact changed authenticated leaf",
        );
      const derivedSubject = SDK.forcedVerdictSubjectV1({
        transactionId: leaf.tx_id,
        sourceKey: source.membership.key,
        rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
      });
      if (
        Data.to(
          derivedSubject as never,
          ZeroInputVerdictSubjectV1Schema as never,
        ) !== artifact.subjectCbor
      )
        throw new Error(
          "zero-input forced artifact injected its verdict subject",
        );
      let forcedRoot: Buffer | null;
      try {
        forcedRoot = MpfProof.fromJSON(
          Buffer.from(
            Data.to(
              source.membership.key as never,
              SDK.OutputReferenceSchema as never,
            ),
            "hex",
          ),
          Buffer.from(
            Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
            "hex",
          ),
          proofSteps(source.membership.proof as never),
        ).verify(true);
      } catch {
        throw new Error(
          "zero-input forced artifact membership cannot be replayed",
        );
      }
      if (forcedRoot?.toString("hex") !== source.membership.phas_root)
        throw new Error("zero-input forced artifact membership root changed");
    }
  } else if (artifact.sourceKind === "forced") {
    const subject = Data.from(
      artifact.subjectCbor,
      SDK.InvalidRangeVerdictSubjectV1Schema as never,
    ) as SDK.VerdictSubjectV1;
    const source = Data.from(
      artifact.forcedSourceCbor,
      SDK.InvalidRangeForcedSourcePayloadV1Schema as never,
    ) as {
      header: SDK.HeaderV1;
      membership: SDK.ForcedTransactionSourceMembershipProof;
      direction: bigint;
    };
    const leaf = source.membership.value;
    if (
      computeHash28(SDK.encodeHeaderV1Cbor(source.header)).toString("hex") !==
        artifact.headerHash ||
      source.direction !== 1n ||
      source.membership.root !== source.header.forcedTransactionsRoot ||
      source.membership.count !== source.header.forcedTransactionCount ||
      leaf.tx_id !== artifact.nativeTxId ||
      leaf.source.compact_cbor !== artifact.nativeTxCompactCbor ||
      Data.to(
        { tx_id: leaf.tx_id, source: leaf.source } as never,
        SDK.L2TransactionSourceV1 as never,
      ) !== artifact.l2TransactionSourceCbor ||
      artifact.txMembershipProofCbor !== "" ||
      artifact.transactionsPhasRoot !== "00".repeat(32) ||
      leaf.verdict === "ForcedTxValid" ||
      (leaf.verdict.ForcedTxInvalid.reason !== "ValidityIntervalMalformed" &&
        leaf.verdict.ForcedTxInvalid.reason !==
          "ValidityIntervalExcludesBlockSlot")
    )
      throw new Error(
        "invalid-range forced artifact changed authenticated leaf",
      );
    const derived = SDK.forcedVerdictSubjectV1({
      transactionId: leaf.tx_id,
      sourceKey: source.membership.key,
      rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
    });
    if (
      Data.to(
        derived as never,
        SDK.InvalidRangeVerdictSubjectV1Schema as never,
      ) !== artifact.subjectCbor
    )
      throw new Error(
        "invalid-range forced artifact injected its verdict subject",
      );
    let root: Buffer | null;
    try {
      root = MpfProof.fromJSON(
        Buffer.from(
          Data.to(
            source.membership.key as never,
            SDK.OutputReferenceSchema as never,
          ),
          "hex",
        ),
        Buffer.from(
          Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
          "hex",
        ),
        proofSteps(source.membership.proof as never),
      ).verify(true);
    } catch {
      throw new Error(
        "invalid-range forced artifact membership cannot be replayed",
      );
    }
    if (root?.toString("hex") !== source.membership.phas_root)
      throw new Error("invalid-range forced artifact membership root changed");
    invalidRangeEvidence = prepareInvalidRangeEvidenceV1({
      subject,
      blockSlot: source.header.blockSlot,
      txBody: nativeTxFromCoreCompact(compact).body,
    });
    if (
      !invalidRangeEvidenceClosesV1(invalidRangeEvidence) ||
      artifact.blockSlot !== source.header.blockSlot.toString() ||
      artifact.violationReason !== leaf.verdict.ForcedTxInvalid.reason ||
      artifact.detectionId !==
        `invalid-range:forced:${artifact.position.toString()}:${artifact.nativeTxId}:${leaf.verdict.ForcedTxInvalid.reason}`
    )
      throw new Error(
        "invalid-range forced artifact does not contradict rejection",
      );
    forcedSource = source as unknown as Readonly<Record<string, unknown>>;
  } else if (
    artifact.inputFieldPreimageCbor !== "" ||
    artifact.inputFieldCommitment !== "00".repeat(32) ||
    artifact.forcedSourceCbor !== ""
  ) {
    throw new Error("invalid-range artifact carried zero-input authority");
  }
  return Object.freeze({
    artifact,
    inclusion,
    invalidRangeEvidence,
    zeroInputEvidence,
    forcedSource,
  });
};

const selectedTxId = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
): string => {
  if (
    classification.category === "invalidRange" &&
    classification.selected.detectionId.startsWith("invalid-range:forced:")
  ) {
    const fields = classification.selected.detectionId.split(":");
    if (
      fields.length !== 5 ||
      !NATURAL.test(fields[2] ?? "") ||
      !HEX_32.test(fields[3] ?? "") ||
      classification.selected.position !== BigInt(fields[2]!)
    )
      throw new Error("invalidRange forced classification is malformed");
    return fields[3]!;
  }
  if (
    classification.category === "zeroInput" &&
    classification.selected.detectionId.startsWith("zero-input:forced:")
  ) {
    const fields = classification.selected.detectionId.split(":");
    if (
      fields.length !== 4 ||
      !NATURAL.test(fields[2] ?? "") ||
      !HEX_32.test(fields[3] ?? "") ||
      classification.selected.position !== BigInt(fields[2]!)
    )
      throw new Error("zeroInput forced classification is malformed");
    return fields[3]!;
  }
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
  let sourceKind: "accepted" | "forced" = "accepted";
  let subjectCbor = "";
  let inputFieldPreimageCbor = "";
  let inputFieldCommitment = "00".repeat(32);
  let forcedSourceCbor = "";
  if (
    category === "invalidRange" &&
    !classification.selected.detectionId.startsWith("invalid-range:forced:")
  ) {
    const prepared = await prepareInvalidRangeFromCanonicalEvidenceV1({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = prepared.tx.violationReason;
    blockSlot = prepared.blockSlot.toString();
    subjectCbor = Data.to(
      SDK.acceptedVerdictSubjectV1(preparedNodeTxId) as never,
      SDK.InvalidRangeVerdictSubjectV1Schema as never,
    );
  } else if (category === "invalidRange") {
    const forced = await prepareInvalidRangeForcedProductionPlanV1({
      block: evidence,
    });
    if (
      forced.detectionId !== classification.selected.detectionId ||
      forced.evidence.subject.transaction_id !== txId
    )
      throw new Error("invalidRange forced plan changed classification");
    const transaction =
      evidence.reconstruction.forcedTransactions[
        Number(classification.selected.position)
      ];
    if (transaction === undefined)
      throw new Error(
        "invalidRange forced transaction disappeared from retained DA",
      );
    sourceKind = "forced";
    preparedHeaderHash = forced.headerHash;
    preparedNodeTxId = txId;
    preparedInclusion = {
      nativeTxId: txId,
      nativeTx: nativeTxFromCoreCompact(
        decodeMidgardNativeTxCompactV1(
          Buffer.from(forced.nativeTxCompactCbor, "hex"),
        ),
      ),
      nativeTxCompactCbor: forced.nativeTxCompactCbor,
      l2TransactionSourceCbor: Data.to(
        {
          tx_id: transaction.value.tx_id,
          source: transaction.value.source,
        } as never,
        SDK.L2TransactionSourceV1 as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
    };
    violationReason = forced.evidence.subject.rejection_reason as string;
    blockSlot = forced.evidence.blockSlot.toString();
    subjectCbor = Data.to(
      forced.evidence.subject as never,
      SDK.InvalidRangeVerdictSubjectV1Schema as never,
    );
    forcedSourceCbor = Data.to(
      forced.forcedSource as never,
      SDK.InvalidRangeForcedSourcePayloadV1Schema as never,
    );
  } else if (
    !classification.selected.detectionId.startsWith("zero-input:forced:")
  ) {
    const prepared = await prepareZeroInputFromCanonicalEvidenceV1({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = null;
    blockSlot = null;
    const retained = evidence.transactions.find(
      (transaction) => transaction.nodeTxId === preparedNodeTxId,
    );
    if (retained === undefined)
      throw new Error(
        "zeroInput accepted transaction disappeared from retained DA",
      );
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(retained.txCbor, "hex"),
    );
    const field = material.fieldPreimages[0];
    if (field === undefined)
      throw new Error("zeroInput accepted field 0 disappeared");
    const acceptedEvidence = prepareZeroInputEvidenceV1({
      finding: { subject: SDK.acceptedVerdictSubjectV1(preparedNodeTxId) },
      inputFieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
    });
    subjectCbor = Data.to(
      acceptedEvidence.subject as never,
      ZeroInputVerdictSubjectV1Schema as never,
    );
    inputFieldPreimageCbor = acceptedEvidence.inputFieldPreimageCbor;
    inputFieldCommitment = acceptedEvidence.inputFieldCommitment;
  } else {
    const forced = await prepareZeroInputForcedProductionPlanV1({
      block: evidence,
    });
    if (
      forced.detectionId !== classification.selected.detectionId ||
      forced.evidence.subject.transaction_id !== txId
    )
      throw new Error("zeroInput forced plan changed classification");
    const transaction =
      evidence.reconstruction.forcedTransactions[
        Number(classification.selected.position)
      ];
    if (transaction === undefined)
      throw new Error(
        "zeroInput forced transaction disappeared from retained DA",
      );
    sourceKind = "forced";
    preparedHeaderHash = forced.headerHash;
    preparedNodeTxId = forced.evidence.subject.transaction_id;
    preparedInclusion = {
      nativeTxId: preparedNodeTxId,
      nativeTx: nativeTxFromCoreCompact(
        decodeMidgardNativeTxCompactV1(
          Buffer.from(forced.nativeTxCompactCbor, "hex"),
        ),
      ),
      nativeTxCompactCbor: forced.nativeTxCompactCbor,
      l2TransactionSourceCbor: Data.to(
        {
          tx_id: transaction.value.tx_id,
          source: transaction.value.source,
        } as never,
        SDK.L2TransactionSourceV1 as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
    };
    violationReason = null;
    blockSlot = null;
    subjectCbor = Data.to(
      forced.evidence.subject as never,
      ZeroInputVerdictSubjectV1Schema as never,
    );
    inputFieldPreimageCbor = forced.evidence.inputFieldPreimageCbor;
    inputFieldCommitment = forced.evidence.inputFieldCommitment;
    forcedSourceCbor = Data.to(
      forced.forcedSource as never,
      ZeroInputForcedSourcePayloadV1Schema as never,
    );
  }
  if (
    classification.selected.detectionId !==
    (category === "invalidRange"
      ? sourceKind === "forced"
        ? `invalid-range:forced:${classification.selected.position.toString()}:${preparedNodeTxId}:${violationReason}`
        : `invalid-range:${classification.selected.position.toString()}:${preparedNodeTxId}:${violationReason}`
      : sourceKind === "forced"
        ? `zero-input:forced:${classification.selected.position.toString()}:${preparedNodeTxId}`
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
    sourceKind,
    subjectCbor,
    inputFieldPreimageCbor,
    inputFieldCommitment,
    forcedSourceCbor,
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
  zeroInputContracts: ZeroInputContractsV1 | null;
  invalidRangeContracts: InvalidRangeContractsV1 | null;
  categoryId: string;
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
      const chunks =
        admitted.artifact.sourceKind === "accepted"
          ? await resolveDirectFirstProofChunksV1({
              action,
              lucid: config.lucid,
              address: config.signer.address,
              proofCbor: admitted.artifact.txMembershipProofCbor,
            })
          : undefined;
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
            void common;
            if (config.category === "invalidRange") {
              if (admitted.artifact.sourceKind === "forced") {
                if (
                  config.invalidRangeContracts === null ||
                  admitted.invalidRangeEvidence === null ||
                  admitted.forcedSource === null
                )
                  throw new Error("invalidRange forced authority disappeared");
                await submitInvalidRangeStep01ForcedV1({
                  lucid: config.lucid,
                  contracts: config.invalidRangeContracts,
                  categoryId: config.categoryId,
                  signer: config.signer,
                  threadOutRef: stringField(input, "threadOutRef"),
                  evidence: admitted.invalidRangeEvidence,
                  forcedSource: admitted.forcedSource,
                  referenceScriptUtxo: config.referenceScripts.steps[0],
                  preSubmitBoundary,
                  awaitConfirmation: false,
                });
              } else {
                if (admitted.inclusion === null)
                  throw new Error(
                    "invalidRange accepted inclusion disappeared",
                  );
                await submitInvalidRangeStep01({
                  ...common,
                  txInclusion: admitted.inclusion,
                });
              }
            } else {
              const contracts = config.zeroInputContracts;
              const evidence = admitted.zeroInputEvidence;
              if (contracts === null || evidence === null)
                throw new Error("zeroInput workflow omitted family authority");
              if (admitted.artifact.sourceKind === "forced") {
                if (admitted.forcedSource === null)
                  throw new Error("zeroInput forced source disappeared");
                await submitZeroInputStep01ForcedV1({
                  lucid: config.lucid,
                  contracts,
                  categoryId: config.categoryId,
                  signer: config.signer,
                  threadOutRef: stringField(input, "threadOutRef"),
                  finding: evidence,
                  forcedSource: admitted.forcedSource,
                  referenceScriptUtxo: config.referenceScripts.steps[0],
                  preSubmitBoundary,
                  awaitConfirmation: false,
                });
              } else {
                if (admitted.inclusion === null)
                  throw new Error("zeroInput accepted inclusion disappeared");
                const thread = await requireLinearFaultThreadUtxoV1({
                  lucid: config.lucid,
                  contracts,
                  categoryId: config.categoryId,
                  family: "zero-input",
                  stepIndex: 0,
                  threadOutRef: stringField(input, "threadOutRef"),
                });
                await submitZeroInputStep01AcceptedV1({
                  lucid: config.lucid,
                  blueprint: config.blueprint,
                  network: config.network,
                  contracts,
                  signer: config.signer,
                  finding: evidence,
                  threadUtxo: thread.threadUtxo,
                  threadToken: thread.threadToken,
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
              }
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
            void common;
            if (config.category === "invalidRange") {
              if (
                config.invalidRangeContracts === null ||
                admitted.invalidRangeEvidence === null
              )
                throw new Error("invalidRange terminal authority disappeared");
              await submitInvalidRangeStep02V1({
                lucid: config.lucid,
                contracts: config.invalidRangeContracts,
                categoryId: config.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                evidence: admitted.invalidRangeEvidence,
                referenceScriptUtxo: config.referenceScripts.steps[1],
                witnessReferenceScripts: config.referenceScripts.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            } else {
              if (
                config.zeroInputContracts === null ||
                admitted.zeroInputEvidence === null
              )
                throw new Error(
                  "zeroInput workflow omitted terminal authority",
                );
              await submitZeroInputStep02V1({
                lucid: config.lucid,
                contracts: config.zeroInputContracts,
                categoryId: config.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                evidence: admitted.zeroInputEvidence,
                nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
                referenceScriptUtxo: config.referenceScripts.steps[1],
                witnessReferenceScripts: config.referenceScripts.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
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
        : [FraudProofComputationThreadStepDatum, ZeroInputStep02DatumV1Schema],
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
  const zeroInputChain = binding.resolvedContracts.contracts.zeroInput;
  const invalidRangeChain = binding.resolvedContracts.contracts.invalidRange;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificatePolicyId =
    binding.fieldPreimageCertificate?.policyId ??
    binding.deploymentInfo.fieldPreimageCertificateMint?.scriptHash;
  const zeroInputContracts: ZeroInputContractsV1 | null =
    category !== "zeroInput"
      ? null
      : zeroInputChain === undefined ||
          stateQueuePolicyId === undefined ||
          certificatePolicyId === undefined
        ? (() => {
            throw new Error("zeroInput deployment chain is incomplete");
          })()
        : {
            steps: zeroInputChain.steps.map((step, index) => ({
              ...step,
              blueprintTitle: [
                "fraud_proofs/zero_input/step_01.main.spend",
                "fraud_proofs/zero_input/step_02.main.spend",
              ][index]!,
              referenceOutRef: `${references.steps[index]!.txHash}#${references.steps[index]!.outputIndex.toString()}`,
            })) as unknown as ZeroInputContractsV1["steps"],
            computationThread:
              binding.resolvedContracts.contracts.computationThread,
            fraudProof: {
              policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
              mintingScript:
                binding.resolvedContracts.contracts.fraudProof.mintingScript,
              spendingScriptAddress:
                binding.resolvedContracts.contracts.fraudProof
                  .spendingScriptAddress,
            },
            hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
            stateQueuePolicyId,
            fieldPreimageCertificatePolicyId: certificatePolicyId,
          };
  const invalidRangeContracts: InvalidRangeContractsV1 | null =
    category !== "invalidRange"
      ? null
      : invalidRangeChain === undefined || stateQueuePolicyId === undefined
        ? (() => {
            throw new Error("invalidRange deployment chain is incomplete");
          })()
        : {
            steps: invalidRangeChain.steps.map((step, index) => ({
              ...step,
              blueprintTitle: [
                "fraud_proofs/invalid_range/step_01.main.spend",
                "fraud_proofs/invalid_range/step_02.main.spend",
              ][index]!,
              referenceOutRef: `${references.steps[index]!.txHash}#${references.steps[index]!.outputIndex.toString()}`,
            })) as unknown as InvalidRangeContractsV1["steps"],
            computationThread:
              binding.resolvedContracts.contracts.computationThread,
            fraudProof: {
              policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
              mintingScript:
                binding.resolvedContracts.contracts.fraudProof.mintingScript,
              spendingScriptAddress:
                binding.resolvedContracts.contracts.fraudProof
                  .spendingScriptAddress,
            },
            hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
            stateQueuePolicyId,
          };
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
    zeroInputContracts,
    invalidRangeContracts,
    categoryId: binding.resolvedContracts.category.categoryId,
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
      return action.input.stage === "step_01" &&
        admitted.artifact.sourceKind === "accepted"
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
