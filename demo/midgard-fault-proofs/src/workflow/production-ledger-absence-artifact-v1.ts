import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  EMPTY_MERKLE_TREE_ROOT,
  encodeMidgardTxInputCanonicalV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  prepareNonExistentInputFromCanonicalEvidenceV1,
  prepareNoReferenceInputFromCanonicalEvidenceV1,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
} from "../field-opening-v1.js";
import { ledgerKeyBytesHex } from "../ne-submit-step-03.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import {
  type CompleteCanonicalReplayContextV1,
  completeCanonicalReplayPredecessorEvidenceV1,
} from "./complete-replay-v1.js";
import {
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
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

export const PRODUCTION_LEDGER_ABSENCE_ARTIFACT_V1 =
  "midgard-production-ledger-absence-artifact-v1" as const;

export type ProductionLedgerAbsenceCategoryV1 =
  | "nonExistentInput"
  | "noReferenceInput";

export type ProductionLedgerAbsenceArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_LEDGER_ABSENCE_ARTIFACT_V1;
    category: ProductionLedgerAbsenceCategoryV1;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: ProductionNativeInclusionArtifactV1;
    inputs: readonly Readonly<{ tx_id: string; output_index: string }>[];
    badInputIndex: number;
    prevUtxosRoot: string;
    ledgerNonMembershipProofCbor: string;
    txsNonMembershipProofCbor: string;
  }>;

export type AdmittedProductionLedgerAbsenceArtifactV1 = Readonly<{
  artifact: ProductionLedgerAbsenceArtifactV1;
  txInclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  inputPreimage: readonly MidgardTxInput[];
  selectedInput: MidgardTxInput;
  fieldPlan: FaultProofFieldOpeningPlanV1;
}>;

const proofSteps = (proof: ProofV1) =>
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

const decodeProof = (
  value: unknown,
  label: string,
): Readonly<{
  cbor: string;
  proof: ProofV1;
}> => {
  const cbor = canonicalHexV1(value, EVEN_HEX_V1, label);
  try {
    const proof = Data.from(cbor, Proof);
    const canonical = Data.to(proof, Proof);
    if (canonical !== cbor) {
      throw new Error(
        `${label} is noncanonical: input=${cbor}, canonical=${canonical}`,
      );
    }
    return Object.freeze({ cbor, proof });
  } catch (cause) {
    throw cause instanceof Error && cause.message.includes("is noncanonical")
      ? cause
      : new Error(`${label} is not canonical proof CBOR`);
  }
};

const canonicalProofCbor = (value: string, label: string): string => {
  try {
    return Data.to(Data.from(value, Proof), Proof);
  } catch {
    throw new Error(`${label} is not proof CBOR`);
  }
};

const replayNonMembership = ({
  proof,
  key,
  expectedRoot,
  label,
}: {
  readonly proof: ProofV1;
  readonly key: string;
  readonly expectedRoot: string;
  readonly label: string;
}): void => {
  let opened: Buffer | null;
  try {
    opened = MpfProof.fromJSON(
      Buffer.from(key, "hex"),
      undefined,
      proofSteps(proof),
    ).verify(false);
  } catch {
    throw new Error(`${label} cannot be replayed`);
  }
  const root =
    opened === null ? EMPTY_MERKLE_TREE_ROOT : opened.toString("hex");
  if (root !== expectedRoot) {
    throw new Error(`${label} does not open its authenticated root`);
  }
};

const selectedIdentity = ({
  category,
  classification,
}: {
  readonly category: ProductionLedgerAbsenceCategoryV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Readonly<{
  position: number;
  inputIndex: number;
  badTxId: string;
  inputKey: string;
}> => {
  const fields = classification.selected.detectionId.split(":");
  const expectedViolation =
    category === "nonExistentInput"
      ? "non-existent-input"
      : "no-reference-input";
  if (
    classification.category !== category ||
    classification.selected.violationId !== expectedViolation ||
    fields.length !== 5 ||
    fields[0] !== expectedViolation ||
    !NATURAL_DECIMAL_V1.test(fields[1] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[2] ?? "") ||
    !HEX_32_V1.test(fields[3] ?? "") ||
    !EVEN_HEX_V1.test(fields[4] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error(`${category} classification identity is malformed`);
  }
  const position = Number(fields[1]);
  const inputIndex = Number(fields[2]);
  if (!Number.isSafeInteger(position) || !Number.isSafeInteger(inputIndex)) {
    throw new Error(`${category} classification index exceeds safe range`);
  }
  return Object.freeze({
    position,
    inputIndex,
    badTxId: fields[3]!,
    inputKey: fields[4]!,
  });
};

export const admitProductionLedgerAbsenceArtifactV1 = (
  value: unknown,
  owner: string,
): AdmittedProductionLedgerAbsenceArtifactV1 => {
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "category",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "inputs",
      "badInputIndex",
      "prevUtxosRoot",
      "ledgerNonMembershipProofCbor",
      "txsNonMembershipProofCbor",
    ],
    "ledger-absence artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_LEDGER_ABSENCE_ARTIFACT_V1 ||
    (parsed.category !== "nonExistentInput" &&
      parsed.category !== "noReferenceInput") ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("ledger-absence artifact identity changed");
  }
  const category = parsed.category;
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "ledger-absence header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "ledger-absence position",
  );
  const badInputIndex = safeNaturalNumberV1(
    parsed.badInputIndex,
    "ledger-absence input index",
  );
  const badTx = admitProductionNativeInclusionArtifactV1(
    parsed.badTx,
    "ledger-absence transaction",
  );
  const inputs = admitProductionTxInputListV1(
    parsed.inputs,
    "ledger-absence input preimage",
  );
  const selectedInput = inputs.inputs[badInputIndex];
  if (selectedInput === undefined) {
    throw new Error("ledger-absence input index is out of bounds");
  }
  const prevUtxosRoot = canonicalHexV1(
    parsed.prevUtxosRoot,
    HEX_32_V1,
    "ledger-absence predecessor root",
  );
  const ledgerProof = decodeProof(
    parsed.ledgerNonMembershipProofCbor,
    "ledger-absence predecessor proof",
  );
  const txsProof = decodeProof(
    parsed.txsNonMembershipProofCbor,
    "ledger-absence transactions proof",
  );
  const inputKey = ledgerKeyBytesHex(selectedInput);
  replayNonMembership({
    proof: ledgerProof.proof,
    key: inputKey,
    expectedRoot: prevUtxosRoot,
    label: "ledger-absence predecessor proof",
  });
  replayNonMembership({
    proof: txsProof.proof,
    key: selectedInput.tx_id,
    expectedRoot: badTx.artifact.transactionsPhasRoot,
    label: "ledger-absence transactions proof",
  });
  const detectionFields = parsed.detectionId.split(":");
  const expectedViolation =
    category === "nonExistentInput"
      ? "non-existent-input"
      : "no-reference-input";
  if (
    detectionFields.length !== 5 ||
    detectionFields[0] !== expectedViolation ||
    detectionFields[1] !== position.toString() ||
    detectionFields[2] !== badInputIndex.toString() ||
    detectionFields[3] !== badTx.artifact.nativeTxId ||
    detectionFields[4] !== inputKey
  ) {
    throw new Error("ledger-absence artifact changed detection identity");
  }
  const fieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex:
      category === "nonExistentInput"
        ? MIDGARD_FIELD_INDEX_V1.spendInputs
        : MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: badTx.artifact.nativeTxId,
    nativeTxCompactCbor: badTx.artifact.nativeTxCompactCbor,
    itemCbors: inputs.inputs.map(encodeMidgardTxInputCanonicalV1),
    owner,
    label: `${category} production input field`,
  });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_LEDGER_ABSENCE_ARTIFACT_V1,
    category,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: badTx.artifact,
    inputs: inputs.json,
    badInputIndex,
    prevUtxosRoot,
    ledgerNonMembershipProofCbor: ledgerProof.cbor,
    txsNonMembershipProofCbor: txsProof.cbor,
  }) satisfies ProductionLedgerAbsenceArtifactV1;
  return Object.freeze({
    artifact,
    txInclusion: badTx.inclusion,
    inputPreimage: inputs.inputs,
    selectedInput,
    fieldPlan,
  });
};

export const prepareProductionLedgerAbsenceArtifactV1 = async ({
  category,
  evidence,
  replayContext,
  classification,
  owner,
}: {
  readonly category: ProductionLedgerAbsenceCategoryV1;
  readonly evidence: Parameters<
    typeof prepareNonExistentInputFromCanonicalEvidenceV1
  >[0]["evidence"];
  readonly replayContext?: CompleteCanonicalReplayContextV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
  readonly owner: string;
}): Promise<ProductionLedgerAbsenceArtifactV1> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error(`${category} classification differs from evidence`);
  }
  const selected = selectedIdentity({ category, classification });
  const previousBlockEvidence = completeCanonicalReplayPredecessorEvidenceV1({
    evidence,
    context: replayContext,
  });
  const prepared = await (async () => {
    if (category === "nonExistentInput") {
      const result = await prepareNonExistentInputFromCanonicalEvidenceV1({
        evidence,
        ...(previousBlockEvidence === undefined
          ? {}
          : { previousBlockEvidence }),
        badTxId: selected.badTxId,
        badInputIndex: selected.inputIndex,
      });
      return Object.freeze({
        result,
        missingInput: result.missingInput,
        inputPreimage: result.inputsPreimage,
      });
    }
    const result = await prepareNoReferenceInputFromCanonicalEvidenceV1({
      evidence,
      ...(previousBlockEvidence === undefined ? {} : { previousBlockEvidence }),
      badTxId: selected.badTxId,
      badReferenceInputIndex: selected.inputIndex,
    });
    return Object.freeze({
      result,
      missingInput: result.missingReferenceInput,
      inputPreimage: result.referenceInputsPreimage,
    });
  })();
  if (ledgerKeyBytesHex(prepared.missingInput) !== selected.inputKey) {
    throw new Error(`${category} prepared evidence changed classification`);
  }
  const result = prepared.result;
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_LEDGER_ABSENCE_ARTIFACT_V1,
    category,
    headerHash: result.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    badTx: {
      nativeTxId: result.txInclusion.nativeTxId,
      nativeTxCompactCbor: result.txInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: result.txInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: result.txInclusion.transactionsPhasRoot,
      txMembershipProofCbor: result.txInclusion.txMembershipProofCbor,
    },
    inputs: prepared.inputPreimage.map((input) => ({
      tx_id: input.txId,
      output_index: input.index.toString(),
    })),
    badInputIndex: selected.inputIndex,
    prevUtxosRoot: result.prevUtxosRoot,
    ledgerNonMembershipProofCbor: canonicalProofCbor(
      result.ledgerNonMembershipProofCbor,
      `${category} predecessor proof`,
    ),
    txsNonMembershipProofCbor: canonicalProofCbor(
      result.txsNonMembershipProofCbor,
      `${category} transactions proof`,
    ),
  }) as ProductionLedgerAbsenceArtifactV1;
  return admitProductionLedgerAbsenceArtifactV1(artifact, owner).artifact;
};
