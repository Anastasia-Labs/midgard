import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  encodeMidgardTxInputCanonical,
  isWithdrawnInputViolation,
  type Proof,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { type JournalJsonObject } from "./journal.js";
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
    anchorSourceKind: 0n,
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

export const selectedIdentity = (
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
