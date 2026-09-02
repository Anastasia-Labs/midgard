import {
  decodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  RejectionReasonV1Schema,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ZERO_INPUT_CATEGORY_V1 = "zeroInput" as const;
export const ZERO_INPUT_CATEGORY_ID_V1 = "00000005" as const;
export const ZERO_INPUT_FIELD_INDEX_V1 = 0 as const;
export const ZERO_INPUT_REASON_V1 = "EmptyInputs" as const;

export type ZeroInputFindingV1 = Readonly<{ subject: VerdictSubjectV1 }>;

export const classifyZeroInputFindingV1 = (
  finding: ZeroInputFindingV1,
): ZeroInputFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    throw new Error("zeroInput: verdict subject is not canonical");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (finding.subject.rejection_reason !== ZERO_INPUT_REASON_V1)
      throw new Error("zeroInput: typed rejection reason changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    throw new Error("zeroInput: direction/reason polarity changed");
  }
  return Object.freeze(finding);
};

export type ZeroInputEvidenceV1 = ZeroInputFindingV1 &
  Readonly<{
    inputCount: number;
    inputFieldPreimageCbor: string;
    inputFieldCommitment: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareZeroInputEvidenceV1 = ({
  finding: rawFinding,
  inputFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ZeroInputFindingV1;
  readonly inputFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ZeroInputEvidenceV1 => {
  const finding = classifyZeroInputFindingV1(rawFinding);
  const commitment =
    midgardFieldCommitmentV1(inputFieldPreimage).toString("hex");
  if (commitment !== committedFieldHashHex)
    throw new Error("zeroInput: retained field 0 changed commitment");
  const inputs = decodeMidgardFieldPreimageV1(inputFieldPreimage);
  return Object.freeze({
    ...finding,
    inputCount: inputs.length,
    inputFieldPreimageCbor: Buffer.from(inputFieldPreimage).toString("hex"),
    inputFieldCommitment: commitment,
    carriage: selectMidgardFieldCarriageTierV1(inputFieldPreimage.length),
  });
};

export const zeroInputFaultHoldsV1 = (
  evidence: Pick<ZeroInputEvidenceV1, "inputCount">,
): boolean => evidence.inputCount === 0;

export const zeroInputEvidenceClosesV1 = (
  evidence: ZeroInputEvidenceV1,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? zeroInputFaultHoldsV1(evidence)
    : !zeroInputFaultHoldsV1(evidence);

export const ZeroInputVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const ZeroInputStateV1Schema = Data.Object({
  subject: ZeroInputVerdictSubjectV1Schema,
});
