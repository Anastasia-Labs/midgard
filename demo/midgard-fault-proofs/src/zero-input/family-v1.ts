import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  RejectionReasonSchema,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ZERO_INPUT_CATEGORY = "zeroInput" as const;
export const ZERO_INPUT_CATEGORY_ID = "00000005" as const;
export const ZERO_INPUT_FIELD_INDEX = 0 as const;
export const ZERO_INPUT_REASON = "EmptyInputs" as const;

export type ZeroInputFinding = Readonly<{ subject: VerdictSubject }>;

export const classifyZeroInputFinding = (
  finding: ZeroInputFinding,
): ZeroInputFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    throw new Error("zeroInput: verdict subject is not canonical");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (finding.subject.rejection_reason !== ZERO_INPUT_REASON)
      throw new Error("zeroInput: typed rejection reason changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    throw new Error("zeroInput: direction/reason polarity changed");
  }
  return Object.freeze(finding);
};

export type ZeroInputEvidence = ZeroInputFinding &
  Readonly<{
    inputCount: number;
    inputFieldPreimageCbor: string;
    inputFieldCommitment: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareZeroInputEvidence = ({
  finding: rawFinding,
  inputFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ZeroInputFinding;
  readonly inputFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ZeroInputEvidence => {
  const finding = classifyZeroInputFinding(rawFinding);
  const commitment = midgardFieldCommitment(inputFieldPreimage).toString("hex");
  if (commitment !== committedFieldHashHex)
    throw new Error("zeroInput: retained field 0 changed commitment");
  const inputs = decodeMidgardFieldPreimage(inputFieldPreimage);
  return Object.freeze({
    ...finding,
    inputCount: inputs.length,
    inputFieldPreimageCbor: Buffer.from(inputFieldPreimage).toString("hex"),
    inputFieldCommitment: commitment,
    carriage: selectMidgardFieldCarriageTier(inputFieldPreimage.length),
  });
};

export const zeroInputFaultHolds = (
  evidence: Pick<ZeroInputEvidence, "inputCount">,
): boolean => evidence.inputCount === 0;

export const zeroInputEvidenceCloses = (
  evidence: ZeroInputEvidence,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? zeroInputFaultHolds(evidence)
    : !zeroInputFaultHolds(evidence);

export const ZeroInputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const ZeroInputStateSchema = Data.Object({
  subject: ZeroInputVerdictSubjectSchema,
});
