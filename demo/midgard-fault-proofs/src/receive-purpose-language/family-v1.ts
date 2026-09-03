import { createHash } from "node:crypto";

import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  type MidgardValidationMerkleMembership,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const RECEIVE_PURPOSE_LANGUAGE_CATEGORY =
  "receivePurposeLanguage" as const;
export const RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID = "00000034" as const;
export const RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID =
  "receive-purpose-plutus-v3-forbidden" as const;
export const RECEIVE_PURPOSE_KIND = 3 as const;
export const PLUTUS_V3_LANGUAGE_TAG = 3 as const;

const fail = (message: string): never => {
  throw new Error(`${RECEIVE_PURPOSE_LANGUAGE_CATEGORY}: ${message}`);
};
const index = (value: number, label: string) =>
  Number.isSafeInteger(value) && value >= 0
    ? value
    : fail(`${label} is not a non-negative safe integer`);
const hex = (value: string, bytes: number, label: string) =>
  new RegExp(`^[0-9a-f]{${bytes * 2}}$`, "u").test(value)
    ? value
    : fail(`${label} is not canonical ${bytes}-byte hex`);

export type ReceivePurposeLanguageFinding = Readonly<{
  subject: VerdictSubject;
  executionIndex: number;
}>;
export const classifyReceivePurposeLanguageFinding = (
  finding: ReceivePurposeLanguageFinding,
): ReceivePurposeLanguageFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    fail("verdict subject is not canonical");
  index(finding.executionIndex, "execution index");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "string" ||
      !("ReceivePurposePlutusV3Forbidden" in reason) ||
      reason.ReceivePurposePlutusV3Forbidden.execution_index !==
        BigInt(finding.executionIndex)
    )
      fail("typed rejection reason or execution coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  return Object.freeze(finding);
};

export type ReceivePurposeLanguageDescriptor = Readonly<{
  sourceIndex: number;
  originKind: 0 | 1;
  sourceKeyHex: string;
  languageTag: 0 | 3 | 128;
  scriptHashHex: string;
  scriptTotalLength: number;
  scriptItemCommitmentHex: string;
  purposeKind: 3;
  purposeIndex: number;
  purposeSubjectHex: string;
  redeemerLeafHex: string;
  purposeMembership: MidgardValidationMerkleMembership;
  sourceMembership: MidgardValidationMerkleMembership;
  executionMembership: MidgardValidationMerkleMembership;
}>;
export type ReceivePurposeLanguageEvidence = Readonly<{
  finding: ReceivePurposeLanguageFinding;
  descriptor: ReceivePurposeLanguageDescriptor;
  purposeLeafHex: string;
  sourceLeafHex: string;
  executionLeafHex: string;
}>;

export const prepareReceivePurposeLanguageEvidence = ({
  finding: raw,
  descriptor,
}: {
  readonly finding: ReceivePurposeLanguageFinding;
  readonly descriptor: ReceivePurposeLanguageDescriptor;
}): ReceivePurposeLanguageEvidence => {
  const finding = classifyReceivePurposeLanguageFinding(raw);
  if (descriptor.purposeKind !== RECEIVE_PURPOSE_KIND)
    fail("authenticated purpose is not receive");
  index(descriptor.sourceIndex, "source index");
  index(descriptor.purposeIndex, "purpose index");
  if (
    descriptor.executionMembership.leafIndex !== finding.executionIndex ||
    descriptor.purposeMembership.leafIndex !== finding.executionIndex
  )
    fail("execution coordinate changed");
  const scriptHash = Buffer.from(
    hex(descriptor.scriptHashHex, 28, "script hash"),
    "hex",
  );
  const commitment = Buffer.from(
    hex(descriptor.scriptItemCommitmentHex, 32, "script item commitment"),
    "hex",
  );
  if (
    descriptor.scriptTotalLength <= 0 ||
    !Number.isSafeInteger(descriptor.scriptTotalLength)
  )
    fail("script length is invalid");
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: 3,
    purposeIndex: BigInt(descriptor.purposeIndex),
    scriptHash,
    subject: Buffer.from(descriptor.purposeSubjectHex, "hex"),
  });
  const sourceLeaf =
    descriptor.originKind === 0
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(descriptor.sourceIndex),
          scriptLanguageTag: descriptor.languageTag,
          scriptHash,
          scriptTotalLength: descriptor.scriptTotalLength,
          itemCommitment: commitment,
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(descriptor.sourceKeyHex, "hex"),
          scriptLanguageTag: descriptor.languageTag,
          scriptHash,
          scriptTotalLength: descriptor.scriptTotalLength,
          itemCommitment: commitment,
        });
  const executionLeaf = hashMidgardScriptExecutionLeaf({
    languageTag: descriptor.languageTag,
    purposeLeaf,
    sourceLeaf,
    redeemerLeaf: Buffer.from(descriptor.redeemerLeafHex, "hex"),
  });
  for (const [membership, leaf, label] of [
    [descriptor.purposeMembership, purposeLeaf, "purpose"],
    [descriptor.sourceMembership, sourceLeaf, "source"],
    [descriptor.executionMembership, executionLeaf, "execution"],
  ] as const)
    if (
      !verifyMidgardValidationMerkleMembership({
        ...membership,
        leafHash: leaf,
      })
    )
      fail(`${label} membership is invalid`);
  return Object.freeze({
    finding,
    descriptor,
    purposeLeafHex: purposeLeaf.toString("hex"),
    sourceLeafHex: sourceLeaf.toString("hex"),
    executionLeafHex: executionLeaf.toString("hex"),
  });
};
export const receivePurposeLanguageFaultHolds = (
  evidence: Pick<ReceivePurposeLanguageEvidence, "descriptor">,
): boolean =>
  evidence.descriptor.purposeKind === 3 &&
  evidence.descriptor.languageTag === 3;
export const receivePurposeLanguageEvidenceCloses = (
  evidence: ReceivePurposeLanguageEvidence,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? receivePurposeLanguageFaultHolds(evidence)
    : !receivePurposeLanguageFaultHolds(evidence);
export const receivePurposeLanguageEvidenceIdentity = (
  evidence: ReceivePurposeLanguageEvidence,
): string =>
  createHash("sha256")
    .update(RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID)
    .update(evidence.finding.subject.transaction_id)
    .update(evidence.finding.executionIndex.toString())
    .update(evidence.executionLeafHex)
    .digest("hex");
