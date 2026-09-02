import { createHash } from "node:crypto";

import {
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  type MidgardValidationMerkleMembershipV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const RECEIVE_PURPOSE_LANGUAGE_CATEGORY_V1 =
  "receivePurposeLanguage" as const;
export const RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID_V1 = "00000034" as const;
export const RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID_V1 =
  "receive-purpose-plutus-v3-forbidden" as const;
export const RECEIVE_PURPOSE_KIND_V1 = 3 as const;
export const PLUTUS_V3_LANGUAGE_TAG_V1 = 3 as const;

const fail = (message: string): never => {
  throw new Error(`${RECEIVE_PURPOSE_LANGUAGE_CATEGORY_V1}: ${message}`);
};
const index = (value: number, label: string) =>
  Number.isSafeInteger(value) && value >= 0
    ? value
    : fail(`${label} is not a non-negative safe integer`);
const hex = (value: string, bytes: number, label: string) =>
  new RegExp(`^[0-9a-f]{${bytes * 2}}$`, "u").test(value)
    ? value
    : fail(`${label} is not canonical ${bytes}-byte hex`);

export type ReceivePurposeLanguageFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  executionIndex: number;
}>;
export const classifyReceivePurposeLanguageFindingV1 = (
  finding: ReceivePurposeLanguageFindingV1,
): ReceivePurposeLanguageFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    fail("verdict subject is not canonical");
  index(finding.executionIndex, "execution index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
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
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  return Object.freeze(finding);
};

export type ReceivePurposeLanguageDescriptorV1 = Readonly<{
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
  purposeMembership: MidgardValidationMerkleMembershipV1;
  sourceMembership: MidgardValidationMerkleMembershipV1;
  executionMembership: MidgardValidationMerkleMembershipV1;
}>;
export type ReceivePurposeLanguageEvidenceV1 = Readonly<{
  finding: ReceivePurposeLanguageFindingV1;
  descriptor: ReceivePurposeLanguageDescriptorV1;
  purposeLeafHex: string;
  sourceLeafHex: string;
  executionLeafHex: string;
}>;

export const prepareReceivePurposeLanguageEvidenceV1 = ({
  finding: raw,
  descriptor,
}: {
  readonly finding: ReceivePurposeLanguageFindingV1;
  readonly descriptor: ReceivePurposeLanguageDescriptorV1;
}): ReceivePurposeLanguageEvidenceV1 => {
  const finding = classifyReceivePurposeLanguageFindingV1(raw);
  if (descriptor.purposeKind !== RECEIVE_PURPOSE_KIND_V1)
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
  const purposeLeaf = hashMidgardScriptPurposeLeafV1({
    purposeKind: 3,
    purposeIndex: BigInt(descriptor.purposeIndex),
    scriptHash,
    subject: Buffer.from(descriptor.purposeSubjectHex, "hex"),
  });
  const sourceLeaf =
    descriptor.originKind === 0
      ? hashMidgardInlineScriptSourceLeafV1({
          sourceIndex: BigInt(descriptor.sourceIndex),
          scriptLanguageTag: descriptor.languageTag,
          scriptHash,
          scriptTotalLength: descriptor.scriptTotalLength,
          itemCommitment: commitment,
        })
      : hashMidgardReferenceScriptSourceLeafV1({
          sourceKey: Buffer.from(descriptor.sourceKeyHex, "hex"),
          scriptLanguageTag: descriptor.languageTag,
          scriptHash,
          scriptTotalLength: descriptor.scriptTotalLength,
          itemCommitment: commitment,
        });
  const executionLeaf = hashMidgardScriptExecutionLeafV1({
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
      !verifyMidgardValidationMerkleMembershipV1({
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
export const receivePurposeLanguageFaultHoldsV1 = (
  evidence: Pick<ReceivePurposeLanguageEvidenceV1, "descriptor">,
): boolean =>
  evidence.descriptor.purposeKind === 3 &&
  evidence.descriptor.languageTag === 3;
export const receivePurposeLanguageEvidenceClosesV1 = (
  evidence: ReceivePurposeLanguageEvidenceV1,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? receivePurposeLanguageFaultHoldsV1(evidence)
    : !receivePurposeLanguageFaultHoldsV1(evidence);
export const receivePurposeLanguageEvidenceIdentityV1 = (
  evidence: ReceivePurposeLanguageEvidenceV1,
): string =>
  createHash("sha256")
    .update(RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID_V1)
    .update(evidence.finding.subject.transaction_id)
    .update(evidence.finding.executionIndex.toString())
    .update(evidence.executionLeafHex)
    .digest("hex");
