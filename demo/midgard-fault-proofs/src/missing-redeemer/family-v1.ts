import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardRedeemerWitnessItemV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptPurposeLeafV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const MISSING_REDEEMER_CATEGORY_V1 = "missingRedeemer" as const;
export const MISSING_REDEEMER_CATEGORY_ID_V1 = "0000002e" as const;
export const MISSING_REDEEMER_VIOLATION_ID_V1 = "redeemer-missing" as const;
export const MISSING_REDEEMER_SCAN_BATCH_V1 = 16;

export type MissingRedeemerPurposeKindV1 = 0 | 1 | 2 | 3;
export type MissingRedeemerPurposeSourceV1 = "witness" | "resolved-reference";

export type AuthenticatedScriptPurposeV1 = Readonly<{
  purposeKind: MissingRedeemerPurposeKindV1;
  purposeIndex: number;
  scriptHashHex: string;
  subjectHex: string;
  source: MissingRedeemerPurposeSourceV1;
  sourceIndex: number;
  sourceOriginKind: 0 | 1;
  sourceKeyHex: string;
  sourceLanguageTag: 3 | 128;
  sourceTotalLength: number;
  sourceItemCommitmentHex: string;
  sourceLeafHashHex: string;
  traceStateHashHex: string;
  workRootHex: string;
}>;

export type MissingRedeemerFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  purposeKind: MissingRedeemerPurposeKindV1;
  purposeIndex: number;
}>;

export type MissingRedeemerEvidenceV1 = MissingRedeemerFindingV1 &
  Readonly<{
    purpose: AuthenticatedScriptPurposeV1;
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemCount: number;
    scannedPointers: readonly string[];
    checkpoints: readonly Readonly<{ cursor: number; found: boolean }>[];
    redeemerMissing: boolean;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

const fail = (message: string): never => {
  throw new Error(`${MISSING_REDEEMER_CATEGORY_V1}: ${message}`);
};

const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

const purposeTag = (kind: MissingRedeemerPurposeKindV1): number =>
  [0, 1, 3, 6][kind]!;

export const classifyMissingRedeemerFindingV1 = (
  finding: MissingRedeemerFindingV1,
): MissingRedeemerFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("verdict subject is not canonical");
  exactIndex(finding.purposeIndex, "purpose index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "string" ||
      !("RedeemerMissing" in reason) ||
      Number(reason.RedeemerMissing.purpose_kind) !== finding.purposeKind ||
      Number(reason.RedeemerMissing.purpose_index) !== finding.purposeIndex
    )
      return fail("typed reason/purpose coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity changed");
  }
  return Object.freeze({ ...finding });
};

const purposeKey = (
  purpose: Pick<AuthenticatedScriptPurposeV1, "purposeKind" | "purposeIndex">,
): string =>
  `${purpose.purposeKind.toString()}:${purpose.purposeIndex.toString()}`;

const authenticatePurpose = (purpose: AuthenticatedScriptPurposeV1): string => {
  exactIndex(purpose.purposeIndex, "frontier purpose index");
  if (!/^[0-9a-f]{56}$/u.test(purpose.scriptHashHex))
    return fail("frontier script hash is malformed");
  if (!/^(?:[0-9a-f]{2})*$/u.test(purpose.subjectHex))
    return fail("frontier subject is malformed");
  exactIndex(purpose.sourceIndex, "frontier source index");
  if (purpose.sourceLanguageTag !== 3 && purpose.sourceLanguageTag !== 128)
    return fail("selected source is not a redeemer-bearing Plutus language");
  if (
    (purpose.source === "witness" && purpose.sourceOriginKind !== 0) ||
    (purpose.source !== "witness" && purpose.sourceOriginKind !== 1)
  )
    return fail("selected source location/origin changed");
  if (!/^(?:[0-9a-f]{2})+$/u.test(purpose.sourceKeyHex))
    return fail("selected source key is malformed");
  exactIndex(purpose.sourceTotalLength, "selected source total length");
  if (purpose.sourceTotalLength === 0)
    return fail("selected source total length must be positive");
  if (!/^[0-9a-f]{64}$/u.test(purpose.sourceItemCommitmentHex))
    return fail("selected source item commitment is malformed");
  if (
    !/^[0-9a-f]{64}$/u.test(purpose.traceStateHashHex) ||
    !/^[0-9a-f]{64}$/u.test(purpose.workRootHex)
  )
    return fail("stage-10 trace commitment is malformed");
  const sourceLeaf =
    purpose.sourceOriginKind === 0
      ? (() => {
          if (
            !Buffer.from(purpose.sourceKeyHex, "hex").equals(
              encodeCbor(BigInt(purpose.sourceIndex)),
            )
          )
            return fail("inline source key/index changed");
          return hashMidgardInlineScriptSourceLeafV1({
            sourceIndex: BigInt(purpose.sourceIndex),
            scriptLanguageTag: purpose.sourceLanguageTag,
            scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
            scriptTotalLength: purpose.sourceTotalLength,
            itemCommitment: Buffer.from(purpose.sourceItemCommitmentHex, "hex"),
          });
        })()
      : hashMidgardReferenceScriptSourceLeafV1({
          sourceKey: Buffer.from(purpose.sourceKeyHex, "hex"),
          scriptLanguageTag: purpose.sourceLanguageTag,
          scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
          scriptTotalLength: purpose.sourceTotalLength,
          itemCommitment: Buffer.from(purpose.sourceItemCommitmentHex, "hex"),
        });
  if (sourceLeaf.toString("hex") !== purpose.sourceLeafHashHex)
    return fail("selected source descriptor/leaf changed");
  return hashMidgardScriptPurposeLeafV1({
    purposeKind: purpose.purposeKind,
    purposeIndex: BigInt(purpose.purposeIndex),
    scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
    subject: Buffer.from(purpose.subjectHex, "hex"),
  }).toString("hex");
};

/** Consumes one retained stage-10 authenticated purpose and every field-8 item. */
export const prepareMissingRedeemerEvidenceV1 = ({
  finding: rawFinding,
  authenticatedPurpose,
  redeemerFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MissingRedeemerFindingV1;
  readonly authenticatedPurpose: AuthenticatedScriptPurposeV1;
  readonly redeemerFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MissingRedeemerEvidenceV1 => {
  const finding = classifyMissingRedeemerFindingV1(rawFinding);
  authenticatePurpose(authenticatedPurpose);
  if (purposeKey(authenticatedPurpose) !== purposeKey(finding))
    return fail("target differs from the authenticated stage-10 purpose");
  const actual = midgardFieldCommitmentV1(redeemerFieldPreimage).toString(
    "hex",
  );
  if (actual !== committedFieldHashHex)
    return fail("retained redeemer field changed commitment");
  const items = decodeMidgardFieldPreimageV1(redeemerFieldPreimage);
  const pointers: string[] = [];
  const checkpoints: { cursor: number; found: boolean }[] = [];
  let found = false;
  items.forEach((item, index) => {
    const decoded = decodeMidgardRedeemerWitnessItemV1(item);
    const pointer = `${purposeTag(finding.purposeKind).toString()}:${finding.purposeIndex.toString()}`;
    const actualTag = (
      {
        Spend: 0,
        Mint: 1,
        Cert: 2,
        Reward: 3,
        Vote: 4,
        Propose: 5,
        Receive: 6,
      } as const
    )[decoded.purpose];
    const actualPointer = `${actualTag.toString()}:${decoded.index.toString()}`;
    pointers.push(actualPointer);
    if (actualPointer === pointer) found = true;
    if (
      (index + 1) % MISSING_REDEEMER_SCAN_BATCH_V1 === 0 ||
      index + 1 === items.length
    )
      checkpoints.push({ cursor: index + 1, found });
  });
  if (items.length === 0) checkpoints.push({ cursor: 0, found: false });
  return Object.freeze({
    ...finding,
    purpose: authenticatedPurpose,
    fieldPreimageHex: Buffer.from(redeemerFieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    itemCount: items.length,
    scannedPointers: Object.freeze(pointers),
    checkpoints: Object.freeze(
      checkpoints.map((checkpoint) => Object.freeze(checkpoint)),
    ),
    redeemerMissing: !found,
    carriage: selectMidgardFieldCarriageTierV1(redeemerFieldPreimage.length),
  });
};

export const missingRedeemerEvidenceClosesV1 = (
  evidence: MissingRedeemerEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, evidence.redeemerMissing);

export const missingRedeemerEvidenceIdentityV1 = (
  evidence: MissingRedeemerEvidenceV1,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    purposeKey(evidence),
    evidence.purpose.traceStateHashHex,
    evidence.purpose.workRootHex,
    evidence.purpose.sourceLeafHashHex,
    evidence.fieldCommitmentHex,
  ].join(":");
