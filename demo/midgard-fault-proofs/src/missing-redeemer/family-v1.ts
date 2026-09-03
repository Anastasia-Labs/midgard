import {
  decodeMidgardFieldPreimage,
  decodeMidgardRedeemerWitnessItem,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const MISSING_REDEEMER_CATEGORY = "missingRedeemer" as const;
export const MISSING_REDEEMER_CATEGORY_ID = "0000002e" as const;
export const MISSING_REDEEMER_VIOLATION_ID = "redeemer-missing" as const;
export const MISSING_REDEEMER_SCAN_BATCH = 16;

export type MissingRedeemerPurposeKind = 0 | 1 | 2 | 3;
export type MissingRedeemerPurposeSource = "witness" | "resolved-reference";

export type AuthenticatedScriptPurpose = Readonly<{
  purposeKind: MissingRedeemerPurposeKind;
  purposeIndex: number;
  scriptHashHex: string;
  subjectHex: string;
  source: MissingRedeemerPurposeSource;
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

export type MissingRedeemerFinding = Readonly<{
  subject: VerdictSubject;
  purposeKind: MissingRedeemerPurposeKind;
  purposeIndex: number;
}>;

export type MissingRedeemerEvidence = MissingRedeemerFinding &
  Readonly<{
    purpose: AuthenticatedScriptPurpose;
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemCount: number;
    scannedPointers: readonly string[];
    checkpoints: readonly Readonly<{ cursor: number; found: boolean }>[];
    redeemerMissing: boolean;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

const fail = (message: string): never => {
  throw new Error(`${MISSING_REDEEMER_CATEGORY}: ${message}`);
};

const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

const purposeTag = (kind: MissingRedeemerPurposeKind): number =>
  [0, 1, 3, 6][kind]!;

export const classifyMissingRedeemerFinding = (
  finding: MissingRedeemerFinding,
): MissingRedeemerFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  exactIndex(finding.purposeIndex, "purpose index");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
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
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity changed");
  }
  return Object.freeze({ ...finding });
};

const purposeKey = (
  purpose: Pick<AuthenticatedScriptPurpose, "purposeKind" | "purposeIndex">,
): string =>
  `${purpose.purposeKind.toString()}:${purpose.purposeIndex.toString()}`;

const authenticatePurpose = (purpose: AuthenticatedScriptPurpose): string => {
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
          return hashMidgardInlineScriptSourceLeaf({
            sourceIndex: BigInt(purpose.sourceIndex),
            scriptLanguageTag: purpose.sourceLanguageTag,
            scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
            scriptTotalLength: purpose.sourceTotalLength,
            itemCommitment: Buffer.from(purpose.sourceItemCommitmentHex, "hex"),
          });
        })()
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(purpose.sourceKeyHex, "hex"),
          scriptLanguageTag: purpose.sourceLanguageTag,
          scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
          scriptTotalLength: purpose.sourceTotalLength,
          itemCommitment: Buffer.from(purpose.sourceItemCommitmentHex, "hex"),
        });
  if (sourceLeaf.toString("hex") !== purpose.sourceLeafHashHex)
    return fail("selected source descriptor/leaf changed");
  return hashMidgardScriptPurposeLeaf({
    purposeKind: purpose.purposeKind,
    purposeIndex: BigInt(purpose.purposeIndex),
    scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
    subject: Buffer.from(purpose.subjectHex, "hex"),
  }).toString("hex");
};

/** Consumes one retained stage-10 authenticated purpose and every field-8 item. */
export const prepareMissingRedeemerEvidence = ({
  finding: rawFinding,
  authenticatedPurpose,
  redeemerFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MissingRedeemerFinding;
  readonly authenticatedPurpose: AuthenticatedScriptPurpose;
  readonly redeemerFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MissingRedeemerEvidence => {
  const finding = classifyMissingRedeemerFinding(rawFinding);
  authenticatePurpose(authenticatedPurpose);
  if (purposeKey(authenticatedPurpose) !== purposeKey(finding))
    return fail("target differs from the authenticated stage-10 purpose");
  const actual = midgardFieldCommitment(redeemerFieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained redeemer field changed commitment");
  const items = decodeMidgardFieldPreimage(redeemerFieldPreimage);
  const pointers: string[] = [];
  const checkpoints: { cursor: number; found: boolean }[] = [];
  let found = false;
  items.forEach((item, index) => {
    const decoded = decodeMidgardRedeemerWitnessItem(item);
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
      (index + 1) % MISSING_REDEEMER_SCAN_BATCH === 0 ||
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
    carriage: selectMidgardFieldCarriageTier(redeemerFieldPreimage.length),
  });
};

export const missingRedeemerEvidenceCloses = (
  evidence: MissingRedeemerEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, evidence.redeemerMissing);

export const missingRedeemerEvidenceIdentity = (
  evidence: MissingRedeemerEvidence,
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
