import {
  decodeMidgardFieldPreimage,
  decodeMidgardMintPolicyItem,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type RejectionReason,
  RejectionReasonSchema,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const MINT_DECLARED_ASSET_LIMIT_CATEGORY =
  "mintDeclaredAssetLimit" as const;
export const MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID = "0000002c" as const;
export const MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX = 5 as const;
export const MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS = 16_384 as const;
export const MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET = 24 as const;

const fail = (message: string): never => {
  throw new Error(`${MINT_DECLARED_ASSET_LIMIT_CATEGORY}: ${message}`);
};

const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

type Header = Readonly<{
  policyId: Buffer;
  declaredCount: number;
  assetsOffset: number;
}>;

const readCanonicalLength = (
  bytes: Uint8Array,
  offset: number,
  major: number,
  label: string,
): Readonly<{ value: number; next: number }> => {
  const head = bytes[offset];
  if (head === undefined || head >> 5 !== major)
    return fail(`${label} has the wrong CBOR major type`);
  const ai = head & 31;
  if (ai < 24) return { value: ai, next: offset + 1 };
  const widths: Readonly<Record<number, number>> = { 24: 1, 25: 2, 26: 4 };
  const width = widths[ai];
  if (width === undefined || offset + 1 + width > bytes.length)
    return fail(`${label} has an unsupported or truncated CBOR head`);
  let value = 0;
  for (let cursor = offset + 1; cursor <= offset + width; cursor += 1)
    value = value * 256 + bytes[cursor]!;
  const minimum = width === 1 ? 24 : width === 2 ? 256 : 65_536;
  if (value < minimum) return fail(`${label} CBOR head is not minimal`);
  return { value, next: offset + 1 + width };
};

/** The exact pre-rejection header read performed by the frozen machine. */
export const decodeMintDeclaredPolicyHeader = (item: Uint8Array): Header => {
  if (item[0] !== 0x82)
    return fail("mint policy item is not the canonical two-element array");
  const policyLength = readCanonicalLength(item, 1, 2, "policy id");
  if (policyLength.value !== 28) return fail("mint policy id is not 28 bytes");
  const policyEnd = policyLength.next + policyLength.value;
  if (policyEnd > item.length) return fail("mint policy id is truncated");
  const map = readCanonicalLength(item, policyEnd, 5, "asset map");
  if (map.value === 0 || map.next >= item.length)
    return fail("mint policy asset map is empty or carries no body bytes");
  return Object.freeze({
    policyId: Buffer.from(item.subarray(policyLength.next, policyEnd)),
    declaredCount: map.value,
    assetsOffset: map.next,
  });
};

const reasonPolicyIndex = (reason: RejectionReason): number => {
  if (typeof reason === "string" || !("MintDeclaredAssetLimit" in reason))
    return fail("typed rejection reason is not MintDeclaredAssetLimit");
  return exactIndex(
    Number(reason.MintDeclaredAssetLimit.policy_index),
    "reason policy index",
  );
};

export type MintDeclaredAssetLimitFinding = Readonly<{
  subject: VerdictSubject;
  policyIndex: number;
}>;

export const classifyMintDeclaredAssetLimitFinding = (
  finding: MintDeclaredAssetLimitFinding,
): MintDeclaredAssetLimitFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  const policyIndex = exactIndex(finding.policyIndex, "policy index");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (finding.subject.rejection_reason === null)
      return fail("wrongful rejection has no typed reason");
    if (reasonPolicyIndex(finding.subject.rejection_reason) !== policyIndex)
      return fail("typed reason policy coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject: finding.subject, policyIndex });
};

export type MintDeclaredAssetLimitFoldResult = Readonly<{
  crossing: boolean;
  accumulatedCount: number;
  targetPolicyId: string;
  targetDeclaredCount: number;
}>;

/**
 * Complete deterministic twin of the policy-level machine order. Earlier
 * items must decode fully; the target crossing is decided from its map header
 * before target-body decoding.
 */
export const foldMintDeclaredAssetLimit = (
  items: readonly Uint8Array[],
  policyIndex: number,
): MintDeclaredAssetLimitFoldResult => {
  exactIndex(policyIndex, "policy index");
  const target = items[policyIndex];
  if (target === undefined) return fail("policy coordinate is outside field 5");
  let accumulatedCount = 0;
  let previousPolicy: Buffer | null = null;
  for (let index = 0; index <= policyIndex; index += 1) {
    const item = items[index]!;
    const header = decodeMintDeclaredPolicyHeader(item);
    if (
      previousPolicy !== null &&
      Buffer.compare(previousPolicy, header.policyId) >= 0
    )
      return fail("mint policy order is not strictly ascending");
    const next = accumulatedCount + header.declaredCount;
    if (index === policyIndex) {
      if (next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
        return Object.freeze({
          crossing: true,
          accumulatedCount,
          targetPolicyId: header.policyId.toString("hex"),
          targetDeclaredCount: header.declaredCount,
        });
      const decoded = decodeMidgardMintPolicyItem(item);
      if (decoded.assets.length !== header.declaredCount)
        return fail("target policy declared/decoded count changed");
      return Object.freeze({
        crossing: false,
        accumulatedCount: next,
        targetPolicyId: header.policyId.toString("hex"),
        targetDeclaredCount: header.declaredCount,
      });
    }
    if (next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
      return fail("an earlier policy is the first declared-count crossing");
    const decoded = decodeMidgardMintPolicyItem(item);
    if (decoded.assets.length !== header.declaredCount)
      return fail("prior policy declared/decoded count changed");
    accumulatedCount = next;
    previousPolicy = header.policyId;
  }
  return fail("unreachable mint declared-count fold state");
};

export type MintDeclaredAssetLimitEvidence = MintDeclaredAssetLimitFinding &
  MintDeclaredAssetLimitFoldResult &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    targetItemHex: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareMintDeclaredAssetLimitEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MintDeclaredAssetLimitFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MintDeclaredAssetLimitEvidence => {
  const finding = classifyMintDeclaredAssetLimitFinding(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("field commitment is not 32-byte lowercase hex");
  const actual = midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field-5 bytes do not match the compact commitment");
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  const target = items[finding.policyIndex];
  if (target === undefined) return fail("policy coordinate is outside field 5");
  return Object.freeze({
    ...finding,
    ...foldMintDeclaredAssetLimit(items, finding.policyIndex),
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    targetItemHex: target.toString("hex"),
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
  });
};

export const mintDeclaredAssetLimitEvidenceCloses = (
  evidence: MintDeclaredAssetLimitEvidence,
): boolean => terminalVerdictContradiction(evidence.subject, evidence.crossing);

export const MintDeclaredAssetLimitVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const MintDeclaredAssetLimitBoundPolicySchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
});

export const MintDeclaredAssetLimitAuthenticationStateSchema = Data.Enum([
  Data.Object({
    Bound: Data.Object({ bound: MintDeclaredAssetLimitBoundPolicySchema }),
  }),
  Data.Object({
    Grammar: Data.Object({
      bound: MintDeclaredAssetLimitBoundPolicySchema,
      checkpoint_hash: Data.Bytes(),
    }),
  }),
]);

export const MintDeclaredAssetLimitFoldStateSchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
  target_policy_id: Data.Bytes(),
  target_declared_count: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  accumulated_count: Data.Integer(),
  previous_policy: Data.Bytes(),
  outcome: Data.Integer(),
});

export const MintDeclaredAssetLimitDecisionStateSchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
  crossing: Data.Boolean(),
});
