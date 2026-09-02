import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardMintPolicyItemV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  type RejectionReasonV1,
  RejectionReasonV1Schema,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1 =
  "mintDeclaredAssetLimit" as const;
export const MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID_V1 = "0000002c" as const;
export const MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1 = 5 as const;
export const MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS_V1 = 16_384 as const;
export const MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET_V1 = 24 as const;

const fail = (message: string): never => {
  throw new Error(`${MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1}: ${message}`);
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
export const decodeMintDeclaredPolicyHeaderV1 = (item: Uint8Array): Header => {
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

const reasonPolicyIndex = (reason: RejectionReasonV1): number => {
  if (typeof reason === "string" || !("MintDeclaredAssetLimit" in reason))
    return fail("typed rejection reason is not MintDeclaredAssetLimit");
  return exactIndex(
    Number(reason.MintDeclaredAssetLimit.policy_index),
    "reason policy index",
  );
};

export type MintDeclaredAssetLimitFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  policyIndex: number;
}>;

export const classifyMintDeclaredAssetLimitFindingV1 = (
  finding: MintDeclaredAssetLimitFindingV1,
): MintDeclaredAssetLimitFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("verdict subject is not canonical");
  const policyIndex = exactIndex(finding.policyIndex, "policy index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (finding.subject.rejection_reason === null)
      return fail("wrongful rejection has no typed reason");
    if (reasonPolicyIndex(finding.subject.rejection_reason) !== policyIndex)
      return fail("typed reason policy coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject: finding.subject, policyIndex });
};

export type MintDeclaredAssetLimitFoldResultV1 = Readonly<{
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
export const foldMintDeclaredAssetLimitV1 = (
  items: readonly Uint8Array[],
  policyIndex: number,
): MintDeclaredAssetLimitFoldResultV1 => {
  exactIndex(policyIndex, "policy index");
  const target = items[policyIndex];
  if (target === undefined) return fail("policy coordinate is outside field 5");
  let accumulatedCount = 0;
  let previousPolicy: Buffer | null = null;
  for (let index = 0; index <= policyIndex; index += 1) {
    const item = items[index]!;
    const header = decodeMintDeclaredPolicyHeaderV1(item);
    if (
      previousPolicy !== null &&
      Buffer.compare(previousPolicy, header.policyId) >= 0
    )
      return fail("mint policy order is not strictly ascending");
    const next = accumulatedCount + header.declaredCount;
    if (index === policyIndex) {
      if (next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS_V1)
        return Object.freeze({
          crossing: true,
          accumulatedCount,
          targetPolicyId: header.policyId.toString("hex"),
          targetDeclaredCount: header.declaredCount,
        });
      const decoded = decodeMidgardMintPolicyItemV1(item);
      if (decoded.assets.length !== header.declaredCount)
        return fail("target policy declared/decoded count changed");
      return Object.freeze({
        crossing: false,
        accumulatedCount: next,
        targetPolicyId: header.policyId.toString("hex"),
        targetDeclaredCount: header.declaredCount,
      });
    }
    if (next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS_V1)
      return fail("an earlier policy is the first declared-count crossing");
    const decoded = decodeMidgardMintPolicyItemV1(item);
    if (decoded.assets.length !== header.declaredCount)
      return fail("prior policy declared/decoded count changed");
    accumulatedCount = next;
    previousPolicy = header.policyId;
  }
  return fail("unreachable mint declared-count fold state");
};

export type MintDeclaredAssetLimitEvidenceV1 = MintDeclaredAssetLimitFindingV1 &
  MintDeclaredAssetLimitFoldResultV1 &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    targetItemHex: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareMintDeclaredAssetLimitEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MintDeclaredAssetLimitFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MintDeclaredAssetLimitEvidenceV1 => {
  const finding = classifyMintDeclaredAssetLimitFindingV1(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("field commitment is not 32-byte lowercase hex");
  const actual = midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field-5 bytes do not match the compact commitment");
  const items = decodeMidgardFieldPreimageV1(fieldPreimage);
  const target = items[finding.policyIndex];
  if (target === undefined) return fail("policy coordinate is outside field 5");
  return Object.freeze({
    ...finding,
    ...foldMintDeclaredAssetLimitV1(items, finding.policyIndex),
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    targetItemHex: target.toString("hex"),
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
  });
};

export const mintDeclaredAssetLimitEvidenceClosesV1 = (
  evidence: MintDeclaredAssetLimitEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, evidence.crossing);

export const MintDeclaredAssetLimitVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const MintDeclaredAssetLimitBoundPolicyV1Schema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectV1Schema,
  policy_index: Data.Integer(),
});

export const MintDeclaredAssetLimitAuthenticationStateV1Schema = Data.Enum([
  Data.Object({
    Bound: Data.Object({ bound: MintDeclaredAssetLimitBoundPolicyV1Schema }),
  }),
  Data.Object({
    Grammar: Data.Object({
      bound: MintDeclaredAssetLimitBoundPolicyV1Schema,
      checkpoint_hash: Data.Bytes(),
    }),
  }),
]);

export const MintDeclaredAssetLimitFoldStateV1Schema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectV1Schema,
  policy_index: Data.Integer(),
  target_policy_id: Data.Bytes(),
  target_declared_count: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  accumulated_count: Data.Integer(),
  previous_policy: Data.Bytes(),
  outcome: Data.Integer(),
});

export const MintDeclaredAssetLimitDecisionStateV1Schema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectV1Schema,
  policy_index: Data.Integer(),
  crossing: Data.Boolean(),
});
