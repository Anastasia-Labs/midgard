import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type RejectionReason,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const MINT_DECLARED_ASSET_LIMIT_CATEGORY =
  "mintDeclaredAssetLimit" as const;

export const MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID = "0000002c" as const;

export const MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX = 5 as const;

export const MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS = 16_384 as const;

/** Field items one step-02 grammar transaction certifies. */
export const MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET = 24 as const;

/** Work units one step-03 fold transaction spends (`staged_fold_budget`). */
export const MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET = 192 as const;

/** Work units opening one policy item costs (`fold_policy_cost`). */
export const MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST = 8 as const;

export const MINT_DECLARED_OUTCOME_SCANNING = 0 as const;

export const MINT_DECLARED_OUTCOME_CROSSING = 1 as const;

export const MINT_DECLARED_OUTCOME_NON_CROSSING = 2 as const;

export const fail = (message: string): never => {
  throw new Error(`${MINT_DECLARED_ASSET_LIMIT_CATEGORY}: ${message}`);
};

export const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

type Header = Readonly<{
  policyId: Buffer;
  declaredCount: number;
  assetsOffset: number;
}>;

const HEAD_WIDTHS: Readonly<Record<number, number>> = { 24: 1, 25: 2, 26: 4 };

export const HEAD_MINIMUMS: Readonly<Record<number, number>> = {
  1: 24,
  2: 256,
  4: 65_536,
};

export const readCanonicalLength = (
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
  const width = HEAD_WIDTHS[ai];
  if (width === undefined || offset + 1 + width > bytes.length)
    return fail(`${label} has an unsupported or truncated CBOR head`);
  let value = 0;
  for (let cursor = offset + 1; cursor <= offset + width; cursor += 1)
    value = value * 256 + bytes[cursor]!;
  if (value < HEAD_MINIMUMS[width]!)
    return fail(`${label} CBOR head is not minimal`);
  return { value, next: offset + 1 + width };
};

/**
 * The machine's `decode_definite_array_header_at`: any definite array head
 * width is accepted, so a two-element head spelled `98 02` opens a policy
 * item for the machine and for this twin alike.
 */
const readDefiniteArrayHead = (
  bytes: Uint8Array,
  offset: number,
): Readonly<{ value: number; next: number }> => {
  const head = bytes[offset];
  if (head === undefined || head >> 5 !== 4)
    return fail("mint policy item is not a definite array");
  const ai = head & 31;
  if (ai < 24) return { value: ai, next: offset + 1 };
  const width = HEAD_WIDTHS[ai];
  if (width === undefined || offset + 1 + width > bytes.length)
    return fail("mint policy item array head is unsupported or truncated");
  let value = 0;
  for (let cursor = offset + 1; cursor <= offset + width; cursor += 1)
    value = value * 256 + bytes[cursor]!;
  return { value, next: offset + 1 + width };
};

/** The machine's `decode_canonical_int_at` (major 0/1, minimal width). */
export const readCanonicalInt = (
  bytes: Uint8Array,
  offset: number,
): Readonly<{ value: bigint; next: number }> => {
  const head = bytes[offset];
  if (head === undefined || head >> 5 > 1)
    return fail("mint asset quantity is not an integer");
  const major = head >> 5;
  const ai = head & 31;
  let magnitude: bigint;
  let next: number;
  if (ai < 24) {
    magnitude = BigInt(ai);
    next = offset + 1;
  } else {
    const width = ai === 27 ? 8 : HEAD_WIDTHS[ai];
    if (width === undefined || offset + 1 + width > bytes.length)
      return fail("mint asset quantity head is unsupported or truncated");
    magnitude = 0n;
    for (let cursor = offset + 1; cursor <= offset + width; cursor += 1)
      magnitude = magnitude * 256n + BigInt(bytes[cursor]!);
    const minimum =
      width === 8 ? 4_294_967_296n : BigInt(HEAD_MINIMUMS[width]!);
    if (magnitude < minimum) return fail("mint asset quantity is not minimal");
    next = offset + 1 + width;
  }
  return { value: major === 0 ? magnitude : -1n - magnitude, next };
};

/** `canonical_bytes_key_precedes`: shorter first, then lexicographic. */
export const canonicalKeyPrecedes = (
  left: Uint8Array,
  right: Uint8Array,
): boolean =>
  left.length < right.length ||
  (left.length === right.length &&
    Buffer.compare(Buffer.from(left), Buffer.from(right)) < 0);

/** The exact pre-rejection header read performed by the frozen machine. */
export const decodeMintDeclaredPolicyHeader = (item: Uint8Array): Header => {
  const array = readDefiniteArrayHead(item, 0);
  if (array.value !== 2)
    return fail("mint policy item is not the canonical two-element array");
  const policyLength = readCanonicalLength(item, array.next, 2, "policy id");
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

/**
 * Off-chain twin of the on-chain `FoldStateV1` cursor fields. `activePolicy`
 * is empty while no policy item is open; while one is open the walk stays on
 * that item and `itemCursor`/`assetsRemaining`/`policyAssetCursor`/
 * `previousAsset` mirror the machine's `MintFoldControlV1`.
 */
export type MintDeclaredFoldCursor = Readonly<{
  accumulatedCount: number;
  previousPolicy: string;
  activePolicy: string;
  itemCursor: number;
  assetsRemaining: number;
  policyAssetCursor: number;
  previousAsset: string;
  outcome: 0 | 1 | 2;
}>;

export type MintDeclaredFoldTarget = Readonly<{
  policyIndex: number;
  targetPolicyId: string;
  targetDeclaredCount: number;
}>;

export const initialMintDeclaredFoldCursor = (): MintDeclaredFoldCursor =>
  Object.freeze({
    accumulatedCount: 0,
    previousPolicy: "",
    activePolicy: "",
    itemCursor: 0,
    assetsRemaining: 0,
    policyAssetCursor: 0,
    previousAsset: "",
    outcome: MINT_DECLARED_OUTCOME_SCANNING,
  });

/** `begin_policy_v1`: opens the item at `itemIndex` or records the crossing. */
export const beginMintDeclaredPolicy = ({
  cursor,
  itemIndex,
  item,
  target,
}: {
  readonly cursor: MintDeclaredFoldCursor;
  readonly itemIndex: number;
  readonly item: Uint8Array;
  readonly target: MintDeclaredFoldTarget;
}): MintDeclaredFoldCursor => {
  if (
    cursor.outcome !== MINT_DECLARED_OUTCOME_SCANNING ||
    cursor.activePolicy !== "" ||
    itemIndex < 0 ||
    itemIndex > target.policyIndex ||
    cursor.accumulatedCount < 0 ||
    cursor.accumulatedCount > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS
  )
    return fail("fold cursor cannot open a policy here");
  const header = decodeMintDeclaredPolicyHeader(item);
  if (
    itemIndex !== 0 &&
    !canonicalKeyPrecedes(
      Buffer.from(cursor.previousPolicy, "hex"),
      header.policyId,
    )
  )
    return fail("mint policy order is not strictly ascending");
  const next = cursor.accumulatedCount + header.declaredCount;
  const open = Object.freeze({
    ...cursor,
    activePolicy: header.policyId.toString("hex"),
    itemCursor: header.assetsOffset,
    assetsRemaining: header.declaredCount,
    policyAssetCursor: 0,
    previousAsset: "",
  });
  if (itemIndex === target.policyIndex) {
    if (
      header.policyId.toString("hex") !== target.targetPolicyId ||
      header.declaredCount !== target.targetDeclaredCount
    )
      return fail("target policy header changed");
    return next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS
      ? Object.freeze({ ...cursor, outcome: MINT_DECLARED_OUTCOME_CROSSING })
      : open;
  }
  if (next > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
    return fail("an earlier policy is the first declared-count crossing");
  return open;
};
