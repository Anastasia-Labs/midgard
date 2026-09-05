import {
  decodeMidgardFieldPreimage,
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
/** Field items one step-02 grammar transaction certifies. */
export const MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET = 24 as const;
/** Work units one step-03 fold transaction spends (`staged_fold_budget`). */
export const MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET = 192 as const;
/** Work units opening one policy item costs (`fold_policy_cost`). */
export const MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST = 8 as const;

export const MINT_DECLARED_OUTCOME_SCANNING = 0 as const;
export const MINT_DECLARED_OUTCOME_CROSSING = 1 as const;
export const MINT_DECLARED_OUTCOME_NON_CROSSING = 2 as const;

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

const HEAD_WIDTHS: Readonly<Record<number, number>> = { 24: 1, 25: 2, 26: 4 };
const HEAD_MINIMUMS: Readonly<Record<number, number>> = {
  1: 24,
  2: 256,
  4: 65_536,
};

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
const readCanonicalInt = (
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
const canonicalKeyPrecedes = (left: Uint8Array, right: Uint8Array): boolean =>
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

/** `consume_asset_v1`: one asset entry of the open item. */
export const consumeMintDeclaredAsset = ({
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
    cursor.activePolicy.length !== 56 ||
    cursor.assetsRemaining <= 0 ||
    cursor.policyAssetCursor < 0
  )
    return fail("fold cursor has no open policy to consume");
  const name = readCanonicalLength(item, cursor.itemCursor, 2, "asset name");
  const nameEnd = name.next + name.value;
  if (name.value > 32 || nameEnd > item.length)
    return fail("mint asset name is wider than 32 bytes or truncated");
  const assetName = item.subarray(name.next, nameEnd);
  const quantity = readCanonicalInt(item, nameEnd);
  const nextCount = cursor.accumulatedCount + 1;
  if (quantity.value === 0n) return fail("mint asset quantity is zero");
  if (nextCount > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
    return fail("mint asset count crossed inside a prior policy");
  if (
    cursor.policyAssetCursor !== 0 &&
    !canonicalKeyPrecedes(Buffer.from(cursor.previousAsset, "hex"), assetName)
  )
    return fail("mint asset order is not strictly ascending");
  const last = cursor.assetsRemaining === 1;
  if (last ? quantity.next !== item.length : quantity.next >= item.length)
    return fail("mint policy body length does not match its declared count");
  if (last)
    return Object.freeze({
      ...cursor,
      accumulatedCount: nextCount,
      previousPolicy: cursor.activePolicy,
      activePolicy: "",
      itemCursor: 0,
      assetsRemaining: 0,
      policyAssetCursor: 0,
      previousAsset: "",
      outcome:
        itemIndex === target.policyIndex
          ? MINT_DECLARED_OUTCOME_NON_CROSSING
          : MINT_DECLARED_OUTCOME_SCANNING,
    });
  return Object.freeze({
    ...cursor,
    accumulatedCount: nextCount,
    itemCursor: quantity.next,
    assetsRemaining: cursor.assetsRemaining - 1,
    policyAssetCursor: cursor.policyAssetCursor + 1,
    previousAsset: Buffer.from(assetName).toString("hex"),
  });
};

/**
 * The step-03 validator's `advance`: spends `budget` work units from
 * `nextItemIndex`, opening policies at `fold_policy_cost` and consuming
 * assets at one unit each. The returned `nextItemIndex` is the walk position
 * after the transaction (unchanged while a policy stays open).
 */
export const advanceMintDeclaredFold = ({
  cursor,
  nextItemIndex,
  items,
  target,
  budget = MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
}: {
  readonly cursor: MintDeclaredFoldCursor;
  readonly nextItemIndex: number;
  readonly items: readonly Uint8Array[];
  readonly target: MintDeclaredFoldTarget;
  readonly budget?: number;
}): Readonly<{ cursor: MintDeclaredFoldCursor; nextItemIndex: number }> => {
  if (
    !Number.isSafeInteger(budget) ||
    budget <= 0 ||
    budget > MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET
  )
    return fail("fold budget is outside 1..staged_fold_budget");
  let state = cursor;
  let index = nextItemIndex;
  let left = budget;
  for (;;) {
    if (state.outcome !== MINT_DECLARED_OUTCOME_SCANNING) break;
    if (
      state.activePolicy === "" &&
      left < MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST
    )
      break;
    const item = items[index];
    if (item === undefined) return fail("fold walked past field 5");
    let opened = state;
    let remaining = left;
    if (state.activePolicy === "") {
      opened = beginMintDeclaredPolicy({
        cursor: state,
        itemIndex: index,
        item,
        target,
      });
      remaining = left - MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST;
    }
    if (opened.outcome !== MINT_DECLARED_OUTCOME_SCANNING) {
      state = opened;
      break;
    }
    let consumed = opened;
    while (remaining > 0 && consumed.assetsRemaining > 0) {
      consumed = consumeMintDeclaredAsset({
        cursor: consumed,
        itemIndex: index,
        item,
        target,
      });
      remaining -= 1;
    }
    state = consumed;
    left = remaining;
    if (consumed.activePolicy !== "") break;
    index += 1;
  }
  return Object.freeze({ cursor: state, nextItemIndex: index });
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
  const targetItem = items[policyIndex];
  if (targetItem === undefined)
    return fail("policy coordinate is outside field 5");
  const header = decodeMintDeclaredPolicyHeader(targetItem);
  const target: MintDeclaredFoldTarget = {
    policyIndex,
    targetPolicyId: header.policyId.toString("hex"),
    targetDeclaredCount: header.declaredCount,
  };
  let cursor = initialMintDeclaredFoldCursor();
  let nextItemIndex = 0;
  while (cursor.outcome === MINT_DECLARED_OUTCOME_SCANNING) {
    const advanced = advanceMintDeclaredFold({
      cursor,
      nextItemIndex,
      items,
      target,
    });
    cursor = advanced.cursor;
    nextItemIndex = advanced.nextItemIndex;
  }
  return Object.freeze({
    crossing: cursor.outcome === MINT_DECLARED_OUTCOME_CROSSING,
    accumulatedCount: cursor.accumulatedCount,
    targetPolicyId: target.targetPolicyId,
    targetDeclaredCount: target.targetDeclaredCount,
  });
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
  active_policy: Data.Bytes(),
  item_cursor: Data.Integer(),
  assets_remaining: Data.Integer(),
  policy_asset_cursor: Data.Integer(),
  previous_asset: Data.Bytes(),
  outcome: Data.Integer(),
});

export type MintDeclaredAssetLimitFoldStateData = Readonly<{
  subject: VerdictSubject;
  policy_index: bigint;
  target_policy_id: string;
  target_declared_count: bigint;
  checkpoint_hash: string;
  accumulated_count: bigint;
  previous_policy: string;
  active_policy: string;
  item_cursor: bigint;
  assets_remaining: bigint;
  policy_asset_cursor: bigint;
  previous_asset: string;
  outcome: bigint;
}>;

/** The wire fields of a step-03 datum for one fold snapshot. */
export const mintDeclaredFoldStateData = ({
  subject,
  target,
  cursor,
  checkpointHash,
}: {
  readonly subject: VerdictSubject;
  readonly target: MintDeclaredFoldTarget;
  readonly cursor: MintDeclaredFoldCursor;
  readonly checkpointHash: string;
}): MintDeclaredAssetLimitFoldStateData =>
  Object.freeze({
    subject,
    policy_index: BigInt(target.policyIndex),
    target_policy_id: target.targetPolicyId,
    target_declared_count: BigInt(target.targetDeclaredCount),
    checkpoint_hash: checkpointHash,
    accumulated_count: BigInt(cursor.accumulatedCount),
    previous_policy: cursor.previousPolicy,
    active_policy: cursor.activePolicy,
    item_cursor: BigInt(cursor.itemCursor),
    assets_remaining: BigInt(cursor.assetsRemaining),
    policy_asset_cursor: BigInt(cursor.policyAssetCursor),
    previous_asset: cursor.previousAsset,
    outcome: BigInt(cursor.outcome),
  });

/** Field-wise equality of two fold datums, the subject aside. */
export const mintDeclaredFoldDataMatches = (
  observed: MintDeclaredAssetLimitFoldStateData,
  expected: MintDeclaredAssetLimitFoldStateData,
): boolean =>
  (Object.keys(expected) as (keyof MintDeclaredAssetLimitFoldStateData)[])
    .filter((key) => key !== "subject")
    .every((key) => observed[key] === expected[key]);

export const MintDeclaredAssetLimitDecisionStateSchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
  crossing: Data.Boolean(),
});
