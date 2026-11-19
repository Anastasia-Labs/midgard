import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  advanceMintItemScan,
  initialMintItemScanControl,
  mintItemPolicyId,
  type MintItemScanControl,
  mintItemScanControlData,
} from "./scan.js";
export { mintItemScanControlData } from "./scan.js";

export const MINT_ITEM_NON_CANONICAL_CATEGORY = "mintItemNonCanonical" as const;
export const MINT_ITEM_NON_CANONICAL_MINT_FIELD = 5 as const;

const fail = (message: string): never => {
  throw new Error(`${MINT_ITEM_NON_CANONICAL_CATEGORY}: ${message}`);
};
const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) fail(`${name} is invalid`);
  return value;
};

/**
 * The terminal outcome of a forced `MintItemMalformed` rejection
 * (`rule.outcome_unreachable`): no machine step emits that reason, so step 01
 * answers the claim without opening the mint field.
 */
export const MINT_ITEM_OUTCOME_UNREACHABLE = 3n;

/** Whether `subject` is a forced rejection with `MintItemMalformed`. */
export const mintItemClaimIsUnreachable = (subject: VerdictSubject): boolean =>
  subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION &&
  subject.rejection_reason !== null &&
  typeof subject.rejection_reason === "object" &&
  "MintItemMalformed" in subject.rejection_reason;

/**
 * The terminal state step 01 writes for a forced `MintItemMalformed`
 * rejection (`rule.unreachable_claim_state_v1`).
 */
export const mintItemUnreachableClaimState = (subject: VerdictSubject) => ({
  subject,
  item_index: 0n,
  item_length: 0n,
  item_hash: "",
  chunk_hashes: [],
  control: mintItemScanControlData(initialMintItemScanControl()),
  outcome: MINT_ITEM_OUTCOME_UNREACHABLE,
});

/**
 * The evidence of a forced `MintItemMalformed` rejection, whatever policy it
 * names: step 01 answers the claim from the subject alone and opens no item,
 * so the evidence carries no field bytes.
 */
export const prepareMintItemUnreachableClaimEvidence = (
  subject: VerdictSubject,
): MintItemEvidence => {
  if (!mintItemClaimIsUnreachable(subject))
    fail("subject is not a forced MintItemMalformed rejection");
  if (!verdictSubjectIsCanonical(subject))
    fail("verdict subject is not canonical");
  return Object.freeze({
    subject,
    fieldIndex: MINT_ITEM_NON_CANONICAL_MINT_FIELD,
    itemIndex: 0,
    fieldPreimageHex: "",
    fieldCommitmentHex: "",
    itemHex: "",
    itemLength: 0,
    itemHash: "",
    chunkHashes: Object.freeze([]),
    carriage: "Inline",
    canonical: true,
    scanControls: Object.freeze([]),
    decisiveFaultHolds: true,
  });
};

export type MintItemFinding = {
  readonly subject: VerdictSubject;
  readonly fieldIndex: 5;
  readonly itemIndex: number;
};

export const classifyMintItemFinding = ({
  subject,
  fieldIndex,
  itemIndex,
}: MintItemFinding): MintItemFinding => {
  if (!verdictSubjectIsCanonical(subject))
    fail("verdict subject is not canonical");
  if (fieldIndex !== 5) fail("only field 5 belongs to this family");
  exactIndex(itemIndex, "policy item index");
  if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  )
    fail("only accepted transactions belong to this family");
  return Object.freeze({ subject, fieldIndex, itemIndex });
};

export type MintItemEvidence = MintItemFinding & {
  readonly fieldPreimageHex: string;
  readonly fieldCommitmentHex: string;
  readonly itemHex: string;
  readonly itemLength: number;
  readonly itemHash: string;
  readonly chunkHashes: readonly string[];
  readonly carriage: "Inline" | "RawUtxo" | "Certified";
  readonly canonical: boolean;
  readonly scanControls: readonly MintItemScanControl[];
  readonly decisiveFaultHolds: boolean;
};

const deriveScan = (item: Buffer, previousPolicy: string) => {
  let control = initialMintItemScanControl(previousPolicy);
  const controls: MintItemScanControl[] = [control];
  for (let step = 0; step <= item.length + 1; step += 1) {
    const chunkStart = Math.floor(control.cursor / 4_095) * 4_095;
    const next = advanceMintItemScan(
      control,
      item.length,
      item.subarray(chunkStart, chunkStart + 8_190),
      control.cursor - chunkStart,
    );
    if (next === null) return { canonical: false, controls };
    controls.push(next);
    if (next.stage === 2) return { canonical: true, controls };
    control = next;
  }
  return fail("mint scan exceeded its strict progress bound");
};

export const prepareMintItemEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MintItemFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MintItemEvidence => {
  const finding = classifyMintItemFinding(rawFinding);
  const actualCommitment =
    midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actualCommitment !== committedFieldHashHex)
    fail("field commitment differs");
  if (fieldPreimage.length > 32_768)
    fail("field belongs to committedFieldShape");
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  const item = items[finding.itemIndex];
  if (item === undefined) fail("policy coordinate is outside field 5");
  if (item.length === 0)
    fail("empty mint item belongs to fieldItemWidthIllegal");
  const previousPolicy =
    finding.itemIndex === 0
      ? ""
      : mintItemPolicyId(items[finding.itemIndex - 1]!);
  if (previousPolicy === null)
    return fail("malformed predecessor must be proved at its own coordinate");
  const scan = deriveScan(item, previousPolicy);
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actualCommitment,
    itemHex: item.toString("hex"),
    itemLength: item.length,
    itemHash: computeHash32(item).toString("hex"),
    chunkHashes: Object.freeze(
      Array.from({ length: Math.ceil(item.length / 4_095) }, (_, index) =>
        computeHash32(
          item.subarray(index * 4_095, (index + 1) * 4_095),
        ).toString("hex"),
      ),
    ),
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    canonical: scan.canonical,
    scanControls: scan.controls,
    decisiveFaultHolds: !scan.canonical,
  });
};

export const mintItemEvidenceCloses = (evidence: MintItemEvidence): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.canonical);

export const MintItemVerdictSubjectSchema = VerdictSubjectSchema;
export const MintFieldCursorSchema = Data.Object({
  next_index: Data.Integer(),
  next_offset: Data.Integer(),
  item_offset: Data.Integer(),
  item_length: Data.Integer(),
  previous_policy: Data.Bytes(),
});
export type MintFieldCursor = Data.Static<typeof MintFieldCursorSchema>;
export const MintItemBoundItemSchema = Data.Object({
  subject: MintItemVerdictSubjectSchema,
  item_index: Data.Integer(),
  field_cursor: Data.Nullable(MintFieldCursorSchema),
});

export const MINT_ITEM_NON_CANONICAL_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type MintItemStage = (typeof MINT_ITEM_NON_CANONICAL_STAGES)[number];
