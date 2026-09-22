import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  RejectionReasonSchema,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  advanceMintItemScan,
  initialMintItemScanControl,
  mintItemPolicyId,
  type MintItemScanControl,
} from "./scan.js";
export { mintItemScanControlData } from "./scan.js";

export const MINT_ITEM_NON_CANONICAL_CATEGORY = "mintItemNonCanonical" as const;
export const MINT_ITEM_NON_CANONICAL_CATEGORY_ID = "00000036" as const;
export const MINT_ITEM_NON_CANONICAL_MINT_FIELD = 5 as const;
export const MINT_ITEM_NON_CANONICAL_MAX_ITEM_BYTES = 32_768;

const fail = (message: string): never => {
  throw new Error(`${MINT_ITEM_NON_CANONICAL_CATEGORY}: ${message}`);
};
const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) fail(`${name} is invalid`);
  return value;
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

export const MintItemVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
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
export const encodeMintItemBoundItem = (finding: MintItemFinding): string => {
  const exact = classifyMintItemFinding(finding);
  return Data.to(
    {
      subject: exact.subject,
      item_index: BigInt(exact.itemIndex),
      field_cursor: null,
    } as never,
    MintItemBoundItemSchema as never,
  );
};
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
export type MintItemJournalEntry = {
  readonly sequence: number;
  readonly identity: string;
  readonly stage: MintItemStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type MintItemJournal = {
  readonly load: (identity: string) => Promise<readonly MintItemJournalEntry[]>;
  readonly append: (entry: MintItemJournalEntry) => Promise<void>;
};
export const mintItemEvidenceIdentity = (evidence: MintItemEvidence): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction,
    evidence.itemIndex,
    evidence.fieldCommitmentHex,
    evidence.itemHash,
  ].join(":");
const stageRank = (stage: MintItemStage): number =>
  MINT_ITEM_NON_CANONICAL_STAGES.indexOf(stage);
export const reconcileMintItemJournal = (
  identity: string,
  entries: readonly MintItemJournalEntry[],
  observedStage: MintItemStage,
): MintItemStage => {
  let sequence = -1;
  let last: MintItemStage = "none";
  for (const entry of entries) {
    if (entry.identity !== identity) fail("journal identity mismatch");
    if (entry.sequence !== sequence + 1)
      fail("journal sequence is not contiguous");
    if (entry.stage !== "cancelled" && stageRank(entry.stage) < stageRank(last))
      fail("journal stage regressed");
    sequence = entry.sequence;
    last = entry.stage;
  }
  if (observedStage === "cancelled" || observedStage === "removed")
    return observedStage;
  if (stageRank(observedStage) < stageRank(last))
    fail("chain state is behind durable journal");
  return observedStage;
};
export type MintItemAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "removeDescendants"
  | "done";
export const nextMintItemAction = (stage: MintItemStage): MintItemAction => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "step04":
      return "submitStep04";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};
export type MintItemSubmissionResult = {
  readonly stage: MintItemStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type MintItemSubmissionAdapter = {
  readonly observe: (identity: string) => Promise<MintItemStage>;
  readonly submit: (
    action: Exclude<MintItemAction, "done">,
    evidence: MintItemEvidence,
  ) => Promise<MintItemSubmissionResult>;
  readonly cancel: (
    stage: "step01" | "step02" | "step03" | "step04",
    evidence: MintItemEvidence,
  ) => Promise<MintItemSubmissionResult>;
};
export const runMintItemProof = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: MintItemEvidence;
  readonly journal: MintItemJournal;
  readonly submission: MintItemSubmissionAdapter;
}): Promise<MintItemStage> => {
  if (!mintItemEvidenceCloses(evidence))
    fail("honest verdict cannot start a proof thread");
  const identity = mintItemEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const stage = reconcileMintItemJournal(
      identity,
      entries,
      await submission.observe(identity),
    );
    const action = nextMintItemAction(stage);
    if (action === "done") return stage;
    const result = await submission.submit(action, evidence);
    await journal.append({
      sequence: entries.length,
      identity,
      stage: result.stage,
      txHash: result.txHash,
      outputReference: result.outputReference,
    });
  }
};
