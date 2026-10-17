/**
 * The walk and grammar checkpoints of the accepted-transaction replay's two
 * field walks (spend inputs, field 0; outputs, field 2), as
 * `native_tx_machine_walk_v1` encodes and hashes them. Thread state holds
 * only each checkpoint's hash, so a step re-derives the checkpoint from the
 * field's item bytes and the position the state implies.
 */
import {
  computeHash32,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");
const U24_MAX = 0xff_ff_ff;

/** The positions of one field's items: wrapper offsets and total length. */
export type TransitionFieldLayout = Readonly<{
  txId: string;
  fieldIndex: number;
  commitment: string;
  totalLength: number;
  /** `offsets[i]` is item `i`'s wrapper offset; `offsets[n]` the length. */
  offsets: readonly number[];
}>;

export const transitionFieldLayout = ({
  txId,
  fieldIndex,
  items,
}: {
  readonly txId: string;
  readonly fieldIndex: number;
  readonly items: readonly Uint8Array[];
}): TransitionFieldLayout => {
  const preimage = encodeMidgardFieldPreimage(items);
  const offsets = [encodeMidgardFieldArrayHeader(items.length).length];
  for (const item of items)
    offsets.push(offsets.at(-1)! + encodeMidgardDefiniteBytes(item).length);
  if (offsets.at(-1) !== preimage.length)
    throw new Error("transition field layout disagrees with its preimage");
  return {
    txId: txId.toLowerCase(),
    fieldIndex,
    commitment: midgardFieldCommitment(preimage).toString("hex"),
    totalLength: preimage.length,
    offsets,
  };
};

const itemCount = (layout: TransitionFieldLayout) => layout.offsets.length - 1;

const u24 = (value: number, label: string): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > U24_MAX)
    throw new Error(`${label} must be an unsigned 24-bit integer`);
  const encoded = Buffer.alloc(3);
  encoded.writeUIntBE(value, 0, 3);
  return encoded;
};

const position = (layout: TransitionFieldLayout, index: number): Buffer => {
  if (!Number.isSafeInteger(index) || index < 0 || index > itemCount(layout))
    throw new Error("transition field position is outside the field");
  return Buffer.concat([
    Buffer.from([0x43]),
    u24(index, "checkpoint next item index"),
    Buffer.from([0x43]),
    u24(layout.offsets[index]!, "checkpoint next offset"),
  ]);
};

/** The 53-byte walk checkpoint standing before item `index`. */
export const encodeTransitionWalkCheckpoint = (
  layout: TransitionFieldLayout,
  index: number,
): Buffer => {
  const encoded = Buffer.concat([
    Buffer.from([0x86, 0x58, 0x20]),
    Buffer.from(layout.txId, "hex"),
    Buffer.from([0x41, layout.fieldIndex, 0x43]),
    u24(layout.totalLength, "walk total length"),
    Buffer.from([0x43]),
    u24(itemCount(layout), "walk item count"),
    position(layout, index),
  ]);
  if (encoded.length !== 53)
    throw new Error("transition walk checkpoint length drifted");
  return encoded;
};

/** The 87-byte grammar checkpoint standing before item `index`. */
export const encodeTransitionGrammarCheckpoint = (
  layout: TransitionFieldLayout,
  index: number,
): Buffer => {
  const encoded = Buffer.concat([
    Buffer.from([0x87, 0x58, 0x20]),
    Buffer.from(layout.txId, "hex"),
    Buffer.from([0x41, layout.fieldIndex, 0x58, 0x20]),
    Buffer.from(layout.commitment, "hex"),
    Buffer.from([0x43]),
    u24(layout.totalLength, "grammar total length"),
    Buffer.from([0x43]),
    u24(itemCount(layout), "grammar declared count"),
    position(layout, index),
  ]);
  if (encoded.length !== 87)
    throw new Error("transition grammar checkpoint length drifted");
  return encoded;
};

export const hashTransitionWalkCheckpoint = (
  layout: TransitionFieldLayout,
  index: number,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeTransitionWalkCheckpoint(layout, index)]),
  ).toString("hex");

export const hashTransitionGrammarCheckpoint = (
  layout: TransitionFieldLayout,
  index: number,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeTransitionGrammarCheckpoint(layout, index),
    ]),
  ).toString("hex");

/**
 * The item index whose checkpoint hashes to `committed`, searched over the
 * positions `0, stride, 2·stride, …` and the field's end.
 */
export const resolveTransitionCheckpointIndex = ({
  layout,
  committed,
  grammar,
  stride = 1,
}: {
  readonly layout: TransitionFieldLayout;
  readonly committed: string;
  readonly grammar: boolean;
  readonly stride?: number;
}): number => {
  const hash = grammar
    ? hashTransitionGrammarCheckpoint
    : hashTransitionWalkCheckpoint;
  const count = itemCount(layout);
  for (let index = 0; ; index = Math.min(count, index + stride)) {
    if (hash(layout, index) === committed) return index;
    if (index === count) break;
  }
  throw new Error(
    "transition field checkpoint is unreachable from the committed field",
  );
};
