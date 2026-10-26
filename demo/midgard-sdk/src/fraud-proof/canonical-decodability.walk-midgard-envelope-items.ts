/** Catalogue violation identifier adjudicated by this family. */
export const CANONICAL_DECODABILITY_VIOLATION_ID =
  "canonical-decodability" as const;

// ## §5.1 verdict codes (byte-for-byte twin of `rule.ak`)

/** The bytes are a §5.1 envelope. The only verdict that is not a violation. */
export const MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL = 0;

/** Zero bytes; §5.1's shortest admissible preimage is the one-byte `80`. */
export const MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER = 1;

/**
 * The leading byte is not a §5.1 `definite_array_header`. §5.1's acceptance set
 * is narrower than CBOR's: the four-byte `9a` head is well-formed CBOR and
 * outside the grammar.
 */
export const MIDGARD_ENVELOPE_VERDICT_NOT_AN_ARRAY_HEADER = 2;

/** A `98`/`99` array header whose count a narrower form spells (§6.1). */
export const MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER = 3;

/** A `98`/`99` array header whose own width leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER = 4;

/** Items remain to be read and no byte remains to start one. */
export const MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER = 5;

/** An item's leading byte is not a §5.1 `definite_bytes_header`. */
export const MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER = 6;

/** A `58`/`59` item header whose length a narrower form spells. */
export const MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER = 7;

/** A `58`/`59` item header whose own width leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER = 8;

/** An item whose declared payload leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_PAYLOAD = 9;

/** All declared items were read and bytes remain; §5.1 admits no trailing content. */
export const MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES = 10;

/**
 * One past the largest verdict code. Every value {@link midgardEnvelopeVerdict}
 * returns is in `0 ..< MIDGARD_ENVELOPE_VERDICT_CODE_COUNT`, which is what
 * lets a step-02 refuse a state carrying a code no walk produces.
 */
export const MIDGARD_ENVELOPE_VERDICT_CODE_COUNT = 11;

/** §2.5's nine committed fields — the range a fault address may name. */
export const MIDGARD_COMMITTED_FIELD_COUNT = 9;

/** Every verdict code paired with the name `rule.ak` gives it. */
export const MIDGARD_ENVELOPE_VERDICT_NAMES = Object.freeze([
  "grammatical",
  "missing_array_header",
  "not_an_array_header",
  "non_minimal_array_header",
  "truncated_array_header",
  "missing_item_header",
  "not_an_item_header",
  "non_minimal_item_header",
  "truncated_item_header",
  "truncated_item_payload",
  "trailing_bytes",
] as const);

// ## The verdict — a total function over arbitrary bytes

/**
 * How many items one step-01 transaction reads. Byte-for-byte twin of
 * `rule.ak`'s `envelope_batch_items`; the on-chain value is chosen by
 * measurement and this one follows it.
 */
export const MIDGARD_ENVELOPE_BATCH_ITEMS = 1000;

/**
 * Where a §5.1 walk stands: finished with a verdict, or between two batches
 * with `remaining` items still to read from byte `offset`. Twin of `rule.ak`'s
 * `EnvelopeWalkV1`.
 */
export type MidgardEnvelopeWalk =
  | Readonly<{ kind: "verdict"; verdict: number }>
  | Readonly<{ kind: "items"; remaining: number; offset: number }>;

const envelopeVerdict = (verdict: number): MidgardEnvelopeWalk =>
  Object.freeze({ kind: "verdict", verdict });

const envelopeItems = (
  remaining: number,
  offset: number,
): MidgardEnvelopeWalk => Object.freeze({ kind: "items", remaining, offset });

/**
 * §5.1 grammaticality as a **verdict**, over any byte string at all: the array
 * head, then every batch until one ends the walk.
 *
 * Total by construction and never throws: every index below is preceded by the
 * bound that makes it safe, and the bound's failure is a return value. A verdict
 * that could throw would be the door again, and the door is what this family
 * exists to route around.
 *
 * It is the door's grammar restated as a decision rather than reused as one —
 * `decodeMidgardFieldPreimageV1` and its Aiken counterparts decide the same
 * predicate by throwing/aborting. Agreement between the two is a conformance
 * obligation discharged by vectors in both directions, not an artefact of shared
 * code.
 */
export const midgardEnvelopeVerdict = (preimage: Uint8Array): number => {
  let walk = midgardEnvelopeFirstBatch(preimage);
  while (walk.kind === "items") {
    walk = midgardEnvelopeItemsBatch(preimage, walk.remaining, walk.offset);
  }
  return walk.verdict;
};

/** The walk's first batch: the array head, then up to one batch of items. */
export const midgardEnvelopeFirstBatch = (
  preimage: Uint8Array,
): MidgardEnvelopeWalk => {
  const head = midgardEnvelopeArrayHead(preimage);
  return head.kind === "items"
    ? midgardEnvelopeItemsBatch(preimage, head.remaining, head.offset)
    : head;
};

/** §5.1's `definite_array_header(N)`, read at offset 0. */
export const midgardEnvelopeArrayHead = (
  preimage: Uint8Array,
): MidgardEnvelopeWalk => {
  const total = preimage.length;
  if (total === 0) {
    return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER);
  }
  const tag = preimage[0]!;
  if (tag >= 0x80 && tag <= 0x97) {
    return envelopeItems(tag - 0x80, 1);
  }
  if (tag === 0x98) {
    if (total < 2) {
      return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER);
    }
    const count = preimage[1]!;
    return count < 24
      ? envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER)
      : envelopeItems(count, 2);
  }
  if (tag === 0x99) {
    if (total < 3) {
      return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER);
    }
    const count = preimage[1]! * 256 + preimage[2]!;
    return count <= 0xff
      ? envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER)
      : envelopeItems(count, 3);
  }
  return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_NOT_AN_ARRAY_HEADER);
};

/**
 * Reads up to {@link MIDGARD_ENVELOPE_BATCH_ITEMS} of the `remaining` items
 * from `offset` and says where the walk stands after them — exactly what one
 * step-01 transaction decides. A batch that reads its full share and lands on
 * the last byte with items still owed is the missing item header at once.
 */
export const midgardEnvelopeItemsBatch = (
  preimage: Uint8Array,
  remaining: number,
  offset: number,
): MidgardEnvelopeWalk => {
  const total = preimage.length;
  if (remaining > MIDGARD_ENVELOPE_BATCH_ITEMS) {
    const reached = readMidgardEnvelopeItems(
      preimage,
      offset,
      MIDGARD_ENVELOPE_BATCH_ITEMS,
    );
    if (reached.kind === "verdict") return reached;
    return reached.offset < total
      ? envelopeItems(remaining - MIDGARD_ENVELOPE_BATCH_ITEMS, reached.offset)
      : envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER);
  }
  const reached = readMidgardEnvelopeItems(preimage, offset, remaining);
  if (reached.kind === "verdict") return reached;
  return envelopeVerdict(
    reached.offset === total
      ? MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL
      : MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES,
  );
};

/**
 * Reads `count` enveloped items from `offset`: the offset just past the last
 * one, or the verdict that stopped the read.
 *
 * Iterative where the Aiken twin recurses: Aiken's recursion is a tail call the
 * compiler turns into a loop, while JavaScript has no such guarantee.
 */
const readMidgardEnvelopeItems = (
  preimage: Uint8Array,
  startOffset: number,
  count: number,
): MidgardEnvelopeWalk => {
  const total = preimage.length;
  let offset = startOffset;
  for (let read = 0; read < count; read += 1) {
    if (offset >= total) {
      return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER);
    }
    const head = preimage[offset]!;
    let payloadOffset: number;
    let length: number;
    if (head >= 0x40 && head <= 0x57) {
      payloadOffset = offset + 1;
      length = head - 0x40;
    } else if (head === 0x58) {
      if (offset + 2 > total) {
        return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER);
      }
      length = preimage[offset + 1]!;
      if (length < 24) {
        return envelopeVerdict(
          MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER,
        );
      }
      payloadOffset = offset + 2;
    } else if (head === 0x59) {
      if (offset + 3 > total) {
        return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER);
      }
      length = preimage[offset + 1]! * 256 + preimage[offset + 2]!;
      if (length <= 0xff) {
        return envelopeVerdict(
          MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER,
        );
      }
      payloadOffset = offset + 3;
    } else {
      return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER);
    }
    if (payloadOffset + length > total) {
      return envelopeVerdict(MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_PAYLOAD);
    }
    offset = payloadOffset + length;
  }
  return envelopeItems(0, offset);
};

/**
 * The adjudicated violation predicate, over the two values step 01 pins into the
 * computation thread.
 *
 * The bounds are §12.1's one-spelling rule applied to a state that crosses a
 * transaction boundary: a state naming a tenth field or a twelfth code is one
 * step 01 could not have written, and admitting it would let one fault finalize
 * under many spellings. They are refusals rather than clamps (§7.3) — this
 * returns `false`, and the on-chain twin aborts.
 */
export const isCanonicalDecodabilityViolation = ({
  fieldIndex,
  verdict,
}: {
  readonly fieldIndex: number;
  readonly verdict: number;
}): boolean =>
  Number.isInteger(fieldIndex) &&
  fieldIndex >= 0 &&
  fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT &&
  Number.isInteger(verdict) &&
  verdict >= 0 &&
  verdict < MIDGARD_ENVELOPE_VERDICT_CODE_COUNT &&
  verdict !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL;

// ## Producer side — the §5.1 envelope, built to be broken

/**
 * `encodeMidgardFieldPreimageV1`'s deliberately-wrong twin: an envelope whose
 * array header declares `declaredCount` while the body carries `items`.
 *
 * It exists because the honest producer cannot express the fixtures this family
 * adjudicates — it writes the count it measures, so no argument to it produces a
 * miscounted envelope, which is exactly the shape §5.1 refuses and a malicious
 * operator commits. Byte-for-byte twin of `rule.ak`'s
 * `miscounted_field_preimage_v1`.
 */
export const miscountedMidgardFieldPreimage = (
  declaredCount: number,
  items: readonly Uint8Array[],
): Buffer =>
  Buffer.concat([
    midgardFieldArrayHeader(declaredCount),
    ...items.map((item) =>
      Buffer.concat([midgardDefiniteBytesHeader(item.length), item]),
    ),
  ]);

/**
 * §5.1's widest header is `99 NNNN` / `59 LLLL`, so both helpers below are
 * unbounded only in the sense that matters — they will write a count or a
 * length its content does not carry — and not beyond the two-byte form §5.1
 * caps at. Outside it `rule.ak`'s twin aborts in `from_int_big_endian(_, 2)`;
 * these throw rather than truncate to the low sixteen bits, so a producer
 * cannot emit bytes the on-chain builder would have refused to.
 */
export const MIDGARD_FIELD_HEADER_MAX = 0xffff;

const assertMidgardFieldHeaderRange = (label: string, value: number): void => {
  if (
    !Number.isInteger(value) ||
    value < 0 ||
    value > MIDGARD_FIELD_HEADER_MAX
  ) {
    throw new Error(
      `${label} must be an integer in 0..${MIDGARD_FIELD_HEADER_MAX}`,
    );
  }
};

/** §5.1's `definite_array_header(N)` in minimal width. */
const midgardFieldArrayHeader = (count: number): Buffer => {
  assertMidgardFieldHeaderRange("declared item count", count);
  if (count <= 23) {
    return Buffer.from([0x80 + count]);
  }
  if (count <= 255) {
    return Buffer.from([0x98, count]);
  }
  return Buffer.from([0x99, (count >> 8) & 0xff, count & 0xff]);
};

/** §5.1's `definite_bytes_header(L)` in minimal width. */
const midgardDefiniteBytesHeader = (length: number): Buffer => {
  assertMidgardFieldHeaderRange("item length", length);
  if (length <= 23) {
    return Buffer.from([0x40 + length]);
  }
  if (length <= 255) {
    return Buffer.from([0x58, length]);
  }
  return Buffer.from([0x59, (length >> 8) & 0xff, length & 0xff]);
};

// ## Evidence

/**
 * Canonical evidence record for one committed field: exactly the triple step 01
 * pins into the computation thread, plus the authenticated bytes it was derived
 * from.
 */
export type CanonicalDecodabilityEvidence = {
  readonly violationId: typeof CANONICAL_DECODABILITY_VIOLATION_ID;
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: string;
  readonly committedPreimageByteCount: number;
  readonly verdict: number;
  readonly verdictName: string;
  readonly isViolation: boolean;
};
