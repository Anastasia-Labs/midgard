import {
  computeHash32,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";

export const MIN_ADA_STAGED_BATCH_LIMIT = 32;
export const MIN_ADA_OUTPUT_FIELD_INDEX = 2;

const U24_MAX = 0xff_ff_ff;
const HASH_32 = /^[0-9a-f]{64}$/u;
const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const SEMANTIC_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

export type MinAdaGrammarCheckpoint = Readonly<{
  txId: string;
  fieldIndex: number;
  fieldCommitment: string;
  totalLength: number;
  declaredCount: number;
  nextItemIndex: number;
  nextOffset: number;
}>;

export type MinAdaSemanticCheckpoint = Readonly<{
  txId: string;
  fieldIndex: number;
  totalLength: number;
  itemCount: number;
  nextItemIndex: number;
  nextOffset: number;
}>;

const assertHash32 = (value: string, label: string): string => {
  const normalized = value.toLowerCase();
  if (!HASH_32.test(normalized)) {
    throw new Error(`${label} must be a 32-byte hexadecimal hash`);
  }
  return normalized;
};

const assertU24 = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > U24_MAX) {
    throw new Error(`${label} must be an unsigned 24-bit integer`);
  }
  return value;
};

const u24 = (value: number, label: string): Buffer => {
  const exact = assertU24(value, label);
  const encoded = Buffer.alloc(3);
  encoded.writeUIntBE(exact, 0, 3);
  return encoded;
};

const assertBudget = (budget: number): number => {
  if (
    !Number.isSafeInteger(budget) ||
    budget <= 0 ||
    budget > MIN_ADA_STAGED_BATCH_LIMIT
  ) {
    throw new Error(
      `min-ada staged item budget must be in 1..${MIN_ADA_STAGED_BATCH_LIMIT.toString()}`,
    );
  }
  return budget;
};

const canonicalPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimage(items);

const offsetAt = (items: readonly Uint8Array[], itemIndex: number): number => {
  if (
    !Number.isSafeInteger(itemIndex) ||
    itemIndex < 0 ||
    itemIndex > items.length
  ) {
    throw new Error("min-ada checkpoint item index is invalid");
  }
  let offset = encodeMidgardFieldArrayHeader(items.length).length;
  for (let index = 0; index < itemIndex; index += 1) {
    offset += encodeMidgardDefiniteBytes(items[index]!).length;
  }
  return offset;
};

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const encodeMinAdaGrammarCheckpoint = (
  checkpoint: MinAdaGrammarCheckpoint,
): Buffer => {
  const txId = Buffer.from(
    assertHash32(checkpoint.txId, "grammar tx id"),
    "hex",
  );
  const fieldCommitment = Buffer.from(
    assertHash32(checkpoint.fieldCommitment, "grammar field commitment"),
    "hex",
  );
  if (checkpoint.fieldIndex !== MIN_ADA_OUTPUT_FIELD_INDEX) {
    throw new Error("min-ada grammar checkpoint must name field 2");
  }
  const encoded = Buffer.concat([
    Buffer.from([0x87, 0x58, 0x20]),
    txId,
    Buffer.from([0x41, checkpoint.fieldIndex, 0x58, 0x20]),
    fieldCommitment,
    Buffer.from([0x43]),
    u24(checkpoint.totalLength, "grammar total length"),
    Buffer.from([0x43]),
    u24(checkpoint.declaredCount, "grammar declared count"),
    Buffer.from([0x43]),
    u24(checkpoint.nextItemIndex, "grammar next item index"),
    Buffer.from([0x43]),
    u24(checkpoint.nextOffset, "grammar next offset"),
  ]);
  if (encoded.length !== 87) {
    throw new Error("min-ada grammar checkpoint length drifted");
  }
  return encoded;
};

export const hashMinAdaGrammarCheckpoint = (
  checkpoint: MinAdaGrammarCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([GRAMMAR_DOMAIN, encodeMinAdaGrammarCheckpoint(checkpoint)]),
  ).toString("hex");

export const decodeMinAdaGrammarCheckpoint = (
  bytes: Uint8Array,
): MinAdaGrammarCheckpoint => {
  const source = Buffer.from(bytes);
  if (
    source.length !== 87 ||
    source[0] !== 0x87 ||
    source[1] !== 0x58 ||
    source[2] !== 0x20 ||
    source[35] !== 0x41 ||
    source[37] !== 0x58 ||
    source[38] !== 0x20 ||
    source[71] !== 0x43 ||
    source[75] !== 0x43 ||
    source[79] !== 0x43 ||
    source[83] !== 0x43
  ) {
    throw new Error("min-ada grammar checkpoint is not canonical");
  }
  const decoded: MinAdaGrammarCheckpoint = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    fieldCommitment: source.subarray(39, 71).toString("hex"),
    totalLength: source.readUIntBE(72, 3),
    declaredCount: source.readUIntBE(76, 3),
    nextItemIndex: source.readUIntBE(80, 3),
    nextOffset: source.readUIntBE(84, 3),
  };
  if (!sameBytes(encodeMinAdaGrammarCheckpoint(decoded), source)) {
    throw new Error("min-ada grammar checkpoint is not canonical");
  }
  return decoded;
};

export const initialMinAdaGrammarCheckpoint = ({
  txId,
  items,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): MinAdaGrammarCheckpoint => {
  const preimage = canonicalPreimage(items);
  return {
    txId: assertHash32(txId, "grammar tx id"),
    fieldIndex: MIN_ADA_OUTPUT_FIELD_INDEX,
    fieldCommitment: midgardFieldCommitment(preimage).toString("hex"),
    totalLength: preimage.length,
    declaredCount: items.length,
    nextItemIndex: 0,
    nextOffset: offsetAt(items, 0),
  };
};

const assertGrammarBound = ({
  checkpoint,
  items,
}: {
  readonly checkpoint: MinAdaGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
}): void => {
  const initial = initialMinAdaGrammarCheckpoint({
    txId: checkpoint.txId,
    items,
  });
  if (
    checkpoint.fieldIndex !== initial.fieldIndex ||
    checkpoint.fieldCommitment !== initial.fieldCommitment ||
    checkpoint.totalLength !== initial.totalLength ||
    checkpoint.declaredCount !== initial.declaredCount ||
    checkpoint.nextItemIndex > checkpoint.declaredCount ||
    checkpoint.nextOffset !== offsetAt(items, checkpoint.nextItemIndex)
  ) {
    throw new Error(
      "min-ada grammar checkpoint is not bound to the exact field preimage",
    );
  }
};

export const advanceMinAdaGrammarCheckpoint = ({
  checkpoint,
  items,
  budget,
}: {
  readonly checkpoint: MinAdaGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MinAdaGrammarCheckpoint => {
  assertGrammarBound({ checkpoint, items });
  const nextItemIndex = Math.min(
    checkpoint.declaredCount,
    checkpoint.nextItemIndex + assertBudget(budget),
  );
  return {
    ...checkpoint,
    nextItemIndex,
    nextOffset: offsetAt(items, nextItemIndex),
  };
};

export const minAdaGrammarCheckpointIsComplete = (
  checkpoint: MinAdaGrammarCheckpoint,
): boolean =>
  checkpoint.nextItemIndex === checkpoint.declaredCount &&
  checkpoint.nextOffset === checkpoint.totalLength;

export const encodeMinAdaSemanticCheckpoint = (
  checkpoint: MinAdaSemanticCheckpoint,
): Buffer => {
  const txId = Buffer.from(
    assertHash32(checkpoint.txId, "semantic tx id"),
    "hex",
  );
  if (checkpoint.fieldIndex !== MIN_ADA_OUTPUT_FIELD_INDEX) {
    throw new Error("min-ada semantic checkpoint must name field 2");
  }
  const encoded = Buffer.concat([
    Buffer.from([0x86, 0x58, 0x20]),
    txId,
    Buffer.from([0x41, checkpoint.fieldIndex, 0x43]),
    u24(checkpoint.totalLength, "semantic total length"),
    Buffer.from([0x43]),
    u24(checkpoint.itemCount, "semantic item count"),
    Buffer.from([0x43]),
    u24(checkpoint.nextItemIndex, "semantic next item index"),
    Buffer.from([0x43]),
    u24(checkpoint.nextOffset, "semantic next offset"),
  ]);
  if (encoded.length !== 53) {
    throw new Error("min-ada semantic checkpoint length drifted");
  }
  return encoded;
};

export const hashMinAdaSemanticCheckpoint = (
  checkpoint: MinAdaSemanticCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      SEMANTIC_DOMAIN,
      encodeMinAdaSemanticCheckpoint(checkpoint),
    ]),
  ).toString("hex");

export const decodeMinAdaSemanticCheckpoint = (
  bytes: Uint8Array,
): MinAdaSemanticCheckpoint => {
  const source = Buffer.from(bytes);
  if (
    source.length !== 53 ||
    source[0] !== 0x86 ||
    source[1] !== 0x58 ||
    source[2] !== 0x20 ||
    source[35] !== 0x41 ||
    source[37] !== 0x43 ||
    source[41] !== 0x43 ||
    source[45] !== 0x43 ||
    source[49] !== 0x43
  ) {
    throw new Error("min-ada semantic checkpoint is not canonical");
  }
  const decoded: MinAdaSemanticCheckpoint = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    totalLength: source.readUIntBE(38, 3),
    itemCount: source.readUIntBE(42, 3),
    nextItemIndex: source.readUIntBE(46, 3),
    nextOffset: source.readUIntBE(50, 3),
  };
  if (!sameBytes(encodeMinAdaSemanticCheckpoint(decoded), source)) {
    throw new Error("min-ada semantic checkpoint is not canonical");
  }
  return decoded;
};

export const initialMinAdaSemanticCheckpoint = ({
  grammar,
  items,
}: {
  readonly grammar: MinAdaGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
}): MinAdaSemanticCheckpoint => {
  assertGrammarBound({ checkpoint: grammar, items });
  if (!minAdaGrammarCheckpointIsComplete(grammar)) {
    throw new Error(
      "min-ada semantic scan requires terminal grammar certification",
    );
  }
  return {
    txId: grammar.txId,
    fieldIndex: grammar.fieldIndex,
    totalLength: grammar.totalLength,
    itemCount: grammar.declaredCount,
    nextItemIndex: 0,
    nextOffset: offsetAt(items, 0),
  };
};

const assertSemanticBound = ({
  checkpoint,
  txId,
  items,
}: {
  readonly checkpoint: MinAdaSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): void => {
  const preimage = canonicalPreimage(items);
  if (
    checkpoint.txId !== assertHash32(txId, "semantic tx id") ||
    checkpoint.fieldIndex !== MIN_ADA_OUTPUT_FIELD_INDEX ||
    checkpoint.totalLength !== preimage.length ||
    checkpoint.itemCount !== items.length ||
    checkpoint.nextItemIndex > checkpoint.itemCount ||
    checkpoint.nextOffset !== offsetAt(items, checkpoint.nextItemIndex)
  ) {
    throw new Error(
      "min-ada semantic checkpoint is not bound to the exact field preimage",
    );
  }
};

export const advanceMinAdaSemanticCheckpoint = ({
  checkpoint,
  txId,
  items,
  budget,
}: {
  readonly checkpoint: MinAdaSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MinAdaSemanticCheckpoint => {
  assertSemanticBound({ checkpoint, txId, items });
  const nextItemIndex = Math.min(
    checkpoint.itemCount,
    checkpoint.nextItemIndex + assertBudget(budget),
  );
  return {
    ...checkpoint,
    nextItemIndex,
    nextOffset: offsetAt(items, nextItemIndex),
  };
};

export const minAdaSemanticCheckpointIsComplete = (
  checkpoint: MinAdaSemanticCheckpoint,
): boolean => checkpoint.nextItemIndex === checkpoint.itemCount;

export const resolveMinAdaGrammarCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = MIN_ADA_STAGED_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MinAdaGrammarCheckpoint => {
  let checkpoint = initialMinAdaGrammarCheckpoint({
    txId,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (hashMinAdaGrammarCheckpoint(checkpoint) === committedHash) {
      return checkpoint;
    }
    if (minAdaGrammarCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceMinAdaGrammarCheckpoint({
      checkpoint,
      items,
      budget,
    });
  }
  throw new Error(
    "min-ada grammar checkpoint is unreachable by the deterministic batch schedule",
  );
};

/** Restart-safe inverse of the fixed semantic batch schedule. */
export const resolveMinAdaSemanticCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = MIN_ADA_STAGED_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MinAdaSemanticCheckpoint => {
  let grammar = initialMinAdaGrammarCheckpoint({
    txId,
    items,
  });
  while (!minAdaGrammarCheckpointIsComplete(grammar)) {
    grammar = advanceMinAdaGrammarCheckpoint({
      checkpoint: grammar,
      items,
      budget,
    });
  }
  let checkpoint = initialMinAdaSemanticCheckpoint({
    grammar,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (hashMinAdaSemanticCheckpoint(checkpoint) === committedHash) {
      return checkpoint;
    }
    if (minAdaSemanticCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceMinAdaSemanticCheckpoint({
      checkpoint,
      txId,
      items,
      budget,
    });
  }
  throw new Error(
    "min-ada semantic checkpoint is unreachable by the deterministic batch schedule",
  );
};
