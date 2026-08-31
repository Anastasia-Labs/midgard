import {
  computeHash32,
  decodeMidgardVersionedScript,
  encodeMidgardDefiniteBytesV1,
  encodeMidgardFieldArrayHeaderV1,
  encodeMidgardFieldPreimageV1,
  hashMidgardVersionedScript,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";

export const MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1 = 32;
export const MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX_V1 = 6;

const U24_MAX = 0xff_ff_ff;
const HASH_32 = /^[0-9a-f]{64}$/u;
const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const SEMANTIC_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

export type MissingNativeScriptTxGrammarCheckpointV1 = Readonly<{
  txId: string;
  fieldIndex: number;
  fieldCommitment: string;
  totalLength: number;
  declaredCount: number;
  nextItemIndex: number;
  nextOffset: number;
}>;

export type MissingNativeScriptTxSemanticCheckpointV1 = Readonly<{
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
    budget > MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1
  ) {
    throw new Error(
      `missing-native-script staged item budget must be in 1..${MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1.toString()}`,
    );
  }
  return budget;
};

const canonicalPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimageV1(items);

const offsetAt = (items: readonly Uint8Array[], itemIndex: number): number => {
  if (
    !Number.isSafeInteger(itemIndex) ||
    itemIndex < 0 ||
    itemIndex > items.length
  ) {
    throw new Error("missing-native-script checkpoint item index is invalid");
  }
  let offset = encodeMidgardFieldArrayHeaderV1(items.length).length;
  for (let index = 0; index < itemIndex; index += 1) {
    offset += encodeMidgardDefiniteBytesV1(items[index]!).length;
  }
  return offset;
};

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const encodeMissingNativeScriptTxGrammarCheckpointV1 = (
  checkpoint: MissingNativeScriptTxGrammarCheckpointV1,
): Buffer => {
  const txId = Buffer.from(
    assertHash32(checkpoint.txId, "grammar tx id"),
    "hex",
  );
  const fieldCommitment = Buffer.from(
    assertHash32(checkpoint.fieldCommitment, "grammar field commitment"),
    "hex",
  );
  if (
    checkpoint.fieldIndex !==
    MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX_V1
  ) {
    throw new Error(
      "missing-native-script grammar checkpoint must name field 6",
    );
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
    throw new Error("missing-native-script grammar checkpoint length drifted");
  }
  return encoded;
};

export const hashMissingNativeScriptTxGrammarCheckpointV1 = (
  checkpoint: MissingNativeScriptTxGrammarCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeMissingNativeScriptTxGrammarCheckpointV1(checkpoint),
    ]),
  ).toString("hex");

export const decodeMissingNativeScriptTxGrammarCheckpointV1 = (
  bytes: Uint8Array,
): MissingNativeScriptTxGrammarCheckpointV1 => {
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
    throw new Error(
      "missing-native-script grammar checkpoint is not canonical",
    );
  }
  const decoded: MissingNativeScriptTxGrammarCheckpointV1 = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    fieldCommitment: source.subarray(39, 71).toString("hex"),
    totalLength: source.readUIntBE(72, 3),
    declaredCount: source.readUIntBE(76, 3),
    nextItemIndex: source.readUIntBE(80, 3),
    nextOffset: source.readUIntBE(84, 3),
  };
  if (
    !sameBytes(encodeMissingNativeScriptTxGrammarCheckpointV1(decoded), source)
  ) {
    throw new Error(
      "missing-native-script grammar checkpoint is not canonical",
    );
  }
  return decoded;
};

export const initialMissingNativeScriptTxGrammarCheckpointV1 = ({
  txId,
  items,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): MissingNativeScriptTxGrammarCheckpointV1 => {
  const preimage = canonicalPreimage(items);
  return {
    txId: assertHash32(txId, "grammar tx id"),
    fieldIndex: MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX_V1,
    fieldCommitment: midgardFieldCommitmentV1(preimage).toString("hex"),
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
  readonly checkpoint: MissingNativeScriptTxGrammarCheckpointV1;
  readonly items: readonly Uint8Array[];
}): void => {
  const initial = initialMissingNativeScriptTxGrammarCheckpointV1({
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
      "missing-native-script grammar checkpoint is not bound to the exact field preimage",
    );
  }
};

export const advanceMissingNativeScriptTxGrammarCheckpointV1 = ({
  checkpoint,
  items,
  budget,
}: {
  readonly checkpoint: MissingNativeScriptTxGrammarCheckpointV1;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MissingNativeScriptTxGrammarCheckpointV1 => {
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

export const missingNativeScriptTxGrammarCheckpointIsCompleteV1 = (
  checkpoint: MissingNativeScriptTxGrammarCheckpointV1,
): boolean =>
  checkpoint.nextItemIndex === checkpoint.declaredCount &&
  checkpoint.nextOffset === checkpoint.totalLength;

export const encodeMissingNativeScriptTxSemanticCheckpointV1 = (
  checkpoint: MissingNativeScriptTxSemanticCheckpointV1,
): Buffer => {
  const txId = Buffer.from(
    assertHash32(checkpoint.txId, "semantic tx id"),
    "hex",
  );
  if (
    checkpoint.fieldIndex !==
    MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX_V1
  ) {
    throw new Error(
      "missing-native-script semantic checkpoint must name field 6",
    );
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
    throw new Error("missing-native-script semantic checkpoint length drifted");
  }
  return encoded;
};

export const hashMissingNativeScriptTxSemanticCheckpointV1 = (
  checkpoint: MissingNativeScriptTxSemanticCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([
      SEMANTIC_DOMAIN,
      encodeMissingNativeScriptTxSemanticCheckpointV1(checkpoint),
    ]),
  ).toString("hex");

export const decodeMissingNativeScriptTxSemanticCheckpointV1 = (
  bytes: Uint8Array,
): MissingNativeScriptTxSemanticCheckpointV1 => {
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
    throw new Error(
      "missing-native-script semantic checkpoint is not canonical",
    );
  }
  const decoded: MissingNativeScriptTxSemanticCheckpointV1 = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    totalLength: source.readUIntBE(38, 3),
    itemCount: source.readUIntBE(42, 3),
    nextItemIndex: source.readUIntBE(46, 3),
    nextOffset: source.readUIntBE(50, 3),
  };
  if (
    !sameBytes(encodeMissingNativeScriptTxSemanticCheckpointV1(decoded), source)
  ) {
    throw new Error(
      "missing-native-script semantic checkpoint is not canonical",
    );
  }
  return decoded;
};

export const initialMissingNativeScriptTxSemanticCheckpointV1 = ({
  grammar,
  items,
}: {
  readonly grammar: MissingNativeScriptTxGrammarCheckpointV1;
  readonly items: readonly Uint8Array[];
}): MissingNativeScriptTxSemanticCheckpointV1 => {
  assertGrammarBound({ checkpoint: grammar, items });
  if (!missingNativeScriptTxGrammarCheckpointIsCompleteV1(grammar)) {
    throw new Error(
      "missing-native-script semantic scan requires terminal grammar certification",
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
  readonly checkpoint: MissingNativeScriptTxSemanticCheckpointV1;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): void => {
  const preimage = canonicalPreimage(items);
  if (
    checkpoint.txId !== assertHash32(txId, "semantic tx id") ||
    checkpoint.fieldIndex !==
      MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX_V1 ||
    checkpoint.totalLength !== preimage.length ||
    checkpoint.itemCount !== items.length ||
    checkpoint.nextItemIndex > checkpoint.itemCount ||
    checkpoint.nextOffset !== offsetAt(items, checkpoint.nextItemIndex)
  ) {
    throw new Error(
      "missing-native-script semantic checkpoint is not bound to the exact field preimage",
    );
  }
};

export const advanceMissingNativeScriptTxSemanticCheckpointV1 = ({
  checkpoint,
  txId,
  items,
  budget,
}: {
  readonly checkpoint: MissingNativeScriptTxSemanticCheckpointV1;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MissingNativeScriptTxSemanticCheckpointV1 => {
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

export const missingNativeScriptTxSemanticCheckpointIsCompleteV1 = (
  checkpoint: MissingNativeScriptTxSemanticCheckpointV1,
): boolean => checkpoint.nextItemIndex === checkpoint.itemCount;

export const missingNativeScriptTxRequiredScriptPresentThroughV1 = ({
  expectedScriptHash,
  items,
  nextItemIndex,
}: {
  readonly expectedScriptHash: string;
  readonly items: readonly Uint8Array[];
  readonly nextItemIndex: number;
}): boolean => {
  const expected = expectedScriptHash.toLowerCase();
  if (!/^[0-9a-f]{56}$/u.test(expected)) {
    throw new Error("expected missing script hash must be 28-byte hexadecimal");
  }
  if (
    !Number.isSafeInteger(nextItemIndex) ||
    nextItemIndex < 0 ||
    nextItemIndex > items.length
  ) {
    throw new Error("semantic scan prefix is outside the witness field");
  }
  return items
    .slice(0, nextItemIndex)
    .some(
      (item) =>
        hashMidgardVersionedScript(decodeMidgardVersionedScript(item)) ===
        expected,
    );
};

/** Restart-safe inverse of the fixed grammar batch schedule. */
export const resolveMissingNativeScriptTxGrammarCheckpointV1 = ({
  txId,
  items,
  committedHash,
  budget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MissingNativeScriptTxGrammarCheckpointV1 => {
  let checkpoint = initialMissingNativeScriptTxGrammarCheckpointV1({
    txId,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (
      hashMissingNativeScriptTxGrammarCheckpointV1(checkpoint) === committedHash
    ) {
      return checkpoint;
    }
    if (missingNativeScriptTxGrammarCheckpointIsCompleteV1(checkpoint)) break;
    checkpoint = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint,
      items,
      budget,
    });
  }
  throw new Error(
    "missing-native-script grammar checkpoint is unreachable by the deterministic batch schedule",
  );
};

/** Restart-safe inverse of the fixed semantic batch schedule. */
export const resolveMissingNativeScriptTxSemanticCheckpointV1 = ({
  txId,
  items,
  committedHash,
  budget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MissingNativeScriptTxSemanticCheckpointV1 => {
  let grammar = initialMissingNativeScriptTxGrammarCheckpointV1({
    txId,
    items,
  });
  while (!missingNativeScriptTxGrammarCheckpointIsCompleteV1(grammar)) {
    grammar = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: grammar,
      items,
      budget,
    });
  }
  let checkpoint = initialMissingNativeScriptTxSemanticCheckpointV1({
    grammar,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (
      hashMissingNativeScriptTxSemanticCheckpointV1(checkpoint) ===
      committedHash
    ) {
      return checkpoint;
    }
    if (missingNativeScriptTxSemanticCheckpointIsCompleteV1(checkpoint)) break;
    checkpoint = advanceMissingNativeScriptTxSemanticCheckpointV1({
      checkpoint,
      txId,
      items,
      budget,
    });
  }
  throw new Error(
    "missing-native-script semantic checkpoint is unreachable by the deterministic batch schedule",
  );
};
