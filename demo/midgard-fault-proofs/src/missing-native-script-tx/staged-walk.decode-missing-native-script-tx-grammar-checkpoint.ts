import {
  computeHash32,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";

export const MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT = 32;

export const MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX = 6;

const U24_MAX = 0xff_ff_ff;

const HASH_32 = /^[0-9a-f]{64}$/u;

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");

const SEMANTIC_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

export type MissingNativeScriptTxGrammarCheckpoint = Readonly<{
  txId: string;
  fieldIndex: number;
  fieldCommitment: string;
  totalLength: number;
  declaredCount: number;
  nextItemIndex: number;
  nextOffset: number;
}>;

export type MissingNativeScriptTxSemanticCheckpoint = Readonly<{
  txId: string;
  fieldIndex: number;
  totalLength: number;
  itemCount: number;
  nextItemIndex: number;
  nextOffset: number;
}>;

export const assertHash32 = (value: string, label: string): string => {
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

export const assertBudget = (budget: number): number => {
  if (
    !Number.isSafeInteger(budget) ||
    budget <= 0 ||
    budget > MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT
  ) {
    throw new Error(
      `missing-native-script staged item budget must be in 1..${MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT.toString()}`,
    );
  }
  return budget;
};

export const canonicalPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimage(items);

export const offsetAt = (
  items: readonly Uint8Array[],
  itemIndex: number,
): number => {
  if (
    !Number.isSafeInteger(itemIndex) ||
    itemIndex < 0 ||
    itemIndex > items.length
  ) {
    throw new Error("missing-native-script checkpoint item index is invalid");
  }
  let offset = encodeMidgardFieldArrayHeader(items.length).length;
  for (let index = 0; index < itemIndex; index += 1) {
    offset += encodeMidgardDefiniteBytes(items[index]!).length;
  }
  return offset;
};

export const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const encodeMissingNativeScriptTxGrammarCheckpoint = (
  checkpoint: MissingNativeScriptTxGrammarCheckpoint,
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
    MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX
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

export const hashMissingNativeScriptTxGrammarCheckpoint = (
  checkpoint: MissingNativeScriptTxGrammarCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeMissingNativeScriptTxGrammarCheckpoint(checkpoint),
    ]),
  ).toString("hex");

export const decodeMissingNativeScriptTxGrammarCheckpoint = (
  bytes: Uint8Array,
): MissingNativeScriptTxGrammarCheckpoint => {
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
  const decoded: MissingNativeScriptTxGrammarCheckpoint = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    fieldCommitment: source.subarray(39, 71).toString("hex"),
    totalLength: source.readUIntBE(72, 3),
    declaredCount: source.readUIntBE(76, 3),
    nextItemIndex: source.readUIntBE(80, 3),
    nextOffset: source.readUIntBE(84, 3),
  };
  if (
    !sameBytes(encodeMissingNativeScriptTxGrammarCheckpoint(decoded), source)
  ) {
    throw new Error(
      "missing-native-script grammar checkpoint is not canonical",
    );
  }
  return decoded;
};

export const initialMissingNativeScriptTxGrammarCheckpoint = ({
  txId,
  items,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): MissingNativeScriptTxGrammarCheckpoint => {
  const preimage = canonicalPreimage(items);
  return {
    txId: assertHash32(txId, "grammar tx id"),
    fieldIndex: MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX,
    fieldCommitment: midgardFieldCommitment(preimage).toString("hex"),
    totalLength: preimage.length,
    declaredCount: items.length,
    nextItemIndex: 0,
    nextOffset: offsetAt(items, 0),
  };
};

export const assertGrammarBound = ({
  checkpoint,
  items,
}: {
  readonly checkpoint: MissingNativeScriptTxGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
}): void => {
  const initial = initialMissingNativeScriptTxGrammarCheckpoint({
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

export const advanceMissingNativeScriptTxGrammarCheckpoint = ({
  checkpoint,
  items,
  budget,
}: {
  readonly checkpoint: MissingNativeScriptTxGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MissingNativeScriptTxGrammarCheckpoint => {
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

export const missingNativeScriptTxGrammarCheckpointIsComplete = (
  checkpoint: MissingNativeScriptTxGrammarCheckpoint,
): boolean =>
  checkpoint.nextItemIndex === checkpoint.declaredCount &&
  checkpoint.nextOffset === checkpoint.totalLength;

export const encodeMissingNativeScriptTxSemanticCheckpoint = (
  checkpoint: MissingNativeScriptTxSemanticCheckpoint,
): Buffer => {
  const txId = Buffer.from(
    assertHash32(checkpoint.txId, "semantic tx id"),
    "hex",
  );
  if (
    checkpoint.fieldIndex !==
    MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX
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

export const hashMissingNativeScriptTxSemanticCheckpoint = (
  checkpoint: MissingNativeScriptTxSemanticCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      SEMANTIC_DOMAIN,
      encodeMissingNativeScriptTxSemanticCheckpoint(checkpoint),
    ]),
  ).toString("hex");
