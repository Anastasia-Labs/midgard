import {
  advanceFieldGrammarCheckpoint,
  assertBudget,
  assertGrammarBound,
  assertHash32,
  canonicalPreimage,
  encodeFieldSemanticCheckpoint,
  type FieldGrammarCheckpoint,
  fieldGrammarCheckpointIsComplete,
  type FieldSemanticCheckpoint,
  hashFieldGrammarCheckpoint,
  hashFieldSemanticCheckpoint,
  initialFieldGrammarCheckpoint,
  offsetAt,
  sameBytes,
  SCRIPT_WITNESS_FIELD_INDEX,
  STAGED_FIELD_WALK_BATCH_LIMIT,
} from "./grammar-checkpoint.js";

export const decodeFieldSemanticCheckpoint = (
  bytes: Uint8Array,
): FieldSemanticCheckpoint => {
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
    throw new Error("staged field-walk semantic checkpoint is not canonical");
  }
  const decoded: FieldSemanticCheckpoint = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    totalLength: source.readUIntBE(38, 3),
    itemCount: source.readUIntBE(42, 3),
    nextItemIndex: source.readUIntBE(46, 3),
    nextOffset: source.readUIntBE(50, 3),
  };
  if (!sameBytes(encodeFieldSemanticCheckpoint(decoded), source)) {
    throw new Error("staged field-walk semantic checkpoint is not canonical");
  }
  return decoded;
};

export const initialFieldSemanticCheckpoint = ({
  grammar,
  items,
}: {
  readonly grammar: FieldGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
}): FieldSemanticCheckpoint => {
  assertGrammarBound({ checkpoint: grammar, items });
  if (!fieldGrammarCheckpointIsComplete(grammar)) {
    throw new Error(
      "staged field-walk semantic scan requires terminal grammar certification",
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
  readonly checkpoint: FieldSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): void => {
  const preimage = canonicalPreimage(items);
  if (
    checkpoint.txId !== assertHash32(txId, "semantic tx id") ||
    checkpoint.fieldIndex !== SCRIPT_WITNESS_FIELD_INDEX ||
    checkpoint.totalLength !== preimage.length ||
    checkpoint.itemCount !== items.length ||
    checkpoint.nextItemIndex > checkpoint.itemCount ||
    checkpoint.nextOffset !== offsetAt(items, checkpoint.nextItemIndex)
  ) {
    throw new Error(
      "staged field-walk semantic checkpoint is not bound to the exact field preimage",
    );
  }
};

export const advanceFieldSemanticCheckpoint = ({
  checkpoint,
  txId,
  items,
  budget,
}: {
  readonly checkpoint: FieldSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): FieldSemanticCheckpoint => {
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

export const fieldSemanticCheckpointIsComplete = (
  checkpoint: FieldSemanticCheckpoint,
): boolean => checkpoint.nextItemIndex === checkpoint.itemCount;

/** Restart-safe inverse of the fixed grammar batch schedule. */
export const resolveFieldGrammarCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = STAGED_FIELD_WALK_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): FieldGrammarCheckpoint => {
  let checkpoint = initialFieldGrammarCheckpoint({
    txId,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (hashFieldGrammarCheckpoint(checkpoint) === committedHash) {
      return checkpoint;
    }
    if (fieldGrammarCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceFieldGrammarCheckpoint({
      checkpoint,
      items,
      budget,
    });
  }
  throw new Error(
    "staged field-walk grammar checkpoint is unreachable by the deterministic batch schedule",
  );
};

/** Restart-safe inverse of the fixed semantic batch schedule. */
export const resolveFieldSemanticCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = STAGED_FIELD_WALK_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): FieldSemanticCheckpoint => {
  let grammar = initialFieldGrammarCheckpoint({
    txId,
    items,
  });
  while (!fieldGrammarCheckpointIsComplete(grammar)) {
    grammar = advanceFieldGrammarCheckpoint({
      checkpoint: grammar,
      items,
      budget,
    });
  }
  let checkpoint = initialFieldSemanticCheckpoint({
    grammar,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (hashFieldSemanticCheckpoint(checkpoint) === committedHash) {
      return checkpoint;
    }
    if (fieldSemanticCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceFieldSemanticCheckpoint({
      checkpoint,
      txId,
      items,
      budget,
    });
  }
  throw new Error(
    "staged field-walk semantic checkpoint is unreachable by the deterministic batch schedule",
  );
};
