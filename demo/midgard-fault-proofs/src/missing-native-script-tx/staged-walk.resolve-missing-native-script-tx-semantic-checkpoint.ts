import {
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  assertBudget,
  assertGrammarBound,
  assertHash32,
  canonicalPreimage,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  hashMissingNativeScriptTxGrammarCheckpoint,
  hashMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
  type MissingNativeScriptTxGrammarCheckpoint,
  missingNativeScriptTxGrammarCheckpointIsComplete,
  type MissingNativeScriptTxSemanticCheckpoint,
  offsetAt,
  sameBytes,
} from "./staged-walk.decode-missing-native-script-tx-grammar-checkpoint.js";

export const decodeMissingNativeScriptTxSemanticCheckpoint = (
  bytes: Uint8Array,
): MissingNativeScriptTxSemanticCheckpoint => {
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
  const decoded: MissingNativeScriptTxSemanticCheckpoint = {
    txId: source.subarray(3, 35).toString("hex"),
    fieldIndex: source[36]!,
    totalLength: source.readUIntBE(38, 3),
    itemCount: source.readUIntBE(42, 3),
    nextItemIndex: source.readUIntBE(46, 3),
    nextOffset: source.readUIntBE(50, 3),
  };
  if (
    !sameBytes(encodeMissingNativeScriptTxSemanticCheckpoint(decoded), source)
  ) {
    throw new Error(
      "missing-native-script semantic checkpoint is not canonical",
    );
  }
  return decoded;
};

export const initialMissingNativeScriptTxSemanticCheckpoint = ({
  grammar,
  items,
}: {
  readonly grammar: MissingNativeScriptTxGrammarCheckpoint;
  readonly items: readonly Uint8Array[];
}): MissingNativeScriptTxSemanticCheckpoint => {
  assertGrammarBound({ checkpoint: grammar, items });
  if (!missingNativeScriptTxGrammarCheckpointIsComplete(grammar)) {
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
  readonly checkpoint: MissingNativeScriptTxSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
}): void => {
  const preimage = canonicalPreimage(items);
  if (
    checkpoint.txId !== assertHash32(txId, "semantic tx id") ||
    checkpoint.fieldIndex !==
      MISSING_NATIVE_SCRIPT_TX_SCRIPT_WITNESS_FIELD_INDEX ||
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

export const advanceMissingNativeScriptTxSemanticCheckpoint = ({
  checkpoint,
  txId,
  items,
  budget,
}: {
  readonly checkpoint: MissingNativeScriptTxSemanticCheckpoint;
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly budget: number;
}): MissingNativeScriptTxSemanticCheckpoint => {
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

export const missingNativeScriptTxSemanticCheckpointIsComplete = (
  checkpoint: MissingNativeScriptTxSemanticCheckpoint,
): boolean => checkpoint.nextItemIndex === checkpoint.itemCount;

export const missingNativeScriptTxRequiredScriptPresentThrough = ({
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
export const resolveMissingNativeScriptTxGrammarCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MissingNativeScriptTxGrammarCheckpoint => {
  let checkpoint = initialMissingNativeScriptTxGrammarCheckpoint({
    txId,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (
      hashMissingNativeScriptTxGrammarCheckpoint(checkpoint) === committedHash
    ) {
      return checkpoint;
    }
    if (missingNativeScriptTxGrammarCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceMissingNativeScriptTxGrammarCheckpoint({
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
export const resolveMissingNativeScriptTxSemanticCheckpoint = ({
  txId,
  items,
  committedHash,
  budget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
}: {
  readonly txId: string;
  readonly items: readonly Uint8Array[];
  readonly committedHash: string;
  readonly budget?: number;
}): MissingNativeScriptTxSemanticCheckpoint => {
  let grammar = initialMissingNativeScriptTxGrammarCheckpoint({
    txId,
    items,
  });
  while (!missingNativeScriptTxGrammarCheckpointIsComplete(grammar)) {
    grammar = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: grammar,
      items,
      budget,
    });
  }
  let checkpoint = initialMissingNativeScriptTxSemanticCheckpoint({
    grammar,
    items,
  });
  for (let batches = 0; batches <= items.length + 1; batches += 1) {
    if (
      hashMissingNativeScriptTxSemanticCheckpoint(checkpoint) === committedHash
    ) {
      return checkpoint;
    }
    if (missingNativeScriptTxSemanticCheckpointIsComplete(checkpoint)) break;
    checkpoint = advanceMissingNativeScriptTxSemanticCheckpoint({
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
