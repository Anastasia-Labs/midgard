import {
  computeHash32,
  decodeMidgardFieldPreimage,
  encodeMidgardDefiniteBytes,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
  type MissingNativeScriptTxGrammarCheckpoint,
  type MissingNativeScriptTxSemanticCheckpoint,
} from "../missing-native-script-tx/staged-walk.js";

export type ConservationFieldIndex = 2 | 5;
const encode = (
  checkpoint:
    | MissingNativeScriptTxGrammarCheckpoint
    | MissingNativeScriptTxSemanticCheckpoint,
  field: ConservationFieldIndex,
): string => {
  const bytes =
    "fieldCommitment" in checkpoint
      ? encodeMissingNativeScriptTxGrammarCheckpoint(checkpoint)
      : encodeMissingNativeScriptTxSemanticCheckpoint(checkpoint);
  bytes[36] = field;
  return bytes.toString("hex");
};
const hash = (bytes: string, grammar: boolean): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from(
        grammar
          ? "MidgardFieldGrammarCheckpointV1"
          : "MidgardFieldWalkCheckpointV1",
        "ascii",
      ),
      Buffer.from(bytes, "hex"),
    ]),
  ).toString("hex");

/** Exact grammar and item extents; payload bytes are never part of a checkpoint. */
export const planConservationField = (
  transactionId: string,
  field: ConservationFieldIndex,
  preimage: Uint8Array,
) => {
  const items = decodeMidgardFieldPreimage(preimage).map(Buffer.from);
  let grammar = initialMissingNativeScriptTxGrammarCheckpoint({
    txId: transactionId,
    items,
  });
  const grammarSteps: {
    before: string;
    after: string;
    afterHash: string;
    complete: boolean;
  }[] = [];
  do {
    const before = encode(grammar, field);
    grammar = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: grammar,
      items,
      budget: 16,
    });
    const after = encode(grammar, field);
    grammarSteps.push({
      before,
      after,
      afterHash: hash(after, true),
      complete: grammar.nextItemIndex === items.length,
    });
  } while (grammar.nextItemIndex !== items.length);
  let walk = initialMissingNativeScriptTxSemanticCheckpoint({ grammar, items });
  const initialWalk = encode(walk, field);
  const extents = items.map((item) => {
    const before = encode(walk, field);
    const offset =
      walk.nextOffset + encodeMidgardDefiniteBytes(item).length - item.length;
    walk = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: walk,
      txId: transactionId,
      items,
      budget: 1,
    });
    const after = encode(walk, field);
    return {
      item,
      offset,
      length: item.length,
      before,
      beforeHash: hash(before, false),
      after,
      afterHash: hash(after, false),
    };
  });
  const terminalWalk = encode(walk, field);
  return {
    field,
    items,
    grammarSteps,
    initialWalk,
    initialWalkHash: hash(initialWalk, false),
    extents,
    terminalWalk,
    terminalWalkHash: hash(terminalWalk, false),
  };
};
