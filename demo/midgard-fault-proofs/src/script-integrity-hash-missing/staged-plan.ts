import {
  computeHash32,
  decodeMidgardNativeByteListPreimage,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  hashMissingNativeScriptTxGrammarCheckpoint,
  hashMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
} from "../missing-native-script-tx/staged-walk.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET = 24;

type Grammar = ReturnType<typeof initialMissingNativeScriptTxGrammarCheckpoint>;
type Semantic = ReturnType<
  typeof initialMissingNativeScriptTxSemanticCheckpoint
>;
export type ScriptIntegrityField8Checkpoint = Grammar & {
  readonly fieldIndex: 8;
};

export const scriptIntegrityField8Checkpoint = (
  checkpoint: Grammar,
): ScriptIntegrityField8Checkpoint =>
  ({ ...checkpoint, fieldIndex: 8 }) as ScriptIntegrityField8Checkpoint;

export const advanceScriptIntegrityField8Checkpoint = ({
  checkpoint,
  items,
  budget = SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
}: {
  readonly checkpoint: ScriptIntegrityField8Checkpoint;
  readonly items: readonly Uint8Array[];
  readonly budget?: number;
}): ScriptIntegrityField8Checkpoint =>
  scriptIntegrityField8Checkpoint(
    advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: { ...checkpoint, fieldIndex: 6 },
      items,
      budget,
    }),
  );

export const encodeScriptIntegrityField8Checkpoint = (
  checkpoint: ScriptIntegrityField8Checkpoint,
): Buffer => {
  const bytes = encodeMissingNativeScriptTxGrammarCheckpoint({
    ...checkpoint,
    fieldIndex: 6,
  });
  // The field index is the 37th byte in the canonical fixed-width checkpoint.
  bytes[36] = 8;
  return bytes;
};

export const hashScriptIntegrityField8Checkpoint = (
  checkpoint: ScriptIntegrityField8Checkpoint,
): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii"),
      encodeScriptIntegrityField8Checkpoint(checkpoint),
    ]),
  ).toString("hex");

export type ScriptIntegrityHashMissingStagedPlan = Readonly<{
  scriptItems: readonly Buffer[];
  redeemerItems: readonly Buffer[];
  grammar: readonly Grammar[];
  semantic: readonly Semantic[];
  redeemerGrammar: readonly ScriptIntegrityField8Checkpoint[];
}>;

/** Deterministic cursor sequence shared by production and maximum-fit tests. */
export const planScriptIntegrityHashMissingStagedWalk = ({
  transactionId,
  scriptWitnessesPreimageCbor,
  redeemersPreimageCbor,
  itemBudget = SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
}: {
  readonly transactionId: string;
  readonly scriptWitnessesPreimageCbor: string;
  readonly redeemersPreimageCbor: string;
  readonly itemBudget?: number;
}): ScriptIntegrityHashMissingStagedPlan => {
  const scriptItems = decodeMidgardNativeByteListPreimage(
    Buffer.from(scriptWitnessesPreimageCbor, "hex"),
    "scriptIntegrityHashMissing script witnesses",
  ).map(Buffer.from);
  const redeemerItems = decodeMidgardNativeByteListPreimage(
    Buffer.from(redeemersPreimageCbor, "hex"),
    "scriptIntegrityHashMissing redeemers",
  ).map(Buffer.from);
  const grammar: Grammar[] = [];
  let grammarCursor = initialMissingNativeScriptTxGrammarCheckpoint({
    txId: transactionId,
    items: scriptItems,
  });
  do {
    grammarCursor = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: grammarCursor,
      items: scriptItems,
      budget: itemBudget,
    });
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < scriptItems.length);
  const semantic: Semantic[] = [];
  let semanticCursor = initialMissingNativeScriptTxSemanticCheckpoint({
    grammar: grammarCursor,
    items: scriptItems,
  });
  do {
    semanticCursor = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: semanticCursor,
      txId: transactionId,
      items: scriptItems,
      budget: itemBudget,
    });
    semantic.push(semanticCursor);
  } while (semanticCursor.nextItemIndex < scriptItems.length);
  const redeemerGrammar: ScriptIntegrityField8Checkpoint[] = [];
  let redeemerCursor = scriptIntegrityField8Checkpoint(
    initialMissingNativeScriptTxGrammarCheckpoint({
      txId: transactionId,
      items: redeemerItems,
    }),
  );
  do {
    redeemerCursor = advanceScriptIntegrityField8Checkpoint({
      checkpoint: redeemerCursor,
      items: redeemerItems,
      budget: itemBudget,
    });
    redeemerGrammar.push(redeemerCursor);
  } while (redeemerCursor.nextItemIndex < redeemerItems.length);
  return Object.freeze({
    scriptItems: Object.freeze(scriptItems),
    redeemerItems: Object.freeze(redeemerItems),
    grammar: Object.freeze(grammar),
    semantic: Object.freeze(semantic),
    redeemerGrammar: Object.freeze(redeemerGrammar),
  });
};

export const scriptIntegrityGrammarHash =
  hashMissingNativeScriptTxGrammarCheckpoint;
export const scriptIntegritySemanticHash =
  hashMissingNativeScriptTxSemanticCheckpoint;
export const encodeScriptIntegrityGrammarCheckpoint =
  encodeMissingNativeScriptTxGrammarCheckpoint;
export const encodeScriptIntegritySemanticCheckpoint =
  encodeMissingNativeScriptTxSemanticCheckpoint;
