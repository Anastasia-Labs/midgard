import {
  computeHash32,
  decodeMidgardNativeByteListPreimage,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpointV1,
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  encodeMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  hashMissingNativeScriptTxGrammarCheckpointV1,
  hashMissingNativeScriptTxSemanticCheckpointV1,
  initialMissingNativeScriptTxGrammarCheckpointV1,
  initialMissingNativeScriptTxSemanticCheckpointV1,
} from "../missing-native-script-tx/staged-walk-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1 = 24;

type Grammar = ReturnType<
  typeof initialMissingNativeScriptTxGrammarCheckpointV1
>;
type Semantic = ReturnType<
  typeof initialMissingNativeScriptTxSemanticCheckpointV1
>;
export type ScriptIntegrityField8CheckpointV1 = Grammar & {
  readonly fieldIndex: 8;
};

export const scriptIntegrityField8CheckpointV1 = (
  checkpoint: Grammar,
): ScriptIntegrityField8CheckpointV1 =>
  ({ ...checkpoint, fieldIndex: 8 }) as ScriptIntegrityField8CheckpointV1;

export const advanceScriptIntegrityField8CheckpointV1 = ({
  checkpoint,
  items,
  budget = SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
}: {
  readonly checkpoint: ScriptIntegrityField8CheckpointV1;
  readonly items: readonly Uint8Array[];
  readonly budget?: number;
}): ScriptIntegrityField8CheckpointV1 =>
  scriptIntegrityField8CheckpointV1(
    advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: { ...checkpoint, fieldIndex: 6 },
      items,
      budget,
    }),
  );

export const encodeScriptIntegrityField8CheckpointV1 = (
  checkpoint: ScriptIntegrityField8CheckpointV1,
): Buffer => {
  const bytes = encodeMissingNativeScriptTxGrammarCheckpointV1({
    ...checkpoint,
    fieldIndex: 6,
  });
  // The field index is the 37th byte in the canonical fixed-width checkpoint.
  bytes[36] = 8;
  return bytes;
};

export const hashScriptIntegrityField8CheckpointV1 = (
  checkpoint: ScriptIntegrityField8CheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii"),
      encodeScriptIntegrityField8CheckpointV1(checkpoint),
    ]),
  ).toString("hex");

export type ScriptIntegrityHashMissingStagedPlanV1 = Readonly<{
  scriptItems: readonly Buffer[];
  redeemerItems: readonly Buffer[];
  grammar: readonly Grammar[];
  semantic: readonly Semantic[];
  redeemerGrammar: readonly ScriptIntegrityField8CheckpointV1[];
}>;

/** Deterministic cursor sequence shared by production and maximum-fit tests. */
export const planScriptIntegrityHashMissingStagedWalkV1 = ({
  transactionId,
  scriptWitnessesPreimageCbor,
  redeemersPreimageCbor,
  itemBudget = SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
}: {
  readonly transactionId: string;
  readonly scriptWitnessesPreimageCbor: string;
  readonly redeemersPreimageCbor: string;
  readonly itemBudget?: number;
}): ScriptIntegrityHashMissingStagedPlanV1 => {
  const scriptItems = decodeMidgardNativeByteListPreimage(
    Buffer.from(scriptWitnessesPreimageCbor, "hex"),
    "scriptIntegrityHashMissing script witnesses",
  ).map(Buffer.from);
  const redeemerItems = decodeMidgardNativeByteListPreimage(
    Buffer.from(redeemersPreimageCbor, "hex"),
    "scriptIntegrityHashMissing redeemers",
  ).map(Buffer.from);
  const grammar: Grammar[] = [];
  let grammarCursor = initialMissingNativeScriptTxGrammarCheckpointV1({
    txId: transactionId,
    items: scriptItems,
  });
  do {
    grammarCursor = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: grammarCursor,
      items: scriptItems,
      budget: itemBudget,
    });
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < scriptItems.length);
  const semantic: Semantic[] = [];
  let semanticCursor = initialMissingNativeScriptTxSemanticCheckpointV1({
    grammar: grammarCursor,
    items: scriptItems,
  });
  do {
    semanticCursor = advanceMissingNativeScriptTxSemanticCheckpointV1({
      checkpoint: semanticCursor,
      txId: transactionId,
      items: scriptItems,
      budget: itemBudget,
    });
    semantic.push(semanticCursor);
  } while (semanticCursor.nextItemIndex < scriptItems.length);
  const redeemerGrammar: ScriptIntegrityField8CheckpointV1[] = [];
  let redeemerCursor = scriptIntegrityField8CheckpointV1(
    initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: transactionId,
      items: redeemerItems,
    }),
  );
  do {
    redeemerCursor = advanceScriptIntegrityField8CheckpointV1({
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

export const scriptIntegrityGrammarHashV1 =
  hashMissingNativeScriptTxGrammarCheckpointV1;
export const scriptIntegritySemanticHashV1 =
  hashMissingNativeScriptTxSemanticCheckpointV1;
export const encodeScriptIntegrityGrammarCheckpointV1 =
  encodeMissingNativeScriptTxGrammarCheckpointV1;
export const encodeScriptIntegritySemanticCheckpointV1 =
  encodeMissingNativeScriptTxSemanticCheckpointV1;
