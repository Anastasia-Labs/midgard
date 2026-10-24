import {
  computeHash32,
  decodeMidgardNativeByteListPreimage,
} from "@al-ft/midgard-core";

import {
  advanceFieldGrammarCheckpoint,
  advanceFieldSemanticCheckpoint,
  encodeFieldGrammarCheckpoint,
  encodeFieldSemanticCheckpoint,
  hashFieldGrammarCheckpoint,
  hashFieldSemanticCheckpoint,
  initialFieldGrammarCheckpoint,
  initialFieldSemanticCheckpoint,
} from "../staged-field-walk/index.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET = 24;

/** `state.direct_field_item_limit`: the per-field item cap of step 03 `Direct`. */
export const SCRIPT_INTEGRITY_HASH_MISSING_DIRECT_FIELD_ITEM_LIMIT = 64;

/**
 * Whether step 03 may take its direct route. The validator caps both opened
 * fields at `direct_field_item_limit` items, so byte size alone does not
 * decide: a small field with many items has no direct route and takes the
 * staged walk, which is open to every field-6 shape.
 */
export const scriptIntegrityHashMissingUsesDirectRoute = ({
  scriptItemCount,
  redeemerItemCount,
  fieldBytes,
  directBudget = 15_148,
}: {
  readonly scriptItemCount: number;
  readonly redeemerItemCount: number;
  readonly fieldBytes: number;
  readonly directBudget?: number;
}): boolean => {
  if (scriptItemCount < 0 || redeemerItemCount < 0 || fieldBytes < 0)
    throw new Error(
      "scriptIntegrityHashMissing evidence sizes cannot be negative",
    );
  return (
    scriptItemCount <= SCRIPT_INTEGRITY_HASH_MISSING_DIRECT_FIELD_ITEM_LIMIT &&
    redeemerItemCount <=
      SCRIPT_INTEGRITY_HASH_MISSING_DIRECT_FIELD_ITEM_LIMIT &&
    fieldBytes <= directBudget
  );
};

type Grammar = ReturnType<typeof initialFieldGrammarCheckpoint>;
type Semantic = ReturnType<typeof initialFieldSemanticCheckpoint>;
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
    advanceFieldGrammarCheckpoint({
      checkpoint: { ...checkpoint, fieldIndex: 6 },
      items,
      budget,
    }),
  );

export const encodeScriptIntegrityField8Checkpoint = (
  checkpoint: ScriptIntegrityField8Checkpoint,
): Buffer => {
  const bytes = encodeFieldGrammarCheckpoint({
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
  let grammarCursor = initialFieldGrammarCheckpoint({
    txId: transactionId,
    items: scriptItems,
  });
  do {
    grammarCursor = advanceFieldGrammarCheckpoint({
      checkpoint: grammarCursor,
      items: scriptItems,
      budget: itemBudget,
    });
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < scriptItems.length);
  const semantic: Semantic[] = [];
  let semanticCursor = initialFieldSemanticCheckpoint({
    grammar: grammarCursor,
    items: scriptItems,
  });
  do {
    semanticCursor = advanceFieldSemanticCheckpoint({
      checkpoint: semanticCursor,
      txId: transactionId,
      items: scriptItems,
      budget: itemBudget,
    });
    semantic.push(semanticCursor);
  } while (semanticCursor.nextItemIndex < scriptItems.length);
  const redeemerGrammar: ScriptIntegrityField8Checkpoint[] = [];
  let redeemerCursor = scriptIntegrityField8Checkpoint(
    initialFieldGrammarCheckpoint({
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

export const scriptIntegrityGrammarHash = hashFieldGrammarCheckpoint;
export const scriptIntegritySemanticHash = hashFieldSemanticCheckpoint;
export const encodeScriptIntegrityGrammarCheckpoint =
  encodeFieldGrammarCheckpoint;
export const encodeScriptIntegritySemanticCheckpoint =
  encodeFieldSemanticCheckpoint;
