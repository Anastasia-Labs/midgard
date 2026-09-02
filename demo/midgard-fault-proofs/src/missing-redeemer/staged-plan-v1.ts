import {
  computeHash32,
  decodeMidgardFieldPreimageV1,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpointV1,
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  encodeMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  initialMissingNativeScriptTxGrammarCheckpointV1,
  initialMissingNativeScriptTxSemanticCheckpointV1,
  type MissingNativeScriptTxGrammarCheckpointV1,
  type MissingNativeScriptTxSemanticCheckpointV1,
} from "../missing-native-script-tx/staged-walk-v1.js";

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");
export type MissingRedeemerGrammarCheckpointV1 =
  MissingNativeScriptTxGrammarCheckpointV1 & { readonly fieldIndex: 8 };
export type MissingRedeemerWalkCheckpointV1 =
  MissingNativeScriptTxSemanticCheckpointV1 & { readonly fieldIndex: 8 };
const to8 = <
  T extends
    | MissingNativeScriptTxGrammarCheckpointV1
    | MissingNativeScriptTxSemanticCheckpointV1,
>(
  value: T,
) => ({ ...value, fieldIndex: 8 }) as T & { fieldIndex: 8 };
const to6 = <
  T extends
    | MissingRedeemerGrammarCheckpointV1
    | MissingRedeemerWalkCheckpointV1,
>(
  value: T,
) => ({ ...value, fieldIndex: 6 });
const rewrite = (encoded: Buffer): Buffer => {
  encoded[36] = 8;
  return encoded;
};
export const encodeMissingRedeemerGrammarCheckpointV1 = (
  value: MissingRedeemerGrammarCheckpointV1,
): Buffer =>
  rewrite(encodeMissingNativeScriptTxGrammarCheckpointV1(to6(value)));
export const encodeMissingRedeemerWalkCheckpointV1 = (
  value: MissingRedeemerWalkCheckpointV1,
): Buffer =>
  rewrite(encodeMissingNativeScriptTxSemanticCheckpointV1(to6(value)));
export const hashMissingRedeemerGrammarCheckpointV1 = (
  value: MissingRedeemerGrammarCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeMissingRedeemerGrammarCheckpointV1(value),
    ]),
  ).toString("hex");
export const hashMissingRedeemerWalkCheckpointV1 = (
  value: MissingRedeemerWalkCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeMissingRedeemerWalkCheckpointV1(value)]),
  ).toString("hex");

export type MissingRedeemerStagedPlanV1 = Readonly<{
  items: readonly Buffer[];
  initialGrammar: MissingRedeemerGrammarCheckpointV1;
  grammar: readonly MissingRedeemerGrammarCheckpointV1[];
  initialWalk: MissingRedeemerWalkCheckpointV1;
  walk: readonly MissingRedeemerWalkCheckpointV1[];
}>;
export const planMissingRedeemerStagedWalkV1 = ({
  transactionId,
  fieldPreimageCbor,
  itemBudget = 16,
}: {
  transactionId: string;
  fieldPreimageCbor: string;
  itemBudget?: number;
}): MissingRedeemerStagedPlanV1 => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 16)
    throw new Error("missingRedeemer item budget must be in 1..16");
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  let grammarCursor = to8(
    initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: transactionId,
      items,
    }),
  );
  const initialGrammar = grammarCursor;
  const grammar: MissingRedeemerGrammarCheckpointV1[] = [];
  do {
    grammarCursor = to8(
      advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: to6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = to8(
    initialMissingNativeScriptTxSemanticCheckpointV1({
      grammar: to6(grammarCursor),
      items,
    }),
  );
  const walk: MissingRedeemerWalkCheckpointV1[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex < items.length) {
    walkCursor = to8(
      advanceMissingNativeScriptTxSemanticCheckpointV1({
        checkpoint: to6(walkCursor),
        txId: transactionId,
        items,
        budget: itemBudget,
      }),
    );
    walk.push(walkCursor);
  }
  return Object.freeze({
    items: Object.freeze(items),
    initialGrammar,
    grammar: Object.freeze(grammar),
    initialWalk,
    walk: Object.freeze(walk),
  });
};
