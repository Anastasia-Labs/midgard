import { computeHash32, decodeMidgardFieldPreimage } from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
  type MissingNativeScriptTxGrammarCheckpoint,
  type MissingNativeScriptTxSemanticCheckpoint,
} from "../missing-native-script-tx/staged-walk-v1.js";

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");
export type MissingRedeemerGrammarCheckpoint =
  MissingNativeScriptTxGrammarCheckpoint & { readonly fieldIndex: 8 };
export type MissingRedeemerWalkCheckpoint =
  MissingNativeScriptTxSemanticCheckpoint & { readonly fieldIndex: 8 };
const to8 = <
  T extends
    | MissingNativeScriptTxGrammarCheckpoint
    | MissingNativeScriptTxSemanticCheckpoint,
>(
  value: T,
) => ({ ...value, fieldIndex: 8 }) as T & { fieldIndex: 8 };
const to6 = <
  T extends MissingRedeemerGrammarCheckpoint | MissingRedeemerWalkCheckpoint,
>(
  value: T,
) => ({ ...value, fieldIndex: 6 });
const rewrite = (encoded: Buffer): Buffer => {
  encoded[36] = 8;
  return encoded;
};
export const encodeMissingRedeemerGrammarCheckpoint = (
  value: MissingRedeemerGrammarCheckpoint,
): Buffer => rewrite(encodeMissingNativeScriptTxGrammarCheckpoint(to6(value)));
export const encodeMissingRedeemerWalkCheckpoint = (
  value: MissingRedeemerWalkCheckpoint,
): Buffer => rewrite(encodeMissingNativeScriptTxSemanticCheckpoint(to6(value)));
export const hashMissingRedeemerGrammarCheckpoint = (
  value: MissingRedeemerGrammarCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeMissingRedeemerGrammarCheckpoint(value),
    ]),
  ).toString("hex");
export const hashMissingRedeemerWalkCheckpoint = (
  value: MissingRedeemerWalkCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeMissingRedeemerWalkCheckpoint(value)]),
  ).toString("hex");

export type MissingRedeemerStagedPlan = Readonly<{
  items: readonly Buffer[];
  initialGrammar: MissingRedeemerGrammarCheckpoint;
  grammar: readonly MissingRedeemerGrammarCheckpoint[];
  initialWalk: MissingRedeemerWalkCheckpoint;
  walk: readonly MissingRedeemerWalkCheckpoint[];
}>;
export const planMissingRedeemerStagedWalk = ({
  transactionId,
  fieldPreimageCbor,
  itemBudget = 16,
}: {
  transactionId: string;
  fieldPreimageCbor: string;
  itemBudget?: number;
}): MissingRedeemerStagedPlan => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 16)
    throw new Error("missingRedeemer item budget must be in 1..16");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  let grammarCursor = to8(
    initialMissingNativeScriptTxGrammarCheckpoint({
      txId: transactionId,
      items,
    }),
  );
  const initialGrammar = grammarCursor;
  const grammar: MissingRedeemerGrammarCheckpoint[] = [];
  do {
    grammarCursor = to8(
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: to6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = to8(
    initialMissingNativeScriptTxSemanticCheckpoint({
      grammar: to6(grammarCursor),
      items,
    }),
  );
  const walk: MissingRedeemerWalkCheckpoint[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex < items.length) {
    walkCursor = to8(
      advanceMissingNativeScriptTxSemanticCheckpoint({
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
