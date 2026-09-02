import {
  computeHash32,
  decodeMidgardFieldPreimageV1,
} from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpointV1,
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  initialMissingNativeScriptTxGrammarCheckpointV1,
  initialMissingNativeScriptTxSemanticCheckpointV1,
  type MissingNativeScriptTxGrammarCheckpointV1,
  type MissingNativeScriptTxSemanticCheckpointV1,
} from "../missing-native-script-tx/staged-walk-v1.js";
import {
  OBSERVER_ORDER_INVALID_ITEM_BUDGET_V1,
  scanObserverOrderInvalidV1,
} from "./family-v1.js";

const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");
type ObserverOrderGrammarCheckpointV1 =
  MissingNativeScriptTxGrammarCheckpointV1 & { readonly fieldIndex: 3 };
export type ObserverOrderWalkCheckpointV1 =
  MissingNativeScriptTxSemanticCheckpointV1 & { readonly fieldIndex: 3 };
const grammar3 = (
  value: MissingNativeScriptTxGrammarCheckpointV1,
): ObserverOrderGrammarCheckpointV1 =>
  ({ ...value, fieldIndex: 3 }) as ObserverOrderGrammarCheckpointV1;
const walk3 = (
  value: MissingNativeScriptTxSemanticCheckpointV1,
): ObserverOrderWalkCheckpointV1 =>
  ({ ...value, fieldIndex: 3 }) as ObserverOrderWalkCheckpointV1;
const grammar6 = (value: ObserverOrderGrammarCheckpointV1) => ({
  ...value,
  fieldIndex: 6,
});
const walk6 = (value: ObserverOrderWalkCheckpointV1) => ({
  ...value,
  fieldIndex: 6,
});

export const encodeObserverOrderWalkCheckpointV1 = (
  value: ObserverOrderWalkCheckpointV1,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxSemanticCheckpointV1(walk6(value));
  encoded[36] = 3;
  return encoded;
};
export const hashObserverOrderWalkCheckpointV1 = (
  value: ObserverOrderWalkCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeObserverOrderWalkCheckpointV1(value)]),
  ).toString("hex");

export type ObserverOrderInvalidStagedPlanV1 = Readonly<{
  items: readonly Buffer[];
  initialWalk: ObserverOrderWalkCheckpointV1;
  walk: readonly ObserverOrderWalkCheckpointV1[];
  violation: boolean;
  previousObserverHex: string;
  observerHex: string;
}>;

export const planObserverOrderInvalidStagedWalkV1 = ({
  transactionId,
  fieldPreimageCbor,
  observerIndex,
  itemBudget = OBSERVER_ORDER_INVALID_ITEM_BUDGET_V1,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly observerIndex: number;
  readonly itemBudget?: number;
}): ObserverOrderInvalidStagedPlanV1 => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 24)
    throw new Error("observerOrderInvalid item budget must be in 1..24");
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  const initialGrammar = grammar3(
    initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: transactionId,
      items,
    }),
  );
  let grammarCursor = initialGrammar;
  do {
    grammarCursor = grammar3(
      advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: grammar6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = walk3(
    initialMissingNativeScriptTxSemanticCheckpointV1({
      grammar: grammar6(grammarCursor),
      items,
    }),
  );
  const walk: ObserverOrderWalkCheckpointV1[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex <= observerIndex) {
    const remaining = observerIndex + 1 - walkCursor.nextItemIndex;
    walkCursor = walk3(
      advanceMissingNativeScriptTxSemanticCheckpointV1({
        checkpoint: walk6(walkCursor),
        txId: transactionId,
        items,
        budget: Math.min(itemBudget, remaining),
      }),
    );
    walk.push(walkCursor);
  }
  return Object.freeze({
    items: Object.freeze(items),
    initialWalk,
    walk: Object.freeze(walk),
    ...scanObserverOrderInvalidV1(items, observerIndex),
  });
};

export const observerOrderPrefixV1 = ({
  items,
  nextItemIndex,
  observerIndex,
}: {
  readonly items: readonly Uint8Array[];
  readonly nextItemIndex: number;
  readonly observerIndex: number;
}): Readonly<{ seen: number; previousObserver: string }> => {
  if (
    !Number.isSafeInteger(nextItemIndex) ||
    nextItemIndex < 0 ||
    nextItemIndex > observerIndex
  )
    throw new Error("observerOrderInvalid prefix cursor is invalid");
  let previousObserver = "";
  for (let index = 0; index < nextItemIndex; index += 1) {
    const item = Buffer.from(items[index] ?? []);
    if (item.length !== 28)
      throw new Error("observerOrderInvalid prefix item width changed");
    if (
      index > 0 &&
      Buffer.compare(Buffer.from(previousObserver, "hex"), item) >= 0
    )
      throw new Error("observerOrderInvalid prefix crossed before target");
    previousObserver = item.toString("hex");
  }
  return Object.freeze({ seen: nextItemIndex, previousObserver });
};
