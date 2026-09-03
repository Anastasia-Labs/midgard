import { computeHash32, decodeMidgardFieldPreimage } from "@al-ft/midgard-core";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
  type MissingNativeScriptTxGrammarCheckpoint,
  type MissingNativeScriptTxSemanticCheckpoint,
} from "../missing-native-script-tx/staged-walk-v1.js";
import {
  OBSERVER_ORDER_INVALID_ITEM_BUDGET,
  scanObserverOrderInvalid,
} from "./family-v1.js";

const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");
type ObserverOrderGrammarCheckpoint = MissingNativeScriptTxGrammarCheckpoint & {
  readonly fieldIndex: 3;
};
export type ObserverOrderWalkCheckpoint =
  MissingNativeScriptTxSemanticCheckpoint & { readonly fieldIndex: 3 };
const grammar3 = (
  value: MissingNativeScriptTxGrammarCheckpoint,
): ObserverOrderGrammarCheckpoint =>
  ({ ...value, fieldIndex: 3 }) as ObserverOrderGrammarCheckpoint;
const walk3 = (
  value: MissingNativeScriptTxSemanticCheckpoint,
): ObserverOrderWalkCheckpoint =>
  ({ ...value, fieldIndex: 3 }) as ObserverOrderWalkCheckpoint;
const grammar6 = (value: ObserverOrderGrammarCheckpoint) => ({
  ...value,
  fieldIndex: 6,
});
const walk6 = (value: ObserverOrderWalkCheckpoint) => ({
  ...value,
  fieldIndex: 6,
});

export const encodeObserverOrderWalkCheckpoint = (
  value: ObserverOrderWalkCheckpoint,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxSemanticCheckpoint(walk6(value));
  encoded[36] = 3;
  return encoded;
};
export const hashObserverOrderWalkCheckpoint = (
  value: ObserverOrderWalkCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeObserverOrderWalkCheckpoint(value)]),
  ).toString("hex");

export type ObserverOrderInvalidStagedPlan = Readonly<{
  items: readonly Buffer[];
  initialWalk: ObserverOrderWalkCheckpoint;
  walk: readonly ObserverOrderWalkCheckpoint[];
  violation: boolean;
  previousObserverHex: string;
  observerHex: string;
}>;

export const planObserverOrderInvalidStagedWalk = ({
  transactionId,
  fieldPreimageCbor,
  observerIndex,
  itemBudget = OBSERVER_ORDER_INVALID_ITEM_BUDGET,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly observerIndex: number;
  readonly itemBudget?: number;
}): ObserverOrderInvalidStagedPlan => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 24)
    throw new Error("observerOrderInvalid item budget must be in 1..24");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  const initialGrammar = grammar3(
    initialMissingNativeScriptTxGrammarCheckpoint({
      txId: transactionId,
      items,
    }),
  );
  let grammarCursor = initialGrammar;
  do {
    grammarCursor = grammar3(
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: grammar6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = walk3(
    initialMissingNativeScriptTxSemanticCheckpoint({
      grammar: grammar6(grammarCursor),
      items,
    }),
  );
  const walk: ObserverOrderWalkCheckpoint[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex <= observerIndex) {
    const remaining = observerIndex + 1 - walkCursor.nextItemIndex;
    walkCursor = walk3(
      advanceMissingNativeScriptTxSemanticCheckpoint({
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
    ...scanObserverOrderInvalid(items, observerIndex),
  });
};

export const observerOrderPrefix = ({
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
