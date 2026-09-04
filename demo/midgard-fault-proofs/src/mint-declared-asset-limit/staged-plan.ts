import {
  computeHash32,
  decodeMidgardFieldPreimage,
  decodeMidgardMintPolicyItem,
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
import {
  decodeMintDeclaredPolicyHeader,
  foldMintDeclaredAssetLimit,
  MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS,
  MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET,
} from "./family.js";

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

export type MintDeclaredGrammarCheckpoint =
  MissingNativeScriptTxGrammarCheckpoint & { readonly fieldIndex: 5 };
export type MintDeclaredWalkCheckpoint =
  MissingNativeScriptTxSemanticCheckpoint & { readonly fieldIndex: 5 };

const grammar5 = (
  value: MissingNativeScriptTxGrammarCheckpoint,
): MintDeclaredGrammarCheckpoint =>
  ({ ...value, fieldIndex: 5 }) as MintDeclaredGrammarCheckpoint;
const walk5 = (
  value: MissingNativeScriptTxSemanticCheckpoint,
): MintDeclaredWalkCheckpoint =>
  ({ ...value, fieldIndex: 5 }) as MintDeclaredWalkCheckpoint;
const grammar6 = (value: MintDeclaredGrammarCheckpoint) => ({
  ...value,
  fieldIndex: 6,
});
const walk6 = (value: MintDeclaredWalkCheckpoint) => ({
  ...value,
  fieldIndex: 6,
});

export const encodeMintDeclaredGrammarCheckpoint = (
  value: MintDeclaredGrammarCheckpoint,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxGrammarCheckpoint(grammar6(value));
  encoded[36] = 5;
  return encoded;
};

export const encodeMintDeclaredWalkCheckpoint = (
  value: MintDeclaredWalkCheckpoint,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxSemanticCheckpoint(walk6(value));
  encoded[36] = 5;
  return encoded;
};

export const hashMintDeclaredGrammarCheckpoint = (
  value: MintDeclaredGrammarCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([GRAMMAR_DOMAIN, encodeMintDeclaredGrammarCheckpoint(value)]),
  ).toString("hex");

export const hashMintDeclaredWalkCheckpoint = (
  value: MintDeclaredWalkCheckpoint,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeMintDeclaredWalkCheckpoint(value)]),
  ).toString("hex");

export type MintDeclaredAssetLimitStagedPlan = Readonly<{
  items: readonly Buffer[];
  initialGrammar: MintDeclaredGrammarCheckpoint;
  initialWalk: MintDeclaredWalkCheckpoint;
  grammar: readonly MintDeclaredGrammarCheckpoint[];
  walk: readonly MintDeclaredWalkCheckpoint[];
  crossing: boolean;
  targetPolicyId: string;
  targetDeclaredCount: number;
  accumulatedCount: number;
}>;

export const planMintDeclaredAssetLimitStagedWalk = ({
  transactionId,
  fieldPreimageCbor,
  policyIndex,
  itemBudget = MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly policyIndex: number;
  readonly itemBudget?: number;
}): MintDeclaredAssetLimitStagedPlan => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 24)
    throw new Error("mintDeclaredAssetLimit item budget must be in 1..24");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  const initialGrammar = grammar5(
    initialMissingNativeScriptTxGrammarCheckpoint({
      txId: transactionId,
      items,
    }),
  );
  const grammar: MintDeclaredGrammarCheckpoint[] = [];
  let grammarCursor = initialGrammar;
  do {
    grammarCursor = grammar5(
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: grammar6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = walk5(
    initialMissingNativeScriptTxSemanticCheckpoint({
      grammar: grammar6(grammarCursor),
      items,
    }),
  );
  const walk: MintDeclaredWalkCheckpoint[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex <= policyIndex) {
    const remaining = policyIndex + 1 - walkCursor.nextItemIndex;
    walkCursor = walk5(
      advanceMissingNativeScriptTxSemanticCheckpoint({
        checkpoint: walk6(walkCursor),
        txId: transactionId,
        items,
        budget: Math.min(itemBudget, remaining),
      }),
    );
    walk.push(walkCursor);
  }
  const decision = foldMintDeclaredAssetLimit(items, policyIndex);
  return Object.freeze({
    items: Object.freeze(items),
    initialGrammar,
    initialWalk,
    grammar: Object.freeze(grammar),
    walk: Object.freeze(walk),
    ...decision,
  });
};

export const mintDeclaredFoldPrefix = ({
  items,
  nextItemIndex,
  policyIndex,
}: {
  readonly items: readonly Uint8Array[];
  readonly nextItemIndex: number;
  readonly policyIndex: number;
}): Readonly<{ accumulatedCount: number; previousPolicy: string }> => {
  if (
    !Number.isSafeInteger(nextItemIndex) ||
    nextItemIndex < 0 ||
    nextItemIndex > policyIndex
  )
    throw new Error("mintDeclaredAssetLimit prefix cursor is invalid");
  let accumulatedCount = 0;
  let previousPolicy = "";
  for (let index = 0; index < nextItemIndex; index += 1) {
    const item = items[index];
    if (item === undefined)
      throw new Error("mintDeclaredAssetLimit prefix exceeds field");
    const header = decodeMintDeclaredPolicyHeader(item);
    if (
      previousPolicy !== "" &&
      Buffer.compare(Buffer.from(previousPolicy, "hex"), header.policyId) >= 0
    )
      throw new Error("mintDeclaredAssetLimit prefix policy order changed");
    const decoded = decodeMidgardMintPolicyItem(item);
    accumulatedCount += decoded.assets.length;
    if (accumulatedCount > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
      throw new Error("mintDeclaredAssetLimit prefix crossed before target");
    previousPolicy = header.policyId.toString("hex");
  }
  return Object.freeze({ accumulatedCount, previousPolicy });
};
