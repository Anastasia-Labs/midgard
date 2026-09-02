import {
  computeHash32,
  decodeMidgardFieldPreimageV1,
  decodeMidgardMintPolicyItemV1,
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
import {
  decodeMintDeclaredPolicyHeaderV1,
  foldMintDeclaredAssetLimitV1,
  MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS_V1,
  MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET_V1,
} from "./family-v1.js";

const GRAMMAR_DOMAIN = Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii");
const WALK_DOMAIN = Buffer.from("MidgardFieldWalkCheckpointV1", "ascii");

export type MintDeclaredGrammarCheckpointV1 =
  MissingNativeScriptTxGrammarCheckpointV1 & { readonly fieldIndex: 5 };
export type MintDeclaredWalkCheckpointV1 =
  MissingNativeScriptTxSemanticCheckpointV1 & { readonly fieldIndex: 5 };

const grammar5 = (
  value: MissingNativeScriptTxGrammarCheckpointV1,
): MintDeclaredGrammarCheckpointV1 =>
  ({ ...value, fieldIndex: 5 }) as MintDeclaredGrammarCheckpointV1;
const walk5 = (
  value: MissingNativeScriptTxSemanticCheckpointV1,
): MintDeclaredWalkCheckpointV1 =>
  ({ ...value, fieldIndex: 5 }) as MintDeclaredWalkCheckpointV1;
const grammar6 = (value: MintDeclaredGrammarCheckpointV1) => ({
  ...value,
  fieldIndex: 6,
});
const walk6 = (value: MintDeclaredWalkCheckpointV1) => ({
  ...value,
  fieldIndex: 6,
});

export const encodeMintDeclaredGrammarCheckpointV1 = (
  value: MintDeclaredGrammarCheckpointV1,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxGrammarCheckpointV1(
    grammar6(value),
  );
  encoded[36] = 5;
  return encoded;
};

export const encodeMintDeclaredWalkCheckpointV1 = (
  value: MintDeclaredWalkCheckpointV1,
): Buffer => {
  const encoded = encodeMissingNativeScriptTxSemanticCheckpointV1(walk6(value));
  encoded[36] = 5;
  return encoded;
};

export const hashMintDeclaredGrammarCheckpointV1 = (
  value: MintDeclaredGrammarCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([
      GRAMMAR_DOMAIN,
      encodeMintDeclaredGrammarCheckpointV1(value),
    ]),
  ).toString("hex");

export const hashMintDeclaredWalkCheckpointV1 = (
  value: MintDeclaredWalkCheckpointV1,
): string =>
  computeHash32(
    Buffer.concat([WALK_DOMAIN, encodeMintDeclaredWalkCheckpointV1(value)]),
  ).toString("hex");

export type MintDeclaredAssetLimitStagedPlanV1 = Readonly<{
  items: readonly Buffer[];
  initialGrammar: MintDeclaredGrammarCheckpointV1;
  initialWalk: MintDeclaredWalkCheckpointV1;
  grammar: readonly MintDeclaredGrammarCheckpointV1[];
  walk: readonly MintDeclaredWalkCheckpointV1[];
  crossing: boolean;
  targetPolicyId: string;
  targetDeclaredCount: number;
  accumulatedCount: number;
}>;

export const planMintDeclaredAssetLimitStagedWalkV1 = ({
  transactionId,
  fieldPreimageCbor,
  policyIndex,
  itemBudget = MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET_V1,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly policyIndex: number;
  readonly itemBudget?: number;
}): MintDeclaredAssetLimitStagedPlanV1 => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 24)
    throw new Error("mintDeclaredAssetLimit item budget must be in 1..24");
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  const initialGrammar = grammar5(
    initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: transactionId,
      items,
    }),
  );
  const grammar: MintDeclaredGrammarCheckpointV1[] = [];
  let grammarCursor = initialGrammar;
  do {
    grammarCursor = grammar5(
      advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: grammar6(grammarCursor),
        items,
        budget: itemBudget,
      }),
    );
    grammar.push(grammarCursor);
  } while (grammarCursor.nextItemIndex < items.length);
  const initialWalk = walk5(
    initialMissingNativeScriptTxSemanticCheckpointV1({
      grammar: grammar6(grammarCursor),
      items,
    }),
  );
  const walk: MintDeclaredWalkCheckpointV1[] = [];
  let walkCursor = initialWalk;
  while (walkCursor.nextItemIndex <= policyIndex) {
    const remaining = policyIndex + 1 - walkCursor.nextItemIndex;
    walkCursor = walk5(
      advanceMissingNativeScriptTxSemanticCheckpointV1({
        checkpoint: walk6(walkCursor),
        txId: transactionId,
        items,
        budget: Math.min(itemBudget, remaining),
      }),
    );
    walk.push(walkCursor);
  }
  const decision = foldMintDeclaredAssetLimitV1(items, policyIndex);
  return Object.freeze({
    items: Object.freeze(items),
    initialGrammar,
    initialWalk,
    grammar: Object.freeze(grammar),
    walk: Object.freeze(walk),
    ...decision,
  });
};

export const mintDeclaredFoldPrefixV1 = ({
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
    const header = decodeMintDeclaredPolicyHeaderV1(item);
    if (
      previousPolicy !== "" &&
      Buffer.compare(Buffer.from(previousPolicy, "hex"), header.policyId) >= 0
    )
      throw new Error("mintDeclaredAssetLimit prefix policy order changed");
    const decoded = decodeMidgardMintPolicyItemV1(item);
    accumulatedCount += decoded.assets.length;
    if (accumulatedCount > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS_V1)
      throw new Error("mintDeclaredAssetLimit prefix crossed before target");
    previousPolicy = header.policyId.toString("hex");
  }
  return Object.freeze({ accumulatedCount, previousPolicy });
};
