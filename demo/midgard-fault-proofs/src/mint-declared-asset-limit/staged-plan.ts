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
} from "../missing-native-script-tx/staged-walk.js";
import {
  advanceMintDeclaredFold,
  decodeMintDeclaredPolicyHeader,
  initialMintDeclaredFoldCursor,
  MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
  MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET,
  MINT_DECLARED_OUTCOME_CROSSING,
  MINT_DECLARED_OUTCOME_SCANNING,
  type MintDeclaredFoldCursor,
  type MintDeclaredFoldTarget,
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

/** The fold cursor and walk position committed after one step-03 transaction. */
export type MintDeclaredFoldSnapshot = Readonly<{
  cursor: MintDeclaredFoldCursor;
  checkpoint: MintDeclaredWalkCheckpoint;
}>;

export type MintDeclaredAssetLimitStagedPlan = Readonly<{
  items: readonly Buffer[];
  initialGrammar: MintDeclaredGrammarCheckpoint;
  initialWalk: MintDeclaredWalkCheckpoint;
  grammar: readonly MintDeclaredGrammarCheckpoint[];
  /** One entry per step-03 transaction; the last one carries the decision. */
  walk: readonly MintDeclaredFoldSnapshot[];
  foldBudget: number;
  target: MintDeclaredFoldTarget;
  crossing: boolean;
  targetPolicyId: string;
  targetDeclaredCount: number;
  accumulatedCount: number;
}>;

export const initialMintDeclaredFoldSnapshot = (
  plan: Pick<MintDeclaredAssetLimitStagedPlan, "initialWalk">,
): MintDeclaredFoldSnapshot =>
  Object.freeze({
    cursor: initialMintDeclaredFoldCursor(),
    checkpoint: plan.initialWalk,
  });

/** One step-03 transaction of `budget` units from `snapshot`. */
export const advanceMintDeclaredFoldSnapshot = ({
  snapshot,
  transactionId,
  items,
  target,
  budget,
}: {
  readonly snapshot: MintDeclaredFoldSnapshot;
  readonly transactionId: string;
  readonly items: readonly Uint8Array[];
  readonly target: MintDeclaredFoldTarget;
  readonly budget: number;
}): MintDeclaredFoldSnapshot => {
  const advanced = advanceMintDeclaredFold({
    cursor: snapshot.cursor,
    nextItemIndex: snapshot.checkpoint.nextItemIndex,
    items,
    target,
    budget,
  });
  const closed = advanced.nextItemIndex - snapshot.checkpoint.nextItemIndex;
  const checkpoint =
    closed === 0
      ? snapshot.checkpoint
      : walk5(
          advanceMissingNativeScriptTxSemanticCheckpoint({
            checkpoint: walk6(snapshot.checkpoint),
            txId: transactionId,
            items,
            budget: closed,
          }),
        );
  return Object.freeze({ cursor: advanced.cursor, checkpoint });
};

export type MintDeclaredAssetLimitFieldPlan = Pick<
  MintDeclaredAssetLimitStagedPlan,
  "items" | "initialGrammar" | "initialWalk" | "grammar" | "target"
>;

/**
 * The grammar certification and initial walk position of field 5 plus the
 * bound target header, without folding: the part of a plan that exists for
 * any authenticated field, foldable to completion or not.
 */
export const planMintDeclaredAssetLimitField = ({
  transactionId,
  fieldPreimageCbor,
  policyIndex,
  itemBudget = MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly policyIndex: number;
  readonly itemBudget?: number;
}): MintDeclaredAssetLimitFieldPlan => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget <= 0 || itemBudget > 24)
    throw new Error("mintDeclaredAssetLimit item budget must be in 1..24");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).map(Buffer.from);
  const targetItem = items[policyIndex];
  if (targetItem === undefined)
    throw new Error(
      "mintDeclaredAssetLimit policy coordinate is outside field 5",
    );
  const header = decodeMintDeclaredPolicyHeader(targetItem);
  const target: MintDeclaredFoldTarget = Object.freeze({
    policyIndex,
    targetPolicyId: header.policyId.toString("hex"),
    targetDeclaredCount: header.declaredCount,
  });
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
  return Object.freeze({
    items: Object.freeze(items),
    initialGrammar,
    initialWalk,
    grammar: Object.freeze(grammar),
    target,
  });
};

export const planMintDeclaredAssetLimitStagedWalk = ({
  transactionId,
  fieldPreimageCbor,
  policyIndex,
  itemBudget = MINT_DECLARED_ASSET_LIMIT_POLICY_BUDGET,
  foldBudget = MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
}: {
  readonly transactionId: string;
  readonly fieldPreimageCbor: string;
  readonly policyIndex: number;
  readonly itemBudget?: number;
  readonly foldBudget?: number;
}): MintDeclaredAssetLimitStagedPlan => {
  if (
    !Number.isSafeInteger(foldBudget) ||
    foldBudget <= 0 ||
    foldBudget > MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET
  )
    throw new Error(
      `mintDeclaredAssetLimit fold budget must be in 1..${MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET.toString()}`,
    );
  const field = planMintDeclaredAssetLimitField({
    transactionId,
    fieldPreimageCbor,
    policyIndex,
    itemBudget,
  });
  const { items, initialWalk, target } = field;
  const walk: MintDeclaredFoldSnapshot[] = [];
  let snapshot = initialMintDeclaredFoldSnapshot({ initialWalk });
  while (snapshot.cursor.outcome === MINT_DECLARED_OUTCOME_SCANNING) {
    snapshot = advanceMintDeclaredFoldSnapshot({
      snapshot,
      transactionId,
      items,
      target,
      budget: foldBudget,
    });
    walk.push(snapshot);
  }
  return Object.freeze({
    ...field,
    walk: Object.freeze(walk),
    foldBudget,
    crossing: snapshot.cursor.outcome === MINT_DECLARED_OUTCOME_CROSSING,
    targetPolicyId: target.targetPolicyId,
    targetDeclaredCount: target.targetDeclaredCount,
    accumulatedCount: snapshot.cursor.accumulatedCount,
  });
};
