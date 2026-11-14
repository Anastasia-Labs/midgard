import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  NETWORK_ID_FORCED_GRAMMAR_BATCH,
  NETWORK_ID_FORCED_SCAN_BATCH,
  type NetworkIdForcedScanState,
} from "@al-ft/midgard-sdk";

import type { FaultProofFieldOpeningPlan } from "../field-opening.js";
import {
  fieldGrammarCheckpointIsComplete,
  initialFieldGrammarCheckpoint,
  initialFieldSemanticCheckpoint,
} from "../staged-field-walk/index.js";
import {
  advanceGrammarBy,
  advanceWalkBy,
  asField2Grammar,
  asField2Walk,
  asField6Grammar,
  hashNetworkIdForcedScanGrammarCheckpoint,
  hashNetworkIdForcedScanWalkCheckpoint,
  NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH,
  NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH,
  NETWORK_ID_OUTPUTS_FIELD_INDEX,
  type NetworkIdForcedScanGrammarCheckpoint,
  type NetworkIdForcedScanPlan,
  type NetworkIdForcedScanStep,
  type NetworkIdForcedScanWalkCheckpoint,
} from "./forced-scan-plan.advance-grammar-by.js";
import { networkIdSubmitError } from "./submit-common.js";

/**
 * The exact action sequence, with its budgets, that walks `outputsCarriagePlan`
 * from the forced door's `Ready` state to step 02's terminal state.
 *
 * Tiers 1–2 open the whole view once and then fold `ceil(n / scan_batch)`
 * batches. Tier 3's §5.1 count is provisional, so it certifies the envelope
 * grammar in `ceil(n / grammar_batch)` batches, converts the terminal
 * certificate into the item-zero walk position, and only then folds.
 */
export const planNetworkIdForcedScan = ({
  outputsCarriagePlan,
  outputCount,
  scanBatch,
  grammarBatch = NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH,
}: {
  readonly outputsCarriagePlan: FaultProofFieldOpeningPlan;
  readonly outputCount: number;
  readonly scanBatch?: bigint;
  readonly grammarBatch?: bigint;
}): NetworkIdForcedScanPlan => {
  if (outputsCarriagePlan.fieldIndex !== NETWORK_ID_OUTPUTS_FIELD_INDEX) {
    throw networkIdSubmitError(
      "forced scan plans §2.5 field 2 only; the supplied carriage opens another field",
    );
  }
  const items = decodeMidgardFieldPreimage(outputsCarriagePlan.preimage).map(
    (item) => Buffer.from(item),
  );
  if (items.length !== outputCount) {
    throw networkIdSubmitError(
      `forced scan output count ${outputCount.toString()} is not the ${items.length.toString()} outputs the authenticated field carries`,
    );
  }
  if (items.length === 0) {
    throw networkIdSubmitError(
      "forced scan requires at least one committed output to fold",
    );
  }
  const requiresGrammar = outputsCarriagePlan.plan.tier === "Certified";
  const effectiveScanBatch =
    scanBatch ??
    (requiresGrammar
      ? NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH
      : NETWORK_ID_FORCED_SCAN_BATCH);
  const scan = Number(effectiveScanBatch);
  const grammarSize = Number(grammarBatch);
  if (
    !Number.isSafeInteger(scan) ||
    scan <= 0 ||
    !Number.isSafeInteger(grammarSize) ||
    grammarSize <= 0
  ) {
    throw networkIdSubmitError("forced scan batch sizes must be positive");
  }
  if (effectiveScanBatch > NETWORK_ID_FORCED_SCAN_BATCH) {
    throw networkIdSubmitError(
      `forced scan semantic batch ${effectiveScanBatch.toString()} is above the ${NETWORK_ID_FORCED_SCAN_BATCH.toString()} scan_batch the validator accepts`,
    );
  }
  if (grammarBatch > NETWORK_ID_FORCED_GRAMMAR_BATCH) {
    throw networkIdSubmitError(
      `forced scan grammar batch ${grammarBatch.toString()} is above the ${NETWORK_ID_FORCED_GRAMMAR_BATCH.toString()} grammar_batch the validator accepts`,
    );
  }
  const txId = outputsCarriagePlan.nativeTxId;
  const initialGrammar = asField2Grammar(
    initialFieldGrammarCheckpoint({ txId, items }),
  );
  const grammar: NetworkIdForcedScanGrammarCheckpoint[] = [];
  let grammarCursor = initialGrammar;
  do {
    grammarCursor = advanceGrammarBy({
      checkpoint: grammarCursor,
      items,
      budget: Math.min(
        grammarSize,
        grammarCursor.declaredCount - grammarCursor.nextItemIndex,
      ),
    });
    grammar.push(grammarCursor);
  } while (!fieldGrammarCheckpointIsComplete(grammarCursor));
  const initialWalk = asField2Walk(
    initialFieldSemanticCheckpoint({
      grammar: asField6Grammar(grammarCursor),
      items,
    }),
  );
  const walk: NetworkIdForcedScanWalkCheckpoint[] = [];
  let walkCursor = initialWalk;
  const advanceBudgets: number[] = [];
  while (walkCursor.nextItemIndex < walkCursor.itemCount) {
    const budget = Math.min(
      scan,
      walkCursor.itemCount - walkCursor.nextItemIndex,
    );
    advanceBudgets.push(budget);
    walkCursor = advanceWalkBy({ checkpoint: walkCursor, txId, items, budget });
    walk.push(walkCursor);
  }
  const steps: NetworkIdForcedScanStep[] = requiresGrammar
    ? [
        {
          kind: "startGrammar",
          itemBudget: BigInt(
            grammar[0]!.nextItemIndex - initialGrammar.nextItemIndex,
          ),
        },
        ...grammar.slice(1).map(
          (checkpoint, index): NetworkIdForcedScanStep => ({
            kind: "resumeGrammar",
            ordinal: index + 1,
            itemBudget: BigInt(
              checkpoint.nextItemIndex - grammar[index]!.nextItemIndex,
            ),
          }),
        ),
        { kind: "finishGrammar" },
      ]
    : [{ kind: "open" }];
  for (const [index, budget] of advanceBudgets.entries()) {
    steps.push({
      kind: "advance",
      ordinal: index,
      itemBudget: BigInt(budget),
      completes: index === advanceBudgets.length - 1,
    });
  }
  return Object.freeze({
    items: Object.freeze(items),
    outputCount: items.length,
    tier: outputsCarriagePlan.plan.tier,
    requiresGrammar,
    initialGrammar,
    grammar: Object.freeze(grammar),
    initialWalk,
    walk: Object.freeze(walk),
    steps: Object.freeze(steps),
  }) as NetworkIdForcedScanPlan;
};

/** The grammar checkpoint a `resumeGrammar`/`finishGrammar` action resumes. */
export const networkIdForcedScanPriorGrammar = (
  plan: NetworkIdForcedScanPlan,
  step: NetworkIdForcedScanStep,
): NetworkIdForcedScanGrammarCheckpoint => {
  if (step.kind === "resumeGrammar") {
    const prior = plan.grammar[step.ordinal - 1];
    if (prior === undefined) {
      throw networkIdSubmitError(
        "forced scan grammar ordinal is outside the planned batch schedule",
      );
    }
    return prior;
  }
  if (step.kind === "finishGrammar") {
    const terminal = plan.grammar.at(-1);
    if (terminal === undefined) {
      throw networkIdSubmitError(
        "forced scan grammar certification has no terminal checkpoint",
      );
    }
    return terminal;
  }
  throw networkIdSubmitError(
    "forced scan action does not resume a grammar checkpoint",
  );
};

/** The walk checkpoint an `advance` action resumes. */
export const networkIdForcedScanPriorWalk = (
  plan: NetworkIdForcedScanPlan,
  ordinal: number,
): NetworkIdForcedScanWalkCheckpoint => {
  if (ordinal === 0) return plan.initialWalk;
  const prior = plan.walk[ordinal - 1];
  if (prior === undefined) {
    throw networkIdSubmitError(
      "forced scan walk ordinal is outside the planned batch schedule",
    );
  }
  return prior;
};

/** The checkpoint hash the thread state must carry before `step` runs. */
export const networkIdForcedScanExpectedStateHash = (
  plan: NetworkIdForcedScanPlan,
  step: NetworkIdForcedScanStep,
): string | undefined => {
  switch (step.kind) {
    case "open":
    case "startGrammar":
      return undefined;
    case "resumeGrammar":
    case "finishGrammar":
      return hashNetworkIdForcedScanGrammarCheckpoint(
        networkIdForcedScanPriorGrammar(plan, step),
      );
    case "advance":
      return hashNetworkIdForcedScanWalkCheckpoint(
        networkIdForcedScanPriorWalk(plan, step.ordinal),
      );
  }
};

/** The checkpoint hash the successor state carries, or `null` at completion. */
export const networkIdForcedScanSuccessorStateHash = (
  plan: NetworkIdForcedScanPlan,
  step: NetworkIdForcedScanStep,
): string | null => {
  switch (step.kind) {
    case "open":
    case "finishGrammar":
      return hashNetworkIdForcedScanWalkCheckpoint(plan.initialWalk);
    case "startGrammar":
      return hashNetworkIdForcedScanGrammarCheckpoint(plan.grammar[0]!);
    case "resumeGrammar":
      return hashNetworkIdForcedScanGrammarCheckpoint(
        plan.grammar[step.ordinal]!,
      );
    case "advance":
      return step.completes
        ? null
        : hashNetworkIdForcedScanWalkCheckpoint(plan.walk[step.ordinal]!);
  }
};

/**
 * The batch a live thread state is waiting for, located from the committed
 * checkpoint digest alone. A digest that is on no planned checkpoint means the
 * thread was advanced against a different field, so the walk is refused rather
 * than resumed at a guessed position.
 */
export const networkIdForcedScanStepForState = (
  plan: NetworkIdForcedScanPlan,
  state: NetworkIdForcedScanState,
): NetworkIdForcedScanStep => {
  if ("Ready" in state) {
    const opening = plan.steps[0];
    if (opening === undefined) {
      throw networkIdSubmitError("forced scan plan has no opening action");
    }
    return opening;
  }
  if ("Grammar" in state) {
    const committed = state.Grammar.checkpoint_hash;
    const index = plan.grammar.findIndex(
      (checkpoint) =>
        hashNetworkIdForcedScanGrammarCheckpoint(checkpoint) === committed,
    );
    if (index < 0) {
      throw networkIdSubmitError(
        `forced scan thread committed grammar checkpoint ${committed}, which is on no batch of the planned certification`,
      );
    }
    const resumed = plan.steps.find(
      (step) => step.kind === "resumeGrammar" && step.ordinal === index + 1,
    );
    if (resumed !== undefined) return resumed;
    const finish = plan.steps.find((step) => step.kind === "finishGrammar");
    if (finish === undefined) {
      throw networkIdSubmitError(
        "forced scan plan certifies no grammar terminal",
      );
    }
    return finish;
  }
  const committed = state.Scanning.checkpoint_hash;
  const unplanned = () =>
    networkIdSubmitError(
      `forced scan thread committed walk checkpoint ${committed}, which is on no batch of the planned walk`,
    );
  let ordinal: number;
  if (hashNetworkIdForcedScanWalkCheckpoint(plan.initialWalk) === committed) {
    ordinal = 0;
  } else {
    const index = plan.walk.findIndex(
      (checkpoint) =>
        hashNetworkIdForcedScanWalkCheckpoint(checkpoint) === committed,
    );
    if (index < 0) throw unplanned();
    ordinal = index + 1;
  }
  const advance = plan.steps.find(
    (step) => step.kind === "advance" && step.ordinal === ordinal,
  );
  if (advance === undefined) throw unplanned();
  return advance;
};

/** The plan-relative ordinal of a step, `0` for the actions that have none. */
export const networkIdForcedScanStepOrdinal = (
  step: NetworkIdForcedScanStep,
): number =>
  step.kind === "resumeGrammar" || step.kind === "advance" ? step.ordinal : 0;
