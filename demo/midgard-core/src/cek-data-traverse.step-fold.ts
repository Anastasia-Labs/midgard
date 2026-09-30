import {
  foldMidgardCekDataFrameListChild,
  foldMidgardCekDataFrameMapPair,
  hashMidgardCekDataFrame,
} from "./cek-data-frame.js";
import {
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import { advanced } from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import {
  stepBytes,
  stepClose,
  stepFinalizeFrame,
  stepHead,
  stepInteger,
  stepLargeConstructor,
  stepLargeFields,
} from "./cek-data-traverse.step-large-constructor.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";

const stepFold = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if ((sourceBytes !== null && sourceBytes !== undefined) || action === null) {
    return null;
  }
  if (
    "frame" in action &&
    !hashMidgardCekDataFrame(action.frame).equals(control.frameRoot)
  ) {
    return null;
  }
  if (action.kind === "foldList") {
    const frame = foldMidgardCekDataFrameListChild({
      frame: action.frame,
      childIndex: action.childIndex,
      child: action.child,
      siblings: action.siblings,
    });
    return frame === null
      ? null
      : advanced({
          ...control,
          frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
        });
  }
  if (action.kind === "foldMap") {
    const frame = foldMidgardCekDataFrameMapPair({
      frame: action.frame,
      pairIndex: action.pairIndex,
      key: action.key,
      value: action.value,
      keySiblings: action.keySiblings,
      valueSiblings: action.valueSiblings,
    });
    return frame === null
      ? null
      : advanced({
          ...control,
          frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
        });
  }
  return action.kind === "finalizeFrame"
    ? stepFinalizeFrame({ control, action })
    : null;
};

export const advanceMidgardCekDataTraverse = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    return null;
  }
  try {
    switch (control.stage) {
      case MidgardCekDataTraverseStages.Head:
        return stepHead({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Integer:
        return stepInteger({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Bytes:
        return stepBytes({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.LargeConstructor:
        return stepLargeConstructor({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStages.LargeFields:
        return stepLargeFields({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStages.Close:
        return stepClose({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Fold:
        return stepFold({ control, sourceBytes, action });
      case MidgardCekDataTraverseStages.Terminal:
        return null;
    }
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataTraverse = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataSummary | null =>
  isWellFormedMidgardCekDataTraverseControl(control) &&
  control.stage === MidgardCekDataTraverseStages.Terminal
    ? control.result
    : null;
