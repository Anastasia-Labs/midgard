import { finalizeValidatedMidgardCekDataBytes } from "./cek-data-bytes.content-plan.js";
import { MidgardCekDataBytesStages } from "./cek-data-bytes.js";
import {
  appendMidgardCekDataFrameChild,
  finalizeMidgardCekDataFrame,
  hashMidgardCekDataFrame,
  type MidgardCekDataFrame,
  validateMidgardCekDataFrame,
} from "./cek-data-frame.js";
import {
  finalizeValidatedMidgardCekDataInteger,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import {
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  summaryIsWellFormed,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  stepHeadMap,
  stepHeadScalar,
  stepHeadSequence,
} from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import { nextValidatedMidgardCekDataTraverseSpan } from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";
import { advanceValidatedMidgardCekDataTraverse } from "./cek-data-traverse.step-fold.js";
import {
  stepClose,
  stepHeadLargeConstructor,
  stepLargeConstructor,
  stepLargeFields,
} from "./cek-data-traverse.step-large-constructor.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";

// Mirrors `cek_data_traverse_v1.unreadable_v1`. `null` is `NoAction`.

const windowIsExact = (
  control: MidgardCekDataTraverseControl,
  sourceBytes: Uint8Array | null | undefined,
): boolean => {
  const span = nextValidatedMidgardCekDataTraverseSpan(control);
  if (span === null) return sourceBytes === null || sourceBytes === undefined;
  return (
    sourceBytes !== null &&
    sourceBytes !== undefined &&
    sourceBytes.length === span.length
  );
};

const noHeadActionAdvances = (
  control: MidgardCekDataTraverseControl,
  bytes: Buffer,
): boolean =>
  stepHeadScalar({ control, bytes }) === null &&
  stepHeadSequence({ control, bytes }) === null &&
  stepHeadMap({ control, bytes }) === null &&
  stepHeadLargeConstructor({ control, bytes }) === null;

/**
 * A completed item whose authenticated parent cannot take it only because of
 * the source extent: the root item ends before the source end (trailing
 * bytes), or a parent that still needs a child or its break reaches it.
 */
/** Throws on a malformed frame witness; the caller maps that to `false`. */
const attachExtentUnreadable = ({
  currentFrameRoot,
  summary,
  parent,
  offset,
  sourceLength,
}: {
  readonly currentFrameRoot: Uint8Array;
  readonly summary: MidgardCekDataSummary;
  readonly parent: MidgardCekDataFrame | null;
  readonly offset: number;
  readonly sourceLength: number;
}): boolean => {
  if (!summaryIsWellFormed(summary)) return false;
  if (currentFrameRoot.length === 0) {
    return parent === null && offset < sourceLength;
  }
  if (parent === null) return false;
  validateMidgardCekDataFrame(parent);
  if (!hashMidgardCekDataFrame(parent).equals(Buffer.from(currentFrameRoot))) {
    return false;
  }
  const nextParent = appendMidgardCekDataFrameChild(parent, summary);
  if (nextParent === null) return false;
  const continues =
    nextParent.kind !== "map" ||
    nextParent.childCount < nextParent.expectedChildren;
  return continues && offset >= sourceLength;
};

type ScalarPhase =
  | { readonly kind: "grammar" }
  | { readonly kind: "content" }
  | {
      readonly kind: "done";
      readonly summary: MidgardCekDataSummary | null;
      readonly offset: number;
    };

const scalarPhase = (control: MidgardCekDataTraverseControl): ScalarPhase => {
  if (control.stage === MidgardCekDataTraverseStages.Integer) {
    const integer = control.integer!;
    if (integer.stage === MidgardCekDataIntegerStages.Terminal) {
      return {
        kind: "done",
        summary: finalizeValidatedMidgardCekDataInteger(integer),
        offset: control.offset + integer.sourceLength,
      };
    }
    return integer.stage === MidgardCekDataIntegerStages.Blob
      ? { kind: "content" }
      : { kind: "grammar" };
  }
  const bytes = control.bytes!;
  if (bytes.stage === MidgardCekDataBytesStages.Terminal) {
    return {
      kind: "done",
      summary: finalizeValidatedMidgardCekDataBytes(bytes),
      offset: control.offset + bytes.sourceLength,
    };
  }
  return bytes.stage === MidgardCekDataBytesStages.Blob
    ? { kind: "content" }
    : { kind: "grammar" };
};

const unreadableValidated = (
  control: MidgardCekDataTraverseControl,
  sourceBytes: Uint8Array | null | undefined,
  action: MidgardCekDataTraverseAction,
): boolean => {
  switch (control.stage) {
    case MidgardCekDataTraverseStages.Fold: {
      if (
        action === null ||
        action.kind !== "finalizeFrame" ||
        (sourceBytes !== null && sourceBytes !== undefined)
      ) {
        return false;
      }
      validateMidgardCekDataFrame(action.frame);
      if (!hashMidgardCekDataFrame(action.frame).equals(control.frameRoot)) {
        return false;
      }
      const summary = finalizeMidgardCekDataFrame(action.frame);
      return (
        summary !== null &&
        attachExtentUnreadable({
          currentFrameRoot: action.frame.tail,
          summary,
          parent: action.parent,
          offset: control.offset,
          sourceLength: control.sourceLength,
        })
      );
    }
    case MidgardCekDataTraverseStages.Integer:
    case MidgardCekDataTraverseStages.Bytes: {
      const phase = scalarPhase(control);
      if (phase.kind === "content") return false;
      if (phase.kind === "grammar") {
        return (
          action === null &&
          windowIsExact(control, sourceBytes) &&
          advanceValidatedMidgardCekDataTraverse({
            control,
            sourceBytes,
            action: null,
          }) === null
        );
      }
      return (
        phase.summary !== null &&
        action !== null &&
        action.kind === "attachScalar" &&
        (sourceBytes === null || sourceBytes === undefined) &&
        attachExtentUnreadable({
          currentFrameRoot: control.frameRoot,
          summary: phase.summary,
          parent: action.parent,
          offset: phase.offset,
          sourceLength: control.sourceLength,
        })
      );
    }
    case MidgardCekDataTraverseStages.Terminal:
      return false;
    case MidgardCekDataTraverseStages.Head:
    case MidgardCekDataTraverseStages.LargeConstructor:
    case MidgardCekDataTraverseStages.LargeFields:
    case MidgardCekDataTraverseStages.Close:
      return readUnreadable(control, sourceBytes, action);
  }
};

/** The byte-reading stages outside a scalar: `NoAction` and the exact window. */
const readUnreadable = (
  control: MidgardCekDataTraverseControl,
  sourceBytes: Uint8Array | null | undefined,
  action: MidgardCekDataTraverseAction,
): boolean => {
  if (action !== null || !windowIsExact(control, sourceBytes)) return false;
  switch (control.stage) {
    case MidgardCekDataTraverseStages.LargeConstructor:
      return (
        control.integer!.stage === MidgardCekDataIntegerStages.Syntax &&
        stepLargeConstructor({ control, sourceBytes, action: null }) === null
      );
    case MidgardCekDataTraverseStages.LargeFields:
      return stepLargeFields({ control, sourceBytes, action: null }) === null;
    case MidgardCekDataTraverseStages.Head:
      return noHeadActionAdvances(control, Buffer.from(sourceBytes!));
    case MidgardCekDataTraverseStages.Close:
      return (
        stepClose({ control, sourceBytes, action: null }) === null &&
        noHeadActionAdvances(control, Buffer.from(sourceBytes!))
      );
    case MidgardCekDataTraverseStages.Integer:
    case MidgardCekDataTraverseStages.Bytes:
    case MidgardCekDataTraverseStages.Fold:
    case MidgardCekDataTraverseStages.Terminal:
      return false;
  }
};

/**
 * `Unreadable`: the source span is not canonical Data, decided at the step
 * that reads the offending byte. Exclusive with
 * `advanceMidgardCekDataTraverse`: it holds only where no admissible action of
 * the stage advances. A byte-reading stage claims it with no action and the
 * exact derived window; an attach (`attachScalar`) or frame finalization
 * (`finalizeFrame`) with the same authenticated frame witness as its advancing
 * action, when the source extent alone refuses it.
 */
export const isUnreadableMidgardCekDataTraverse = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): boolean => {
  if (!isWellFormedMidgardCekDataTraverseControl(control)) return false;
  try {
    return unreadableValidated(control, sourceBytes, action);
  } catch {
    return false;
  }
};
