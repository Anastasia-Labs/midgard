import { finalizeValidatedMidgardCekDataBytes } from "./cek-data-bytes.content-plan.js";
import { MidgardCekDataBytesStages } from "./cek-data-bytes.js";
import {
  appendMidgardCekDataFrameChild,
  finalizeMidgardCekDataFrame,
  foldMidgardCekDataFrameListChild,
  foldMidgardCekDataFrameMapPair,
  hashMidgardCekDataFrame,
  hashMidgardCekDataFrameChild,
  initialMidgardCekDataLargeConstrFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataMapFrame,
  initialMidgardCekDataSmallConstrFrame,
  type MidgardCekDataFrame,
} from "./cek-data-frame.js";
import {
  finalizeValidatedMidgardCekDataInteger,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import {
  initialMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  type MidgardCekDataTraverseStage,
  MidgardCekDataTraverseStages,
  type MidgardCekDataTraverseTraceStep,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  nextValidatedMidgardCekDataTraverseSpan,
  parseSmallConstructorHead,
  readCanonicalCborArgument,
} from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";
import {
  advanceValidatedMidgardCekDataTraverse,
  finalizeMidgardCekDataTraverse,
} from "./cek-data-traverse.step-fold.js";
import { isUnreadableMidgardCekDataTraverse } from "./cek-data-traverse.unreadable.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import { finalizeValidatedMidgardCekSourceBlob } from "./cek-source-blob.js";
import { buildMidgardValidationMerkleMembershipIndex } from "./validation-merkle.js";

// The honest prover over `cek_data_traverse_v1`: every step, then either the
// terminal summary or the step `Unreadable` holds at.

type OpenFrame = {
  frame: MidgardCekDataFrame;
  readonly children: MidgardCekDataSummary[];
  /** Built once, when the frame starts folding (its children are final). */
  membership?: ReturnType<typeof buildMidgardValidationMerkleMembershipIndex>;
};

export type MidgardCekDataTraverseOutcome =
  | {
      readonly verdict: "accepted";
      readonly summary: MidgardCekDataSummary;
      readonly steps: readonly MidgardCekDataTraverseTraceStep[];
    }
  | {
      readonly verdict: "unreadable";
      readonly stage: MidgardCekDataTraverseStage;
      /** The advancing steps before the refusal. */
      readonly steps: readonly MidgardCekDataTraverseTraceStep[];
      /** The control, exact window and action `Unreadable` holds for. */
      readonly refusal: {
        readonly control: MidgardCekDataTraverseControl;
        readonly sourceBytes: Buffer | null;
        readonly action: MidgardCekDataTraverseAction;
      };
    };

const HEAD_ACTIONS: readonly MidgardCekDataTraverseAction[] = [
  { kind: "headScalar" },
  { kind: "headSequence" },
  { kind: "headMap" },
  { kind: "headLargeConstructor" },
];

const foldAction = (stack: OpenFrame[]): MidgardCekDataTraverseAction => {
  const top = stack[stack.length - 1]!;
  const parent = stack.length > 1 ? stack[stack.length - 2]!.frame : null;
  const { frame, children } = top;
  const memberships = () =>
    (top.membership ??= buildMidgardValidationMerkleMembershipIndex(
      children.map((child, index) =>
        hashMidgardCekDataFrameChild(index, child),
      ),
    ));
  if (frame.kind === "map") {
    const pairs = frame.expectedChildren / 2;
    if (frame.foldCursor < pairs) {
      const pairIndex = pairs - frame.foldCursor - 1;
      const index = memberships();
      return {
        kind: "foldMap",
        frame,
        pairIndex,
        key: children[pairIndex * 2]!,
        value: children[pairIndex * 2 + 1]!,
        keySiblings: index.membershipAt(pairIndex * 2).siblings,
        valueSiblings: index.membershipAt(pairIndex * 2 + 1).siblings,
      };
    }
  } else if (frame.foldCursor < frame.childCount) {
    const childIndex = frame.childCount - frame.foldCursor - 1;
    return {
      kind: "foldList",
      frame,
      childIndex,
      child: children[childIndex]!,
      siblings: memberships().membershipAt(childIndex).siblings,
    };
  }
  return { kind: "finalizeFrame", frame, parent };
};

const scalarIsComplete = (control: MidgardCekDataTraverseControl): boolean =>
  (control.stage === MidgardCekDataTraverseStages.Integer &&
    control.integer!.stage === MidgardCekDataIntegerStages.Terminal) ||
  (control.stage === MidgardCekDataTraverseStages.Bytes &&
    control.bytes!.stage === MidgardCekDataBytesStages.Terminal);

const offeredActions = (
  control: MidgardCekDataTraverseControl,
  stack: OpenFrame[],
): readonly MidgardCekDataTraverseAction[] => {
  switch (control.stage) {
    case MidgardCekDataTraverseStages.Head:
      return HEAD_ACTIONS;
    case MidgardCekDataTraverseStages.Close:
      return [null, ...HEAD_ACTIONS];
    case MidgardCekDataTraverseStages.Fold:
      return [foldAction(stack)];
    case MidgardCekDataTraverseStages.Integer:
    case MidgardCekDataTraverseStages.Bytes:
    case MidgardCekDataTraverseStages.LargeConstructor:
    case MidgardCekDataTraverseStages.LargeFields:
    case MidgardCekDataTraverseStages.Terminal:
      return scalarIsComplete(control)
        ? [
            {
              kind: "attachScalar",
              parent:
                stack.length === 0 ? null : stack[stack.length - 1]!.frame,
            },
          ]
        : [null];
  }
};

const attachChild = (stack: OpenFrame[], summary: MidgardCekDataSummary) => {
  const top = stack[stack.length - 1];
  if (top === undefined) return;
  const frame = appendMidgardCekDataFrameChild(top.frame, summary);
  if (frame === null) throw new Error("CEK Data driver lost a child");
  top.frame = frame;
  top.children.push(summary);
};

/** The frame a sequence or map head opened, rebuilt from the window it read. */
const openedFrame = (
  control: MidgardCekDataTraverseControl,
  bytes: Buffer,
  action: MidgardCekDataTraverseAction,
): MidgardCekDataFrame | null => {
  const tail = control.frameRoot;
  if (action?.kind === "headMap") {
    const argument = readCanonicalCborArgument(bytes, 0)!;
    return initialMidgardCekDataMapFrame({
      tail,
      expectedChildren: argument.value * 2,
    });
  }
  if (action?.kind === "headSequence") {
    const small = parseSmallConstructorHead(bytes);
    return small === null
      ? initialMidgardCekDataListFrame({ tail })
      : initialMidgardCekDataSmallConstrFrame({
          constructor: small.constructor,
          tail,
        });
  }
  if (
    action === null &&
    control.stage === MidgardCekDataTraverseStages.LargeFields
  ) {
    const integer = control.integer!;
    return initialMidgardCekDataLargeConstrFrame({
      constructorCborRoot: finalizeValidatedMidgardCekSourceBlob(
        integer.blob!,
      )!,
      constructorCborLength: BigInt(integer.sourceLength),
      constructorMemory: integer.memory,
      tail,
    });
  }
  return null;
};

const followStack = (
  stack: OpenFrame[],
  control: MidgardCekDataTraverseControl,
  bytes: Buffer | null,
  action: MidgardCekDataTraverseAction,
): void => {
  const opened = openedFrame(control, bytes ?? Buffer.alloc(0), action);
  if (opened !== null) {
    stack.push({ frame: opened, children: [] });
    return;
  }
  if (action === null) return;
  switch (action.kind) {
    case "attachScalar":
      attachChild(
        stack,
        control.stage === MidgardCekDataTraverseStages.Integer
          ? finalizeValidatedMidgardCekDataInteger(control.integer!)!
          : finalizeValidatedMidgardCekDataBytes(control.bytes!)!,
      );
      return;
    case "foldList": {
      const frame = foldMidgardCekDataFrameListChild(action);
      if (frame === null) throw new Error("CEK Data driver lost a fold");
      stack[stack.length - 1]!.frame = frame;
      return;
    }
    case "foldMap": {
      const frame = foldMidgardCekDataFrameMapPair(action);
      if (frame === null) throw new Error("CEK Data driver lost a fold");
      stack[stack.length - 1]!.frame = frame;
      return;
    }
    case "finalizeFrame": {
      stack.pop();
      const summary = finalizeMidgardCekDataFrame(action.frame);
      if (summary === null) throw new Error("CEK Data driver lost a frame");
      attachChild(stack, summary);
      return;
    }
    case "headScalar":
    case "headSequence":
    case "headMap":
    case "headLargeConstructor":
      return;
  }
};

/**
 * Drives the streamed traverse over `source` the way an honest prover does
 * and returns its verdict: `accepted` with the Data summary when the span is
 * canonical Data, otherwise `unreadable` at the step that reads the offending
 * byte, with the witness that step's `Unreadable` claim carries. The traverse
 * admits exactly one of the two for every span; this function throws only if
 * that invariant is broken.
 */
export const driveMidgardCekDataTraverse = ({
  sourceStart = 0,
  source,
}: {
  readonly sourceStart?: number;
  readonly source: Uint8Array;
}): MidgardCekDataTraverseOutcome => {
  const bytes = Buffer.from(source);
  let control = initialMidgardCekDataTraverseControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const stack: OpenFrame[] = [];
  const steps: MidgardCekDataTraverseTraceStep[] = [];
  for (;;) {
    if (control.stage === MidgardCekDataTraverseStages.Terminal) {
      const summary = finalizeMidgardCekDataTraverse(control);
      if (summary === null || stack.length !== 0) {
        throw new Error("CEK Data driver reached a malformed terminal");
      }
      return Object.freeze({
        verdict: "accepted",
        summary,
        steps: Object.freeze(steps),
      });
    }
    const span = nextValidatedMidgardCekDataTraverseSpan(control);
    const window =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const actions = offeredActions(control, stack);
    const current = control;
    const successors = actions.flatMap((action) => {
      const next = advanceValidatedMidgardCekDataTraverse({
        control: current,
        sourceBytes: window,
        action,
      });
      return next === null ? [] : [{ action, next }];
    });
    const claim = [null, ...actions].find((action) =>
      isUnreadableMidgardCekDataTraverse({
        control: current,
        sourceBytes: window,
        action,
      }),
    );
    if (successors.length === 0) {
      if (claim === undefined) {
        throw new Error(
          `CEK Data traverse has no successor and no refusal at stage ${String(control.stage)}`,
        );
      }
      return Object.freeze({
        verdict: "unreadable",
        stage: control.stage,
        steps: Object.freeze(steps),
        refusal: Object.freeze({
          control,
          sourceBytes: window === null ? null : Buffer.from(window),
          action: claim,
        }),
      });
    }
    if (successors.length > 1 || claim !== undefined) {
      throw new Error(
        `CEK Data traverse has more than one outcome at stage ${String(control.stage)}`,
      );
    }
    const { action, next } = successors[0]!;
    followStack(stack, control, window, action);
    const top = stack[stack.length - 1];
    if (
      next.stage !== MidgardCekDataTraverseStages.Terminal &&
      !(top === undefined
        ? next.frameRoot.length === 0
        : hashMidgardCekDataFrame(top.frame).equals(next.frameRoot))
    ) {
      throw new Error("CEK Data driver lost track of the open frames");
    }
    steps.push({
      control,
      sourceBytes: window === null ? null : Buffer.from(window),
      action,
      next,
    });
    control = next;
  }
};

/**
 * Whether `source` is canonical Data (in the image of `serialiseData`), as
 * the on-chain streamed traverse decides it.
 */
export const isCanonicalMidgardCekData = (source: Uint8Array): boolean =>
  source.length > 0 &&
  driveMidgardCekDataTraverse({ source }).verdict === "accepted";
