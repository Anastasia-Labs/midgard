import {
  bindMidgardNativeScriptDecodingMachine,
  MidgardNativeScriptDecodingBindKinds,
  type MidgardNativeScriptDecodingBindResult,
  type MidgardNativeScriptDecodingRefusalClass,
  midgardNativeScriptDecodingSafeTokenRead,
  type MidgardNativeScriptDecodingScanOutcome,
  MidgardNativeScriptDecodingScanOutcomeKinds,
  type MidgardNativeScriptDecodingScanWindow,
  refusalClassOfResultKind,
} from "./native-script-decoding-engine.parse-midgard-versioned-script-header.js";
import {
  advanceMidgardNativeScriptStructureFrame,
  advanceMidgardNativeScriptStructureToken,
  finalizeMidgardNativeScriptStructure,
  isExactMidgardNativeScriptStructureTerminal,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  MidgardNativeScriptKinds,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureResultKinds,
  MidgardNativeScriptStructureStages,
  readMidgardNativeScriptStructureToken,
} from "./native-script-scan.js";

/// Twin of `engine.budgeted_scan_v1`: up to `maxSteps` primitive steps,
/// stopping (never refusing) at the terminal, on budget exhaustion, when the
/// frame witnesses run out, when there is no window on a token stage, or
/// when the safe-read margin blocks a token read. A frame witness that does
/// not hash-chain to the control's stack root throws — the on-chain fold
/// aborts the transaction there (witness error, never a verdict), so the
/// twin must never present it as an outcome.
export const budgetedMidgardNativeScriptDecodingScan = ({
  control,
  window,
  frames,
  maxSteps,
}: {
  readonly control: MidgardNativeScriptStructureControl;
  readonly window: MidgardNativeScriptDecodingScanWindow | null;
  readonly frames: readonly MidgardNativeScriptScanFrame[];
  readonly maxSteps: number;
}): MidgardNativeScriptDecodingScanOutcome => {
  let current = control;
  let frameIndex = 0;
  let remainingSteps = maxSteps;
  for (;;) {
    if (
      remainingSteps <= 0 ||
      current.stage === MidgardNativeScriptStructureStages.Terminal
    ) {
      return {
        kind: MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
        control: current,
        framesConsumed: frameIndex,
      };
    }
    let result;
    if (current.stage === MidgardNativeScriptStructureStages.Token) {
      if (window === null) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      const windowEnd = window.startOffset + window.bytes.length;
      if (
        !midgardNativeScriptDecodingSafeTokenRead({
          control: current,
          windowStart: window.startOffset,
          windowEnd,
        })
      ) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      result = advanceMidgardNativeScriptStructureToken({
        control: current,
        window: window.bytes,
        windowOffset: current.cursor - window.startOffset,
      });
      if (result === null) {
        throw new Error("V1 decoding scan token step rejected its control");
      }
    } else if (current.stage === MidgardNativeScriptStructureStages.Frame) {
      if (frameIndex >= frames.length) {
        return {
          kind: MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
          control: current,
          framesConsumed: frameIndex,
        };
      }
      result = advanceMidgardNativeScriptStructureFrame({
        control: current,
        frame: frames[frameIndex],
      });
      if (result === null) {
        throw new Error(
          "V1 decoding scan frame witness does not hash-chain to the stack root",
        );
      }
      frameIndex += 1;
    } else {
      result = finalizeMidgardNativeScriptStructure(current);
      if (result === null) {
        throw new Error("V1 decoding scan finalize rejected its control");
      }
    }
    if (result.kind !== MidgardNativeScriptStructureResultKinds.Advanced) {
      return {
        kind: MidgardNativeScriptDecodingScanOutcomeKinds.Refused,
        refusalClass: refusalClassOfResultKind(result.kind),
        framesConsumed: frameIndex,
      };
    }
    current = result.control;
    remainingSteps -= 1;
  }
};

export type MidgardNativeScriptDecodingTraceStep = {
  readonly control: MidgardNativeScriptStructureControl;
  readonly next: MidgardNativeScriptStructureControl;
  /// The frame witness this step consumed (frame-stage steps only).
  readonly frame: MidgardNativeScriptScanFrame | null;
};

export const MidgardNativeScriptDecodingTraceOutcomeKinds = Object.freeze({
  Terminal: "terminal",
  Refused: "refused",
} as const);

export type MidgardNativeScriptDecodingTraceOutcome =
  | {
      readonly kind: typeof MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal;
      readonly control: MidgardNativeScriptStructureControl;
    }
  | {
      readonly kind: typeof MidgardNativeScriptDecodingTraceOutcomeKinds.Refused;
      readonly refusalClass: MidgardNativeScriptDecodingRefusalClass;
      /// The control the refusing primitive step consumes: the last carried
      /// control of the fold, where the single-step Verdict fold exhibits
      /// the refusal.
      readonly control: MidgardNativeScriptStructureControl;
    };

export type MidgardNativeScriptDecodingTrace = {
  readonly bind: MidgardNativeScriptDecodingBindResult;
  /// The advanced primitive steps of the payload scan, in fold order; empty
  /// unless `bind.kind` is `"bound"`.
  readonly steps: readonly MidgardNativeScriptDecodingTraceStep[];
  /// `null` unless `bind.kind` is `"bound"`.
  readonly outcome: MidgardNativeScriptDecodingTraceOutcome | null;
};

const isContainerPush = ({
  before,
  after,
}: {
  readonly before: MidgardNativeScriptStructureControl;
  readonly after: MidgardNativeScriptStructureControl;
}): boolean => after.stackDepth === before.stackDepth + 1;

/// The refusal-capturing whole-item trace: binds the machine over the item
/// bytes and folds the payload scan to its end, capturing a refusal as an
/// outcome instead of throwing (unlike the frozen
/// `buildMidgardNativeScriptStructureTraceV1`, which only accepts canonical
/// scripts). Direction A plans stop one step before `outcome.control`;
/// direction B plans fold through to the exact terminal.
export const buildMidgardNativeScriptDecodingTrace = (
  itemBytes: Uint8Array,
): MidgardNativeScriptDecodingTrace => {
  const bytes = Buffer.from(itemBytes);
  const bind = bindMidgardNativeScriptDecodingMachine({
    firstChunk: bytes,
    totalLength: bytes.length,
  });
  if (bind.kind !== MidgardNativeScriptDecodingBindKinds.Bound) {
    return { bind, steps: [], outcome: null };
  }
  let control = bind.control;
  const frameStack: MidgardNativeScriptScanFrame[] = [];
  const steps: MidgardNativeScriptDecodingTraceStep[] = [];
  const maximumSteps = MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES * 3 + 1;
  while (control.stage !== MidgardNativeScriptStructureStages.Terminal) {
    if (steps.length >= maximumSteps) {
      throw new Error("V1 decoding trace exceeded the frozen machine's bounds");
    }
    let result;
    let frame: MidgardNativeScriptScanFrame | null = null;
    if (control.stage === MidgardNativeScriptStructureStages.Token) {
      result = advanceMidgardNativeScriptStructureToken({
        control,
        window: bytes,
        windowOffset: control.cursor,
      });
      if (result === null) {
        throw new Error("V1 decoding trace token step rejected its control");
      }
      if (
        result.kind === MidgardNativeScriptStructureResultKinds.Advanced &&
        isContainerPush({ before: control, after: result.control })
      ) {
        const token = readMidgardNativeScriptStructureToken({
          control,
          window: bytes,
          windowOffset: control.cursor,
        });
        frameStack.push({
          tail: control.stackRoot,
          kind: token.kind as MidgardNativeScriptScanFrame["kind"],
          childCount: token.childCount,
          remaining: token.childCount,
          validCount: 0,
          required:
            token.kind === MidgardNativeScriptKinds.AtLeast
              ? token.required
              : 0n,
        });
      }
    } else if (control.stage === MidgardNativeScriptStructureStages.Frame) {
      const top = frameStack.at(-1);
      if (top === undefined) {
        throw new Error("V1 decoding trace lost its stack frame");
      }
      frame = top;
      result = advanceMidgardNativeScriptStructureFrame({ control, frame });
      if (result === null) {
        throw new Error(
          "V1 decoding trace frame does not hash-chain to the stack root",
        );
      }
      if (frame.remaining === 1) {
        frameStack.pop();
      } else {
        frameStack[frameStack.length - 1] = {
          ...frame,
          remaining: frame.remaining - 1,
        };
      }
    } else {
      result = finalizeMidgardNativeScriptStructure(control);
      if (result === null) {
        throw new Error("V1 decoding trace finalize rejected its control");
      }
    }
    if (result.kind !== MidgardNativeScriptStructureResultKinds.Advanced) {
      return {
        bind,
        steps,
        outcome: {
          kind: MidgardNativeScriptDecodingTraceOutcomeKinds.Refused,
          refusalClass: refusalClassOfResultKind(result.kind),
          control,
        },
      };
    }
    steps.push({ control, next: result.control, frame });
    control = result.control;
  }
  if (
    frameStack.length !== 0 ||
    !isExactMidgardNativeScriptStructureTerminal(control)
  ) {
    throw new Error("V1 decoding trace did not terminate exactly");
  }
  return {
    bind,
    steps,
    outcome: {
      kind: MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal,
      control,
    },
  };
};
