import { blake2b } from "@noble/hashes/blake2.js";

import {
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";
import {
  FRAME_DOMAIN,
  isMidgardNativeScriptContainerKind,
  isWellFormedMidgardNativeScriptStructureControl,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  type MidgardNativeScriptKind,
  MidgardNativeScriptKinds,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureResultKinds,
  MidgardNativeScriptStructureStages,
  type MidgardNativeScriptStructureStepResult,
  type MidgardNativeScriptToken,
} from "./native-script-scan.is-well-formed-midgard-native-script-structure-control.js";

export const midgardNativeScriptScanFrameIsWellFormed = (
  frame: MidgardNativeScriptScanFrame,
): boolean => {
  const processed = frame.childCount - frame.remaining;
  return (
    (frame.tail.length === 0 || frame.tail.length === 32) &&
    Number.isSafeInteger(frame.kind) &&
    frame.kind >= MidgardNativeScriptKinds.All &&
    frame.kind <= MidgardNativeScriptKinds.AtLeast &&
    Number.isSafeInteger(frame.childCount) &&
    frame.childCount > 0 &&
    frame.childCount <= MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES &&
    Number.isSafeInteger(frame.remaining) &&
    frame.remaining > 0 &&
    frame.remaining <= frame.childCount &&
    Number.isSafeInteger(frame.validCount) &&
    frame.validCount >= 0 &&
    frame.validCount <= processed &&
    (frame.kind === MidgardNativeScriptKinds.AtLeast
      ? frame.required >= 0n
      : frame.required === 0n)
  );
};

export const hashMidgardNativeScriptScanFrame = (
  frame: MidgardNativeScriptScanFrame,
): Buffer => {
  if (!midgardNativeScriptScanFrameIsWellFormed(frame)) {
    throw new Error("Invalid V1 native-script scan frame");
  }
  return Buffer.from(
    blake2b(
      Buffer.concat([
        FRAME_DOMAIN,
        encodeCbor([
          frame.tail,
          BigInt(frame.kind),
          BigInt(frame.childCount),
          BigInt(frame.remaining),
          BigInt(frame.validCount),
          frame.required,
        ]),
      ]),
      { dkLen: 32 },
    ),
  );
};

const absoluteOffset = ({
  cursor,
  windowOffset,
  localOffset,
}: {
  readonly cursor: number;
  readonly windowOffset: number;
  readonly localOffset: number;
}): number => cursor + localOffset - windowOffset;

export const readToken = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardNativeScriptStructureControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardNativeScriptToken => {
  const outer = readCborArrayHeader(window, windowOffset, "native_script");
  const tag = readCborUnsigned(window, outer.nextOffset, "native_script.tag");
  if (tag.value > BigInt(MidgardNativeScriptKinds.Before)) {
    throw new Error("Unsupported V1 native-script tag");
  }
  const kind = Number(tag.value) as MidgardNativeScriptKind;
  if (
    (kind === MidgardNativeScriptKinds.AtLeast && outer.length !== 3) ||
    (kind !== MidgardNativeScriptKinds.AtLeast && outer.length !== 2)
  ) {
    throw new Error("Invalid V1 native-script outer shape");
  }
  let nextOffset = tag.nextOffset;
  let childCount = 0;
  let required = 0n;
  if (kind === MidgardNativeScriptKinds.Signature) {
    const keyHash = readCborBytes(window, nextOffset, "native_script.key_hash");
    if (keyHash.value.length !== 28) {
      throw new Error("Invalid V1 native signature key hash");
    }
    nextOffset = keyHash.nextOffset;
  } else if (
    kind === MidgardNativeScriptKinds.All ||
    kind === MidgardNativeScriptKinds.Any
  ) {
    const children = readCborArrayHeader(
      window,
      nextOffset,
      "native_script.children",
    );
    childCount = children.length;
    nextOffset = children.nextOffset;
  } else if (kind === MidgardNativeScriptKinds.AtLeast) {
    const threshold = readCborUnsigned(
      window,
      nextOffset,
      "native_script.required",
    );
    const children = readCborArrayHeader(
      window,
      threshold.nextOffset,
      "native_script.children",
    );
    required = threshold.value;
    childCount = children.length;
    nextOffset = children.nextOffset;
  } else {
    const slot = readCborUnsigned(window, nextOffset, "native_script.slot");
    nextOffset = slot.nextOffset;
  }
  return {
    kind,
    nextOffset: absoluteOffset({
      cursor: control.cursor,
      windowOffset,
      localOffset: nextOffset,
    }),
    childCount,
    required,
  };
};

// The exact parser the token step consumes, exported so the decoding-fault
// engine twin can reconstruct pushed frames without a second CBOR parser.
export const readMidgardNativeScriptStructureToken = readToken;

const advanced = (
  control: MidgardNativeScriptStructureControl,
): MidgardNativeScriptStructureStepResult =>
  isWellFormedMidgardNativeScriptStructureControl(control)
    ? {
        kind: MidgardNativeScriptStructureResultKinds.Advanced,
        control,
      }
    : { kind: MidgardNativeScriptStructureResultKinds.Invalid };

export const advanceMidgardNativeScriptStructureToken = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardNativeScriptStructureControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardNativeScriptStructureStepResult | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControl(control) ||
    control.stage !== MidgardNativeScriptStructureStages.Token ||
    !Number.isSafeInteger(windowOffset) ||
    windowOffset < 0 ||
    windowOffset >= window.length
  ) {
    return null;
  }
  try {
    const token = readToken({ control, window, windowOffset });
    if (
      token.nextOffset <= control.cursor ||
      token.nextOffset > control.endOffset
    ) {
      return { kind: MidgardNativeScriptStructureResultKinds.Invalid };
    }
    const nodeCount = control.nodeCount + 1;
    if (nodeCount > MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES) {
      return { kind: MidgardNativeScriptStructureResultKinds.NodeLimit };
    }
    if (
      isMidgardNativeScriptContainerKind(token.kind) &&
      token.childCount > 0
    ) {
      const stackDepth = control.stackDepth + 1;
      if (stackDepth > MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH) {
        return {
          kind: MidgardNativeScriptStructureResultKinds.DepthLimit,
        };
      }
      const frame = {
        tail: control.stackRoot,
        kind: token.kind,
        childCount: token.childCount,
        remaining: token.childCount,
        validCount: 0,
        required: token.required,
      } satisfies MidgardNativeScriptScanFrame;
      return advanced({
        ...control,
        cursor: token.nextOffset,
        stackRoot: hashMidgardNativeScriptScanFrame(frame),
        stackDepth,
        nodeCount,
      });
    }
    return advanced({
      ...control,
      stage:
        control.stackDepth > 0
          ? MidgardNativeScriptStructureStages.Frame
          : MidgardNativeScriptStructureStages.Finalize,
      cursor: token.nextOffset,
      nodeCount,
    });
  } catch {
    return { kind: MidgardNativeScriptStructureResultKinds.Invalid };
  }
};

export const advanceMidgardNativeScriptStructureFrame = ({
  control,
  frame,
}: {
  readonly control: MidgardNativeScriptStructureControl;
  readonly frame: MidgardNativeScriptScanFrame;
}): MidgardNativeScriptStructureStepResult | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControl(control) ||
    control.stage !== MidgardNativeScriptStructureStages.Frame ||
    !midgardNativeScriptScanFrameIsWellFormed(frame)
  ) {
    return null;
  }
  try {
    if (!hashMidgardNativeScriptScanFrame(frame).equals(control.stackRoot)) {
      return null;
    }
    if (frame.remaining === 1) {
      const stackDepth = control.stackDepth - 1;
      return advanced({
        ...control,
        stage:
          stackDepth > 0
            ? MidgardNativeScriptStructureStages.Frame
            : MidgardNativeScriptStructureStages.Finalize,
        stackRoot: frame.tail,
        stackDepth,
      });
    }
    const nextFrame = {
      ...frame,
      remaining: frame.remaining - 1,
    } satisfies MidgardNativeScriptScanFrame;
    return advanced({
      ...control,
      stage: MidgardNativeScriptStructureStages.Token,
      stackRoot: hashMidgardNativeScriptScanFrame(nextFrame),
    });
  } catch {
    return null;
  }
};

export const finalizeMidgardNativeScriptStructure = (
  control: MidgardNativeScriptStructureControl,
): MidgardNativeScriptStructureStepResult | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControl(control) ||
    control.stage !== MidgardNativeScriptStructureStages.Finalize
  ) {
    return null;
  }
  return control.cursor === control.endOffset && control.nodeCount > 0
    ? advanced({
        ...control,
        stage: MidgardNativeScriptStructureStages.Terminal,
      })
    : { kind: MidgardNativeScriptStructureResultKinds.Invalid };
};

export const isExactMidgardNativeScriptStructureTerminal = (
  control: MidgardNativeScriptStructureControl,
): boolean =>
  isWellFormedMidgardNativeScriptStructureControl(control) &&
  control.stage === MidgardNativeScriptStructureStages.Terminal;
