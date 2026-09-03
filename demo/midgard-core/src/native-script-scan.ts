import { blake2b } from "@noble/hashes/blake2.js";

import {
  decodeSingleCbor,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";

export const MIDGARD_NATIVE_SCRIPT_SCAN_VERSION = 1 as const;
export const MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES = 16_384 as const;
export const MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH = 16_384 as const;

export const MidgardNativeScriptKinds = Object.freeze({
  Signature: 0,
  All: 1,
  Any: 2,
  AtLeast: 3,
  After: 4,
  Before: 5,
} as const);

export type MidgardNativeScriptKind =
  (typeof MidgardNativeScriptKinds)[keyof typeof MidgardNativeScriptKinds];

type MidgardNativeScriptContainerKind =
  | typeof MidgardNativeScriptKinds.All
  | typeof MidgardNativeScriptKinds.Any
  | typeof MidgardNativeScriptKinds.AtLeast;

const isMidgardNativeScriptContainerKind = (
  kind: MidgardNativeScriptKind,
): kind is MidgardNativeScriptContainerKind =>
  kind === MidgardNativeScriptKinds.All ||
  kind === MidgardNativeScriptKinds.Any ||
  kind === MidgardNativeScriptKinds.AtLeast;

export const MidgardNativeScriptStructureStages = Object.freeze({
  Token: 0,
  Frame: 1,
  Finalize: 2,
  Terminal: 3,
} as const);

export type MidgardNativeScriptStructureStage =
  (typeof MidgardNativeScriptStructureStages)[keyof typeof MidgardNativeScriptStructureStages];

export const MidgardNativeScriptStructureResultKinds = Object.freeze({
  Advanced: "advanced",
  Invalid: "invalid",
  NodeLimit: "nodeLimit",
  DepthLimit: "depthLimit",
} as const);

export type MidgardNativeScriptStructureControl = {
  readonly version: typeof MIDGARD_NATIVE_SCRIPT_SCAN_VERSION;
  readonly stage: MidgardNativeScriptStructureStage;
  readonly startOffset: number;
  readonly cursor: number;
  readonly endOffset: number;
  readonly stackRoot: Buffer;
  readonly stackDepth: number;
  readonly nodeCount: number;
};

export type MidgardNativeScriptScanFrame = {
  readonly tail: Buffer;
  readonly kind: MidgardNativeScriptContainerKind;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

export type MidgardNativeScriptToken = {
  readonly kind: MidgardNativeScriptKind;
  readonly nextOffset: number;
  readonly childCount: number;
  readonly required: bigint;
};

export type MidgardNativeScriptStructureStepResult =
  | {
      readonly kind: typeof MidgardNativeScriptStructureResultKinds.Advanced;
      readonly control: MidgardNativeScriptStructureControl;
    }
  | {
      readonly kind:
        | typeof MidgardNativeScriptStructureResultKinds.Invalid
        | typeof MidgardNativeScriptStructureResultKinds.NodeLimit
        | typeof MidgardNativeScriptStructureResultKinds.DepthLimit;
    };

export type MidgardNativeScriptStructureTraceStep = {
  readonly control: MidgardNativeScriptStructureControl;
  readonly next: MidgardNativeScriptStructureControl;
  readonly frame: MidgardNativeScriptScanFrame | null;
};

const FRAME_DOMAIN = Buffer.from("MidgardNativeScriptScanFrameV1", "ascii");

const exactSafeInteger = ({
  value,
  field,
  minimum,
  maximum = Number.MAX_SAFE_INTEGER,
}: {
  readonly value: number;
  readonly field: string;
  readonly minimum: number;
  readonly maximum?: number;
}): number => {
  if (!Number.isSafeInteger(value) || value < minimum || value > maximum) {
    throw new Error(`Invalid V1 native-script scan ${field}`);
  }
  return value;
};

const decodedSafeInteger = (value: unknown, field: string): number => {
  if (typeof value === "number" && Number.isSafeInteger(value)) {
    return value;
  }
  if (
    typeof value === "bigint" &&
    value >= BigInt(Number.MIN_SAFE_INTEGER) &&
    value <= BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return Number(value);
  }
  throw new Error(`Invalid V1 native-script scan ${field}`);
};

export const isWellFormedMidgardNativeScriptStructureControl = (
  control: MidgardNativeScriptStructureControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_NATIVE_SCRIPT_SCAN_VERSION ||
      exactSafeInteger({
        value: control.stage,
        field: "stage",
        minimum: MidgardNativeScriptStructureStages.Token,
        maximum: MidgardNativeScriptStructureStages.Terminal,
      }) !== control.stage
    ) {
      return false;
    }
    exactSafeInteger({
      value: control.startOffset,
      field: "start offset",
      minimum: 0,
    });
    exactSafeInteger({
      value: control.cursor,
      field: "cursor",
      minimum: control.startOffset,
      maximum: control.endOffset,
    });
    exactSafeInteger({
      value: control.endOffset,
      field: "end offset",
      minimum: control.startOffset + 1,
    });
    exactSafeInteger({
      value: control.stackDepth,
      field: "stack depth",
      minimum: 0,
      maximum: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
    });
    exactSafeInteger({
      value: control.nodeCount,
      field: "node count",
      minimum: 0,
      maximum: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
    });
    const emptyStack = control.stackRoot.length === 0;
    const committedStack = control.stackRoot.length === 32;
    if (
      (control.stackDepth === 0 && !emptyStack) ||
      (control.stackDepth > 0 && !committedStack)
    ) {
      return false;
    }
    if (control.stage === MidgardNativeScriptStructureStages.Token) {
      return control.cursor < control.endOffset;
    }
    if (control.stage === MidgardNativeScriptStructureStages.Frame) {
      return control.stackDepth > 0 && committedStack;
    }
    if (control.stage === MidgardNativeScriptStructureStages.Finalize) {
      return control.stackDepth === 0 && emptyStack;
    }
    return (
      control.cursor === control.endOffset &&
      control.stackDepth === 0 &&
      emptyStack &&
      control.nodeCount > 0
    );
  } catch {
    return false;
  }
};

export const initialMidgardNativeScriptStructureControl = ({
  startOffset,
  totalLength,
}: {
  readonly startOffset: number;
  readonly totalLength: number;
}): MidgardNativeScriptStructureControl => {
  exactSafeInteger({
    value: totalLength,
    field: "total length",
    minimum: 1,
  });
  const control = {
    version: MIDGARD_NATIVE_SCRIPT_SCAN_VERSION,
    stage: MidgardNativeScriptStructureStages.Token,
    startOffset,
    cursor: startOffset,
    endOffset: startOffset + totalLength,
    stackRoot: Buffer.alloc(0),
    stackDepth: 0,
    nodeCount: 0,
  } satisfies MidgardNativeScriptStructureControl;
  if (!isWellFormedMidgardNativeScriptStructureControl(control)) {
    throw new Error("Invalid V1 native-script scan span");
  }
  return control;
};

export const encodeMidgardNativeScriptStructureControl = (
  control: MidgardNativeScriptStructureControl,
): Buffer => {
  if (!isWellFormedMidgardNativeScriptStructureControl(control)) {
    throw new Error("Invalid V1 native-script structure control");
  }
  return encodeCbor([
    BigInt(MIDGARD_NATIVE_SCRIPT_SCAN_VERSION),
    BigInt(control.stage),
    BigInt(control.startOffset),
    BigInt(control.cursor),
    BigInt(control.endOffset),
    control.stackRoot,
    BigInt(control.stackDepth),
    BigInt(control.nodeCount),
  ]);
};

export const decodeMidgardNativeScriptStructureControl = (
  controlCbor: Uint8Array,
): MidgardNativeScriptStructureControl => {
  const value = decodeSingleCbor(controlCbor);
  if (
    !Array.isArray(value) ||
    value.length !== 8 ||
    !(value[5] instanceof Uint8Array)
  ) {
    throw new Error("Invalid V1 native-script structure control");
  }
  const control = {
    version: decodedSafeInteger(
      value[0],
      "version",
    ) as typeof MIDGARD_NATIVE_SCRIPT_SCAN_VERSION,
    stage: decodedSafeInteger(
      value[1],
      "stage",
    ) as MidgardNativeScriptStructureStage,
    startOffset: decodedSafeInteger(value[2], "start offset"),
    cursor: decodedSafeInteger(value[3], "cursor"),
    endOffset: decodedSafeInteger(value[4], "end offset"),
    stackRoot: Buffer.from(value[5]),
    stackDepth: decodedSafeInteger(value[6], "stack depth"),
    nodeCount: decodedSafeInteger(value[7], "node count"),
  } satisfies MidgardNativeScriptStructureControl;
  if (
    !isWellFormedMidgardNativeScriptStructureControl(control) ||
    !encodeMidgardNativeScriptStructureControl(control).equals(
      Buffer.from(controlCbor),
    )
  ) {
    throw new Error("Non-canonical V1 native-script structure control");
  }
  return control;
};

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

const readToken = ({
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

export const buildMidgardNativeScriptStructureTrace = (
  scriptBytes: Uint8Array,
  startOffset = 0,
): readonly MidgardNativeScriptStructureTraceStep[] => {
  const bytes = Buffer.from(scriptBytes);
  let control = initialMidgardNativeScriptStructureControl({
    startOffset,
    totalLength: bytes.length,
  });
  const frames: MidgardNativeScriptScanFrame[] = [];
  const steps: MidgardNativeScriptStructureTraceStep[] = [];
  const maximumSteps = MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES * 2 + 1;
  while (
    control.stage !== MidgardNativeScriptStructureStages.Terminal &&
    steps.length < maximumSteps
  ) {
    let result: MidgardNativeScriptStructureStepResult | null;
    let frame: MidgardNativeScriptScanFrame | null = null;
    if (control.stage === MidgardNativeScriptStructureStages.Token) {
      const token = readToken({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      result = advanceMidgardNativeScriptStructureToken({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      if (
        isMidgardNativeScriptContainerKind(token.kind) &&
        token.childCount > 0
      ) {
        frames.push({
          tail: control.stackRoot,
          kind: token.kind,
          childCount: token.childCount,
          remaining: token.childCount,
          validCount: 0,
          required: token.required,
        });
      }
    } else if (control.stage === MidgardNativeScriptStructureStages.Frame) {
      frame = frames.at(-1) ?? null;
      if (frame === null) {
        throw new Error("V1 native-script trace lost its stack frame");
      }
      result = advanceMidgardNativeScriptStructureFrame({
        control,
        frame,
      });
      if (frame.remaining === 1) {
        frames.pop();
      } else {
        frames[frames.length - 1] = {
          ...frame,
          remaining: frame.remaining - 1,
        };
      }
    } else {
      result = finalizeMidgardNativeScriptStructure(control);
    }
    if (
      result === null ||
      result.kind !== MidgardNativeScriptStructureResultKinds.Advanced
    ) {
      throw new Error(
        `Canonical V1 native-script scan failed: ${result?.kind ?? "malformed"}`,
      );
    }
    steps.push({ control, next: result.control, frame });
    control = result.control;
  }
  if (
    frames.length !== 0 ||
    !isExactMidgardNativeScriptStructureTerminal(control)
  ) {
    throw new Error("Canonical V1 native-script scan did not terminate");
  }
  return steps;
};
