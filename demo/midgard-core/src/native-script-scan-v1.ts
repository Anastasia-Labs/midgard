import { blake2b } from "@noble/hashes/blake2.js";

import {
  decodeSingleCbor,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";

export const MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION = 1 as const;
export const MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1 = 16_384 as const;
export const MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1 = 16_384 as const;

export const MidgardNativeScriptKindsV1 = Object.freeze({
  Signature: 0,
  All: 1,
  Any: 2,
  AtLeast: 3,
  After: 4,
  Before: 5,
} as const);

export type MidgardNativeScriptKindV1 =
  (typeof MidgardNativeScriptKindsV1)[keyof typeof MidgardNativeScriptKindsV1];

type MidgardNativeScriptContainerKindV1 =
  | typeof MidgardNativeScriptKindsV1.All
  | typeof MidgardNativeScriptKindsV1.Any
  | typeof MidgardNativeScriptKindsV1.AtLeast;

const isMidgardNativeScriptContainerKindV1 = (
  kind: MidgardNativeScriptKindV1,
): kind is MidgardNativeScriptContainerKindV1 =>
  kind === MidgardNativeScriptKindsV1.All ||
  kind === MidgardNativeScriptKindsV1.Any ||
  kind === MidgardNativeScriptKindsV1.AtLeast;

export const MidgardNativeScriptStructureStagesV1 = Object.freeze({
  Token: 0,
  Frame: 1,
  Finalize: 2,
  Terminal: 3,
} as const);

export type MidgardNativeScriptStructureStageV1 =
  (typeof MidgardNativeScriptStructureStagesV1)[keyof typeof MidgardNativeScriptStructureStagesV1];

export const MidgardNativeScriptStructureResultKindsV1 = Object.freeze({
  Advanced: "advanced",
  Invalid: "invalid",
  NodeLimit: "nodeLimit",
  DepthLimit: "depthLimit",
} as const);

export type MidgardNativeScriptStructureControlV1 = {
  readonly version: typeof MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION;
  readonly stage: MidgardNativeScriptStructureStageV1;
  readonly startOffset: number;
  readonly cursor: number;
  readonly endOffset: number;
  readonly stackRoot: Buffer;
  readonly stackDepth: number;
  readonly nodeCount: number;
};

export type MidgardNativeScriptScanFrameV1 = {
  readonly tail: Buffer;
  readonly kind: MidgardNativeScriptContainerKindV1;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

export type MidgardNativeScriptTokenV1 = {
  readonly kind: MidgardNativeScriptKindV1;
  readonly nextOffset: number;
  readonly childCount: number;
  readonly required: bigint;
};

export type MidgardNativeScriptStructureStepResultV1 =
  | {
      readonly kind: typeof MidgardNativeScriptStructureResultKindsV1.Advanced;
      readonly control: MidgardNativeScriptStructureControlV1;
    }
  | {
      readonly kind:
        | typeof MidgardNativeScriptStructureResultKindsV1.Invalid
        | typeof MidgardNativeScriptStructureResultKindsV1.NodeLimit
        | typeof MidgardNativeScriptStructureResultKindsV1.DepthLimit;
    };

export type MidgardNativeScriptStructureTraceStepV1 = {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly next: MidgardNativeScriptStructureControlV1;
  readonly frame: MidgardNativeScriptScanFrameV1 | null;
};

const FRAME_DOMAIN_V1 = Buffer.from("MidgardNativeScriptScanFrameV1", "ascii");

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

export const isWellFormedMidgardNativeScriptStructureControlV1 = (
  control: MidgardNativeScriptStructureControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION ||
      exactSafeInteger({
        value: control.stage,
        field: "stage",
        minimum: MidgardNativeScriptStructureStagesV1.Token,
        maximum: MidgardNativeScriptStructureStagesV1.Terminal,
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
      maximum: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
    });
    exactSafeInteger({
      value: control.nodeCount,
      field: "node count",
      minimum: 0,
      maximum: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
    });
    const emptyStack = control.stackRoot.length === 0;
    const committedStack = control.stackRoot.length === 32;
    if (
      (control.stackDepth === 0 && !emptyStack) ||
      (control.stackDepth > 0 && !committedStack)
    ) {
      return false;
    }
    if (control.stage === MidgardNativeScriptStructureStagesV1.Token) {
      return control.cursor < control.endOffset;
    }
    if (control.stage === MidgardNativeScriptStructureStagesV1.Frame) {
      return control.stackDepth > 0 && committedStack;
    }
    if (control.stage === MidgardNativeScriptStructureStagesV1.Finalize) {
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

export const initialMidgardNativeScriptStructureControlV1 = ({
  startOffset,
  totalLength,
}: {
  readonly startOffset: number;
  readonly totalLength: number;
}): MidgardNativeScriptStructureControlV1 => {
  exactSafeInteger({
    value: totalLength,
    field: "total length",
    minimum: 1,
  });
  const control = {
    version: MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION,
    stage: MidgardNativeScriptStructureStagesV1.Token,
    startOffset,
    cursor: startOffset,
    endOffset: startOffset + totalLength,
    stackRoot: Buffer.alloc(0),
    stackDepth: 0,
    nodeCount: 0,
  } satisfies MidgardNativeScriptStructureControlV1;
  if (!isWellFormedMidgardNativeScriptStructureControlV1(control)) {
    throw new Error("Invalid V1 native-script scan span");
  }
  return control;
};

export const encodeMidgardNativeScriptStructureControlV1 = (
  control: MidgardNativeScriptStructureControlV1,
): Buffer => {
  if (!isWellFormedMidgardNativeScriptStructureControlV1(control)) {
    throw new Error("Invalid V1 native-script structure control");
  }
  return encodeCbor([
    BigInt(MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION),
    BigInt(control.stage),
    BigInt(control.startOffset),
    BigInt(control.cursor),
    BigInt(control.endOffset),
    control.stackRoot,
    BigInt(control.stackDepth),
    BigInt(control.nodeCount),
  ]);
};

export const decodeMidgardNativeScriptStructureControlV1 = (
  controlCbor: Uint8Array,
): MidgardNativeScriptStructureControlV1 => {
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
    ) as typeof MIDGARD_NATIVE_SCRIPT_SCAN_V1_VERSION,
    stage: decodedSafeInteger(
      value[1],
      "stage",
    ) as MidgardNativeScriptStructureStageV1,
    startOffset: decodedSafeInteger(value[2], "start offset"),
    cursor: decodedSafeInteger(value[3], "cursor"),
    endOffset: decodedSafeInteger(value[4], "end offset"),
    stackRoot: Buffer.from(value[5]),
    stackDepth: decodedSafeInteger(value[6], "stack depth"),
    nodeCount: decodedSafeInteger(value[7], "node count"),
  } satisfies MidgardNativeScriptStructureControlV1;
  if (
    !isWellFormedMidgardNativeScriptStructureControlV1(control) ||
    !encodeMidgardNativeScriptStructureControlV1(control).equals(
      Buffer.from(controlCbor),
    )
  ) {
    throw new Error("Non-canonical V1 native-script structure control");
  }
  return control;
};

export const midgardNativeScriptScanFrameIsWellFormedV1 = (
  frame: MidgardNativeScriptScanFrameV1,
): boolean => {
  const processed = frame.childCount - frame.remaining;
  return (
    (frame.tail.length === 0 || frame.tail.length === 32) &&
    Number.isSafeInteger(frame.kind) &&
    frame.kind >= MidgardNativeScriptKindsV1.All &&
    frame.kind <= MidgardNativeScriptKindsV1.AtLeast &&
    Number.isSafeInteger(frame.childCount) &&
    frame.childCount > 0 &&
    frame.childCount <= MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1 &&
    Number.isSafeInteger(frame.remaining) &&
    frame.remaining > 0 &&
    frame.remaining <= frame.childCount &&
    Number.isSafeInteger(frame.validCount) &&
    frame.validCount >= 0 &&
    frame.validCount <= processed &&
    (frame.kind === MidgardNativeScriptKindsV1.AtLeast
      ? frame.required >= 0n
      : frame.required === 0n)
  );
};

export const hashMidgardNativeScriptScanFrameV1 = (
  frame: MidgardNativeScriptScanFrameV1,
): Buffer => {
  if (!midgardNativeScriptScanFrameIsWellFormedV1(frame)) {
    throw new Error("Invalid V1 native-script scan frame");
  }
  return Buffer.from(
    blake2b(
      Buffer.concat([
        FRAME_DOMAIN_V1,
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
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardNativeScriptTokenV1 => {
  const outer = readCborArrayHeader(window, windowOffset, "native_script");
  const tag = readCborUnsigned(window, outer.nextOffset, "native_script.tag");
  if (tag.value > BigInt(MidgardNativeScriptKindsV1.Before)) {
    throw new Error("Unsupported V1 native-script tag");
  }
  const kind = Number(tag.value) as MidgardNativeScriptKindV1;
  if (
    (kind === MidgardNativeScriptKindsV1.AtLeast && outer.length !== 3) ||
    (kind !== MidgardNativeScriptKindsV1.AtLeast && outer.length !== 2)
  ) {
    throw new Error("Invalid V1 native-script outer shape");
  }
  let nextOffset = tag.nextOffset;
  let childCount = 0;
  let required = 0n;
  if (kind === MidgardNativeScriptKindsV1.Signature) {
    const keyHash = readCborBytes(window, nextOffset, "native_script.key_hash");
    if (keyHash.value.length !== 28) {
      throw new Error("Invalid V1 native signature key hash");
    }
    nextOffset = keyHash.nextOffset;
  } else if (
    kind === MidgardNativeScriptKindsV1.All ||
    kind === MidgardNativeScriptKindsV1.Any
  ) {
    const children = readCborArrayHeader(
      window,
      nextOffset,
      "native_script.children",
    );
    childCount = children.length;
    nextOffset = children.nextOffset;
  } else if (kind === MidgardNativeScriptKindsV1.AtLeast) {
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
export const readMidgardNativeScriptStructureTokenV1 = readToken;

const advanced = (
  control: MidgardNativeScriptStructureControlV1,
): MidgardNativeScriptStructureStepResultV1 =>
  isWellFormedMidgardNativeScriptStructureControlV1(control)
    ? {
        kind: MidgardNativeScriptStructureResultKindsV1.Advanced,
        control,
      }
    : { kind: MidgardNativeScriptStructureResultKindsV1.Invalid };

export const advanceMidgardNativeScriptStructureTokenV1 = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardNativeScriptStructureStepResultV1 | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControlV1(control) ||
    control.stage !== MidgardNativeScriptStructureStagesV1.Token ||
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
      return { kind: MidgardNativeScriptStructureResultKindsV1.Invalid };
    }
    const nodeCount = control.nodeCount + 1;
    if (nodeCount > MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1) {
      return { kind: MidgardNativeScriptStructureResultKindsV1.NodeLimit };
    }
    if (
      isMidgardNativeScriptContainerKindV1(token.kind) &&
      token.childCount > 0
    ) {
      const stackDepth = control.stackDepth + 1;
      if (stackDepth > MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1) {
        return {
          kind: MidgardNativeScriptStructureResultKindsV1.DepthLimit,
        };
      }
      const frame = {
        tail: control.stackRoot,
        kind: token.kind,
        childCount: token.childCount,
        remaining: token.childCount,
        validCount: 0,
        required: token.required,
      } satisfies MidgardNativeScriptScanFrameV1;
      return advanced({
        ...control,
        cursor: token.nextOffset,
        stackRoot: hashMidgardNativeScriptScanFrameV1(frame),
        stackDepth,
        nodeCount,
      });
    }
    return advanced({
      ...control,
      stage:
        control.stackDepth > 0
          ? MidgardNativeScriptStructureStagesV1.Frame
          : MidgardNativeScriptStructureStagesV1.Finalize,
      cursor: token.nextOffset,
      nodeCount,
    });
  } catch {
    return { kind: MidgardNativeScriptStructureResultKindsV1.Invalid };
  }
};

export const advanceMidgardNativeScriptStructureFrameV1 = ({
  control,
  frame,
}: {
  readonly control: MidgardNativeScriptStructureControlV1;
  readonly frame: MidgardNativeScriptScanFrameV1;
}): MidgardNativeScriptStructureStepResultV1 | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControlV1(control) ||
    control.stage !== MidgardNativeScriptStructureStagesV1.Frame ||
    !midgardNativeScriptScanFrameIsWellFormedV1(frame)
  ) {
    return null;
  }
  try {
    if (!hashMidgardNativeScriptScanFrameV1(frame).equals(control.stackRoot)) {
      return null;
    }
    if (frame.remaining === 1) {
      const stackDepth = control.stackDepth - 1;
      return advanced({
        ...control,
        stage:
          stackDepth > 0
            ? MidgardNativeScriptStructureStagesV1.Frame
            : MidgardNativeScriptStructureStagesV1.Finalize,
        stackRoot: frame.tail,
        stackDepth,
      });
    }
    const nextFrame = {
      ...frame,
      remaining: frame.remaining - 1,
    } satisfies MidgardNativeScriptScanFrameV1;
    return advanced({
      ...control,
      stage: MidgardNativeScriptStructureStagesV1.Token,
      stackRoot: hashMidgardNativeScriptScanFrameV1(nextFrame),
    });
  } catch {
    return null;
  }
};

export const finalizeMidgardNativeScriptStructureV1 = (
  control: MidgardNativeScriptStructureControlV1,
): MidgardNativeScriptStructureStepResultV1 | null => {
  if (
    !isWellFormedMidgardNativeScriptStructureControlV1(control) ||
    control.stage !== MidgardNativeScriptStructureStagesV1.Finalize
  ) {
    return null;
  }
  return control.cursor === control.endOffset && control.nodeCount > 0
    ? advanced({
        ...control,
        stage: MidgardNativeScriptStructureStagesV1.Terminal,
      })
    : { kind: MidgardNativeScriptStructureResultKindsV1.Invalid };
};

export const isExactMidgardNativeScriptStructureTerminalV1 = (
  control: MidgardNativeScriptStructureControlV1,
): boolean =>
  isWellFormedMidgardNativeScriptStructureControlV1(control) &&
  control.stage === MidgardNativeScriptStructureStagesV1.Terminal;

export const buildMidgardNativeScriptStructureTraceV1 = (
  scriptBytes: Uint8Array,
  startOffset = 0,
): readonly MidgardNativeScriptStructureTraceStepV1[] => {
  const bytes = Buffer.from(scriptBytes);
  let control = initialMidgardNativeScriptStructureControlV1({
    startOffset,
    totalLength: bytes.length,
  });
  const frames: MidgardNativeScriptScanFrameV1[] = [];
  const steps: MidgardNativeScriptStructureTraceStepV1[] = [];
  const maximumSteps = MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1 * 2 + 1;
  while (
    control.stage !== MidgardNativeScriptStructureStagesV1.Terminal &&
    steps.length < maximumSteps
  ) {
    let result: MidgardNativeScriptStructureStepResultV1 | null;
    let frame: MidgardNativeScriptScanFrameV1 | null = null;
    if (control.stage === MidgardNativeScriptStructureStagesV1.Token) {
      const token = readToken({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      result = advanceMidgardNativeScriptStructureTokenV1({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      if (
        isMidgardNativeScriptContainerKindV1(token.kind) &&
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
    } else if (control.stage === MidgardNativeScriptStructureStagesV1.Frame) {
      frame = frames.at(-1) ?? null;
      if (frame === null) {
        throw new Error("V1 native-script trace lost its stack frame");
      }
      result = advanceMidgardNativeScriptStructureFrameV1({
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
      result = finalizeMidgardNativeScriptStructureV1(control);
    }
    if (
      result === null ||
      result.kind !== MidgardNativeScriptStructureResultKindsV1.Advanced
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
    !isExactMidgardNativeScriptStructureTerminalV1(control)
  ) {
    throw new Error("Canonical V1 native-script scan did not terminate");
  }
  return steps;
};
