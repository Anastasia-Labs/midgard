import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";

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

export const isMidgardNativeScriptContainerKind = (
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

export const FRAME_DOMAIN = Buffer.from(
  "MidgardNativeScriptScanFrameV1",
  "ascii",
);

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
