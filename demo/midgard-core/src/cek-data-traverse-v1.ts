import { blake2b } from "@noble/hashes/blake2.js";

import {
  advanceMidgardCekDataBytesV1,
  encodeMidgardCekDataBytesControlV1,
  finalizeMidgardCekDataBytesV1,
  initialMidgardCekDataBytesControlV1,
  isWellFormedMidgardCekDataBytesControlV1,
  type MidgardCekDataBytesControlV1,
  MidgardCekDataBytesStagesV1,
  nextMidgardCekDataBytesSpanV1,
} from "./cek-data-bytes-v1.js";
import {
  appendMidgardCekDataFrameChildV1,
  finalizeMidgardCekDataFrameV1,
  foldMidgardCekDataFrameListChildV1,
  foldMidgardCekDataFrameMapPairV1,
  hashMidgardCekDataFrameV1,
  initialMidgardCekDataLargeConstrFrameV1,
  initialMidgardCekDataListFrameV1,
  initialMidgardCekDataMapFrameV1,
  initialMidgardCekDataSmallConstrFrameV1,
  type MidgardCekDataFrameV1,
} from "./cek-data-frame-v1.js";
import {
  advanceMidgardCekDataIntegerV1,
  encodeMidgardCekDataIntegerControlV1,
  finalizeMidgardCekDataIntegerV1,
  initialMidgardCekDataIntegerControlV1,
  isWellFormedMidgardCekDataIntegerControlV1,
  type MidgardCekDataIntegerControlV1,
  MidgardCekDataIntegerStagesV1,
  nextMidgardCekDataIntegerSpanV1,
  parseMidgardCekDataLargeConstructorSyntaxV1,
} from "./cek-data-integer-v1.js";
import {
  type MidgardCekDataSummaryV1,
} from "./cek-semantic.js";
import {
  finalizeMidgardCekSourceBlobV1,
  type MidgardCekSourceBlobSpanV1,
} from "./cek-source-blob-v1.js";
import {
  encodeCbor,
  encodeCborArrayRaw,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

export const MIDGARD_CEK_DATA_TRAVERSE_V1_VERSION = 1 as const;
export const MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES_V1 = 14;
export const MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1 = 132;

const CONTROL_DOMAIN = Buffer.from(
  "MidgardCekDataTraverseControlV1",
  "ascii",
);
const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MidgardCekDataTraverseStagesV1 = Object.freeze({
  Head: 0,
  Integer: 1,
  Bytes: 2,
  LargeConstructor: 3,
  LargeFields: 4,
  Close: 5,
  Fold: 6,
  Terminal: 7,
} as const);

export type MidgardCekDataTraverseStageV1 =
  (typeof MidgardCekDataTraverseStagesV1)[keyof typeof MidgardCekDataTraverseStagesV1];

export type MidgardCekDataTraverseControlV1 = {
  readonly version: typeof MIDGARD_CEK_DATA_TRAVERSE_V1_VERSION;
  readonly stage: MidgardCekDataTraverseStageV1;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly offset: number;
  readonly frameRoot: Buffer;
  readonly pendingLargeExpectedChildren: number | null;
  readonly integer: MidgardCekDataIntegerControlV1 | null;
  readonly bytes: MidgardCekDataBytesControlV1 | null;
  readonly result: MidgardCekDataSummaryV1 | null;
};

export type MidgardCekDataTraverseActionV1 =
  | {
      readonly kind: "headScalar";
      readonly itemLength: number;
    }
  | {
      readonly kind: "headSequence";
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "headMap";
    }
  | {
      readonly kind: "headLargeConstructor";
      readonly constructorCborLength: number;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "attachScalar";
      readonly parent: MidgardCekDataFrameV1 | null;
    }
  | {
      readonly kind: "foldList";
      readonly frame: MidgardCekDataFrameV1;
      readonly childIndex: number;
      readonly child: MidgardCekDataSummaryV1;
      readonly siblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "foldMap";
      readonly frame: MidgardCekDataFrameV1;
      readonly pairIndex: number;
      readonly key: MidgardCekDataSummaryV1;
      readonly value: MidgardCekDataSummaryV1;
      readonly keySiblings: readonly Uint8Array[];
      readonly valueSiblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "finalizeFrame";
      readonly frame: MidgardCekDataFrameV1;
      readonly parent: MidgardCekDataFrameV1 | null;
    }
  | null;

export type MidgardCekDataTraverseTraceStepV1 = {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes: Buffer | null;
  readonly action: MidgardCekDataTraverseActionV1;
  readonly next: MidgardCekDataTraverseControlV1;
};

type CborArgument = {
  readonly major: number;
  readonly value: number;
  readonly nextOffset: number;
};

type SmallConstructorHead = {
  readonly constructor: bigint;
  readonly prefixLength: number;
};

const exactUint32 = (
  value: number,
  fieldName: string,
): number => {
  if (
    !Number.isSafeInteger(value) ||
    value < 0 ||
    value > UINT32_MAX
  ) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

const optionalHashIsWellFormed = (value: Uint8Array): boolean =>
  value.length === 0 || value.length === 32;

const summaryIsWellFormed = (
  summary: MidgardCekDataSummaryV1,
): boolean => {
  try {
    ensureHash32(summary.root, "cek_data_traverse.result.root");
    return (
      summary.cborLength > 0n &&
      summary.cborLength <= UINT64_MAX &&
      summary.memory >= 4n &&
      summary.memory <= UINT64_MAX
    );
  } catch {
    return false;
  }
};

const nestedIntegerFits = (
  control: MidgardCekDataTraverseControlV1,
  integer: MidgardCekDataIntegerControlV1,
  startsAtCursor: boolean,
): boolean => {
  const absoluteCursor = control.sourceStart + control.offset;
  return (
    isWellFormedMidgardCekDataIntegerControlV1(integer) &&
    (startsAtCursor
      ? integer.sourceStart === absoluteCursor
      : integer.sourceStart + integer.sourceLength ===
        absoluteCursor) &&
    integer.sourceStart >= control.sourceStart &&
    integer.sourceStart + integer.sourceLength <=
      control.sourceStart + control.sourceLength
  );
};

export const isWellFormedMidgardCekDataTraverseControlV1 = (
  control: MidgardCekDataTraverseControlV1,
): boolean => {
  try {
    if (
      control.version !==
        MIDGARD_CEK_DATA_TRAVERSE_V1_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataTraverseStagesV1.Head ||
      control.stage >
        MidgardCekDataTraverseStagesV1.Terminal ||
      exactUint32(
        control.sourceStart,
        "cek_data_traverse.source_start",
      ) !== control.sourceStart ||
      exactUint32(
        control.sourceLength,
        "cek_data_traverse.source_length",
      ) !== control.sourceLength ||
      control.sourceLength === 0 ||
      !Number.isSafeInteger(
        control.sourceStart + control.sourceLength,
      ) ||
      exactUint32(
        control.offset,
        "cek_data_traverse.offset",
      ) !== control.offset ||
      control.offset > control.sourceLength ||
      !optionalHashIsWellFormed(control.frameRoot) ||
      (control.pendingLargeExpectedChildren !== null &&
        exactUint32(
          control.pendingLargeExpectedChildren,
          "cek_data_traverse.pending_large_children",
        ) !== control.pendingLargeExpectedChildren) ||
      (control.result !== null &&
        !summaryIsWellFormed(control.result))
    ) {
      return false;
    }
    switch (control.stage) {
      case MidgardCekDataTraverseStagesV1.Head:
        return (
          control.offset < control.sourceLength &&
          (control.frameRoot.length === 32 ||
            control.offset === 0) &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null
        );
      case MidgardCekDataTraverseStagesV1.Integer:
        return (
          control.pendingLargeExpectedChildren === null &&
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true)
        );
      case MidgardCekDataTraverseStagesV1.Bytes:
        return (
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes !== null &&
          control.result === null &&
          isWellFormedMidgardCekDataBytesControlV1(
            control.bytes,
          ) &&
          control.bytes.sourceStart ===
            control.sourceStart + control.offset &&
          control.offset + control.bytes.sourceLength <=
            control.sourceLength
        );
      case MidgardCekDataTraverseStagesV1.LargeConstructor:
        return (
          control.pendingLargeExpectedChildren !== null &&
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true) &&
          control.offset + control.integer.sourceLength <
            control.sourceLength
        );
      case MidgardCekDataTraverseStagesV1.LargeFields:
        return (
          control.pendingLargeExpectedChildren !== null &&
          control.integer !== null &&
          control.integer.stage ===
            MidgardCekDataIntegerStagesV1.Terminal &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, false) &&
          control.offset < control.sourceLength
        );
      case MidgardCekDataTraverseStagesV1.Close:
      case MidgardCekDataTraverseStagesV1.Fold:
        return (
          control.frameRoot.length === 32 &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null &&
          (control.stage !==
            MidgardCekDataTraverseStagesV1.Close ||
            control.offset < control.sourceLength)
        );
      case MidgardCekDataTraverseStagesV1.Terminal:
        return (
          control.offset === control.sourceLength &&
          control.frameRoot.length === 0 &&
          control.pendingLargeExpectedChildren === null &&
          control.integer === null &&
          control.bytes === null &&
          control.result !== null
        );
    }
  } catch {
    return false;
  }
};

export const initialMidgardCekDataTraverseControlV1 = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataTraverseControlV1 => {
  const control = {
    version: MIDGARD_CEK_DATA_TRAVERSE_V1_VERSION,
    stage: MidgardCekDataTraverseStagesV1.Head,
    sourceStart,
    sourceLength,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result: null,
  } satisfies MidgardCekDataTraverseControlV1;
  if (!isWellFormedMidgardCekDataTraverseControlV1(control)) {
    throw new Error("Invalid V1 CEK Data traversal source");
  }
  return control;
};

const optionalIntCbor = (value: number | null): Buffer =>
  value === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeCbor(BigInt(value)),
        Buffer.from([0xff]),
      ]);

const optionalControlCbor = (
  control:
    | MidgardCekDataIntegerControlV1
    | MidgardCekDataBytesControlV1
    | null,
): Buffer => {
  if (control === null) return Buffer.from("d87a80", "hex");
  const nested =
    "memory" in control
      ? encodeMidgardCekDataIntegerControlV1(control)
      : encodeMidgardCekDataBytesControlV1(control);
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    nested,
    Buffer.from([0xff]),
  ]);
};

const optionalSummaryCbor = (
  summary: MidgardCekDataSummaryV1 | null,
): Buffer =>
  summary === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeCbor([
          Buffer.from(summary.root),
          summary.cborLength,
          summary.memory,
        ]),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekDataTraverseControlV1 = (
  control: MidgardCekDataTraverseControlV1,
): Buffer => {
  if (!isWellFormedMidgardCekDataTraverseControlV1(control)) {
    throw new Error("Invalid V1 CEK Data traversal control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(control.version)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(BigInt(control.offset)),
    encodeCbor(control.frameRoot),
    optionalIntCbor(control.pendingLargeExpectedChildren),
    optionalControlCbor(control.integer),
    optionalControlCbor(control.bytes),
    optionalSummaryCbor(control.result),
  ]);
};

export const hashMidgardCekDataTraverseControlV1 = (
  control: MidgardCekDataTraverseControlV1,
): Hash32 =>
  ensureHash32(
    blake2b(
      Buffer.concat([
        CONTROL_DOMAIN,
        encodeMidgardCekDataTraverseControlV1(control),
      ]),
      { dkLen: 32 },
    ),
    "cek_data_traverse_control_hash",
  );

export const nextMidgardCekDataTraverseSpanV1 = (
  control: MidgardCekDataTraverseControlV1,
): MidgardCekSourceBlobSpanV1 | null => {
  if (!isWellFormedMidgardCekDataTraverseControlV1(control)) {
    return null;
  }
  switch (control.stage) {
    case MidgardCekDataTraverseStagesV1.Head:
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: Math.min(
          MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES_V1,
          control.sourceLength - control.offset,
        ),
      };
    case MidgardCekDataTraverseStagesV1.Integer:
    case MidgardCekDataTraverseStagesV1.LargeConstructor:
      return nextMidgardCekDataIntegerSpanV1(control.integer!);
    case MidgardCekDataTraverseStagesV1.Bytes:
      return nextMidgardCekDataBytesSpanV1(control.bytes!);
    case MidgardCekDataTraverseStagesV1.LargeFields:
    case MidgardCekDataTraverseStagesV1.Close:
      return {
        absoluteStart: control.sourceStart + control.offset,
        length: 1,
      };
    case MidgardCekDataTraverseStagesV1.Fold:
    case MidgardCekDataTraverseStagesV1.Terminal:
      return null;
  }
};

const readCanonicalCborArgument = (
  bytes: Uint8Array,
  offset: number,
): CborArgument | null => {
  if (offset < 0 || offset >= bytes.length) return null;
  const initial = bytes[offset]!;
  const major = initial >>> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return {
      major,
      value: additional,
      nextOffset: offset + 1,
    };
  }
  const byteLength =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : null;
  if (
    byteLength === null ||
    offset + 1 + byteLength > bytes.length
  ) {
    return null;
  }
  let value = 0;
  for (let index = 0; index < byteLength; index += 1) {
    value = value * 256 + bytes[offset + 1 + index]!;
  }
  if (
    (additional === 24 && value < 24) ||
    (additional === 25 && value <= 0xff) ||
    (additional === 26 && value <= 0xffff)
  ) {
    return null;
  }
  return { major, value, nextOffset: offset + 1 + byteLength };
};

const parseSmallConstructorHead = (
  bytes: Uint8Array,
): SmallConstructorHead | null => {
  if (
    bytes.length >= 2 &&
    bytes[0] === 0xd8 &&
    bytes[1]! >= 121 &&
    bytes[1]! <= 127
  ) {
    return {
      constructor: BigInt(bytes[1]! - 121),
      prefixLength: 2,
    };
  }
  if (bytes.length < 3 || bytes[0] !== 0xd9) return null;
  const tag = bytes[1]! * 256 + bytes[2]!;
  const constructor = tag - 1_280 + 7;
  return tag >= 1_280 &&
    tag <= 1_400 &&
    constructor <= 127
    ? { constructor: BigInt(constructor), prefixLength: 3 }
    : null;
};

const exactSourceBytes = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
}): Buffer | null => {
  const span = nextMidgardCekDataTraverseSpanV1(control);
  if (
    span === null ||
    sourceBytes === null ||
    sourceBytes === undefined ||
    sourceBytes.length !== span.length
  ) {
    return null;
  }
  return Buffer.from(sourceBytes);
};

const advanced = (
  control: MidgardCekDataTraverseControlV1,
): MidgardCekDataTraverseControlV1 | null =>
  isWellFormedMidgardCekDataTraverseControlV1(control)
    ? control
    : null;

const nextParentStage = (
  frame: MidgardCekDataFrameV1,
): MidgardCekDataTraverseStageV1 => {
  if (frame.childCount < frame.expectedChildren) {
    return MidgardCekDataTraverseStagesV1.Head;
  }
  return frame.kind === "map"
    ? MidgardCekDataTraverseStagesV1.Fold
    : MidgardCekDataTraverseStagesV1.Close;
};

const attachSummary = ({
  control,
  summary,
  parent,
  offset,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly summary: MidgardCekDataSummaryV1;
  readonly parent: MidgardCekDataFrameV1 | null;
  readonly offset: number;
}): MidgardCekDataTraverseControlV1 | null => {
  if (!summaryIsWellFormed(summary)) return null;
  if (control.frameRoot.length === 0) {
    if (parent !== null || offset !== control.sourceLength) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStagesV1.Terminal,
      offset,
      frameRoot: Buffer.alloc(0),
      pendingLargeExpectedChildren: null,
      integer: null,
      bytes: null,
      result: summary,
    });
  }
  if (
    parent === null ||
    !hashMidgardCekDataFrameV1(parent).equals(
      control.frameRoot,
    )
  ) {
    return null;
  }
  const nextParent = appendMidgardCekDataFrameChildV1(
    parent,
    summary,
  );
  if (nextParent === null) return null;
  return advanced({
    ...control,
    stage: nextParentStage(nextParent),
    offset,
    frameRoot: Buffer.from(
      hashMidgardCekDataFrameV1(nextParent),
    ),
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result: null,
  });
};

const stepHeadScalar = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseActionV1,
    { readonly kind: "headScalar" }
  >;
}): MidgardCekDataTraverseControlV1 | null => {
  const itemLength = exactUint32(
    action.itemLength,
    "cek_data_traverse.scalar_length",
  );
  if (
    itemLength === 0 ||
    control.offset + itemLength > control.sourceLength
  ) {
    return null;
  }
  const first = bytes[0]!;
  if ((first >>> 5) <= 1 || first === 0xc2 || first === 0xc3) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStagesV1.Integer,
      integer: initialMidgardCekDataIntegerControlV1({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  if ((first >>> 5) === 2) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStagesV1.Bytes,
      bytes: initialMidgardCekDataBytesControlV1({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  return null;
};

const stepHeadSequence = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseActionV1,
    { readonly kind: "headSequence" }
  >;
}): MidgardCekDataTraverseControlV1 | null => {
  const expectedChildren = exactUint32(
    action.expectedChildren,
    "cek_data_traverse.expected_children",
  );
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const small = parseSmallConstructorHead(bytes);
  let frame: MidgardCekDataFrameV1;
  let headLength: number;
  if (small !== null) {
    if (bytes[small.prefixLength] !== sequenceHeader) return null;
    frame = initialMidgardCekDataSmallConstrFrameV1({
      constructor: small.constructor,
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = small.prefixLength + 1;
  } else {
    if (bytes[0] !== sequenceHeader) return null;
    frame = initialMidgardCekDataListFrameV1({
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = 1;
  }
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStagesV1.Fold
        : MidgardCekDataTraverseStagesV1.Head,
    offset: control.offset + headLength,
    frameRoot: Buffer.from(hashMidgardCekDataFrameV1(frame)),
  });
};

const stepHeadMap = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControlV1 | null => {
  const argument = readCanonicalCborArgument(bytes, 0);
  if (
    argument === null ||
    argument.major !== 5 ||
    argument.value > Math.floor(UINT32_MAX / 2)
  ) {
    return null;
  }
  const expectedChildren = argument.value * 2;
  const frame = initialMidgardCekDataMapFrameV1({
    tail: control.frameRoot,
    expectedChildren,
  });
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStagesV1.Fold
        : MidgardCekDataTraverseStagesV1.Head,
    offset: control.offset + argument.nextOffset,
    frameRoot: Buffer.from(hashMidgardCekDataFrameV1(frame)),
  });
};

const stepHeadLargeConstructor = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseActionV1,
    { readonly kind: "headLargeConstructor" }
  >;
}): MidgardCekDataTraverseControlV1 | null => {
  const constructorCborLength = exactUint32(
    action.constructorCborLength,
    "cek_data_traverse.constructor_cbor_length",
  );
  const expectedChildren = exactUint32(
    action.expectedChildren,
    "cek_data_traverse.expected_children",
  );
  if (
    constructorCborLength === 0 ||
    bytes.length < 3 ||
    !bytes.subarray(0, 3).equals(
      Buffer.from("d86682", "hex"),
    ) ||
    control.offset + 3 + constructorCborLength >=
      control.sourceLength
  ) {
    return null;
  }
  const offset = control.offset + 3;
  return advanced({
    ...control,
    stage: MidgardCekDataTraverseStagesV1.LargeConstructor,
    offset,
    pendingLargeExpectedChildren: expectedChildren,
    integer: initialMidgardCekDataIntegerControlV1({
      sourceStart: control.sourceStart + offset,
      sourceLength: constructorCborLength,
    }),
  });
};

const stepHead = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  const bytes = exactSourceBytes({ control, sourceBytes });
  if (bytes === null || action === null) return null;
  switch (action.kind) {
    case "headScalar":
      return stepHeadScalar({ control, bytes, action });
    case "headSequence":
      return stepHeadSequence({ control, bytes, action });
    case "headMap":
      return stepHeadMap({ control, bytes });
    case "headLargeConstructor":
      return stepHeadLargeConstructor({
        control,
        bytes,
        action,
      });
    default:
      return null;
  }
};

const stepInteger = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStagesV1.Terminal) {
    if (
      sourceBytes !== null &&
      sourceBytes !== undefined
    ) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataIntegerV1(integer);
    return summary === null
      ? null
      : attachSummary({
          control,
          summary,
          parent: action.parent,
          offset: control.offset + integer.sourceLength,
        });
  }
  if (action !== null) return null;
  const nextInteger = advanceMidgardCekDataIntegerV1({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

const stepBytes = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  const byteControl = control.bytes!;
  if (
    byteControl.stage === MidgardCekDataBytesStagesV1.Terminal
  ) {
    if (
      sourceBytes !== null &&
      sourceBytes !== undefined
    ) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataBytesV1(byteControl);
    return summary === null
      ? null
      : attachSummary({
          control,
          summary,
          parent: action.parent,
          offset:
            control.offset + byteControl.sourceLength,
        });
  }
  if (action !== null) return null;
  const nextBytes = advanceMidgardCekDataBytesV1({
    control: byteControl,
    sourceBytes,
  });
  return nextBytes === null
    ? null
    : advanced({ ...control, bytes: nextBytes });
};

const stepLargeConstructor = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStagesV1.Terminal) {
    if (
      action !== null ||
      (sourceBytes !== null && sourceBytes !== undefined)
    ) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStagesV1.LargeFields,
      offset: control.offset + integer.sourceLength,
    });
  }
  if (action !== null) return null;
  if (
    integer.stage === MidgardCekDataIntegerStagesV1.Syntax &&
    (sourceBytes === null ||
      sourceBytes === undefined ||
      parseMidgardCekDataLargeConstructorSyntaxV1({
        syntaxBytes: sourceBytes,
        sourceLength: integer.sourceLength,
      }) === null)
  ) {
    return null;
  }
  const nextInteger = advanceMidgardCekDataIntegerV1({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

const stepLargeFields = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  const expectedChildren =
    control.pendingLargeExpectedChildren!;
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const integer = control.integer!;
  const constructorCborRoot =
    finalizeMidgardCekSourceBlobV1(integer.blob!);
  if (
    bytes === null ||
    bytes[0] !== sequenceHeader ||
    constructorCborRoot === null
  ) {
    return null;
  }
  const frame = initialMidgardCekDataLargeConstrFrameV1({
    constructorCborRoot,
    constructorCborLength: BigInt(integer.sourceLength),
    constructorMemory: integer.memory,
    tail: control.frameRoot,
    expectedChildren,
  });
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStagesV1.Fold
        : MidgardCekDataTraverseStagesV1.Head,
    offset: control.offset + 1,
    frameRoot: Buffer.from(hashMidgardCekDataFrameV1(frame)),
    pendingLargeExpectedChildren: null,
    integer: null,
  });
};

const stepClose = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  return bytes !== null && bytes[0] === 0xff
    ? advanced({
        ...control,
        stage: MidgardCekDataTraverseStagesV1.Fold,
        offset: control.offset + 1,
      })
    : null;
};

const stepFinalizeFrame = ({
  control,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly action: Extract<
    MidgardCekDataTraverseActionV1,
    { readonly kind: "finalizeFrame" }
  >;
}): MidgardCekDataTraverseControlV1 | null => {
  if (
    !hashMidgardCekDataFrameV1(action.frame).equals(
      control.frameRoot,
    )
  ) {
    return null;
  }
  const summary = finalizeMidgardCekDataFrameV1(action.frame);
  if (summary === null) return null;
  if (action.frame.tail.length === 0) {
    return attachSummary({
      control: { ...control, frameRoot: Buffer.alloc(0) },
      summary,
      parent: action.parent,
      offset: control.offset,
    });
  }
  if (
    action.parent === null ||
    !hashMidgardCekDataFrameV1(action.parent).equals(
      action.frame.tail,
    )
  ) {
    return null;
  }
  return attachSummary({
    control: {
      ...control,
      frameRoot: Buffer.from(action.frame.tail),
    },
    summary,
    parent: action.parent,
    offset: control.offset,
  });
};

const stepFold = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  if (
    (sourceBytes !== null && sourceBytes !== undefined) ||
    action === null
  ) {
    return null;
  }
  if (
    "frame" in action &&
    !hashMidgardCekDataFrameV1(action.frame).equals(
      control.frameRoot,
    )
  ) {
    return null;
  }
  if (action.kind === "foldList") {
    const frame = foldMidgardCekDataFrameListChildV1({
      frame: action.frame,
      childIndex: action.childIndex,
      child: action.child,
      siblings: action.siblings,
    });
    return frame === null
      ? null
      : advanced({
          ...control,
          frameRoot: Buffer.from(hashMidgardCekDataFrameV1(frame)),
        });
  }
  if (action.kind === "foldMap") {
    const frame = foldMidgardCekDataFrameMapPairV1({
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
          frameRoot: Buffer.from(hashMidgardCekDataFrameV1(frame)),
        });
  }
  return action.kind === "finalizeFrame"
    ? stepFinalizeFrame({ control, action })
    : null;
};

export const advanceMidgardCekDataTraverseV1 = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControlV1;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseActionV1;
}): MidgardCekDataTraverseControlV1 | null => {
  if (!isWellFormedMidgardCekDataTraverseControlV1(control)) {
    return null;
  }
  try {
    switch (control.stage) {
      case MidgardCekDataTraverseStagesV1.Head:
        return stepHead({ control, sourceBytes, action });
      case MidgardCekDataTraverseStagesV1.Integer:
        return stepInteger({ control, sourceBytes, action });
      case MidgardCekDataTraverseStagesV1.Bytes:
        return stepBytes({ control, sourceBytes, action });
      case MidgardCekDataTraverseStagesV1.LargeConstructor:
        return stepLargeConstructor({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStagesV1.LargeFields:
        return stepLargeFields({
          control,
          sourceBytes,
          action,
        });
      case MidgardCekDataTraverseStagesV1.Close:
        return stepClose({ control, sourceBytes, action });
      case MidgardCekDataTraverseStagesV1.Fold:
        return stepFold({ control, sourceBytes, action });
      case MidgardCekDataTraverseStagesV1.Terminal:
        return null;
    }
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataTraverseV1 = (
  control: MidgardCekDataTraverseControlV1,
): MidgardCekDataSummaryV1 | null =>
  isWellFormedMidgardCekDataTraverseControlV1(control) &&
  control.stage === MidgardCekDataTraverseStagesV1.Terminal
    ? control.result
    : null;
