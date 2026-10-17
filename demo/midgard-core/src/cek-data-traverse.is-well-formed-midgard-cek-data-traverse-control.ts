import {
  isWellFormedMidgardCekDataBytesControl,
  type MidgardCekDataBytesControl,
} from "./cek-data-bytes.js";
import {
  encodeValidatedMidgardCekDataBytesControl,
  isWellFormedMidgardCekDataBytesControlWithValidatedBlob,
} from "./cek-data-bytes.parse-midgard-cek-data-bytes-syntax.js";
import { type MidgardCekDataFrame } from "./cek-data-frame.js";
import {
  encodeValidatedMidgardCekDataIntegerControl,
  isWellFormedMidgardCekDataIntegerControl,
  isWellFormedMidgardCekDataIntegerControlWithValidatedBlob,
  type MidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import { ensureHash32 } from "./codec/hash.js";

export const MIDGARD_CEK_DATA_TRAVERSE_VERSION = 1 as const;

export const MIDGARD_CEK_DATA_TRAVERSE_HEAD_BYTES = 14;

export const MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN = 132;

export const CONTROL_DOMAIN = Buffer.from(
  "MidgardCekDataTraverseControlV1",
  "ascii",
);

export const UINT32_MAX = 0xffff_ffff;

export const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MidgardCekDataTraverseStages = Object.freeze({
  Head: 0,
  Integer: 1,
  Bytes: 2,
  LargeConstructor: 3,
  LargeFields: 4,
  Close: 5,
  Fold: 6,
  Terminal: 7,
} as const);

export type MidgardCekDataTraverseStage =
  (typeof MidgardCekDataTraverseStages)[keyof typeof MidgardCekDataTraverseStages];

export type MidgardCekDataTraverseControl = {
  readonly version: typeof MIDGARD_CEK_DATA_TRAVERSE_VERSION;
  readonly stage: MidgardCekDataTraverseStage;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly offset: number;
  readonly frameRoot: Buffer;
  readonly integer: MidgardCekDataIntegerControl | null;
  readonly bytes: MidgardCekDataBytesControl | null;
  readonly result: MidgardCekDataSummary | null;
};

/**
 * Every head action takes no argument: the item length, the constructor
 * length and the child count are all read from the authenticated head window
 * (an indefinite sequence closes at its authenticated 0xff break; an
 * indefinite byte string is measured by the bytes sub-control).
 */
export type MidgardCekDataTraverseAction =
  | {
      readonly kind: "headScalar";
    }
  | {
      readonly kind: "headSequence";
    }
  | {
      readonly kind: "headMap";
    }
  | {
      readonly kind: "headLargeConstructor";
    }
  | {
      readonly kind: "attachScalar";
      readonly parent: MidgardCekDataFrame | null;
    }
  | {
      readonly kind: "foldList";
      readonly frame: MidgardCekDataFrame;
      readonly childIndex: number;
      readonly child: MidgardCekDataSummary;
      readonly siblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "foldMap";
      readonly frame: MidgardCekDataFrame;
      readonly pairIndex: number;
      readonly key: MidgardCekDataSummary;
      readonly value: MidgardCekDataSummary;
      readonly keySiblings: readonly Uint8Array[];
      readonly valueSiblings: readonly Uint8Array[];
    }
  | {
      readonly kind: "finalizeFrame";
      readonly frame: MidgardCekDataFrame;
      readonly parent: MidgardCekDataFrame | null;
    }
  | null;

export type MidgardCekDataTraverseTraceStep = {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes: Buffer | null;
  readonly action: MidgardCekDataTraverseAction;
  readonly next: MidgardCekDataTraverseControl;
};

export type MidgardCekDataTraverseTrace = {
  readonly initial: MidgardCekDataTraverseControl;
  readonly steps: readonly MidgardCekDataTraverseTraceStep[];
  readonly terminal: MidgardCekDataTraverseControl;
};

export type CborArgument = {
  readonly major: number;
  readonly value: number;
  readonly nextOffset: number;
};

export type WideCborArgument = {
  readonly major: number;
  readonly value: bigint;
  readonly nextOffset: number;
};

export type SmallConstructorHead = {
  readonly constructor: bigint;
  readonly prefixLength: number;
};

export const exactUint32 = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > UINT32_MAX) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

const optionalHashIsWellFormed = (value: Uint8Array): boolean =>
  value.length === 0 || value.length === 32;

export const summaryIsWellFormed = (
  summary: MidgardCekDataSummary,
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
  control: MidgardCekDataTraverseControl,
  integer: MidgardCekDataIntegerControl,
  startsAtCursor: boolean,
  blobsValidated: boolean,
): boolean => {
  const absoluteCursor = control.sourceStart + control.offset;
  return (
    (blobsValidated
      ? isWellFormedMidgardCekDataIntegerControlWithValidatedBlob(integer)
      : isWellFormedMidgardCekDataIntegerControl(integer)) &&
    (startsAtCursor
      ? integer.sourceStart === absoluteCursor
      : integer.sourceStart + integer.sourceLength === absoluteCursor) &&
    integer.sourceStart >= control.sourceStart &&
    integer.sourceStart + integer.sourceLength <=
      control.sourceStart + control.sourceLength
  );
};

/**
 * `blobsValidated` is true only inside this package's own call chains, when
 * the source blob of the integer or bytes child is null, an initial blob, part
 * of a control validated earlier in the same chain, or a successor that passed
 * the blob machine's exit check. The child's own fields are always checked.
 * Every exported entry point validates with it false.
 */
const wellFormedTraverse = (
  control: MidgardCekDataTraverseControl,
  blobsValidated: boolean,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_TRAVERSE_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataTraverseStages.Head ||
      control.stage > MidgardCekDataTraverseStages.Terminal ||
      exactUint32(control.sourceStart, "cek_data_traverse.source_start") !==
        control.sourceStart ||
      exactUint32(control.sourceLength, "cek_data_traverse.source_length") !==
        control.sourceLength ||
      control.sourceLength === 0 ||
      !Number.isSafeInteger(control.sourceStart + control.sourceLength) ||
      exactUint32(control.offset, "cek_data_traverse.offset") !==
        control.offset ||
      control.offset > control.sourceLength ||
      !optionalHashIsWellFormed(control.frameRoot) ||
      (control.result !== null && !summaryIsWellFormed(control.result))
    ) {
      return false;
    }
    switch (control.stage) {
      case MidgardCekDataTraverseStages.Head:
        return (
          control.offset < control.sourceLength &&
          (control.frameRoot.length === 32 || control.offset === 0) &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null
        );
      case MidgardCekDataTraverseStages.Integer:
        return (
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true, blobsValidated)
        );
      case MidgardCekDataTraverseStages.Bytes:
        return (
          control.integer === null &&
          control.bytes !== null &&
          control.result === null &&
          (blobsValidated
            ? isWellFormedMidgardCekDataBytesControlWithValidatedBlob(
                control.bytes,
              )
            : isWellFormedMidgardCekDataBytesControl(control.bytes)) &&
          control.bytes.sourceStart === control.sourceStart + control.offset &&
          control.offset + control.bytes.sourceLength <= control.sourceLength
        );
      case MidgardCekDataTraverseStages.LargeConstructor:
        return (
          control.integer !== null &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, true, blobsValidated) &&
          control.offset + control.integer.sourceLength < control.sourceLength
        );
      case MidgardCekDataTraverseStages.LargeFields:
        return (
          control.integer !== null &&
          control.integer.stage === MidgardCekDataIntegerStages.Terminal &&
          control.bytes === null &&
          control.result === null &&
          nestedIntegerFits(control, control.integer, false, blobsValidated) &&
          control.offset < control.sourceLength
        );
      case MidgardCekDataTraverseStages.Close:
      case MidgardCekDataTraverseStages.Fold:
        return (
          control.frameRoot.length === 32 &&
          control.integer === null &&
          control.bytes === null &&
          control.result === null &&
          (control.stage !== MidgardCekDataTraverseStages.Close ||
            control.offset < control.sourceLength)
        );
      case MidgardCekDataTraverseStages.Terminal:
        return (
          control.offset === control.sourceLength &&
          control.frameRoot.length === 0 &&
          control.integer === null &&
          control.bytes === null &&
          control.result !== null
        );
    }
  } catch {
    return false;
  }
};

export const isWellFormedMidgardCekDataTraverseControl = (
  control: MidgardCekDataTraverseControl,
): boolean => wellFormedTraverse(control, false);

/**
 * Package-internal (not re-exported by the `cek-data-traverse` facade): the
 * checks of `isWellFormedMidgardCekDataTraverseControl`, including every
 * field of the integer or bytes child, for a control whose child source blob
 * the caller has already validated in the same synchronous call chain.
 */
export const isWellFormedMidgardCekDataTraverseControlWithValidatedBlobs = (
  control: MidgardCekDataTraverseControl,
): boolean => wellFormedTraverse(control, true);

export const initialMidgardCekDataTraverseControl = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataTraverseControl => {
  const control = {
    version: MIDGARD_CEK_DATA_TRAVERSE_VERSION,
    stage: MidgardCekDataTraverseStages.Head,
    sourceStart,
    sourceLength,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    integer: null,
    bytes: null,
    result: null,
  } satisfies MidgardCekDataTraverseControl;
  if (!isWellFormedMidgardCekDataTraverseControl(control)) {
    throw new Error("Invalid V1 CEK Data traversal source");
  }
  return control;
};

/** The child of a traversal control the caller has already validated. */
export const optionalControlCbor = (
  control: MidgardCekDataIntegerControl | MidgardCekDataBytesControl | null,
): Buffer => {
  if (control === null) return Buffer.from("d87a80", "hex");
  const nested =
    "memory" in control
      ? encodeValidatedMidgardCekDataIntegerControl(control)
      : encodeValidatedMidgardCekDataBytesControl(control);
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    nested,
    Buffer.from([0xff]),
  ]);
};
