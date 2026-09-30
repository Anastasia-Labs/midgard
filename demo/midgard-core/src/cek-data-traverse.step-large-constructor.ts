import {
  advanceMidgardCekDataBytes,
  finalizeMidgardCekDataBytes,
  MidgardCekDataBytesStages,
} from "./cek-data-bytes.js";
import {
  finalizeMidgardCekDataFrame,
  hashMidgardCekDataFrame,
  initialMidgardCekDataLargeConstrFrame,
} from "./cek-data-frame.js";
import {
  advanceMidgardCekDataInteger,
  finalizeMidgardCekDataInteger,
  initialMidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
  parseMidgardCekDataLargeConstructorSyntax,
} from "./cek-data-integer.js";
import {
  exactUint32,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  advanced,
  attachSummary,
  exactSourceBytes,
  stepHeadMap,
  stepHeadScalar,
  stepHeadSequence,
} from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import { finalizeMidgardCekSourceBlob } from "./cek-source-blob.js";

const stepHeadLargeConstructor = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headLargeConstructor" }
  >;
}): MidgardCekDataTraverseControl | null => {
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
    !bytes.subarray(0, 3).equals(Buffer.from("d86682", "hex")) ||
    control.offset + 3 + constructorCborLength >= control.sourceLength
  ) {
    return null;
  }
  const offset = control.offset + 3;
  return advanced({
    ...control,
    stage: MidgardCekDataTraverseStages.LargeConstructor,
    offset,
    pendingLargeExpectedChildren: expectedChildren,
    integer: initialMidgardCekDataIntegerControl({
      sourceStart: control.sourceStart + offset,
      sourceLength: constructorCborLength,
    }),
  });
};

export const stepHead = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const bytes = exactSourceBytes({ control, sourceBytes });
  if (bytes === null || action === null) return null;
  // Non-head actions are rejected by this phase-specific dispatcher.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
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

export const stepInteger = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStages.Terminal) {
    if (sourceBytes !== null && sourceBytes !== undefined) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataInteger(integer);
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
  const nextInteger = advanceMidgardCekDataInteger({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

export const stepBytes = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const byteControl = control.bytes!;
  if (byteControl.stage === MidgardCekDataBytesStages.Terminal) {
    if (sourceBytes !== null && sourceBytes !== undefined) {
      return null;
    }
    if (action === null || action.kind !== "attachScalar") {
      return null;
    }
    const summary = finalizeMidgardCekDataBytes(byteControl);
    return summary === null
      ? null
      : attachSummary({
          control,
          summary,
          parent: action.parent,
          offset: control.offset + byteControl.sourceLength,
        });
  }
  if (action !== null) return null;
  const nextBytes = advanceMidgardCekDataBytes({
    control: byteControl,
    sourceBytes,
  });
  return nextBytes === null ? null : advanced({ ...control, bytes: nextBytes });
};

export const stepLargeConstructor = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  const integer = control.integer!;
  if (integer.stage === MidgardCekDataIntegerStages.Terminal) {
    if (
      action !== null ||
      (sourceBytes !== null && sourceBytes !== undefined)
    ) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.LargeFields,
      offset: control.offset + integer.sourceLength,
    });
  }
  if (action !== null) return null;
  if (
    integer.stage === MidgardCekDataIntegerStages.Syntax &&
    (sourceBytes === null ||
      sourceBytes === undefined ||
      parseMidgardCekDataLargeConstructorSyntax({
        syntaxBytes: sourceBytes,
        sourceLength: integer.sourceLength,
      }) === null)
  ) {
    return null;
  }
  const nextInteger = advanceMidgardCekDataInteger({
    control: integer,
    sourceBytes,
  });
  return nextInteger === null
    ? null
    : advanced({ ...control, integer: nextInteger });
};

export const stepLargeFields = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  const expectedChildren = control.pendingLargeExpectedChildren!;
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const integer = control.integer!;
  const constructorCborRoot = finalizeMidgardCekSourceBlob(integer.blob!);
  if (
    bytes === null ||
    bytes[0] !== sequenceHeader ||
    constructorCborRoot === null
  ) {
    return null;
  }
  const frame = initialMidgardCekDataLargeConstrFrame({
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
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + 1,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
    pendingLargeExpectedChildren: null,
    integer: null,
  });
};

export const stepClose = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (action !== null) return null;
  const bytes = exactSourceBytes({ control, sourceBytes });
  return bytes !== null && bytes[0] === 0xff
    ? advanced({
        ...control,
        stage: MidgardCekDataTraverseStages.Fold,
        offset: control.offset + 1,
      })
    : null;
};

export const stepFinalizeFrame = ({
  control,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "finalizeFrame" }
  >;
}): MidgardCekDataTraverseControl | null => {
  if (!hashMidgardCekDataFrame(action.frame).equals(control.frameRoot)) {
    return null;
  }
  const summary = finalizeMidgardCekDataFrame(action.frame);
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
    !hashMidgardCekDataFrame(action.parent).equals(action.frame.tail)
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
