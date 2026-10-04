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
  initialMidgardCekDataIntegerMeasureControl,
  MidgardCekDataIntegerStages,
  parseMidgardCekDataLargeConstructorSyntax,
} from "./cek-data-integer.js";
import {
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  advanced,
  attachSummary,
  exactSourceBytes,
  integerItemLength,
  sequenceHeaderStage,
  stepHeadMap,
  stepHeadScalar,
  stepHeadSequence,
} from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import { finalizeMidgardCekSourceBlob } from "./cek-source-blob.js";

/**
 * A large constructor head (tag 102 over a two-element array): the
 * constructor integer's encoded length is read from the window after the
 * three-byte prefix; its field sequence header is read once the integer is
 * streamed (`stepLargeFields`).
 */
const stepHeadLargeConstructor = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControl | null => {
  if (
    bytes.length < 3 ||
    !bytes.subarray(0, 3).equals(Buffer.from("d86682", "hex"))
  ) {
    return null;
  }
  if (bytes[3] === 0xc2 && bytes[4] === 0x5f) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.LargeConstructor,
      offset: control.offset + 3,
      integer: initialMidgardCekDataIntegerMeasureControl({
        sourceStart: control.sourceStart + control.offset + 3,
      }),
    });
  }
  const constructorCborLength = integerItemLength(bytes, 3);
  const offset = control.offset + 3;
  if (
    constructorCborLength === null ||
    offset + constructorCborLength >= control.sourceLength
  ) {
    return null;
  }
  return advanced({
    ...control,
    stage: MidgardCekDataTraverseStages.LargeConstructor,
    offset,
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
      return stepHeadScalar({ control, bytes });
    case "headSequence":
      return stepHeadSequence({ control, bytes });
    case "headMap":
      return stepHeadMap({ control, bytes });
    case "headLargeConstructor":
      return stepHeadLargeConstructor({ control, bytes });
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
    sourceEnd: control.sourceStart + control.sourceLength,
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
    sourceEnd: control.sourceStart + control.sourceLength,
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
    sourceEnd: control.sourceStart + control.sourceLength,
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
  const integer = control.integer!;
  const constructorCborRoot = finalizeMidgardCekSourceBlob(integer.blob!);
  const stage = bytes === null ? null : sequenceHeaderStage(bytes[0]);
  if (stage === null || constructorCborRoot === null) {
    return null;
  }
  const frame = initialMidgardCekDataLargeConstrFrame({
    constructorCborRoot,
    constructorCborLength: BigInt(integer.sourceLength),
    constructorMemory: integer.memory,
    tail: control.frameRoot,
  });
  return advanced({
    ...control,
    stage,
    offset: control.offset + 1,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
    integer: null,
  });
};

/**
 * `Close` reads the head window: with no action it closes the open-ended
 * frame at the authenticated 0xff break; a head action instead reads the next
 * child, whose head the window then holds.
 */
export const stepClose = ({
  control,
  sourceBytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly action: MidgardCekDataTraverseAction;
}): MidgardCekDataTraverseControl | null => {
  if (action !== null) return stepHead({ control, sourceBytes, action });
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
