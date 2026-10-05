import {
  initialMidgardCekDataBytesControl,
  initialMidgardCekDataBytesMeasureControl,
} from "./cek-data-bytes.js";
import {
  appendMidgardCekDataFrameChild,
  hashMidgardCekDataFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataMapFrame,
  initialMidgardCekDataSmallConstrFrame,
  type MidgardCekDataFrame,
} from "./cek-data-frame.js";
import {
  initialMidgardCekDataIntegerControl,
  initialMidgardCekDataIntegerMeasureControl,
} from "./cek-data-integer.js";
import {
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseControl,
  type MidgardCekDataTraverseStage,
  MidgardCekDataTraverseStages,
  summaryIsWellFormed,
  UINT32_MAX,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import { hasNonCanonicalDataHead } from "./cek-data-traverse.noncanonical-sequence-head.js";
import { parseDataNodeHead } from "./cek-data-traverse.parse-data-node-head.js";
import {
  type DataParserFrame,
  nextMidgardCekDataTraverseSpan,
  type ParsedDataNode,
  parseSmallConstructorHead,
  readCanonicalCborArgument,
} from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";

const parseDataNodes = (
  source: Buffer,
  stopAtSequenceRefusal: boolean,
): {
  readonly nodes: readonly ParsedDataNode[];
  readonly refusalOffset: number | null;
} => {
  if (source.length === 0 || source.length > UINT32_MAX) {
    throw new Error("V1 CEK Data traversal source must fit uint32");
  }
  const nodes: ParsedDataNode[] = [];
  const frames: DataParserFrame[] = [];
  if (stopAtSequenceRefusal && hasNonCanonicalDataHead(source)) {
    return { nodes, refusalOffset: 0 };
  }
  const root = parseDataNodeHead(source, 0, stopAtSequenceRefusal);
  nodes.push(root.node);
  if (
    root.node.kind !== "scalar" &&
    "refusalOffset" in root &&
    root.refusalOffset !== undefined
  )
    return { nodes, refusalOffset: root.refusalOffset };
  let cursor = root.nextOffset;
  if (root.remainingChildren !== 0) {
    frames.push({
      nodeIndex: 0,
      remainingChildren: root.remainingChildren,
    });
  } else if (root.node.kind !== "scalar") {
    root.node.end = cursor;
  }

  while (frames.length > 0) {
    const frame = frames[frames.length - 1]!;
    const node = nodes[frame.nodeIndex]!;
    if (node.kind === "scalar") {
      throw new Error("V1 CEK Data parser frame cannot be scalar");
    }
    const closesWithBreak = node.kind !== "map" && node.closesWithBreak;
    const isComplete =
      frame.remainingChildren === 0 ||
      (frame.remainingChildren === null && source[cursor] === 0xff);
    if (isComplete) {
      if (closesWithBreak) {
        if (
          frame.remainingChildren !== null ||
          node.children.length === 0 ||
          source[cursor] !== 0xff
        ) {
          throw new Error(
            "V1 CEK Data traversal rejected a noncanonical sequence",
          );
        }
        cursor += 1;
      }
      node.end = cursor;
      frames.pop();
      continue;
    }
    if (cursor >= source.length || source[cursor] === 0xff) {
      throw new Error("V1 CEK Data traversal rejected an incomplete container");
    }
    if (
      stopAtSequenceRefusal &&
      hasNonCanonicalDataHead(source.subarray(cursor))
    ) {
      const refusalOffset = cursor;
      // The next operation names the refusal cursor rather than a parsed Data node.
      node.children.push(nodes.length);
      // Open-ended ancestor frames derive their arity from authenticated traversal,
      // so unvisited suffix nodes need not be parsed or assigned invented summaries.
      return { nodes, refusalOffset };
    }
    const child = parseDataNodeHead(source, cursor, stopAtSequenceRefusal);
    const childIndex = nodes.length;
    nodes.push(child.node);
    node.children.push(childIndex);
    if (
      child.node.kind !== "scalar" &&
      "refusalOffset" in child &&
      child.refusalOffset !== undefined
    )
      return { nodes, refusalOffset: child.refusalOffset };
    if (frame.remainingChildren !== null) {
      frame.remainingChildren -= 1;
    }
    cursor = child.nextOffset;
    if (child.remainingChildren !== 0) {
      frames.push({
        nodeIndex: childIndex,
        remainingChildren: child.remainingChildren,
      });
    } else if (child.node.kind !== "scalar") {
      child.node.end = cursor;
    }
  }

  if (nodes[0]!.kind === "scalar") {
    cursor = nodes[0]!.end;
  }
  if (cursor !== source.length) {
    throw new Error("V1 CEK Data traversal rejected trailing source bytes");
  }
  return { nodes, refusalOffset: null };
};

export const parseMidgardCekDataNodes = (
  source: Buffer,
): readonly ParsedDataNode[] => parseDataNodes(source, false).nodes;

export const parseMidgardCekDataNodesPrefix = (source: Buffer) =>
  parseDataNodes(source, true);

export const exactSourceBytes = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly sourceBytes?: Uint8Array | null;
}): Buffer | null => {
  const span = nextMidgardCekDataTraverseSpan(control);
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

export const advanced = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataTraverseControl | null =>
  isWellFormedMidgardCekDataTraverseControl(control) ? control : null;

/**
 * After a child attaches, a map continues to its next header-counted child or
 * to its fold; every open-ended frame moves to `Close`, whose head window
 * either holds the authenticated 0xff break or the next child's head.
 */
const nextParentStage = (
  frame: MidgardCekDataFrame,
): MidgardCekDataTraverseStage => {
  if (frame.kind === "map") {
    return frame.childCount < frame.expectedChildren
      ? MidgardCekDataTraverseStages.Head
      : MidgardCekDataTraverseStages.Fold;
  }
  return MidgardCekDataTraverseStages.Close;
};

export const attachSummary = ({
  control,
  summary,
  parent,
  offset,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly summary: MidgardCekDataSummary;
  readonly parent: MidgardCekDataFrame | null;
  readonly offset: number;
}): MidgardCekDataTraverseControl | null => {
  if (!summaryIsWellFormed(summary)) return null;
  if (control.frameRoot.length === 0) {
    if (parent !== null || offset !== control.sourceLength) {
      return null;
    }
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Terminal,
      offset,
      frameRoot: Buffer.alloc(0),
      integer: null,
      bytes: null,
      result: summary,
    });
  }
  if (
    parent === null ||
    !hashMidgardCekDataFrame(parent).equals(control.frameRoot)
  ) {
    return null;
  }
  const nextParent = appendMidgardCekDataFrameChild(parent, summary);
  if (nextParent === null) return null;
  return advanced({
    ...control,
    stage: nextParentStage(nextParent),
    offset,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(nextParent)),
    integer: null,
    bytes: null,
    result: null,
  });
};

/**
 * The encoded length of the integer item whose head starts at `offset` of the
 * authenticated window: the canonical argument of a major-0/1 head, or a c2/c3
 * tag with its definite major-2 magnitude. Anything else (including an
 * indefinite magnitude) has no length here and fails closed.
 */
export const integerItemLength = (
  bytes: Uint8Array,
  offset: number,
): number | null => {
  if (offset >= bytes.length) return null;
  const first = bytes[offset]!;
  if (first >>> 5 <= 1) {
    const argument = readCanonicalCborArgument(bytes, offset);
    return argument === null ? null : argument.nextOffset - offset;
  }
  if (first !== 0xc2 && first !== 0xc3) return null;
  const magnitude = readCanonicalCborArgument(bytes, offset + 1);
  return magnitude === null || magnitude.major !== 2 || magnitude.value > 64
    ? null
    : magnitude.nextOffset - offset + magnitude.value;
};

/**
 * A scalar head: the integer or definite byte-string length is read from the
 * window; an indefinite byte string (0x5f) starts the bytes sub-control's
 * measuring pass instead. The derived extent must fit the source.
 */
export const stepHeadScalar = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControl | null => {
  const first = bytes[0]!;
  const sourceStart = control.sourceStart + control.offset;
  if (first === 0x5f) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Bytes,
      bytes: initialMidgardCekDataBytesMeasureControl({ sourceStart }),
    });
  }
  if (first >>> 5 === 2) {
    const argument = readCanonicalCborArgument(bytes, 0);
    if (argument === null) return null;
    const itemLength = argument.nextOffset + argument.value;
    if (control.offset + itemLength > control.sourceLength) return null;
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Bytes,
      bytes: initialMidgardCekDataBytesControl({
        sourceStart,
        sourceLength: itemLength,
      }),
    });
  }
  if ((first === 0xc2 || first === 0xc3) && bytes[1] === 0x5f) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Integer,
      integer: initialMidgardCekDataIntegerMeasureControl({ sourceStart }),
    });
  }
  const itemLength = integerItemLength(bytes, 0);
  if (
    itemLength === null ||
    control.offset + itemLength > control.sourceLength
  ) {
    return null;
  }
  return advanced({
    ...control,
    stage: MidgardCekDataTraverseStages.Integer,
    integer: initialMidgardCekDataIntegerControl({
      sourceStart,
      sourceLength: itemLength,
    }),
  });
};

/**
 * The stage an open-ended sequence header opens: 0x80 is the canonical empty
 * sequence (straight to the fold), 0x9f an indefinite one whose first child is
 * mandatory (`Head` has no close transition); every later child or the 0xff
 * break is read at `Close`. Any other header fails closed.
 */
export const sequenceHeaderStage = (
  header: number | undefined,
): MidgardCekDataTraverseStage | null =>
  header === 0x80
    ? MidgardCekDataTraverseStages.Fold
    : header === 0x9f
      ? MidgardCekDataTraverseStages.Head
      : null;

export const stepHeadSequence = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControl | null => {
  const small = parseSmallConstructorHead(bytes);
  const headerOffset = small === null ? 0 : small.prefixLength;
  if (headerOffset >= bytes.length) return null;
  const stage = sequenceHeaderStage(bytes[headerOffset]);
  if (stage === null) return null;
  const frame =
    small === null
      ? initialMidgardCekDataListFrame({ tail: control.frameRoot })
      : initialMidgardCekDataSmallConstrFrame({
          constructor: small.constructor,
          tail: control.frameRoot,
        });
  return advanced({
    ...control,
    stage,
    offset: control.offset + headerOffset + 1,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
  });
};

export const stepHeadMap = ({
  control,
  bytes,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
}): MidgardCekDataTraverseControl | null => {
  const argument = readCanonicalCborArgument(bytes, 0);
  if (
    argument === null ||
    argument.major !== 5 ||
    argument.value > Math.floor(UINT32_MAX / 2)
  ) {
    return null;
  }
  const expectedChildren = argument.value * 2;
  const frame = initialMidgardCekDataMapFrame({
    tail: control.frameRoot,
    expectedChildren,
  });
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + argument.nextOffset,
    frameRoot: Buffer.from(hashMidgardCekDataFrame(frame)),
  });
};
