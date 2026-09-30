import { initialMidgardCekDataBytesControl } from "./cek-data-bytes.js";
import {
  appendMidgardCekDataFrameChild,
  hashMidgardCekDataFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataMapFrame,
  initialMidgardCekDataSmallConstrFrame,
  type MidgardCekDataFrame,
} from "./cek-data-frame.js";
import { initialMidgardCekDataIntegerControl } from "./cek-data-integer.js";
import {
  exactUint32,
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  type MidgardCekDataTraverseStage,
  MidgardCekDataTraverseStages,
  summaryIsWellFormed,
  UINT32_MAX,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import { parseDataNodeHead } from "./cek-data-traverse.parse-data-node-head.js";
import {
  type DataParserFrame,
  nextMidgardCekDataTraverseSpan,
  type ParsedDataNode,
  parseSmallConstructorHead,
  readCanonicalCborArgument,
} from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";

export const parseMidgardCekDataNodes = (
  source: Buffer,
): readonly ParsedDataNode[] => {
  if (source.length === 0 || source.length > UINT32_MAX) {
    throw new Error("V1 CEK Data traversal source must fit uint32");
  }
  const nodes: ParsedDataNode[] = [];
  const frames: DataParserFrame[] = [];
  const root = parseDataNodeHead(source, 0);
  nodes.push(root.node);
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
    const child = parseDataNodeHead(source, cursor);
    const childIndex = nodes.length;
    nodes.push(child.node);
    node.children.push(childIndex);
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
  return nodes;
};

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

const nextParentStage = (
  frame: MidgardCekDataFrame,
): MidgardCekDataTraverseStage => {
  if (frame.childCount < frame.expectedChildren) {
    return MidgardCekDataTraverseStages.Head;
  }
  return frame.kind === "map"
    ? MidgardCekDataTraverseStages.Fold
    : MidgardCekDataTraverseStages.Close;
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
      pendingLargeExpectedChildren: null,
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
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result: null,
  });
};

export const stepHeadScalar = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headScalar" }
  >;
}): MidgardCekDataTraverseControl | null => {
  const itemLength = exactUint32(
    action.itemLength,
    "cek_data_traverse.scalar_length",
  );
  if (itemLength === 0 || control.offset + itemLength > control.sourceLength) {
    return null;
  }
  const first = bytes[0]!;
  if (first >>> 5 <= 1 || first === 0xc2 || first === 0xc3) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Integer,
      integer: initialMidgardCekDataIntegerControl({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  if (first >>> 5 === 2) {
    return advanced({
      ...control,
      stage: MidgardCekDataTraverseStages.Bytes,
      bytes: initialMidgardCekDataBytesControl({
        sourceStart: control.sourceStart + control.offset,
        sourceLength: itemLength,
      }),
    });
  }
  return null;
};

export const stepHeadSequence = ({
  control,
  bytes,
  action,
}: {
  readonly control: MidgardCekDataTraverseControl;
  readonly bytes: Buffer;
  readonly action: Extract<
    MidgardCekDataTraverseAction,
    { readonly kind: "headSequence" }
  >;
}): MidgardCekDataTraverseControl | null => {
  const expectedChildren = exactUint32(
    action.expectedChildren,
    "cek_data_traverse.expected_children",
  );
  const sequenceHeader = expectedChildren === 0 ? 0x80 : 0x9f;
  const small = parseSmallConstructorHead(bytes);
  let frame: MidgardCekDataFrame;
  let headLength: number;
  if (small !== null) {
    if (bytes[small.prefixLength] !== sequenceHeader) return null;
    frame = initialMidgardCekDataSmallConstrFrame({
      constructor: small.constructor,
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = small.prefixLength + 1;
  } else {
    if (bytes[0] !== sequenceHeader) return null;
    frame = initialMidgardCekDataListFrame({
      tail: control.frameRoot,
      expectedChildren,
    });
    headLength = 1;
  }
  return advanced({
    ...control,
    stage:
      expectedChildren === 0
        ? MidgardCekDataTraverseStages.Fold
        : MidgardCekDataTraverseStages.Head,
    offset: control.offset + headLength,
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
