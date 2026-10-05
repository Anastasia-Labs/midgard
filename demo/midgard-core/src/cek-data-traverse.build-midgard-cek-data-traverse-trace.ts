import {
  finalizeMidgardCekDataBytes,
  MidgardCekDataBytesStages,
} from "./cek-data-bytes.js";
import {
  appendMidgardCekDataFrameChild,
  finalizeMidgardCekDataFrame,
  foldMidgardCekDataFrameListChild,
  foldMidgardCekDataFrameMapPair,
  hashMidgardCekDataFrame,
  hashMidgardCekDataFrameChild,
  initialMidgardCekDataLargeConstrFrame,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataMapFrame,
  initialMidgardCekDataSmallConstrFrame,
  type MidgardCekDataFrame,
} from "./cek-data-frame.js";
import {
  finalizeMidgardCekDataInteger,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import {
  initialMidgardCekDataTraverseControl,
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseStage,
  MidgardCekDataTraverseStages,
  type MidgardCekDataTraverseTrace,
  type MidgardCekDataTraverseTraceStep,
} from "./cek-data-traverse.is-well-formed-midgard-cek-data-traverse-control.js";
import {
  parseMidgardCekDataNodes,
  parseMidgardCekDataNodesPrefix,
} from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import {
  type DataTraceFrame,
  type DataTraceOperation,
  nextMidgardCekDataTraverseSpan,
  type ParsedDataNode,
  readCanonicalCborArgument,
} from "./cek-data-traverse.read-canonical-cbor-argument-wide.js";
import {
  advanceMidgardCekDataTraverse,
  finalizeMidgardCekDataTraverse,
} from "./cek-data-traverse.step-fold.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import { finalizeMidgardCekSourceBlob } from "./cek-source-blob.js";
import { buildMidgardValidationMerkleMembershipIndex } from "./validation-merkle.js";

const buildDataTrace = ({
  sourceStart,
  source,
  stopAtSequenceRefusal,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
  readonly stopAtSequenceRefusal: boolean;
}) => {
  const bytes = Buffer.from(source);
  const parsed = stopAtSequenceRefusal
    ? parseMidgardCekDataNodesPrefix(bytes)
    : { nodes: parseMidgardCekDataNodes(bytes), refusalOffset: null };
  const nodes = parsed.nodes;
  const initial = initialMidgardCekDataTraverseControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataTraverseTraceStep[] = [];
  let control = initial;
  const currentStage = (): MidgardCekDataTraverseStage => control.stage;

  const emit = (action: MidgardCekDataTraverseAction): void => {
    const span = nextMidgardCekDataTraverseSpan(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataTraverse({
      control,
      sourceBytes,
      action,
    });
    if (next === null || !isWellFormedMidgardCekDataTraverseControl(next)) {
      throw new Error("V1 CEK Data traversal evidence failed closed");
    }
    steps.push({
      control,
      sourceBytes: sourceBytes === null ? null : Buffer.from(sourceBytes),
      action,
      next,
    });
    control = next;
  };

  const appendToParent = (
    parent: DataTraceFrame | null,
    summary: MidgardCekDataSummary,
  ): void => {
    if (parent === null) return;
    const next = appendMidgardCekDataFrameChild(parent.frame, summary);
    if (next === null) {
      throw new Error("V1 CEK Data traversal rejected a child summary");
    }
    parent.frame = next;
    parent.childSummaries.push(summary);
  };

  const initialFrame = (
    node: Exclude<ParsedDataNode, { readonly kind: "scalar" }>,
    parent: DataTraceFrame | null,
    largeConstructor: {
      readonly root: Buffer;
      readonly memory: bigint;
    } | null,
  ): MidgardCekDataFrame => {
    const tail =
      parent === null
        ? Buffer.alloc(0)
        : Buffer.from(hashMidgardCekDataFrame(parent.frame));
    switch (node.kind) {
      case "list":
        return initialMidgardCekDataListFrame({ tail });
      case "map": {
        // A prefix may stop before all declared map entries have been visited.
        // The nullary head action derives this count from the same original header.
        const header = readCanonicalCborArgument(bytes, node.start);
        if (header === null || header.major !== 5)
          throw new Error(
            "V1 CEK Data traversal lost its canonical map header",
          );
        return initialMidgardCekDataMapFrame({
          tail,
          expectedChildren: header.value * 2,
        });
      }
      case "constrSmall":
        return initialMidgardCekDataSmallConstrFrame({
          constructor: node.constructor,
          tail,
        });
      case "constrLarge":
        if (largeConstructor === null) {
          throw new Error("V1 CEK Data traversal lost a large constructor");
        }
        return initialMidgardCekDataLargeConstrFrame({
          constructorCborRoot: largeConstructor.root,
          constructorCborLength: BigInt(node.constructorCborLength),
          constructorMemory: largeConstructor.memory,
          tail,
        });
    }
  };

  const operations: DataTraceOperation[] = [
    { kind: "visit", nodeIndex: 0, parent: null },
  ];
  while (operations.length > 0) {
    const operation = operations.pop()!;
    if (operation.kind === "visit") {
      if (
        parsed.refusalOffset !== null &&
        operation.nodeIndex === nodes.length
      ) {
        if (
          (control.stage !== MidgardCekDataTraverseStages.Head &&
            control.stage !== MidgardCekDataTraverseStages.Close) ||
          control.offset !== parsed.refusalOffset
        )
          throw new Error("sequence refusal prefix lost its source position");
        return {
          initial,
          steps: Object.freeze(steps),
          control,
          refusalOffset: parsed.refusalOffset,
        };
      }
      const node = nodes[operation.nodeIndex]!;
      // A first child is read at `Head`; every later child of an open-ended
      // frame is read from the `Close` window that would otherwise hold the
      // break.
      if (
        (control.stage !== MidgardCekDataTraverseStages.Head &&
          control.stage !== MidgardCekDataTraverseStages.Close) ||
        control.offset !== node.start
      ) {
        throw new Error("V1 CEK Data traversal evidence lost source position");
      }
      if (node.kind === "scalar") {
        emit({ kind: "headScalar" });
        while (
          (currentStage() === MidgardCekDataTraverseStages.Integer &&
            control.integer!.stage !== MidgardCekDataIntegerStages.Terminal) ||
          (currentStage() === MidgardCekDataTraverseStages.Bytes &&
            control.bytes!.stage !== MidgardCekDataBytesStages.Terminal)
        ) {
          emit(null);
        }
        const summary =
          control.integer !== null
            ? finalizeMidgardCekDataInteger(control.integer)
            : finalizeMidgardCekDataBytes(control.bytes!);
        if (summary === null) {
          throw new Error("V1 CEK Data traversal rejected a scalar");
        }
        emit({
          kind: "attachScalar",
          parent: operation.parent?.frame ?? null,
        });
        appendToParent(operation.parent, summary);
        continue;
      }

      let largeConstructor: {
        readonly root: Buffer;
        readonly memory: bigint;
      } | null = null;
      if (node.kind === "map") {
        emit({ kind: "headMap" });
      } else if (node.kind === "constrLarge") {
        emit({ kind: "headLargeConstructor" });
        while (
          currentStage() === MidgardCekDataTraverseStages.LargeConstructor
        ) {
          emit(null);
        }
        if (
          currentStage() !== MidgardCekDataTraverseStages.LargeFields ||
          control.integer === null ||
          control.integer.blob === null
        ) {
          throw new Error("V1 CEK Data traversal rejected a large constructor");
        }
        if (
          parsed.refusalOffset !== null &&
          control.offset === parsed.refusalOffset
        ) {
          return {
            initial,
            steps: Object.freeze(steps),
            control,
            refusalOffset: parsed.refusalOffset,
          };
        }
        const root = finalizeMidgardCekSourceBlob(control.integer.blob);
        if (root === null) {
          throw new Error("V1 CEK Data traversal lost constructor bytes");
        }
        largeConstructor = {
          root: Buffer.from(root),
          memory: control.integer.memory,
        };
      } else {
        emit({ kind: "headSequence" });
      }
      const frame = initialFrame(node, operation.parent, largeConstructor);
      if (node.kind === "constrLarge") {
        emit(null);
      }
      const context: DataTraceFrame = {
        frame,
        childSummaries: [],
        parent: operation.parent,
        node,
      };
      operations.push({ kind: "finish", context });
      for (let index = node.children.length - 1; index >= 0; index -= 1) {
        operations.push({
          kind: "visit",
          nodeIndex: node.children[index]!,
          parent: context,
        });
      }
      continue;
    }

    const { context } = operation;
    const { node, childSummaries } = context;
    if (
      context.frame.childCount !== node.children.length ||
      childSummaries.length !== node.children.length
    ) {
      throw new Error("V1 CEK Data traversal evidence lost container children");
    }
    if (node.kind !== "map" && node.closesWithBreak) {
      emit(null);
    }
    const leaves = childSummaries.map((child, index) =>
      hashMidgardCekDataFrameChild(index, child),
    );
    const memberships = buildMidgardValidationMerkleMembershipIndex(leaves);
    let frame = context.frame;
    if (node.kind === "map") {
      for (
        let pairIndex = childSummaries.length / 2 - 1;
        pairIndex >= 0;
        pairIndex -= 1
      ) {
        const keyIndex = pairIndex * 2;
        const valueIndex = keyIndex + 1;
        const key = childSummaries[keyIndex]!;
        const value = childSummaries[valueIndex]!;
        const keySiblings = memberships.membershipAt(keyIndex).siblings;
        const valueSiblings = memberships.membershipAt(valueIndex).siblings;
        emit({
          kind: "foldMap",
          frame,
          pairIndex,
          key,
          value,
          keySiblings,
          valueSiblings,
        });
        const next = foldMidgardCekDataFrameMapPair({
          frame,
          pairIndex,
          key,
          value,
          keySiblings,
          valueSiblings,
        });
        if (next === null) {
          throw new Error("V1 CEK Data traversal rejected a map fold");
        }
        frame = next;
      }
    } else {
      for (
        let childIndex = childSummaries.length - 1;
        childIndex >= 0;
        childIndex -= 1
      ) {
        const child = childSummaries[childIndex]!;
        const siblings = memberships.membershipAt(childIndex).siblings;
        emit({
          kind: "foldList",
          frame,
          childIndex,
          child,
          siblings,
        });
        const next = foldMidgardCekDataFrameListChild({
          frame,
          childIndex,
          child,
          siblings,
        });
        if (next === null) {
          throw new Error("V1 CEK Data traversal rejected a sequence fold");
        }
        frame = next;
      }
    }
    const summary = finalizeMidgardCekDataFrame(frame);
    if (summary === null) {
      throw new Error("V1 CEK Data traversal rejected container finalization");
    }
    emit({
      kind: "finalizeFrame",
      frame,
      parent: context.parent?.frame ?? null,
    });
    appendToParent(context.parent, summary);
  }

  if (
    currentStage() !== MidgardCekDataTraverseStages.Terminal ||
    finalizeMidgardCekDataTraverse(control) === null
  ) {
    throw new Error("V1 CEK Data traversal evidence did not terminate");
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    control,
    refusalOffset: null,
  });
};

export const buildMidgardCekDataTraversePrefix = (input: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}) => buildDataTrace({ ...input, stopAtSequenceRefusal: true });
export const buildMidgardCekDataTraverseTrace = (input: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataTraverseTrace => {
  const trace = buildDataTrace({ ...input, stopAtSequenceRefusal: false });
  return Object.freeze({
    initial: trace.initial,
    steps: trace.steps,
    terminal: trace.control,
  });
};
