import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
} from "@al-ft/midgard-core";
import { DataConstr, DataList } from "@harmoniclabs/plutus-data";

import {
  hashMidgardCekDirectValueWitness,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import {
  constantMemoryFromPayloadNode,
  exactTopMaterial,
} from "./cek-machine.data-node-topology-matches.js";
import {
  type MidgardCekSemanticBuiltinWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  constantParts,
  dataNodeSummary,
  type DataSequenceSummary,
  listSequenceFromNode,
} from "./cek-machine.verify-case-select.js";
import { dataListLinkMatches } from "./cek-machine.verify-map-conversion-start.js";
import {
  listDataSummary,
  prependListSequence,
  resultMatchesParts,
  sameConstantType,
  sameDataSummary,
  semanticSummary,
} from "./cek-machine.verify-semantic-builtin-control.js";

export const semanticListSource = (
  source: MidgardCekDirectValueWitness,
  node: MidgardCekDataNode,
  lists: readonly MidgardCekDataListNode[],
): {
  readonly element: MidgardCekConstantType;
  readonly sequence: DataSequenceSummary;
  readonly memory: bigint;
} | null => {
  const parts = constantParts(source);
  const sequence = listSequenceFromNode(node);
  if (
    parts === null ||
    parts.type.kind !== "list" ||
    sequence === null ||
    !exactTopMaterial(parts.payload, node, lists, []) ||
    (source.kind !== "constant" &&
      parts.memory !== constantMemoryFromPayloadNode(parts.type, node))
  ) {
    return null;
  }
  return {
    element: parts.type.element,
    sequence,
    memory: parts.memory,
  };
};

export const verifySemanticList = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (
    material.pairNodes.length !== 0 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  if (tag === 31n || tag === 35n) {
    if (material.dataNodes.length !== 1) return false;
    const node = material.dataNodes[0]!;
    const expectedListCount =
      node.kind === "list" && node.itemsCount > 0n ? 1 : 0;
    if (material.listNodes.length !== expectedListCount) return false;
    const source = semanticListSource(arguments_[0]!, node, material.listNodes);
    if (source === null) return false;
    if (tag === 31n) {
      if (arguments_.length !== 3) return false;
      const selected =
        source.sequence.length === 0n ? arguments_[1]! : arguments_[2]!;
      return sameBytes(
        hashMidgardCekDirectValueWitness(result),
        hashMidgardCekDirectValueWitness(selected),
      );
    }
    return (
      arguments_.length === 1 &&
      resultMatchesParts(
        result,
        { kind: "boolean" },
        semanticSummary(
          new DataConstr(source.sequence.length === 0n ? 1n : 0n, []),
        ),
        1n,
      )
    );
  }
  if (tag === 32n) {
    if (arguments_.length !== 2 || material.dataNodes.length !== 1) {
      return false;
    }
    const node = material.dataNodes[0]!;
    const expectedListCount =
      node.kind === "list" && node.itemsCount > 0n ? 1 : 0;
    if (material.listNodes.length !== expectedListCount) return false;
    const source = semanticListSource(arguments_[1]!, node, material.listNodes);
    const item = constantParts(arguments_[0]!);
    if (
      source === null ||
      item === null ||
      !sameConstantType(item.type, source.element)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "list", element: source.element },
      listDataSummary(prependListSequence(item.payload, source.sequence)),
      item.memory + source.memory,
    );
  }
  if (
    (tag !== 33n && tag !== 34n) ||
    arguments_.length !== 1 ||
    material.dataNodes.length !== 2 ||
    (material.listNodes.length !== 1 && material.listNodes.length !== 2)
  ) {
    return false;
  }
  const [sourceNode, headNode] = material.dataNodes;
  const [firstLink, tailLink] = material.listNodes;
  if (
    sourceNode === undefined ||
    headNode === undefined ||
    firstLink === undefined ||
    firstLink.length <= 0n ||
    (firstLink.length === 1n) !== (tailLink === undefined)
  ) {
    return false;
  }
  const source = semanticListSource(arguments_[0]!, sourceNode, [firstLink]);
  let headMemory: bigint | null;
  if (arguments_[0]?.kind === "constant" && source !== null) {
    const decoded = decodeMidgardCekConstantWitness(arguments_[0].witness);
    if (
      !(decoded.payload instanceof DataList) ||
      decoded.payload.list.length === 0 ||
      !sameDataSummary(
        semanticSummary(decoded.payload.list[0]!),
        dataNodeSummary(headNode),
      )
    ) {
      return false;
    }
    headMemory = midgardCekConstantMemorySize(
      source.element,
      decoded.payload.list[0]!,
    );
  } else {
    headMemory = constantMemoryFromPayloadNode(
      source?.element ?? { kind: "data" },
      headNode,
    );
  }
  if (
    source === null ||
    headMemory === null ||
    firstLink.length !== source.sequence.length ||
    !dataListLinkMatches(firstLink, headNode, tailLink ?? null)
  ) {
    return false;
  }
  if (tag === 33n) {
    return resultMatchesParts(
      result,
      source.element,
      dataNodeSummary(headNode),
      headMemory,
    );
  }
  const tail: DataSequenceSummary = {
    root: firstLink.tail,
    length: firstLink.length - 1n,
    payloadCborLength: firstLink.payloadCborLength - firstLink.headCborLength,
    memory: firstLink.memory - firstLink.headMemory,
  };
  return resultMatchesParts(
    result,
    { kind: "list", element: source.element },
    listDataSummary(tail),
    source.memory - headMemory,
  );
};

export const verifySemanticChooseData = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (
    arguments_.length !== 6 ||
    material.dataNodes.length !== 1 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const source = constantParts(arguments_[0]!);
  const node = material.dataNodes[0]!;
  if (
    source === null ||
    source.type.kind !== "data" ||
    source.memory !== source.payload.memory ||
    !exactTopMaterial(
      source.payload,
      node,
      material.listNodes,
      material.pairNodes,
    )
  ) {
    return false;
  }
  const selected =
    node.kind === "constrSmall" || node.kind === "constrLarge"
      ? arguments_[1]!
      : node.kind === "map"
        ? arguments_[2]!
        : node.kind === "list"
          ? arguments_[3]!
          : node.kind === "integer"
            ? arguments_[4]!
            : arguments_[5]!;
  return sameBytes(
    hashMidgardCekDirectValueWitness(result),
    hashMidgardCekDirectValueWitness(selected),
  );
};
