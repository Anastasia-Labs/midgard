import { commitMidgardCekBlob } from "@al-ft/midgard-core";
import { DataB, DataConstr, DataI } from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekPlutusData,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import {
  commitMidgardCekDataTree,
  encodeMidgardCekDataTreeInteger,
} from "./cek-data-tree.js";
import {
  canonicalBytesLeaf,
  canonicalIntegerLeaf,
  directUnit,
  exactTopMaterial,
} from "./cek-machine.data-node-topology-matches.js";
import {
  type MidgardCekSemanticBuiltinWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  constantParts,
  type DataSequenceSummary,
  listSequenceFromNode,
} from "./cek-machine.verify-case-select.js";
import {
  constrDataSummary,
  emptyListSequence,
  listDataSummary,
  pairDataSummary,
  resultMatchesParts,
  sameDataSummary,
  semanticSummary,
} from "./cek-machine.verify-semantic-builtin-control.js";
import { semanticListSource } from "./cek-machine.verify-semantic-list.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

export const verifySemanticData = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (tag === 47n) {
    if (
      arguments_.length !== 2 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const left = constantParts(arguments_[0]!);
    const right = constantParts(arguments_[1]!);
    if (
      left?.type.kind !== "data" ||
      right?.type.kind !== "data" ||
      left.memory !== left.payload.memory ||
      right.memory !== right.payload.memory
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "boolean" },
      semanticSummary(
        new DataConstr(
          sameDataSummary(left.payload, right.payload) ? 1n : 0n,
          [],
        ),
      ),
      1n,
    );
  }
  if (tag === 48n) {
    if (
      arguments_.length !== 2 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const first = constantParts(arguments_[0]!);
    const second = constantParts(arguments_[1]!);
    if (
      first?.type.kind !== "data" ||
      second?.type.kind !== "data" ||
      first.memory !== first.payload.memory ||
      second.memory !== second.payload.memory
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      {
        kind: "pair",
        first: { kind: "data" },
        second: { kind: "data" },
      },
      pairDataSummary(first.payload, second.payload),
      first.memory + second.memory,
    );
  }
  if (tag === 49n || tag === 50n) {
    if (
      arguments_.length !== 1 ||
      !directUnit(arguments_[0]!) ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const element: MidgardCekConstantType =
      tag === 49n
        ? { kind: "data" }
        : {
            kind: "pair",
            first: { kind: "data" },
            second: { kind: "data" },
          };
    return resultMatchesParts(
      result,
      { kind: "list", element },
      listDataSummary(emptyListSequence()),
      0n,
    );
  }
  if (tag === 51n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const raw = material.scalarPreimages[0]!;
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      raw.length === 0 ||
      raw.length > 9_215
    ) {
      return false;
    }
    const decoded = plutusDataFromCborIterative(raw);
    if (!sameBytes(encodeMidgardCekPlutusData(decoded), raw)) {
      return false;
    }
    const tree = commitMidgardCekDataTree(decoded);
    if (
      !sameDataSummary(source.payload, {
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      })
    ) {
      return false;
    }
    const payload = semanticSummary(new DataB(raw));
    return resultMatchesParts(
      result,
      { kind: "bytes" },
      payload,
      BigInt(Math.max(1, raw.length)),
    );
  }
  if (tag === 37n || tag === 39n) {
    const fieldsArgument = arguments_[tag === 37n ? 1 : 0];
    const fieldsNode =
      material.dataNodes[
        tag === 37n && arguments_[0]?.kind === "semanticConstant" ? 1 : 0
      ];
    if (
      fieldsArgument === undefined ||
      fieldsNode === undefined ||
      material.pairNodes.length !== 0
    ) {
      return false;
    }
    const fields = semanticListSource(
      fieldsArgument,
      fieldsNode,
      material.listNodes,
    );
    if (fields === null || fields.element.kind !== "data") return false;
    if (tag === 39n) {
      return (
        arguments_.length === 1 &&
        material.scalarPreimages.length === 0 &&
        resultMatchesParts(
          result,
          { kind: "data" },
          listDataSummary(fields.sequence),
          4n + fields.sequence.memory,
        )
      );
    }
    if (arguments_.length !== 2) return false;
    let constructor: bigint | null = null;
    if (arguments_[0]?.kind === "constant") {
      const index = decodeMidgardCekConstantWitness(arguments_[0].witness);
      if (
        index.type.kind === "integer" &&
        index.payload instanceof DataI &&
        material.dataNodes.length === 1 &&
        material.scalarPreimages.length === 0
      ) {
        constructor = index.payload.int;
      }
    } else if (
      arguments_[0]?.kind === "semanticConstant" &&
      material.dataNodes.length === 2 &&
      material.scalarPreimages.length === 1
    ) {
      const index = constantParts(arguments_[0]);
      if (index?.type.kind === "integer") {
        constructor = canonicalIntegerLeaf(
          index.payload,
          material.dataNodes[0]!,
          material.scalarPreimages[0]!,
        );
      }
    }
    if (constructor === null || constructor < 0n) return false;
    const summary = constrDataSummary(constructor, fields.sequence);
    return resultMatchesParts(
      result,
      { kind: "data" },
      summary,
      summary.memory,
    );
  }
  if (tag === 40n || tag === 45n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    if (
      source === null ||
      source.type.kind !== (tag === 40n ? "integer" : "data")
    ) {
      return false;
    }
    const integer = canonicalIntegerLeaf(
      source.payload,
      material.dataNodes[0]!,
      material.scalarPreimages[0]!,
    );
    if (
      integer === null ||
      (tag === 40n
        ? source.memory !== source.payload.memory - 4n
        : source.memory !== source.payload.memory)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: tag === 40n ? "data" : "integer" },
      source.payload,
      tag === 40n ? source.payload.memory : source.payload.memory - 4n,
    );
  }
  if (tag === 41n || tag === 46n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    if (
      source === null ||
      source.type.kind !== (tag === 41n ? "bytes" : "data") ||
      !canonicalBytesLeaf(
        source.payload,
        material.dataNodes[0]!,
        material.scalarPreimages[0]!,
      ) ||
      (tag === 41n
        ? source.memory !== source.payload.memory - 4n
        : source.memory !== source.payload.memory)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: tag === 41n ? "data" : "bytes" },
      source.payload,
      tag === 41n ? source.payload.memory : source.payload.memory - 4n,
    );
  }
  if (tag === 44n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const node = material.dataNodes[0]!;
    const sequence = listSequenceFromNode(node);
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      sequence === null ||
      !exactTopMaterial(source.payload, node, material.listNodes, [])
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "list", element: { kind: "data" } },
      source.payload,
      sequence.memory,
    );
  }
  if (tag === 42n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length > 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const node = material.dataNodes[0]!;
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      !exactTopMaterial(source.payload, node, material.listNodes, [])
    ) {
      return false;
    }
    let constructor: bigint | null = null;
    if (node.kind === "constrSmall" && material.scalarPreimages.length === 0) {
      constructor = node.constructor;
    } else if (
      node.kind === "constrLarge" &&
      material.scalarPreimages.length === 1
    ) {
      const raw = material.scalarPreimages[0]!;
      if (
        raw.length <= 9_215 &&
        sameBytes(node.constructorCborRoot, commitMidgardCekBlob(raw).root) &&
        node.constructorCborLength === BigInt(raw.length)
      ) {
        const decoded = plutusDataFromCborIterative(raw);
        if (
          decoded instanceof DataI &&
          decoded.int > 127n &&
          sameBytes(encodeMidgardCekDataTreeInteger(decoded.int), raw)
        ) {
          constructor = decoded.int;
        }
      }
    }
    if (
      constructor === null ||
      (node.kind !== "constrSmall" && node.kind !== "constrLarge")
    ) {
      return false;
    }
    const fields: DataSequenceSummary = {
      root: node.fieldsRoot,
      length: node.fieldsCount,
      payloadCborLength: material.listNodes[0]?.payloadCborLength ?? 0n,
      memory: material.listNodes[0]?.memory ?? 0n,
    };
    const constructorSummary = semanticSummary(new DataI(constructor));
    const fieldsSummary = listDataSummary(fields);
    return resultMatchesParts(
      result,
      {
        kind: "pair",
        first: { kind: "integer" },
        second: {
          kind: "list",
          element: { kind: "data" },
        },
      },
      pairDataSummary(constructorSummary, fieldsSummary),
      constructorSummary.memory - 4n + fields.memory,
    );
  }
  return false;
};
