import {
  type MidgardCekDirectValueWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCbor,
  type MidgardCekConstantType,
} from "./cek-constant.js";

/**
 * Whether the builtin is proved as one semantic step: tags 29 to 50 except
 * mapData (38) and unMapData (43), which start the map-conversion steps.
 */
const isSemanticStepTag = (tag: bigint): boolean =>
  tag >= 29n && tag <= 50n && tag !== 38n && tag !== 43n;

/**
 * Whether a semantic list source of this element type has its memory derived
 * from the payload node (`constantMemoryFromPayloadNode`).
 */
const listElementMemoryFromNode = (element: MidgardCekConstantType): boolean =>
  element.kind === "data" ||
  element.kind === "integer" ||
  element.kind === "bytes" ||
  element.kind === "unit" ||
  element.kind === "boolean" ||
  (element.kind === "pair" &&
    element.first.kind === "data" &&
    element.second.kind === "data");

/**
 * Whether a semantic pair component of this type has its memory derived from
 * its payload node (`constantMemoryFromPayloadNode`).
 */
const componentMemoryFromNode = (type: MidgardCekConstantType): boolean => {
  switch (type.kind) {
    case "integer":
    case "bytes":
    case "string":
    case "unit":
    case "boolean":
    case "data":
      return true;
    case "list":
      return listElementMemoryFromNode(type.element);
    case "pair":
      return (
        (type.first.kind === "data" && type.second.kind === "data") ||
        (type.first.kind === "integer" &&
          type.second.kind === "list" &&
          type.second.element.kind === "data")
      );
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      return false;
  }
};

/**
 * Whether a semantic argument of this type can be proved: a Data value
 * always, and a list or pair whose memory the verifier derives from the
 * payload nodes.
 */
const semanticArgumentType = (type: MidgardCekConstantType): boolean =>
  type.kind === "data" ||
  (type.kind === "list" && listElementMemoryFromNode(type.element)) ||
  (type.kind === "pair" &&
    componentMemoryFromNode(type.first) &&
    componentMemoryFromNode(type.second));

/**
 * Whether a mkCons (32) item of this type can be presented. The verifier binds
 * a presented item's summary memory to its memory only for the element types
 * `listItemPayloadWords` (`cek-machine.verify-semantic-list.ts`) relates; of
 * those, Data and `pair<data, data>` items are presented and the scalars stay
 * revealed.
 */
const listItemArgumentType = (type: MidgardCekConstantType): boolean =>
  type.kind === "data" ||
  (type.kind === "pair" &&
    type.first.kind === "data" &&
    type.second.kind === "data");

const semanticResultType = (type: MidgardCekConstantType): boolean =>
  type.kind === "data" || type.kind === "list" || type.kind === "pair";

const present = (
  tag: bigint,
  value: MidgardCekDirectValueWitness,
  admits: (type: MidgardCekConstantType) => boolean,
): MidgardCekDirectValueWitness =>
  isSemanticStepTag(tag) &&
  value.kind === "constant" &&
  admits(decodeMidgardCekConstantTypeCbor(value.witness.typeCbor))
    ? semanticizeMidgardCekDirectConstant(value)
    : value;

/**
 * The presentation of a semantic step's result. A revealed Data, list or pair
 * result is presented by its semantic summary, which has the same value root
 * and memory; a scalar result and every other value are presented as given.
 */
export const presentMidgardCekSemanticResult = (
  tag: bigint,
  result: MidgardCekDirectValueWitness,
): MidgardCekDirectValueWitness => present(tag, result, semanticResultType);

/**
 * The presentation of a semantic step's arguments. A revealed Data argument,
 * and a list or pair argument whose memory the verifier derives from its
 * payload nodes, is presented by its semantic summary, except that a mkCons
 * item is presented only when its type is `listItemArgumentType`; every other
 * argument is presented as given.
 */
export const presentMidgardCekSemanticArguments = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): readonly MidgardCekDirectValueWitness[] =>
  arguments_.map((argument, index) =>
    present(
      tag,
      argument,
      tag === 32n && index === 0 ? listItemArgumentType : semanticArgumentType,
    ),
  );
