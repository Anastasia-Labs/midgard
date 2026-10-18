import { type Data, DataConstr, DataList } from "@harmoniclabs/plutus-data";

import { type MidgardCekConstantType } from "./cek-constant.semantic-data.js";

/** Cardano's PV 11 bound on a constant type's flat size (`mbHeader`). */
export const MIDGARD_CEK_MAX_CONSTANT_TYPE_FLAT_SIZE = 32;

const itemsHoldBlsValue = (
  element: MidgardCekConstantType,
  payload: Data,
): boolean =>
  payload instanceof DataList &&
  payload.list.some((item) => midgardCekConstantHoldsBlsValue(element, item));

/**
 * True when a program constant holds a BLS value that Cardano's flat decoder
 * would have to read: a BLS constant, a BLS pair component, or an element of a
 * non-empty list or array, at any nesting. An empty BLS-typed list holds none,
 * because the decoder never reads its element type's values.
 */
export const midgardCekConstantHoldsBlsValue = (
  type: MidgardCekConstantType,
  payload: Data,
): boolean => {
  if (
    type.kind === "blsG1" ||
    type.kind === "blsG2" ||
    type.kind === "blsMillerLoopResult"
  ) {
    return true;
  }
  if (type.kind === "list" || type.kind === "array") {
    return itemsHoldBlsValue(type.element, payload);
  }
  if (type.kind === "pair") {
    return (
      payload instanceof DataConstr &&
      payload.fields.length === 2 &&
      (midgardCekConstantHoldsBlsValue(type.first, payload.fields[0]!) ||
        midgardCekConstantHoldsBlsValue(type.second, payload.fields[1]!))
    );
  }
  return false;
};
