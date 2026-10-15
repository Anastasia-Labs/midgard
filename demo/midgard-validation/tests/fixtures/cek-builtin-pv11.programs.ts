import { DataB, DataConstr, DataI, DataList } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  type UPLCBuiltinTag,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

import { type MidgardCekPv11Constant } from "../../src/cek-builtin.pv11.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.js";
import { type Pv11Case } from "./cek-builtin-pv11.cases.js";

export const call = (
  tag: number,
  forces: number,
  ...arguments_: UPLCTerm[]
): UPLCTerm => {
  let term: UPLCTerm = new Builtin(tag as UPLCBuiltinTag);
  for (let index = 0; index < forces; index++) term = new Force(term);
  return arguments_.reduce<UPLCTerm>(
    (fn, argument) => new Application(fn, argument),
    term,
  );
};

/** Source expressions for constants whose Flat encoding forbids BLS values. */
export const pv11ConstantTerm = ({
  type,
  payload,
}: MidgardCekPv11Constant): UPLCTerm => {
  if (type.kind === "integer" && payload instanceof DataI)
    return UPLCConst.int(payload.int);
  if (type.kind === "bytes" && payload instanceof DataB)
    return UPLCConst.byteString(Uint8Array.from(payload.bytes));
  if (type.kind === "data") return UPLCConst.data(payload);
  if (type.kind === "boolean" && payload instanceof DataConstr)
    return UPLCConst.bool(payload.constr === 1n);
  if (type.kind === "unit") return UPLCConst.unit;
  if (type.kind === "value") return call(99, 0, UPLCConst.data(payload));
  if (
    (type.kind === "blsG1" || type.kind === "blsG2") &&
    payload instanceof DataB
  ) {
    return call(
      type.kind === "blsG1" ? 59 : 66,
      0,
      UPLCConst.byteString(Uint8Array.from(payload.bytes)),
    );
  }
  if (type.kind === "array")
    return call(
      90,
      1,
      pv11ConstantTerm({
        type: { kind: "list", element: type.element },
        payload,
      }),
    );
  if (type.kind === "list" && payload instanceof DataList) {
    if (type.element.kind === "boolean") {
      let result: UPLCTerm = UPLCConst.listOf(constT.bool)([]);
      for (const item of [...payload.list].reverse()) {
        result = call(
          32,
          1,
          pv11ConstantTerm({ type: type.element, payload: item }),
          result,
        );
      }
      return result;
    }
    if (type.element.kind === "integer")
      return UPLCConst.listOf(constT.int)(
        payload.list.map((item) => (item as DataI).int),
      );
    if (type.element.kind === "blsG1" || type.element.kind === "blsG2") {
      let result: UPLCTerm = UPLCConst.listOf(
        type.element.kind === "blsG1"
          ? constT.bls12_381_G1_element
          : constT.bls12_381_G2_element,
      )([]);
      for (const item of [...payload.list].reverse())
        result = call(
          32,
          1,
          pv11ConstantTerm({ type: type.element, payload: item }),
          result,
        );
      return result;
    }
  }
  throw new Error(`PV 11 fixture cannot encode ${type.kind}`);
};

const and = (left: UPLCTerm, right: UPLCTerm): UPLCTerm =>
  call(26, 1, left, right, UPLCConst.bool(false));
const equals = (
  actual: UPLCTerm,
  expected: MidgardCekPv11Constant | boolean,
): UPLCTerm => {
  if (typeof expected === "boolean")
    return expected
      ? actual
      : call(26, 1, actual, UPLCConst.bool(false), UPLCConst.bool(true));
  const { type, payload } = expected;
  if (type.kind === "integer")
    return call(7, 0, actual, pv11ConstantTerm(expected));
  if (type.kind === "bytes")
    return call(15, 0, actual, pv11ConstantTerm(expected));
  if (type.kind === "data") return call(47, 0, actual, UPLCConst.data(payload));
  if (type.kind === "value")
    return call(47, 0, call(98, 0, actual), UPLCConst.data(payload));
  if (type.kind === "blsG1" || type.kind === "blsG2") {
    return call(
      15,
      0,
      call(type.kind === "blsG1" ? 58 : 65, 0, actual),
      pv11ConstantTerm({ type: { kind: "bytes" }, payload }),
    );
  }
  if (
    (type.kind === "list" || type.kind === "array") &&
    payload instanceof DataList
  ) {
    let predicate: UPLCTerm =
      type.kind === "array"
        ? call(7, 0, call(89, 1, actual), UPLCConst.int(payload.list.length))
        : UPLCConst.bool(true);
    let tail = actual;
    payload.list.forEach((item, index) => {
      const element =
        type.kind === "array"
          ? call(91, 1, actual, UPLCConst.int(index))
          : call(33, 1, tail);
      predicate = and(
        predicate,
        equals(element, { type: type.element, payload: item }),
      );
      if (type.kind === "list") tail = call(34, 1, tail);
    });
    return type.kind === "list" ? and(predicate, call(35, 1, tail)) : predicate;
  }
  throw new Error(`PV 11 fixture cannot compare ${type.kind}`);
};

export const pv11CaseProgram = (entry: Pv11Case): Buffer => {
  const actual = call(
    Number(entry.tag),
    entry.tag >= 88n && entry.tag <= 91n ? 1 : 0,
    ...entry.arguments.map(pv11ConstantTerm),
  );
  const body =
    entry.expected === "failure"
      ? new Application(new Lambda(UPLCConst.unit), actual)
      : new Force(
          call(
            26,
            1,
            equals(actual, entry.expected),
            new Delay(UPLCConst.unit),
            new Delay(new ErrorUPLC()),
          ),
        );
  return encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram([1, 1, 0], new Lambda(body)),
  );
};
