import { DataB, DataI, DataMap, DataPair } from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
  verifyMidgardCekBuiltinTypeFailure,
  verifyMidgardCekDirectBuiltin,
  verifyMidgardCekDirectBuiltinFailure,
} from "../src/cek-builtin.js";
import {
  direct,
  directBuiltinRoot,
} from "./cek-builtin.v1-builtin-runtime-type-failures.js";

// Each builtin step has one successor: an application that a known failure,
// a type failure and a map-conversion start could all describe is admitted by
// exactly one of those arms.
describe("V1 builtin arm exclusivity", () => {
  it("admits mapData and unMapData only through the map-conversion arm", () => {
    const map = [
      direct(
        UPLCConst.data(
          new DataMap([new DataPair(new DataI(1n), new DataI(2n))]),
        ),
      ),
    ];
    const unmapped = evaluateMidgardCekDirectBuiltin(43n, map);
    expect(unmapped.kind).toBe("success");
    if (unmapped.kind !== "success") return;
    expect(
      verifyMidgardCekDirectBuiltin(
        43n,
        directBuiltinRoot(43n, map),
        map,
        unmapped.result,
      ),
    ).toBe(false);

    const pairs = [unmapped.result];
    const remapped = evaluateMidgardCekDirectBuiltin(38n, pairs);
    expect(remapped.kind).toBe("success");
    if (remapped.kind !== "success") return;
    expect(
      verifyMidgardCekDirectBuiltin(
        38n,
        directBuiltinRoot(38n, pairs),
        pairs,
        remapped.result,
      ),
    ).toBe(false);
  });

  it("leaves an ill-typed division to the type-failure arm", () => {
    const illTyped = [
      direct(UPLCConst.byteString(new DataB(Buffer.from("01", "hex")).bytes)),
      direct(UPLCConst.int(0)),
    ];
    for (const tag of [4n, 5n, 6n]) {
      const root = directBuiltinRoot(tag, illTyped);
      expect(verifyMidgardCekDirectBuiltinFailure(tag, root, illTyped)).toBe(
        false,
      );
      expect(
        verifyMidgardCekBuiltinTypeFailure(
          tag,
          root,
          illTyped.map((argument) => {
            if (argument.kind !== "constant") {
              throw new Error("expected a direct constant");
            }
            return argument;
          }),
        ),
      ).toBe(true);
    }
  });

  it("leaves a negative index over a non-bytes source to the type-failure arm", () => {
    const illTyped = [direct(UPLCConst.int(5)), direct(UPLCConst.int(-1))];
    const wellTyped = [
      direct(UPLCConst.byteString(new DataB(Buffer.from("aa", "hex")).bytes)),
      direct(UPLCConst.int(-1)),
    ];
    for (const tag of [14n, 79n]) {
      const root = directBuiltinRoot(tag, illTyped);
      expect(verifyMidgardCekDirectBuiltinFailure(tag, root, illTyped)).toBe(
        false,
      );
      expect(
        verifyMidgardCekBuiltinTypeFailure(
          tag,
          root,
          illTyped.map((argument) => {
            if (argument.kind !== "constant") {
              throw new Error("expected a direct constant");
            }
            return argument;
          }),
        ),
      ).toBe(true);
      expect(
        verifyMidgardCekDirectBuiltinFailure(
          tag,
          directBuiltinRoot(tag, wellTyped),
          wellTyped,
        ),
      ).toBe(true);
    }
  });

  it("gives an opaque presentation of a sized argument no budget", () => {
    const dividend = direct(UPLCConst.int(1));
    const divisor = direct(UPLCConst.int(0));
    const opaque = [
      { kind: "opaque", root: hashMidgardCekDirectValueWitness(dividend) },
      divisor,
    ] as const satisfies readonly MidgardCekDirectValueWitness[];
    const revealed = [dividend, divisor];
    // Both presentations commit to the same builtin value.
    expect(directBuiltinRoot(4n, opaque)).toEqual(
      directBuiltinRoot(4n, revealed),
    );
    expect(
      verifyMidgardCekDirectBuiltinFailure(
        4n,
        directBuiltinRoot(4n, revealed),
        revealed,
      ),
    ).toBe(true);
    expect(
      verifyMidgardCekDirectBuiltinFailure(
        4n,
        directBuiltinRoot(4n, opaque),
        opaque,
      ),
    ).toBe(false);
    expect(() => midgardCekDirectBuiltinBudget(4n, opaque)).toThrow(
      "no memory size",
    );
  });
});
