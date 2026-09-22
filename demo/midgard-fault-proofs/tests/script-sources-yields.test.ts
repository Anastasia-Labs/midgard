import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { Constr } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { scriptSourcesMiddleYieldIndex } from "../src/validation-dispute/script-sources-yields.js";

const control = (stage: bigint, count = 30) => {
  const fields: unknown[] = Array.from({ length: count }, () => 0n);
  fields[0] = Buffer.alloc(314, 7);
  fields[9] = stage;
  return encodeCbor(fields).toString("hex");
};
it.each([
  [2n, 0, 0],
  [3n, 7, 1],
  [3n, 0, 2],
  [4n, 29, 3],
  [4n, 0, 4],
  [6n, 1, 5],
  [6n, 39, 6],
  [6n, 0, 7],
] as const)(
  "routes stage %s auxiliary %s with canonical long byte strings",
  (stage, auxiliary, expected) => {
    expect(
      scriptSourcesMiddleYieldIndex(control(stage), new Constr(auxiliary, [])),
    ).toBe(expected);
  },
);
it.each([
  [2n, 39],
  [4n, 7],
  [6n, 29],
  [7n, 0],
] as const)(
  "refuses stage %s with cross-arm auxiliary %s",
  (stage, auxiliary) => {
    expect(() =>
      scriptSourcesMiddleYieldIndex(control(stage), new Constr(auxiliary, [])),
    ).toThrow("does not match");
  },
);
it("refuses a wrong control width and trailing CBOR", () => {
  expect(() =>
    scriptSourcesMiddleYieldIndex(control(2n, 31), new Constr(0, [])),
  ).toThrow();
  expect(() =>
    scriptSourcesMiddleYieldIndex(control(2n) + "00", new Constr(0, [])),
  ).toThrow();
});
