/** The content digest the validation-trace adapter keys a passed artifact
 * check on: equal for two separately built values that hold the same data,
 * whatever order their keys were set in, and different for any change to the
 * data, down to one byte, one UTF-16 code unit, a number's kind or an
 * object's kind; a value that is not data (a function, a symbol key, a cycle,
 * a class instance, a boxed primitive, a bare array buffer or a sparse array)
 * has none. */
import { describe, expect, it } from "vitest";

import { validationInputsDigest } from "../src/validation-trace-dispute/validation-inputs-digest.js";

const sample = () => ({
  headerHash: "ab".repeat(32),
  bytes: Buffer.from("cafe00", "hex"),
  step: 7,
  amount: 9_007_199_254_740_993n,
  flags: [true, false, null, undefined],
  index: new Map<string, unknown>([["a", { at: 1 }]]),
});

describe("validation inputs digest", () => {
  it("is the same for two separately built values holding the same data", () => {
    const reordered = Object.fromEntries(Object.entries(sample()).reverse());
    expect(validationInputsDigest(reordered)).toBe(
      validationInputsDigest(sample()),
    );
  });

  it.each([
    ["one byte", { bytes: Buffer.from("cafe01", "hex") }],
    ["one hex digit of a string", { headerHash: `${"ab".repeat(31)}ac` }],
    ["a bigint for a number", { step: 7n }],
    ["a string for a number", { step: "7" }],
    ["an undefined for a null", { flags: [true, false, undefined, undefined] }],
    ["a dropped trailing element", { flags: [true, false, null] }],
    ["a map entry's value", { index: new Map([["a", { at: 2 }]]) }],
    ["a plain object for a map", { index: { a: { at: 1 } } }],
    ["bytes of another view kind", { bytes: new Uint8Array([0xca, 0xfe, 0]) }],
  ])("changes with %s", (_, change) => {
    expect(validationInputsDigest({ ...sample(), ...change })).not.toBe(
      validationInputsDigest(sample()),
    );
  });

  it("tells apart strings that encode alike in UTF-8", () => {
    expect(validationInputsDigest("\uD800")).not.toBe(
      validationInputsDigest("�"),
    );
  });

  it("tells apart a key that is absent from one that holds undefined", () => {
    expect(validationInputsDigest({ a: undefined })).not.toBe(
      validationInputsDigest({}),
    );
  });

  it("has no digest for a function, a symbol key or a cycle", () => {
    const cyclic: { self?: unknown } = {};
    cyclic.self = cyclic;
    expect(() => validationInputsDigest({ f: () => 0 })).toThrow(
      "a function has no content digest",
    );
    expect(() => validationInputsDigest({ [Symbol("s")]: 1 })).toThrow(
      "a symbol-keyed object has no content digest",
    );
    expect(() => validationInputsDigest(cyclic)).toThrow(
      "a cyclic value has no content digest",
    );
  });

  it.each([
    ["an array buffer", () => new ArrayBuffer(1)],
    [
      "a class instance",
      () =>
        new (class Held {
          readonly #value = 1;
          get value() {
            return this.#value;
          }
        })(),
    ],
    ["a boxed number", () => new Number(1)],
  ])("has no digest for %s", (_, value) => {
    expect(() => validationInputsDigest({ value: value() })).toThrow(
      "has no content digest",
    );
  });

  it("has no digest for a sparse array, so a hole never matches an undefined", () => {
    // eslint-disable-next-line no-sparse-arrays -- the hole is the case.
    expect(() => validationInputsDigest([1, , 3])).toThrow(
      "a sparse array has no content digest",
    );
    expect(() => validationInputsDigest([1, undefined, 3])).not.toThrow();
  });
});
