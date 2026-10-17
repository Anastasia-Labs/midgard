import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  makeWithdrawalValidator,
  memoized,
  PLUTUS_V3_SCRIPT_CACHE_LIMIT,
} from "../src/fraud-proof/contracts/blueprint.get-unapplied-script.js";

describe("Plutus V3 script hash and address caches", () => {
  it("computes once per key and returns the cached value on a hit", () => {
    const cache = new Map<string, string>();
    let computed = 0;
    const compute = () => {
      computed += 1;
      return "value";
    };
    expect(memoized(cache, "a", compute)).toBe("value");
    expect(memoized(cache, "a", compute)).toBe("value");
    expect(computed).toBe(1);
  });

  it("caches nothing when the computation throws", () => {
    const cache = new Map<string, string>();
    expect(() =>
      memoized(cache, "a", () => {
        throw new Error("bad script");
      }),
    ).toThrow("bad script");
    expect(cache.size).toBe(0);
  });

  it("stops growing at its cap, evicting the oldest entry first", () => {
    const cache = new Map<string, string>();
    for (let i = 0; i <= PLUTUS_V3_SCRIPT_CACHE_LIMIT; i += 1) {
      memoized(cache, `script-${i.toString()}`, () => `hash-${i.toString()}`);
    }
    expect(cache.size).toBe(PLUTUS_V3_SCRIPT_CACHE_LIMIT);
    expect(cache.has("script-0")).toBe(false);
    expect(cache.has("script-1")).toBe(true);
    expect(cache.has(`script-${PLUTUS_V3_SCRIPT_CACHE_LIMIT.toString()}`)).toBe(
      true,
    );
  });

  it("evicts a key that is then recomputed, not served stale", () => {
    const cache = new Map<string, string>();
    memoized(cache, "a", () => "first", 1);
    memoized(cache, "b", () => "b", 1);
    expect(memoized(cache, "a", () => "second", 1)).toBe("second");
    expect([...cache.keys()]).toEqual(["a"]);
  });

  it("hashes a script the same as Lucid through the cache", () => {
    const script = "49480100002221200101";
    const expected = validatorToScriptHash({ type: "PlutusV3", script });
    expect(makeWithdrawalValidator(script).withdrawalScriptHash).toBe(expected);
    expect(makeWithdrawalValidator(script).withdrawalScriptHash).toBe(expected);
  });
});
