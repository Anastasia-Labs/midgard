import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { Proof } from "../src/common.js";

const CANONICAL_PROOF_CBOR =
  "9fd8799f0041aaffd87a9f01d8799f0241bb41ccffffd87b9f0341dd41eeffff";

const canonicalProof: Proof = [
  { Branch: { skip: 0n, neighbors: "aa" } },
  {
    Fork: {
      skip: 1n,
      neighbor: { nibble: 2n, prefix: "bb", root: "cc" },
    },
  },
  { Leaf: { skip: 3n, key: "dd", value: "ee" } },
];

describe("canonical Aiken MPF proof ABI", () => {
  it("encodes every ProofStep tag and arity exactly", () => {
    expect(Data.to(canonicalProof, Proof)).toBe(CANONICAL_PROOF_CBOR);
    expect(Data.from(CANONICAL_PROOF_CBOR, Proof)).toEqual(canonicalProof);
  });

  it("rejects the obsolete double-wrapped Fork neighbor", () => {
    const obsoleteDoubleWrapped = "9fd87a9f01d8799fd8799f0241bb41ccffffffff";

    expect(() => Data.from(obsoleteDoubleWrapped, Proof)).toThrow();
  });
});
