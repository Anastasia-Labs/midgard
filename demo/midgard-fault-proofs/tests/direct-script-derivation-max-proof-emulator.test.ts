import { expect, it } from "vitest";

import { runDirectSourceMaximumProof } from "./support/direct-source-maximum-proof.js";

// These are opaque-sibling protocol-ceiling envelopes, with explicit seeded
// model roots/counts. They do not claim materialized honest 64-level tries.
it.each(["64Branch", "63LongFork"] as const)(
  "fits the actual %s ledger authentication envelope and refuses changed proofs",
  async (kind) => {
    const result = await runDirectSourceMaximumProof(kind, "authenticate");
    expect(result.proofSteps).toBe(64);
    expect(result.measurements).toHaveLength(1);
    console.log("N6 actual maximum proof envelope", JSON.stringify(result));
  },
  300_000,
);
it.each(["64Branch", "63LongFork"] as const)(
  "fits the actual %s map insertion envelope and refuses changed proofs",
  async (kind) => {
    const result = await runDirectSourceMaximumProof(kind, "map");
    expect(result.proofSteps).toBe(64);
    expect(result.measurements).toHaveLength(1);
    console.log("N6 actual maximum proof envelope", JSON.stringify(result));
  },
  300_000,
);
