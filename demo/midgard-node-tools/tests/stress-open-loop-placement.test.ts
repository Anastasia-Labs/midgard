import { describe, expect, it } from "vitest";

import { buildOpenLoopPlacementProof } from "../src/commands/stress-open-loop.js";

const cleanEnv = (): NodeJS.ProcessEnv => ({ PATH: "/usr/bin" });

describe("open-loop placement proof", () => {
  it("treats the tooling CLI as a load generator outside the node process", () => {
    const proof = buildOpenLoopPlacementProof({
      argv: [
        "/usr/bin/node",
        "/repo/demo/midgard-node-tools/dist/index.js",
        "e2e-stress-l2-throughput",
        "--load-model",
        "open-loop-upper-bound",
      ],
      env: cleanEnv(),
    });
    expect(proof.insideMidgardNodeProcess).toBe(false);
    expect(proof.insideMidgardNodeContainer).toBe(false);
    expect(proof.validForUpperBoundClaim).toBe(true);
    expect(proof.notes).toEqual([]);
  });

  it("flags the operator binary itself as the node process", () => {
    for (const argv of [
      ["/usr/bin/node", "/repo/demo/midgard-node/dist/index.js", "listen"],
      ["/usr/bin/node", "/repo/demo/midgard-node/dist/index.js", "utxos"],
      ["/usr/bin/node", "C:\\repo\\demo\\midgard-node\\dist\\index.js", "x"],
      ["/usr/bin/node", "./dist/index.js", "listen"],
    ]) {
      const proof = buildOpenLoopPlacementProof({ argv, env: cleanEnv() });
      expect(proof.insideMidgardNodeProcess).toBe(true);
      expect(proof.validForUpperBoundClaim).toBe(false);
      expect(proof.notes).toContain("load_generator_inside_node_process");
    }
  });

  it("flags the node container independently of the process", () => {
    const proof = buildOpenLoopPlacementProof({
      argv: ["/usr/bin/node", "/repo/demo/midgard-node-tools/dist/index.js"],
      env: { ...cleanEnv(), MIDGARD_NODE_CONTAINER: "1" },
    });
    expect(proof.insideMidgardNodeProcess).toBe(false);
    expect(proof.insideMidgardNodeContainer).toBe(true);
    expect(proof.validForUpperBoundClaim).toBe(false);
    expect(proof.notes).toEqual([
      "load_generator_inside_midgard_node_container",
    ]);
  });
});
