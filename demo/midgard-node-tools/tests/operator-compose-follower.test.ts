/**
 * The operator compose stack (docker-compose.yaml plus
 * docker-compose.kupmios.yaml) sets the node's three local-node keys and
 * leaves L1_ORIGIN to the operator's .env. Read with the node's own parsers,
 * that environment must keep the node up but unready by name until the
 * operator sets L1_ORIGIN, and run the follower once it is set.
 * scripts/operator-compose.test.mjs checks the rendered mounts and keys.
 */
import { readFileSync } from "node:fs";

import { l1FollowerReadiness } from "midgard-node/services/l1-follower.readiness";
import { describe, expect, it } from "vitest";

import { nodeFollowerPlan } from "./node-follower-plan.js";

const operatorFile = (name: string) =>
  readFileSync(new URL(`../../midgard-node/${name}`, import.meta.url), "utf8");

/** The local-node keys the overlay sets on midgard-node, as written there. */
const composeLocalNodeKeys = (): Record<string, string> => {
  const overlay = operatorFile("docker-compose.kupmios.yaml");
  return Object.fromEntries(
    [
      "L1_NODE_SOCKET_PATH",
      "L1_NODE_CONFIG_PATH",
      "L1_NODE_TRANSPORT_BINARY_PATH",
    ].map((key) => {
      const match = new RegExp(`^ {6}${key}: (\\S+)$`, "mu").exec(overlay);
      if (match === null) throw new Error(`the overlay does not set ${key}`);
      return [key, match[1]!];
    }),
  );
};

const ONE_SHOT = {
  HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
};

describe("operator compose and the node's L1 follower", () => {
  it("ships an empty L1_ORIGIN in .env.example for the operator to fill", () => {
    expect(operatorFile(".env.example")).toMatch(/^L1_ORIGIN=$/mu);
  });

  it("keeps the node up, unready by name, with the local node and no origin", () => {
    const plan = nodeFollowerPlan({
      ...composeLocalNodeKeys(),
      ...ONE_SHOT,
      L1_ORIGIN: "",
    });
    expect(plan).toEqual({
      kind: "unconfigured",
      detail: "L1_ORIGIN is not set",
    });
    if (plan.kind !== "unconfigured") throw new Error("unreachable");
    expect(l1FollowerReadiness(plan).reasons).toEqual([
      "l1_follower_unconfigured",
    ]);
  });

  it("runs the follower through the in-stack node once L1_ORIGIN is set", () => {
    expect(
      nodeFollowerPlan({
        ...composeLocalNodeKeys(),
        ...ONE_SHOT,
        L1_ORIGIN: `1234.${"cd".repeat(32)}`,
      }),
    ).toMatchObject({
      kind: "run",
      socketPath: "/ipc/node.socket",
      nodeConfigPath: "/cardano-config/config.json",
      binaryPath: "/usr/local/bin/midgard-l1-node-transport",
    });
  });
});
