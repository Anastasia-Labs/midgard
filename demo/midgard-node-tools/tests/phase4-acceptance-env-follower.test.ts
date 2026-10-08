/**
 * The journal-kill-recovery gate waits for each node's /readyz, which stays
 * failed (l1_follower_unconfigured) unless the node's L1 follower runs. This
 * checks, without a devnet, that the acceptance.env write-acceptance-env.sh
 * produces passes the gate's own env validation, survives into the child
 * env the gate starts nodes with, and makes the node plan to run its
 * follower and read the run's network magic through the host node config.
 */
import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { loadDotenvFile } from "midgard-node/e2e/env";
import { nativeLedgerNetworkMagic } from "midgard-node/services/native-ledger";
import { describe, expect, it } from "vitest";

import {
  buildPhase4IsolatedChildEnv,
  validatePhase4ProcessIsolationValues,
} from "../src/commands/e2e-journal-kill-recovery-acceptance.js";
import { nodeFollowerPlan } from "./node-follower-plan.js";

type AssetFixtures = {
  readonly acceptanceEnvRun: (nodeEnv: (ownerBinary: string) => string) => {
    readonly runDir: string;
  };
  readonly writeAcceptanceEnv: (
    runDir: string,
  ) => Promise<{ readonly status: number | null; readonly stderr: string }>;
};

const loadFixtures = async () =>
  (await import(
    new URL("../devnet/phase4-process/tests/assets.run.mjs", import.meta.url)
      .href
  )) as AssetFixtures;

const NETWORK_MAGIC = 424242;

/** A run whose host config names a genesis carrying the run's magic. */
const acceptanceEnv = async () => {
  const fixtures = await loadFixtures();
  const { runDir } = fixtures.acceptanceEnvRun(
    (ownerBinary) => `MPF_NATIVE_OWNER_BINARY_PATH=${ownerBinary}\n`,
  );
  mkdirSync(join(runDir, "genesis"));
  mkdirSync(join(runDir, "cardano/ipc"), { recursive: true });
  writeFileSync(
    join(runDir, "genesis/shelley-genesis.json"),
    JSON.stringify({ networkMagic: NETWORK_MAGIC }),
  );
  writeFileSync(
    join(runDir, "config/host-config.json"),
    JSON.stringify({
      ShelleyGenesisFile: join(runDir, "genesis/shelley-genesis.json"),
    }),
  );
  // The run's node socket does not exist yet: the magic read needs none.
  const result = await fixtures.writeAcceptanceEnv(runDir);
  expect(result.status, result.stderr).toBe(0);
  return {
    runDir,
    values: await loadDotenvFile(join(runDir, "secrets/acceptance.env")),
  };
};

describe("Phase 4 acceptance env and the node's L1 follower", () => {
  it("runs the follower in every node the gate starts", async () => {
    const { runDir, values } = await acceptanceEnv();
    try {
      expect(validatePhase4ProcessIsolationValues(values)).toMatchObject({
        networkMagic: NETWORK_MAGIC,
      });
      const child = buildPhase4IsolatedChildEnv({
        values,
        deploymentManifestPath: join(
          runDir,
          "deploymentInfo/contract-deployment-info.json",
        ),
        baseEnv: {},
      });
      const plan = nodeFollowerPlan(child);
      expect(plan).toMatchObject({
        kind: "run",
        socketPath: join(runDir, "cardano/ipc/node.socket"),
        nodeConfigPath: join(runDir, "config/host-config.json"),
        binaryPath: join(runDir, "bin/midgard-l1-node-transport"),
      });
      if (plan.kind !== "run") throw new Error(plan.detail);
      // The follower's first step after planning: the run's network magic.
      await expect(
        nativeLedgerNetworkMagic(
          { nodeConfigPath: plan.nodeConfigPath },
          "Custom",
        ),
      ).resolves.toBe(NETWORK_MAGIC);
    } finally {
      rmSync(runDir, { recursive: true, force: true });
    }
  });

  it("refuses, by name, an env that would leave the follower unconfigured", async () => {
    const { runDir, values } = await acceptanceEnv();
    try {
      const { L1_ORIGIN: _origin, ...withoutOrigin } = values;
      expect(nodeFollowerPlan(withoutOrigin)).toEqual({
        kind: "unconfigured",
        detail: "L1_ORIGIN is not set",
      });
      expect(() => validatePhase4ProcessIsolationValues(withoutOrigin)).toThrow(
        expect.objectContaining({
          name: "NodeFollowerUnconfiguredError",
          message: expect.stringContaining("L1_ORIGIN is not set"),
        }),
      );
    } finally {
      rmSync(runDir, { recursive: true, force: true });
    }
  });
});
