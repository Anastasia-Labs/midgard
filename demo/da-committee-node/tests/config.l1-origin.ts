import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { computeDeploymentManifestId } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import { loadCommitteeConfig } from "../src/config.js";
import { committeeL1FollowerPlan } from "../src/l1/follower/committee-follower-config.js";
import { tempDir } from "./helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "./helpers/committee-config-files.js";
import { readDaDeploymentFixture } from "./helpers/deployment-fixture.js";

/** The deployment's recorded origin T. */
const T = { slot: 4_200, blockHash: "ab".repeat(32) };

/** A committee env over a deployment whose manifest records `l1Origin` T. */
const recordedEnv = async () => {
  const dir = await tempDir();
  const {
    manifestId: _manifestId,
    l1Origin: _l1Origin,
    ...identity
  } = await readDaDeploymentFixture();
  const withOrigin = { ...identity, l1Origin: T };
  const manifestId = computeDeploymentManifestId(withOrigin);
  const deploymentInfoPath = join(dir, "deployment.json");
  const manifestPath = join(dir, "manifest.json");
  await writeFile(
    deploymentInfoPath,
    JSON.stringify({ ...withOrigin, manifestId }),
  );
  await writeFile(
    manifestPath,
    JSON.stringify(
      libp2pManifest("01".repeat(32), ["committee", "retrieval"], manifestId),
    ),
  );
  return {
    ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
    DA_SIGNER_INDEX: "0",
    DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    CARDANO_LOCAL_NODE_SOCKET_PATH: "/run/cardano/node.socket",
    CARDANO_LOCAL_NODE_CONFIG_PATH: "/etc/cardano/config.json",
    CARDANO_L1_NODE_TRANSPORT_BINARY_PATH:
      "/opt/midgard/midgard-l1-node-transport",
  };
};

describe("loadCommitteeConfig: L1_ORIGIN", () => {
  it("reads L1_ORIGIN as the operator-configured origin, absent when unset", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const env = {
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    };
    const hash = "cd".repeat(32);
    expect((await loadCommitteeConfig(env)).l1Origin).toBeUndefined();
    expect(
      (await loadCommitteeConfig({ ...env, L1_ORIGIN: ` 4242.${hash} ` }))
        .l1Origin,
    ).toEqual({ slot: 4242, blockHash: hash });
    for (const malformed of [`4242#${hash}`, `4242.${hash.slice(2)}`])
      await expect(
        loadCommitteeConfig({ ...env, L1_ORIGIN: malformed }),
      ).rejects.toThrow(/^L1_ORIGIN/u);
  });

  it("C-a: with no L1_ORIGIN, the follower starts at the manifest's recorded origin and checks the store against it", async () => {
    const config = await loadCommitteeConfig(await recordedEnv());
    expect(config.l1Origin).toBeUndefined();
    const T_POINT = { slot: T.slot, hash: Buffer.from(T.blockHash, "hex") };
    expect(committeeL1FollowerPlan(config)).toMatchObject({
      kind: "run",
      origin: { origin: T_POINT, recordedOrigin: T_POINT },
    });
  });

  it("C-b: an L1_ORIGIN override is where the follower starts, still checked against the manifest's recorded origin", async () => {
    const override = { slot: 4_300, blockHash: "cd".repeat(32) };
    const config = await loadCommitteeConfig({
      ...(await recordedEnv()),
      L1_ORIGIN: `${override.slot.toString()}.${override.blockHash}`,
    });
    expect(committeeL1FollowerPlan(config)).toMatchObject({
      kind: "run",
      origin: {
        origin: {
          slot: override.slot,
          hash: Buffer.from(override.blockHash, "hex"),
        },
        recordedOrigin: { slot: T.slot, hash: Buffer.from(T.blockHash, "hex") },
      },
    });
  });
});
