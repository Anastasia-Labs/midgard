import { writeFile } from "node:fs/promises";

import { DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { loadWatcherConfig } from "../src/config.js";
import { tempDir } from "./helpers.js";
import { writeDaDeploymentFixture } from "./helpers/deployment-fixture.js";

describe("canonical V1 startup release gate", () => {
  it("fails closed while validator-hash-bound release evidence is unavailable", async () => {
    const dir = await tempDir();
    const manifestPath = `${dir}/runtime.json`;
    const deploymentInfoPath = `${dir}/deployment.json`;
    await writeFile(
      manifestPath,
      JSON.stringify({
        schemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        network: "Preview",
        deployment: {},
        runtime_topology: {},
        da_transport: {},
        da_committee: {},
      }),
    );
    await writeDaDeploymentFixture(deploymentInfoPath);

    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      }),
    ).rejects.toThrow(/not activated/u);
  });
});
