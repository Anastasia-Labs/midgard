import { describe, expect, it } from "vitest";

import { loadCommitteeConfig } from "../src/config.js";
import { tempDir } from "./helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "./helpers/committee-config-files.js";

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
});
