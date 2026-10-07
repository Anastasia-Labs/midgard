// Tests the committee soak plugin; deleted with it at the C1 cutover.
import { isShadowPlugin } from "@al-ft/midgard-l1-follower/shadow";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { loadCommitteeConfig } from "../../src/config.js";
import { tempDir } from "../helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "../helpers/l1-recovery-cli-config.js";

describe("committee soak plugin", () => {
  it("builds the committee projection from the committee's configuration and checks its options", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const env = {
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    };
    const config = await loadCommitteeConfig(env);
    // The plugin reads the committee's environment at module load.
    const saved = Object.fromEntries(
      Object.keys(env).map((key) => [key, process.env[key]]),
    );
    Object.assign(process.env, env);
    let loaded: { default: unknown };
    try {
      loaded = (await import(
        "../../src/l1/follower-shadow/soak-plugin.js"
      )) as {
        default: unknown;
      };
    } finally {
      for (const [key, value] of Object.entries(saved))
        if (value === undefined) delete process.env[key];
        else process.env[key] = value;
    }
    expect(isShadowPlugin(loaded.default)).toBe(true);
    const plugin = loaded.default as Parameters<typeof isShadowPlugin>[0] & {
      role: string;
      projections: readonly {
        name: string;
        trackedSet: { addresses: Set<string>; policies: Set<string> };
      }[];
      comparators: (env: unknown) => Promise<unknown>;
    };
    expect(plugin.role).toBe("committee");
    expect(plugin.projections.map((p) => p.name)).toEqual(["committee"]);
    const [projection] = plugin.projections;
    expect([...projection!.trackedSet.addresses]).toEqual([
      getAddressDetails(config.stateQueueAddress).address.hex,
    ]);
    expect([...projection!.trackedSet.policies]).toEqual([
      config.stateQueuePolicyId.toLowerCase(),
    ]);
    await expect(
      plugin.comparators({ options: { securityParameter: 0 } }),
    ).rejects.toThrow(/securityParameter/u);
  });
});
