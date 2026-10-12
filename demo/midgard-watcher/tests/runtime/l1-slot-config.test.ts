import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution, SlotConfig } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherManifestL1SlotConfig } from "../../src/fault-proofs/fault-proof-application.bind-watcher-deployment-authority.js";
import {
  assertWatcherL1SlotConfigMatchesDeployment,
  watcherRuntimeL1SlotConfig,
} from "../../src/runtime/custom-network.js";

// The watcher replays script contexts whose validity ranges are POSIX
// milliseconds through the deployment's L1 slot mapping. A watcher running
// with any other mapping would replay different verdicts than the deployed
// validator reaches, so it refuses to start.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };
const customNetwork = { networkMagic: 42, slotConfig: DEVNET };

const lucidWith = (slotConfig: SlotConfig): LucidEvolution =>
  ({ config: () => ({ slotConfig }) }) as unknown as LucidEvolution;

describe("watcherRuntimeL1SlotConfig", () => {
  it("takes a named network's static mapping and a Custom network's configured clock", () => {
    expect(watcherRuntimeL1SlotConfig({ targetNetwork: "Preprod" })).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    );
    expect(
      watcherRuntimeL1SlotConfig({ targetNetwork: "Custom", customNetwork }),
    ).toEqual(DEVNET);
    expect(() =>
      watcherRuntimeL1SlotConfig({ targetNetwork: "Custom" }),
    ).toThrow(/missing its slot clock/u);
  });
});

describe("assertWatcherL1SlotConfigMatchesDeployment", () => {
  it("passes when the watcher runs with the deployment's mapping", () => {
    expect(() =>
      assertWatcherL1SlotConfigMatchesDeployment(
        NAMED_NETWORK_L1_SLOT_CONFIGS.Mainnet,
        { targetNetwork: "Mainnet" },
      ),
    ).not.toThrow();
    expect(() =>
      assertWatcherL1SlotConfigMatchesDeployment(DEVNET, {
        targetNetwork: "Custom",
        customNetwork,
      }),
    ).not.toThrow();
  });

  it("refuses a watcher whose mapping differs, naming both", () => {
    expect(() =>
      assertWatcherL1SlotConfigMatchesDeployment(
        { ...DEVNET, zeroTime: DEVNET.zeroTime + 1000 },
        { targetNetwork: "Custom", customNetwork },
      ),
    ).toThrow(
      "l1_slot_config_mismatch: runtime zeroTime=1789056000000,zeroSlot=0,slotLength=1000 differs from the deployment's zeroTime=1789056001000,zeroSlot=0,slotLength=1000",
    );
    expect(() =>
      assertWatcherL1SlotConfigMatchesDeployment(
        NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        { targetNetwork: "Preview" },
      ),
    ).toThrow(/^l1_slot_config_mismatch: /u);
  });
});

describe("watcherManifestL1SlotConfig", () => {
  it("returns the manifest's mapping when the watcher's Lucid runs with it", () => {
    expect(
      watcherManifestL1SlotConfig(
        { l1SlotConfig: DEVNET },
        lucidWith({ ...DEVNET }),
      ),
    ).toEqual(DEVNET);
  });

  it("refuses a Lucid mapping that differs from the manifest's", () => {
    expect(() =>
      watcherManifestL1SlotConfig(
        { l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
        lucidWith(NAMED_NETWORK_L1_SLOT_CONFIGS.Preview),
      ),
    ).toThrow(/^l1_slot_config_mismatch: /u);
  });

  it("refuses a manifest without a well-formed mapping", () => {
    expect(() => watcherManifestL1SlotConfig({}, lucidWith(DEVNET))).toThrow(
      /deployment manifest l1SlotConfig/u,
    );
    expect(() =>
      watcherManifestL1SlotConfig(
        { l1SlotConfig: { ...DEVNET, slotLength: 0 } },
        lucidWith(DEVNET),
      ),
    ).toThrow(/slotLength: must be positive/u);
  });
});
