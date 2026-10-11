import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution, SlotConfig } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  assertLucidL1SlotConfigMatchesDeployment,
  deploymentL1SlotConfig,
  l1SlotConfigForDerivedDeployment,
} from "../src/services/midgard-contracts.load-reference-script-auth-validator.js";

// A deployment's cek-context validator is applied with one L1 slot mapping,
// and every process building script contexts off chain must use the same
// one. A derived (manifest-less) bundle takes a named network's static
// mapping, or `MIDGARD_L1_SLOT_CONFIG` for a Custom one; a process whose
// Lucid reads a different mapping off the ledger is refused.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };
const ENV = "MIDGARD_L1_SLOT_CONFIG";
const saved = process.env[ENV];

afterEach(() => {
  if (saved === undefined) delete process.env[ENV];
  else process.env[ENV] = saved;
});

const lucidWith = (slotConfig: SlotConfig | undefined): LucidEvolution =>
  ({ config: () => ({ slotConfig }) }) as unknown as LucidEvolution;

describe("l1SlotConfigForDerivedDeployment", () => {
  it("takes a named network's static mapping with no environment value", () => {
    delete process.env[ENV];
    expect(l1SlotConfigForDerivedDeployment("Preprod")).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    );
    expect(l1SlotConfigForDerivedDeployment("Mainnet")).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Mainnet,
    );
  });

  it("accepts an environment value equal to the named network's mapping", () => {
    process.env[ENV] = JSON.stringify(NAMED_NETWORK_L1_SLOT_CONFIGS.Preview);
    expect(l1SlotConfigForDerivedDeployment("Preview")).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preview,
    );
  });

  it("refuses an environment value that differs from the named network's mapping", () => {
    process.env[ENV] = JSON.stringify(DEVNET);
    expect(() => l1SlotConfigForDerivedDeployment("Preprod")).toThrow(
      /MIDGARD_L1_SLOT_CONFIG zeroTime=1789056000000,zeroSlot=0,slotLength=1000 differs from Preprod's/u,
    );
  });

  it("requires the environment value for a Custom network and takes it exactly", () => {
    delete process.env[ENV];
    expect(() => l1SlotConfigForDerivedDeployment("Custom")).toThrow(
      /MIDGARD_L1_SLOT_CONFIG must be set to the Custom network's genesis slot mapping/u,
    );
    process.env[ENV] = "   ";
    expect(() => l1SlotConfigForDerivedDeployment("Custom")).toThrow(
      /must be set/u,
    );
    process.env[ENV] = JSON.stringify(DEVNET);
    expect(l1SlotConfigForDerivedDeployment("Custom")).toEqual(DEVNET);
  });

  it.each([
    ["not JSON", "{zeroTime: 1}", /must be JSON/u],
    [
      "an extra key",
      JSON.stringify({ ...DEVNET, era: 1 }),
      /^MIDGARD_L1_SLOT_CONFIG: must have exactly the keys/u,
    ],
    [
      "a zero slot length",
      JSON.stringify({ ...DEVNET, slotLength: 0 }),
      /^MIDGARD_L1_SLOT_CONFIG\.slotLength: must be positive/u,
    ],
  ])("refuses an environment value that is %s", (_label, raw, message) => {
    process.env[ENV] = raw;
    expect(() => l1SlotConfigForDerivedDeployment("Custom")).toThrow(message);
    expect(() => l1SlotConfigForDerivedDeployment("Preprod")).toThrow(message);
  });
});

describe("deploymentL1SlotConfig", () => {
  it("reads a finalized manifest's mapping and ignores the environment", () => {
    process.env[ENV] = JSON.stringify(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod);
    expect(
      deploymentL1SlotConfig(
        {
          kind: "manifest",
          manifest: { l1SlotConfig: DEVNET },
        } as unknown as Parameters<typeof deploymentL1SlotConfig>[0],
        "Custom",
      ),
    ).toEqual(DEVNET);
  });

  it("derives a manifest-less bundle's mapping from the network", () => {
    delete process.env[ENV];
    expect(
      deploymentL1SlotConfig(
        { kind: "derived" } as unknown as Parameters<
          typeof deploymentL1SlotConfig
        >[0],
        "Preprod",
      ),
    ).toEqual(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod);
    expect(() =>
      deploymentL1SlotConfig(
        { kind: "derived" } as unknown as Parameters<
          typeof deploymentL1SlotConfig
        >[0],
        "Custom",
      ),
    ).toThrow(/MIDGARD_L1_SLOT_CONFIG must be set/u);
  });
});

describe("assertLucidL1SlotConfigMatchesDeployment", () => {
  it("passes when Lucid reads the deployment's mapping", () => {
    expect(() =>
      assertLucidL1SlotConfigMatchesDeployment(
        DEVNET,
        lucidWith({ ...DEVNET }),
      ),
    ).not.toThrow();
  });

  it("refuses a Lucid mapping that differs, naming both", () => {
    expect(() =>
      assertLucidL1SlotConfigMatchesDeployment(
        DEVNET,
        lucidWith({ ...DEVNET, zeroTime: DEVNET.zeroTime + 1000 }),
      ),
    ).toThrow(
      "l1_slot_config_mismatch: runtime zeroTime=1789056001000,zeroSlot=0,slotLength=1000 differs from the deployment's zeroTime=1789056000000,zeroSlot=0,slotLength=1000",
    );
  });

  it("refuses a Lucid instance with no slot mapping", () => {
    expect(() =>
      assertLucidL1SlotConfigMatchesDeployment(DEVNET, lucidWith(undefined)),
    ).toThrow(/does not expose a canonical slot configuration/u);
  });
});
