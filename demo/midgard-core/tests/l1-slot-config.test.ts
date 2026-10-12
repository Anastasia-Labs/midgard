import { SLOT_CONFIG_NETWORK, slotToUnixTime } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { verifyDeploymentManifestL1SlotConfig } from "../src/deployment-manifest-identity.js";
import {
  formatL1SlotConfig,
  L1SlotConfigFormatError,
  l1SlotConfigMismatch,
  l1SlotToPosixTimeMs,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
  parseL1SlotConfig,
} from "../src/l1-slot-config.js";

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };

describe("named-network L1 slot mappings", () => {
  it.each(["Mainnet", "Preprod", "Preview"] as const)(
    "%s is Lucid's static mapping",
    (network) => {
      expect(NAMED_NETWORK_L1_SLOT_CONFIGS[network]).toEqual(
        SLOT_CONFIG_NETWORK[network],
      );
    },
  );

  it.each([
    ["Mainnet", 4_492_800n],
    ["Mainnet", 140_000_000n],
    ["Preprod", 86_400n],
    ["Preprod", 87_400n],
    ["Preprod", 75_000_000n],
    ["Preview", 0n],
    ["Preview", 60_000_000n],
  ] as const)(
    "maps %s slot %s to the POSIX milliseconds Lucid does",
    (network, slot) => {
      expect(
        l1SlotToPosixTimeMs(NAMED_NETWORK_L1_SLOT_CONFIGS[network], slot),
      ).toBe(BigInt(slotToUnixTime(network, Number(slot))));
    },
  );

  it("anchors a Custom network at its genesis start", () => {
    expect(l1SlotToPosixTimeMs(DEVNET, 0n)).toBe(1_789_056_000_000n);
    expect(l1SlotToPosixTimeMs(DEVNET, 25n)).toBe(1_789_056_025_000n);
    expect(l1SlotToPosixTimeMs({ ...DEVNET, slotLength: 100 }, 25n)).toBe(
      1_789_056_002_500n,
    );
  });
});

describe("parseL1SlotConfig", () => {
  it("accepts exactly {zeroTime, zeroSlot, slotLength}", () => {
    expect(parseL1SlotConfig({ ...DEVNET })).toEqual(DEVNET);
    expect(parseL1SlotConfig(SLOT_CONFIG_NETWORK.Mainnet)).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Mainnet,
    );
  });

  it.each([
    ["a non-object", 1000],
    ["null", null],
    ["an array", [0, 0, 1000]],
    ["a missing key", { zeroTime: 0, zeroSlot: 0 }],
    ["an extra key", { ...DEVNET, era: "Shelley" }],
    ["a zero slot length", { ...DEVNET, slotLength: 0 }],
    ["a negative zero time", { ...DEVNET, zeroTime: -1 }],
    ["a fractional zero slot", { ...DEVNET, zeroSlot: 0.5 }],
    ["an unsafe zero time", { ...DEVNET, zeroTime: 2 ** 53 }],
    ["a string slot length", { ...DEVNET, slotLength: "1000" }],
  ])("refuses %s, naming the field", (_label, value) => {
    expect(() => parseL1SlotConfig(value, "SLOTS")).toThrow(
      L1SlotConfigFormatError,
    );
    expect(() => parseL1SlotConfig(value, "SLOTS")).toThrow(/^SLOTS/u);
  });
});

describe("l1SlotConfigMismatch", () => {
  it("is null for an equal mapping", () => {
    expect(
      l1SlotConfigMismatch(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, {
        ...SLOT_CONFIG_NETWORK.Preprod,
      }),
    ).toBeNull();
  });

  it.each([
    ["zeroTime", { ...DEVNET, zeroTime: DEVNET.zeroTime + 1 }],
    ["zeroSlot", { ...DEVNET, zeroSlot: 1 }],
    ["slotLength", { ...DEVNET, slotLength: 100 }],
  ])("names both mappings when %s differs", (_field, runtime) => {
    expect(l1SlotConfigMismatch(DEVNET, runtime)).toBe(
      `l1_slot_config_mismatch: runtime ${formatL1SlotConfig(runtime)} differs from the deployment's ${formatL1SlotConfig(DEVNET)}`,
    );
  });
});

describe("verifyDeploymentManifestL1SlotConfig", () => {
  it("accepts each named network's static mapping", () => {
    for (const network of ["Mainnet", "Preprod", "Preview"] as const)
      expect(() =>
        verifyDeploymentManifestL1SlotConfig(
          network,
          NAMED_NETWORK_L1_SLOT_CONFIGS[network],
        ),
      ).not.toThrow();
  });

  it("refuses a named network carrying any other mapping", () => {
    expect(() =>
      verifyDeploymentManifestL1SlotConfig(
        "Preprod",
        NAMED_NETWORK_L1_SLOT_CONFIGS.Preview,
      ),
    ).toThrow(/l1SlotConfig must equal Preprod's/u);
    expect(() =>
      verifyDeploymentManifestL1SlotConfig("Mainnet", {
        ...NAMED_NETWORK_L1_SLOT_CONFIGS.Mainnet,
        zeroSlot: 0,
      }),
    ).toThrow(/l1SlotConfig must equal Mainnet's/u);
  });

  it("accepts a Custom network's genesis anchor, shape-checked only", () => {
    expect(() =>
      verifyDeploymentManifestL1SlotConfig("Custom", DEVNET),
    ).not.toThrow();
    expect(() =>
      verifyDeploymentManifestL1SlotConfig("Custom", undefined),
    ).toThrow(L1SlotConfigFormatError);
    expect(() =>
      verifyDeploymentManifestL1SlotConfig("Custom", {
        ...DEVNET,
        slotLength: 0,
      }),
    ).toThrow(L1SlotConfigFormatError);
  });
});
