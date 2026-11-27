import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution, SlotConfig } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertLucidL1SlotConfigMatchesDeployment,
  deploymentL1SlotConfig,
  derivedDeploymentL1SlotConfig,
} from "../src/services/midgard-contracts.load-reference-script-auth-validator.js";

// A deployment's cek-context validator is applied with one L1 slot mapping,
// and every process building script contexts off chain must use the same
// one. A derived (manifest-less) bundle takes the mapping its L1 access reads
// off the ledger, which on a named network must be its static one; a
// process whose Lucid reads a different mapping is refused.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };

const lucidWith = (slotConfig: SlotConfig | undefined): LucidEvolution =>
  ({ config: () => ({ slotConfig }) }) as unknown as LucidEvolution;

describe("derivedDeploymentL1SlotConfig", () => {
  it("takes a Custom network's mapping as the L1 access reads it", () => {
    expect(derivedDeploymentL1SlotConfig("Custom", { ...DEVNET })).toEqual(
      DEVNET,
    );
  });

  it("takes a named network's static mapping when the ledger reports it", () => {
    for (const network of ["Mainnet", "Preprod", "Preview"] as const)
      expect(
        derivedDeploymentL1SlotConfig(network, {
          ...NAMED_NETWORK_L1_SLOT_CONFIGS[network],
        }),
      ).toEqual(NAMED_NETWORK_L1_SLOT_CONFIGS[network]);
  });

  it("refuses a named network whose ledger reports another mapping", () => {
    expect(() => derivedDeploymentL1SlotConfig("Preprod", DEVNET)).toThrow(
      /^l1_slot_config_mismatch: the L1 access reports zeroTime=1789056000000,zeroSlot=0,slotLength=1000, not Preprod's zeroTime=1655769600000,zeroSlot=86400,slotLength=1000$/u,
    );
  });

  it("refuses a malformed ledger mapping", () => {
    expect(() =>
      derivedDeploymentL1SlotConfig("Custom", { ...DEVNET, slotLength: 0 }),
    ).toThrow(/slotLength: must be positive/u);
  });
});

describe("deploymentL1SlotConfig", () => {
  it("reads a finalized manifest's mapping", () => {
    expect(
      deploymentL1SlotConfig({
        kind: "manifest",
        manifest: { l1SlotConfig: DEVNET },
      } as unknown as Parameters<typeof deploymentL1SlotConfig>[0]),
    ).toEqual(DEVNET);
  });

  it("reads the mapping a derived bundle recorded, and refuses one with none", () => {
    expect(
      deploymentL1SlotConfig({
        kind: "derived",
        l1SlotConfig: DEVNET,
      } as unknown as Parameters<typeof deploymentL1SlotConfig>[0]),
    ).toEqual(DEVNET);
    expect(() =>
      deploymentL1SlotConfig({ kind: "derived" } as unknown as Parameters<
        typeof deploymentL1SlotConfig
      >[0]),
    ).toThrow(/records no L1 slot mapping/u);
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
