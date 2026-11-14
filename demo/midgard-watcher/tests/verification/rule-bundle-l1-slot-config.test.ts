import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { describe, expect, it } from "vitest";

import {
  computeWatcherRuleBundleCommitment,
  loadWatcherRuleBundle,
  makeWatcherCanonicalRuleBundle,
  parseWatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { h32 } from "./rule-bundle.canonical-manifest-identity.js";
import { clone, fixture, rejected } from "./rule-bundle.fixture.js";

// Script contexts carry a body's validity range in POSIX milliseconds through
// the deployment's L1 slot mapping, so the mapping is part of what the watcher
// replays under: the signed rule bundle carries it, exactly, and a named
// network's must be that network's static mapping.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };

describe("watcher rule bundle L1 slot mapping", () => {
  it("carries the deployment's mapping under the signed commitment", () => {
    const { authority, bundle } = fixture();
    expect(bundle.l1SlotConfig).toEqual(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod);
    const loaded = loadWatcherRuleBundle({ ...authority, ruleBundle: bundle });
    expect(loaded.ruleBundle.l1SlotConfig).toEqual(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    );
    expect(Object.isFrozen(loaded.ruleBundle.l1SlotConfig)).toBe(true);
  });

  it("refuses a missing or malformed mapping", () => {
    const { bundle } = fixture();

    const missing = clone(bundle) as Record<string, unknown>;
    delete missing.l1SlotConfig;
    rejected(
      () => parseWatcherRuleBundle(missing),
      "missing_field",
      "$.l1SlotConfig",
    );

    for (const malformed of [
      null,
      { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, era: "Shelley" },
      { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, slotLength: 0 },
      { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, zeroSlot: "86400" },
    ]) {
      const candidate = clone(bundle) as Record<string, unknown>;
      candidate.l1SlotConfig = malformed;
      rejected(
        () => parseWatcherRuleBundle(candidate),
        "invalid_field",
        "$.l1SlotConfig",
      );
    }
  });

  it("refuses a named network carrying any mapping but its static one", () => {
    const { bundle } = fixture();
    for (const other of [NAMED_NETWORK_L1_SLOT_CONFIGS.Preview, DEVNET]) {
      const candidate = clone(bundle);
      candidate.l1SlotConfig = other;
      rejected(
        () => parseWatcherRuleBundle(candidate),
        "invalid_field",
        "$.l1SlotConfig",
      );
    }
    expect(() =>
      makeWatcherCanonicalRuleBundle({
        constructionIdentity: {
          manifestId: h32("1"),
          network: "Preprod",
          blueprintHash: h32("2"),
          l1SlotConfig: DEVNET,
          programCommitments: {},
        },
        targetParameterSnapshot: {},
      }),
    ).toThrow();
  });

  it("accepts a Custom network's genesis anchor", () => {
    const { bundle } = fixture();
    const custom = clone(bundle);
    custom.network = "Custom";
    custom.l1SlotConfig = DEVNET;
    expect(parseWatcherRuleBundle(custom).l1SlotConfig).toEqual(DEVNET);
  });

  it("binds the mapping into the commitment and refuses a re-mapped bundle on load", () => {
    const { authority, bundle } = fixture();
    const custom = clone(bundle);
    custom.network = "Custom";
    custom.l1SlotConfig = DEVNET;
    const shortSlots = clone(custom);
    shortSlots.l1SlotConfig = { ...DEVNET, slotLength: 100 };
    expect(computeWatcherRuleBundleCommitment(shortSlots)).not.toBe(
      computeWatcherRuleBundleCommitment(custom),
    );
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: { ...clone(bundle), l1SlotConfig: DEVNET },
        }),
      "invalid_field",
      "$.l1SlotConfig",
    );
  });
});
