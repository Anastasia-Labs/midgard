import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.js";
import {
  launchScopeRequires,
  PREDECESSOR_LEDGER_PROOF_CATEGORIES,
} from "../src/workflow/replay-requirements.js";

/**
 * The replay requirement set the header classifier reads is pinned here,
 * once, next to its relation with the registry's host-infrastructure flags.
 * The classifier itself carries no category literal.
 */
describe("replay requirement sets", () => {
  it("pins the families whose proof replays against prev_utxos_root", () => {
    expect([...PREDECESSOR_LEDGER_PROOF_CATEGORIES].sort()).toEqual([
      "executionNativeScriptInvalid",
      "minAda",
      "noReferenceInput",
      "nonExistentInput",
      "resolvedOutputNonCanonical",
      "spendInputSignerMissing",
      "transitionTrace",
    ]);
  });

  it("names only catalogue categories, each once", () => {
    expect(new Set(PREDECESSOR_LEDGER_PROOF_CATEGORIES).size).toBe(
      PREDECESSOR_LEDGER_PROOF_CATEGORIES.length,
    );
    for (const category of PREDECESSOR_LEDGER_PROOF_CATEGORIES)
      expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toContain(category);
  });

  it("requires the admitted replay context for every predecessor-ledger family", () => {
    // Each of these families reads the classifier-admitted predecessor from
    // the replay context; the registry's replay-context set is wider because
    // other families re-derive their artifact from the same context without
    // opening the previous ledger.
    const replayContextFamilies = Object.values(FAMILY_APPLICATION_REGISTRY)
      .filter((record) => record.requires.includes("replayContext"))
      .map((record) => record.category);
    for (const category of PREDECESSOR_LEDGER_PROOF_CATEGORIES)
      expect(replayContextFamilies).toContain(category);
    expect(replayContextFamilies.length).toBeGreaterThan(
      PREDECESSOR_LEDGER_PROOF_CATEGORIES.length,
    );
  });

  it("answers a launch scope by membership", () => {
    expect(
      launchScopeRequires(
        ["zeroInput", "minAda"],
        PREDECESSOR_LEDGER_PROOF_CATEGORIES,
      ),
    ).toBe(true);
    expect(
      launchScopeRequires(["zeroInput"], PREDECESSOR_LEDGER_PROOF_CATEGORIES),
    ).toBe(false);
    expect(launchScopeRequires([], PREDECESSOR_LEDGER_PROOF_CATEGORIES)).toBe(
      false,
    );
  });
});
