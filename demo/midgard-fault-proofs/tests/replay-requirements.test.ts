import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.js";
import {
  HISTORICAL_CORPUS_REPLAY_CATEGORIES,
  launchScopeRequires,
  PREDECESSOR_LEDGER_PROOF_CATEGORIES,
} from "../src/workflow/replay-requirements.js";

/**
 * The two replay requirement sets the header classifier reads are pinned
 * here, once, next to their relation with the registry's host-infrastructure
 * flags. The classifier itself carries no category literal.
 */
describe("replay requirement sets", () => {
  it("pins the families whose proof opens prev_utxos_root", () => {
    expect([...PREDECESSOR_LEDGER_PROOF_CATEGORIES].sort()).toEqual([
      "minAda",
      "missingNativeScriptUtxo",
      "noReferenceInput",
      "nonExistentInput",
    ]);
  });

  it("pins the families whose replay reads the historical corpus", () => {
    expect([...HISTORICAL_CORPUS_REPLAY_CATEGORIES].sort()).toEqual([
      "executionNativeScriptInvalid",
      "missingNativeScriptUtxo",
      "resolvedOutputNonCanonical",
      "spendInputSignerMissing",
      "transitionTrace",
    ]);
  });

  it("names only catalogue categories, each once", () => {
    for (const set of [
      PREDECESSOR_LEDGER_PROOF_CATEGORIES,
      HISTORICAL_CORPUS_REPLAY_CATEGORIES,
    ]) {
      expect(new Set(set).size).toBe(set.length);
      for (const category of set)
        expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toContain(category);
    }
  });

  it("keeps the corpus set inside the registry's historical-authority families", () => {
    // Detection needs the corpus for these five; the registry flag is wider
    // because three more families need the authority for their artifact.
    const authorityFamilies = Object.values(FAMILY_APPLICATION_REGISTRY)
      .filter((record) =>
        record.requires.includes("historicalNativeScriptAuthority"),
      )
      .map((record) => record.category);
    for (const category of HISTORICAL_CORPUS_REPLAY_CATEGORIES)
      expect(authorityFamilies).toContain(category);
    expect(authorityFamilies.length).toBeGreaterThan(
      HISTORICAL_CORPUS_REPLAY_CATEGORIES.length,
    );
  });

  it("is not the registry's replay-context set", () => {
    // Two families sit in both: their artifact re-derives from the admitted
    // context and carries a predecessor non-membership proof. The other
    // members of each set are distinct facts about distinct families.
    const replayContextFamilies = Object.values(FAMILY_APPLICATION_REGISTRY)
      .filter((record) => record.requires.includes("replayContext"))
      .map((record) => record.category)
      .sort();
    expect(
      PREDECESSOR_LEDGER_PROOF_CATEGORIES.filter((category) =>
        replayContextFamilies.includes(category),
      ).sort(),
    ).toEqual(["noReferenceInput", "nonExistentInput"]);
    expect(replayContextFamilies).not.toEqual(
      [...PREDECESSOR_LEDGER_PROOF_CATEGORIES].sort(),
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
    expect(launchScopeRequires([], HISTORICAL_CORPUS_REPLAY_CATEGORIES)).toBe(
      false,
    );
  });
});
