import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  encodeCatalogueKey,
  encodeCatalogueValue,
  trieRootHex,
} from "./catalogue.js";
import { makeFaultProofEmulatorHarness } from "./harness.js";

/**
 * The 11 foundational categories are registered by the base deployment; the
 * rest are appended by the families that reached central registration. The
 * split point is a property of the canonical order, so the appended slice is
 * derived from it rather than from a hand-maintained count; only its
 * non-vacuity is asserted, so an empty slice cannot report green.
 */
const FOUNDATIONAL_CATEGORY_COUNT = 11;
const APPENDED_CATEGORY_NAMES = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(
  FOUNDATIONAL_CATEGORY_COUNT,
);

describe("fault-proof emulator catalogue registration", () => {
  it.each([
    ["missingNativeScriptUtxo", "realMissingNativeScriptUtxo"],
    ["nativeScriptInvalid", "realNativeScriptInvalid"],
    ["minAda", "realMinAda"],
  ] as const)(
    "registers the selected real %s first step",
    async (categoryName, optionName) => {
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: {
          [optionName]: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const family = harness.contracts[categoryName];
      expect(family).toBeDefined();
      expect(harness.catalogue.categories[categoryName].scriptHash).toBe(
        family!.steps[0].spendingScriptHash,
      );
    },
    120_000,
  );

  it("registers every appended production category from its canonical chain", async () => {
    const harness = await makeFaultProofEmulatorHarness();

    expect(
      APPENDED_CATEGORY_NAMES.length,
      "the appended-category loop must not be vacuous",
    ).toBeGreaterThan(0);
    for (const name of APPENDED_CATEGORY_NAMES) {
      const category = harness.catalogue.categories[name];
      const firstStep = harness.contracts.fraudProofContracts[name].firstStep;

      expect(category.categoryId).toBe(
        FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
      );
      expect(category.scriptHash).toBe(firstStep.spendingScriptHash);
      expect(harness.contracts.fraudProofs[name].spendingScriptHash).toBe(
        firstStep.spendingScriptHash,
      );
    }
  });

  it("publishes a membership proof that reproves each pair under the published root", async () => {
    const harness = await makeFaultProofEmulatorHarness();

    // Independent reference model: rebuild the catalogue trie with the
    // third-party MPF library from the declared (categoryId, scriptHash)
    // pairs, then require the published root and every published proof to be
    // exactly what that trie yields. A category wired under the wrong hash or
    // id, or a proof that does not actually witness its pair, fails here --
    // `membershipProofCbor !== ""` could not tell the difference.
    const store = new Store(undefined);
    await store.ready();
    const reference = new Trie(store);
    for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      const category = harness.catalogue.categories[name];
      await reference.insert(
        encodeCatalogueKey(category.categoryId),
        encodeCatalogueValue(category.scriptHash),
      );
    }

    expect(trieRootHex(reference)).toBe(harness.catalogue.root);
    for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      const category = harness.catalogue.categories[name];
      const proof = await reference.prove(
        encodeCatalogueKey(category.categoryId),
      );
      expect(
        proof.verify(true)?.toString("hex"),
        `${name} membership proof must recompute the catalogue root`,
      ).toBe(harness.catalogue.root);
      expect(
        category.membershipProofCbor,
        `${name} published membership proof`,
      ).toBe(proof.toCBOR().toString("hex"));
    }
  }, 120_000);
});
