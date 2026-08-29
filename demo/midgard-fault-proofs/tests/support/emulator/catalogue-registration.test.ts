import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { makeFaultProofEmulatorHarnessV1 } from "./harness.js";

const APPENDED_CATEGORY_NAMES = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(11);

describe("fault-proof emulator catalogue registration", () => {
  it("registers every appended production category from its canonical chain", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1();

    expect(APPENDED_CATEGORY_NAMES).toHaveLength(17);
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
      expect(category.membershipProofCbor).not.toBe("");
    }
  });
});
