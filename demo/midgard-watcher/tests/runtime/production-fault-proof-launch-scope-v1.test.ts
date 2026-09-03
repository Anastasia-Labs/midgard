import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { assertWatcherFaultProofLaunchScope } from "../../src/runtime/production-watcher-runtime-v1.js";

describe("production fault-proof launch scope", () => {
  it("admits only the exact complete canonical catalogue", () => {
    expect(() =>
      assertWatcherFaultProofLaunchScope(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER),
    ).not.toThrow();

    const narrow = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(0, -1);
    expect(() => assertWatcherFaultProofLaunchScope(narrow)).toThrow(
      "exact canonical catalogue",
    );

    const swapped = [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER];
    [swapped[0], swapped[1]] = [swapped[1]!, swapped[0]!];
    expect(() => assertWatcherFaultProofLaunchScope(swapped)).toThrow(
      "exact canonical catalogue",
    );

    expect(() =>
      assertWatcherFaultProofLaunchScope([
        ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
        "callerInventedFamily",
      ]),
    ).toThrow("exact canonical catalogue");
  });
});
