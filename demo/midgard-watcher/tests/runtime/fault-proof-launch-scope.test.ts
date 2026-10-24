import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_MISSING_WORKFLOW_CATEGORIES,
} from "../../src/fault-proofs/fault-proof-application.js";
import { assertWatcherFaultProofLaunchScope } from "../../src/runtime/watcher-runtime.js";

const INSTALLED = [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES];
const UNINSTALLED = WATCHER_MISSING_WORKFLOW_CATEGORIES[0]!;

describe("production fault-proof launch scope", () => {
  it("admits the scope startup actually hands it", () => {
    // `watcher-runtime` calls this guard with the production application's
    // `installedCategories`, which is `WATCHER_INSTALLED_WORKFLOW_CATEGORIES`.
    expect(() =>
      assertWatcherFaultProofLaunchScope([
        ...WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      ]),
    ).not.toThrow();
  });

  it("is narrower than the catalogue while some families have no workflow", () => {
    expect(WATCHER_MISSING_WORKFLOW_CATEGORIES.length).toBeGreaterThan(0);
    expect(INSTALLED.length).toBeLessThan(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
  });

  const rejected = [
    {
      name: "the installed set missing its last category",
      scope: INSTALLED.slice(0, -1),
    },
    {
      name: "the installed set missing an interior category",
      scope: INSTALLED.filter((_, index) => index !== 1),
    },
    {
      name: "two categories in swapped order",
      scope: [INSTALLED[1]!, INSTALLED[0]!, ...INSTALLED.slice(2)],
    },
    {
      name: "the installed set extended with an uninstalled catalogue category",
      scope: [...INSTALLED, UNINSTALLED],
    },
    {
      name: "the full catalogue",
      scope: [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER],
    },
    {
      name: "an installed-length scope substituting a foreign family",
      scope: [...INSTALLED.slice(0, -1), "callerInventedFamily"],
    },
    {
      name: "an installed-length scope repeating one category",
      scope: [...INSTALLED.slice(0, -1), INSTALLED[0]!],
    },
    {
      name: "the installed set extended with a foreign family",
      scope: [...INSTALLED, "callerInventedFamily"],
    },
    {
      name: "an empty scope",
      scope: [],
    },
  ] as const;

  it.each(rejected)("refuses $name", ({ scope }) => {
    expect(() => assertWatcherFaultProofLaunchScope(scope)).toThrow(
      "does not cover the exact installed workflow set",
    );
  });
});
