import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { assertWatcherFaultProofLaunchScope } from "../../src/runtime/watcher-runtime.js";

const CANONICAL = [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER];

describe("production fault-proof launch scope", () => {
  it("admits the scope startup actually hands it", () => {
    // `watcher-runtime` calls this guard with the production application's
    // `installedCategories`, which is `WATCHER_INSTALLED_WORKFLOW_CATEGORIES`.
    // Feeding the guard that same roster is the case that must not refuse; the
    // guard's own comparison basis is the SDK catalogue, so the two rosters
    // drifting apart is refused here rather than at startup.
    expect(() =>
      assertWatcherFaultProofLaunchScope([
        ...WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      ]),
    ).not.toThrow();
  });

  const rejected = [
    {
      name: "a truncated catalogue",
      scope: CANONICAL.slice(0, -1),
    },
    {
      name: "a catalogue missing an interior family",
      scope: CANONICAL.filter((category) => category !== "missingSignature"),
    },
    {
      name: "two families in swapped order",
      scope: [CANONICAL[1]!, CANONICAL[0]!, ...CANONICAL.slice(2)],
    },
    {
      name: "a catalogue-length scope substituting a foreign family",
      scope: [...CANONICAL.slice(0, -1), "callerInventedFamily"],
    },
    {
      name: "a catalogue-length scope repeating one family",
      scope: [...CANONICAL.slice(0, -1), CANONICAL[0]!],
    },
    {
      name: "the catalogue extended with a foreign family",
      scope: [...CANONICAL, "callerInventedFamily"],
    },
    {
      name: "an empty scope",
      scope: [],
    },
  ] as const;

  it.each(rejected)("refuses $name", ({ scope }) => {
    expect(() => assertWatcherFaultProofLaunchScope(scope)).toThrow(
      "does not cover the exact canonical catalogue",
    );
  });
});
