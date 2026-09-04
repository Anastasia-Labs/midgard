import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  RejectionReasonSchema,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  assertNonInteractiveReasonsInstalled,
  directCategoryOfTypedReason,
  INTERACTIVE_TYPED_REASON_ARMS,
  missingNonInteractiveInstallations,
  NON_INTERACTIVE_DIRECT_CATEGORIES,
  TYPED_REASON_ARMS,
  TYPED_REASON_DISPOSITIONS,
} from "../src/workflow/reason-disposition.js";
import { WORKFLOW_RUNNER_FACTORIES } from "../src/workflow/runtime.js";

/** Constructor tags of the SDK `RejectionReason` schema, in declaration order. */
const schemaArms = (): readonly string[] => {
  const schema = RejectionReasonSchema as unknown as {
    readonly anyOf: readonly Record<string, unknown>[];
  };
  return schema.anyOf.map((variant) => {
    if (typeof variant.const === "string") return variant.const;
    if (typeof variant.title === "string" && variant.properties === undefined)
      return variant.title;
    const properties = variant.properties as Record<string, unknown>;
    const [arm] = Object.keys(properties);
    if (arm === undefined) throw new Error("variant without constructor tag");
    return arm;
  });
};

describe("typed rejection-reason disposition", () => {
  it("covers every RejectionReason constructor exactly once", () => {
    const arms = schemaArms();
    expect(arms).toHaveLength(47);
    expect([...TYPED_REASON_ARMS].sort()).toEqual([...arms].sort());
  });

  it("leaves PlutusExecutionFailed as the sole interactive reason", () => {
    const interactive = TYPED_REASON_ARMS.filter(
      (arm) => TYPED_REASON_DISPOSITIONS[arm].proving === "interactive",
    );
    expect(interactive).toEqual([...INTERACTIVE_TYPED_REASON_ARMS]);
    expect(TYPED_REASON_DISPOSITIONS.PlutusExecutionFailed.categories).toEqual([
      "validationTraceDispute",
    ]);
    for (const arm of TYPED_REASON_ARMS) {
      if (arm === "PlutusExecutionFailed") continue;
      const { categories } = TYPED_REASON_DISPOSITIONS[arm];
      expect(categories.length, arm).toBeGreaterThan(0);
      expect(categories, arm).not.toContain("validationTraceDispute");
      expect(categories, arm).not.toContain("transitionTrace");
    }
  });

  it("names only registered catalogue categories", () => {
    const registered = new Set(Object.keys(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS));
    for (const arm of TYPED_REASON_ARMS) {
      for (const category of TYPED_REASON_DISPOSITIONS[arm].categories) {
        expect(registered.has(category), `${arm} -> ${category}`).toBe(true);
      }
    }
    expect(NON_INTERACTIVE_DIRECT_CATEGORIES).toHaveLength(35);
  });

  it("splits InputNotFound by source kind and refuses other coordinates", () => {
    expect(
      directCategoryOfTypedReason("InputNotFound", { source_kind: 0n }),
    ).toBe("nonExistentInput");
    expect(
      directCategoryOfTypedReason("InputNotFound", { source_kind: 1n }),
    ).toBe("noReferenceInput");
    expect(() => directCategoryOfTypedReason("InputNotFound")).toThrow(
      /source_kind 0 or 1/u,
    );
    expect(directCategoryOfTypedReason("ValueNotPreserved")).toBe(
      "valueNotPreserved",
    );
  });

  it("pins the runner-registry residue the program closure must empty", () => {
    // Program closure (§8 step 5) requires this list to be empty: every
    // remaining entry is a typed reason that production classification would
    // still route to validationTraceDispute. Shrink it, never grow it.
    expect(
      missingNonInteractiveInstallations(
        Object.keys(WORKFLOW_RUNNER_FACTORIES),
      ),
    ).toEqual([
      "ResolvedReferenceScriptMalformed -> nativeScriptDecoding",
      "ResolvedReferenceScriptNodeLimit -> nativeScriptDecoding",
      "ResolvedReferenceScriptDepthLimit -> nativeScriptDecoding",
      "ValueNotPreserved -> valueNotPreserved",
    ]);
  });

  it("lists every omission when a surface lacks a direct category", () => {
    expect(() =>
      assertNonInteractiveReasonsInstalled({
        surface: "probe",
        installedCategories: NON_INTERACTIVE_DIRECT_CATEGORIES.filter(
          (category) =>
            category !== "valueNotPreserved" && category !== "noReferenceInput",
        ),
      }),
    ).toThrow(
      /probe would route .*InputNotFound -> noReferenceInput.*ValueNotPreserved -> valueNotPreserved/u,
    );
  });
});
