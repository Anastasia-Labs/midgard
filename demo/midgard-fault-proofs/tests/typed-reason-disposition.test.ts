import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  RejectionReasonSchema,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  assertNonInteractiveReasonsInstalled,
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
    expect(arms).toHaveLength(49);
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
    expect(NON_INTERACTIVE_DIRECT_CATEGORIES).toHaveLength(36);
  });

  it("keeps excluded global routes explicit and closes every supported direct route", () => {
    const installed = Object.keys(WORKFLOW_RUNNER_FACTORIES);
    const excluded =
      DEPLOYMENT_PROFILES["preprod-public"].excluded_fraud_proof_categories;
    // The complete global taxonomy keeps these P1-excluded routes visible.
    // Their workflows retired because DA carries no validation-trace states.
    expect(missingNonInteractiveInstallations(installed)).toEqual([
      "ScriptSourceMissing -> missingScriptSource",
      "RedeemerMissing -> missingRedeemer",
      "UnusedScriptWitness -> unusedScriptWitness",
      "UnusedRedeemer -> unusedRedeemer",
      "ExecutionNativeScriptMalformed -> executionSourceScriptDecoding",
      "ExecutionNativeScriptNodeLimit -> executionSourceScriptDecoding",
      "ExecutionNativeScriptDepthLimit -> executionSourceScriptDecoding",
      "ExecutionNativeScriptFalse -> executionNativeScriptInvalid",
      "ScriptIntegrityHashMismatch -> scriptIntegrityHashMismatch",
      "ReceivePurposePlutusV3Forbidden -> receivePurposeLanguage",
      "InputAssetAccumulationLimit -> distinctAssetAccumulationLimit",
      "OutputAssetAccumulationLimit -> distinctAssetAccumulationLimit",
      "MintAssetAccumulationLimit -> distinctAssetAccumulationLimit",
    ]);
    expect(
      missingNonInteractiveInstallations([...installed, ...excluded]),
    ).toEqual([]);
    // The strict global guard still refuses this incomplete global surface.
    expect(() =>
      assertNonInteractiveReasonsInstalled({
        surface: "global registry",
        installedCategories: installed,
      }),
    ).toThrow(/ScriptSourceMissing -> missingScriptSource/u);
  });

  it("refuses an omitted supported category with every P1 exclusion accounted for", () => {
    const excluded =
      DEPLOYMENT_PROFILES["preprod-public"].excluded_fraud_proof_categories;
    const installed = Object.keys(WORKFLOW_RUNNER_FACTORIES).filter(
      (category) => category !== "minFee",
    );
    expect(
      missingNonInteractiveInstallations([...installed, ...excluded]),
    ).toEqual(["FeeBelowMinimum -> minFee"]);
    expect(() =>
      assertNonInteractiveReasonsInstalled({
        surface: "supported registry",
        installedCategories: [...installed, ...excluded],
      }),
    ).toThrow(/FeeBelowMinimum -> minFee/u);
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
