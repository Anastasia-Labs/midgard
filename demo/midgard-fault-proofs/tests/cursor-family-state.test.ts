import type { EvidenceProvenance } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import { CURSOR_FAMILY_SPECS } from "../src/workflow/cursor-family-spec.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
  type CursorFamilyStep,
} from "../src/workflow/cursor-family-state.js";
import { THIRTEEN_STEP_CURSOR_SPEC } from "./support/thirteen-step-cursor-spec.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

// Step 4 forks to a direct proof token or the staged 5/6 self-loop batches.
const spec = THIRTEEN_STEP_CURSOR_SPEC;

type SpecStep = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13;

const step = (ordinal: SpecStep, ref: string) => ({
  kind: "step" as const,
  step: ordinal,
  threadOutRef: ref,
  stateQueueBlockOutRef: outRef("10"),
});

describe("production cursor-family authenticated state V1", () => {
  it("preserves two-digit stage ordinals for the 13-script execution family", () => {
    const observed = cursorFamilyObservation({
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 13,
        threadOutRef: outRef("13"),
        stateQueueBlockOutRef: outRef("10"),
      },
    });
    expect(observed).toMatchObject({
      kind: "action_required",
      action: {
        actionId: `step_13:${outRef("13")}:${outRef("10")}`,
        input: { stage: "step_13", ordinal: 13 },
      },
    });
  });

  it("drives a 39-step chain to its last step and refuses a 40th", () => {
    const linear = (stepCount: number): CursorFamilySpec =>
      ({
        category: "missingRedeemer",
        stepCount,
        successors: Object.fromEntries(
          Array.from({ length: stepCount }, (_, index) => [
            index + 1,
            index + 1 === stepCount ? ["proof_token"] : [index + 2],
          ]),
        ),
      }) as CursorFamilySpec;
    const longest = linear(39);
    const at = (ordinal: number, ref: string) => ({
      kind: "step" as const,
      step: ordinal as CursorFamilyStep,
      threadOutRef: ref,
      stateQueueBlockOutRef: outRef("10"),
    });
    const observe = (ordinal: number, ref: string) =>
      cursorFamilyObservation({
        spec: longest,
        headerHash,
        provenance,
        stage: at(ordinal, ref),
      });
    expect(observe(38, outRef("38"))).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "step_38", ordinal: 38 } },
    });
    expect(observe(39, outRef("39"))).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "step_39", ordinal: 39 } },
    });
    expect(() =>
      cursorFamilyObservation({
        spec: linear(40),
        headerHash,
        provenance,
        stage: at(1, outRef("11")),
      }),
    ).toThrow("invalid step count");
  });

  it("rejects unauthenticated provenance", () => {
    expect(() =>
      cursorFamilyObservation({
        spec,
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: step(1, outRef("11")),
      }),
    ).toThrow(/prohibited_trust_class|not authenticated Cardano L1/u);
  });

  it("rejects incomplete successor tables and out-of-range chain steps", () => {
    expect(() =>
      cursorFamilyObservation({
        spec: { ...spec, successors: { ...spec.successors, 13: [] } },
        headerHash,
        provenance,
        stage: step(1, outRef("11")),
      }),
    ).toThrow("omits an exact legal successor");
    expect(() =>
      cursorFamilyObservation({
        spec: { ...spec, stepCount: 12, successors: { ...spec.successors } },
        headerHash,
        provenance,
        stage: step(13, outRef("88")),
      }),
    ).toThrow();
  });

  it("admits every closed bespoke topology without implying readiness", () => {
    expect(Object.keys(CURSOR_FAMILY_SPECS)).toEqual([
      "nativeScriptDecoding",
      "withdrawalMistag",
      "valueNotPreserved",
      "mintAuthorization",
      "missingRedeemer",
      "missingScriptSource",
      "unusedRedeemer",
      "receivePurposeLanguage",
      "scriptIntegrityHashMismatch",
      "executionSourceScriptDecoding",
      "executionNativeScriptInvalid",
      "distinctAssetAccumulationLimit",
      "unusedScriptWitness",
      "canonicalDecodability",
      "committedFieldShape",
    ]);
    for (const candidate of Object.values(CURSOR_FAMILY_SPECS)) {
      expect(Object.isFrozen(candidate)).toBe(true);
      expect(Object.isFrozen(candidate.successors)).toBe(true);
      for (const successors of Object.values(candidate.successors)) {
        expect(Object.isFrozen(successors)).toBe(true);
      }
      expect(() =>
        cursorFamilyObservation({
          spec: candidate,
          headerHash,
          provenance,
          stage: {
            kind: "not_started",
            stateQueueBlockOutRef: outRef("10"),
          },
        }),
      ).not.toThrow();
    }
  });
});
