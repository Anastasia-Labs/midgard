import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../src/execution-native-script-invalid/workflow-spec.js";
import {
  CURSOR_FAMILY_SPECS,
  MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
} from "../src/workflow/cursor-family-spec.js";
import {
  cursorFamilyObservation,
  reconcileCursorFamilyAction,
} from "../src/workflow/cursor-family-state.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const spec = MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC;

const step = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, ref: string) => ({
  kind: "step" as const,
  step: ordinal,
  threadOutRef: ref,
  stateQueueBlockOutRef: outRef("10"),
});

const actionFor = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, ref: string) => {
  const observed = cursorFamilyObservation({
    spec,
    headerHash,
    provenance,
    stage: step(ordinal, ref),
  });
  if (observed.kind !== "action_required") throw new Error("missing action");
  return observed.action;
};

describe("production cursor-family authenticated state V1", () => {
  it("accepts the exact direct and staged missing-native-script successors", async () => {
    const step06Action = actionFor(6, outRef("60"));
    const directTx = hash("61");
    await expect(
      reconcileCursorFamilyAction({
        spec,
        headerHash,
        action: step06Action,
        txHash: directTx,
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: `${directTx}#0`,
          stateQueueBlockOutRef: outRef("10"),
          nextRemovalOutRef: outRef("55"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: directTx });

    const stagedTx = hash("62");
    await expect(
      reconcileCursorFamilyAction({
        spec,
        headerHash,
        action: step06Action,
        txHash: stagedTx,
        provenance,
        stage: step(7, `${stagedTx}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: stagedTx });
  });

  it("content-addresses and reconciles repeated step-07/08 cursor batches", async () => {
    for (const ordinal of [7, 8] as const) {
      const current = outRef(ordinal === 7 ? "70" : "80");
      const txHash = hash(ordinal === 7 ? "71" : "81");
      const required = actionFor(ordinal, current);
      await expect(
        reconcileCursorFamilyAction({
          spec,
          headerHash,
          action: required,
          txHash,
          provenance,
          stage: step(ordinal, `${txHash}#1`),
          transactionConfirmed: async () => true,
        }),
      ).resolves.toEqual({ kind: "confirmed", txHash });
    }
  });

  it("preserves two-digit stage ordinals for the 13-script execution family", () => {
    const observed = cursorFamilyObservation({
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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

  it("rejects skipped, substituted, and unauthenticated successors", async () => {
    const action = actionFor(6, outRef("60"));
    await expect(
      reconcileCursorFamilyAction({
        spec,
        headerHash,
        action,
        txHash: hash("61"),
        provenance,
        stage: step(8, `${hash("61")}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
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
        spec: { ...spec, successors: { ...spec.successors, 8: [] } },
        headerHash,
        provenance,
        stage: step(1, outRef("11")),
      }),
    ).toThrow("omits an exact legal successor");
    expect(() =>
      cursorFamilyObservation({
        spec: { ...spec, stepCount: 7, successors: { ...spec.successors } },
        headerHash,
        provenance,
        stage: step(8, outRef("88")),
      }),
    ).toThrow();
  });

  it("admits every closed bespoke topology without implying readiness", () => {
    expect(Object.keys(CURSOR_FAMILY_SPECS)).toEqual([
      "nativeScriptDecoding",
      "missingNativeScriptTx",
      "withdrawalMistag",
      "crossBlockDuplicateEvent",
      "valueNotPreserved",
      "mintAuthorization",
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
