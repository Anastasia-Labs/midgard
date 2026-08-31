import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
  PRODUCTION_CURSOR_FAMILY_SPECS_V1,
} from "../src/workflow/production-cursor-family-spec-v1.js";
import {
  productionCursorFamilyObservationV1,
  reconcileProductionCursorFamilyActionV1,
} from "../src/workflow/production-cursor-family-state-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const spec = MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1;

const step = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, ref: string) => ({
  kind: "step" as const,
  step: ordinal,
  threadOutRef: ref,
  stateQueueBlockOutRef: outRef("10"),
});

const actionFor = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, ref: string) => {
  const observed = productionCursorFamilyObservationV1({
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
      reconcileProductionCursorFamilyActionV1({
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
      reconcileProductionCursorFamilyActionV1({
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
        reconcileProductionCursorFamilyActionV1({
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

  it("rejects skipped, substituted, and unauthenticated successors", async () => {
    const action = actionFor(6, outRef("60"));
    await expect(
      reconcileProductionCursorFamilyActionV1({
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
      productionCursorFamilyObservationV1({
        spec,
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: step(1, outRef("11")),
      }),
    ).toThrow(/prohibited_trust_class|not authenticated Cardano L1/u);
  });

  it("rejects incomplete successor tables and out-of-range chain steps", () => {
    expect(() =>
      productionCursorFamilyObservationV1({
        spec: { ...spec, successors: { ...spec.successors, 8: [] } },
        headerHash,
        provenance,
        stage: step(1, outRef("11")),
      }),
    ).toThrow("omits an exact legal successor");
    expect(() =>
      productionCursorFamilyObservationV1({
        spec: { ...spec, stepCount: 7, successors: { ...spec.successors } },
        headerHash,
        provenance,
        stage: step(8, outRef("88")),
      }),
    ).toThrow();
  });

  it("admits every closed bespoke topology without implying readiness", () => {
    expect(Object.keys(PRODUCTION_CURSOR_FAMILY_SPECS_V1)).toEqual([
      "nativeScriptDecoding",
      "missingNativeScriptTx",
      "withdrawalMistag",
      "crossBlockDuplicateEvent",
      "valueNotPreserved",
      "mintAuthorization",
    ]);
    for (const candidate of Object.values(PRODUCTION_CURSOR_FAMILY_SPECS_V1)) {
      expect(Object.isFrozen(candidate)).toBe(true);
      expect(Object.isFrozen(candidate.successors)).toBe(true);
      for (const successors of Object.values(candidate.successors)) {
        expect(Object.isFrozen(successors)).toBe(true);
      }
      expect(() =>
        productionCursorFamilyObservationV1({
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
