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
  it("recovers a changed header reference only while the exact computation step remains current", async () => {
    const initial = cursorFamilyObservation({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
    });
    if (initial.kind !== "action_required") throw new Error("missing step");
    const input = {
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      headerHash,
      provenance,
      action: initial.action,
      txHash: hash("55"),
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("20"),
      },
      transactionConfirmed: async () => false,
    } as const;
    await expect(reconcileCursorFamilyAction(input)).resolves.toEqual({
      kind: "pending",
      txHash: hash("55"),
    });
    const recovery = {
      ...input,
      recoverUnconfirmedTransaction: async () =>
        ({ kind: "not_found" }) as const,
    };
    await expect(reconcileCursorFamilyAction(recovery)).resolves.toEqual({
      kind: "not_found",
    });
    await expect(
      reconcileCursorFamilyAction({
        ...recovery,
        stage: { ...input.stage, threadOutRef: outRef("12") },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcileCursorFamilyAction({
        ...recovery,
        stage: { ...input.stage, step: 2 },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcileCursorFamilyAction({
        ...recovery,
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("reconciles signed init after the authenticated header output is recreated", async () => {
    const spec = MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC;
    const initial = cursorFamilyObservation({
      spec,
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    });
    if (initial.kind !== "action_required") throw new Error("missing init");
    const input = {
      spec,
      headerHash,
      provenance,
      action: initial.action,
      txHash: hash("55"),
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("20") },
      transactionConfirmed: async () => false,
    } as const;
    await expect(reconcileCursorFamilyAction(input)).resolves.toEqual({
      kind: "pending",
      txHash: hash("55"),
    });
    await expect(
      reconcileCursorFamilyAction({
        ...input,
        recoverUnconfirmedTransaction: async () => ({ kind: "not_found" }),
      }),
    ).resolves.toEqual({ kind: "not_found" });
    await expect(
      reconcileCursorFamilyAction({
        ...input,
        transactionConfirmed: async () => true,
        recoverUnconfirmedTransaction: async () => ({ kind: "not_found" }),
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("keeps known transactions pending while finalized state is unchanged", async () => {
    for (const stage of [
      { kind: "not_started" as const, stateQueueBlockOutRef: outRef("10") },
      {
        kind: "step" as const,
        step: 1 as const,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
      {
        kind: "proof_token" as const,
        fraudProofOutRef: outRef("44"),
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: outRef("33"),
      },
    ]) {
      const observed = cursorFamilyObservation({
        spec,
        headerHash,
        provenance,
        stage,
      });
      if (observed.kind !== "action_required")
        throw new Error("missing action");
      const input = {
        spec,
        headerHash,
        provenance,
        stage,
        action: observed.action,
        transactionConfirmed: async () => false,
      } as const;
      await expect(
        reconcileCursorFamilyAction({ ...input, txHash: hash("55") }),
      ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      await expect(reconcileCursorFamilyAction(input)).resolves.toEqual({
        kind: "not_found",
      });
      if (stage.kind === "proof_token") {
        await expect(
          reconcileCursorFamilyAction({
            ...input,
            txHash: hash("55"),
            stage: { ...stage, nextRemovalOutRef: outRef("34") },
          }),
        ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      }
    }
  });

  it("rejects changed successors when transaction history does not confirm the intent", async () => {
    const observed = cursorFamilyObservation({
      spec,
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    });
    if (observed.kind !== "action_required") throw new Error("missing action");
    for (const threadOutRef of [outRef("55"), outRef("99")]) {
      await expect(
        reconcileCursorFamilyAction({
          spec,
          headerHash,
          provenance,
          action: observed.action,
          txHash: hash("55"),
          stage: {
            kind: "step",
            step: 1,
            threadOutRef,
            stateQueueBlockOutRef: outRef("10"),
          },
          transactionConfirmed: async () => false,
        }),
      ).resolves.toMatchObject({ kind: "conflict" });
    }
  });

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
