import "./cursor-family-state.descendant-removal.js";

import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { CURSOR_FAMILY_SPECS } from "../src/workflow/cursor-family-spec.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
  type CursorFamilyStep,
  reconcileCursorFamilyAction,
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

const actionFor = (ordinal: SpecStep, ref: string) => {
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
      spec,
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
      spec,
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

  it("accepts the exact direct and staged successors of a branching topology", async () => {
    const step04Action = actionFor(4, outRef("40"));
    const directTx = hash("61");
    await expect(
      reconcileCursorFamilyAction({
        spec,
        headerHash,
        action: step04Action,
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
        action: step04Action,
        txHash: stagedTx,
        provenance,
        stage: step(5, `${stagedTx}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: stagedTx });
  });

  it("content-addresses and reconciles repeated step-05/06 cursor batches", async () => {
    for (const ordinal of [5, 6] as const) {
      const current = outRef(ordinal === 5 ? "50" : "60");
      const txHash = hash(ordinal === 5 ? "51" : "61");
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

  it("drives a 39-step chain to its last step and refuses a 40th", async () => {
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
    const step38 = observe(38, outRef("38"));
    if (step38.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileCursorFamilyAction({
        spec: longest,
        headerHash,
        action: step38.action,
        txHash: hash("39"),
        provenance,
        stage: at(39, `${hash("39")}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: hash("39") });
    const step39 = observe(39, outRef("39"));
    expect(step39).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "step_39", ordinal: 39 } },
    });
    if (step39.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileCursorFamilyAction({
        spec: longest,
        headerHash,
        action: step39.action,
        txHash: hash("3a"),
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: `${hash("3a")}#0`,
          stateQueueBlockOutRef: outRef("10"),
          nextRemovalOutRef: outRef("55"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: hash("3a") });
    expect(() =>
      cursorFamilyObservation({
        spec: linear(40),
        headerHash,
        provenance,
        stage: at(1, outRef("11")),
      }),
    ).toThrow("invalid step count");
    const beyond = {
      ...step39.action,
      actionId: `step_40:${outRef("39")}:${outRef("10")}`,
      input: { ...step39.action.input, stage: "step_40", ordinal: 40 },
    };
    await expect(
      reconcileCursorFamilyAction({
        spec: longest,
        headerHash,
        action: beyond,
        txHash: hash("3b"),
        provenance,
        stage: at(39, `${hash("3b")}#0`),
        transactionConfirmed: async () => true,
      }),
    ).rejects.toThrow("unknown stage");
  });

  it("rejects skipped, substituted, and unauthenticated successors", async () => {
    const action = actionFor(4, outRef("40"));
    await expect(
      reconcileCursorFamilyAction({
        spec,
        headerHash,
        action,
        txHash: hash("41"),
        provenance,
        stage: step(6, `${hash("41")}#0`),
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
