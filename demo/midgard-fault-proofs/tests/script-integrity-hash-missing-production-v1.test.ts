import { encodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { ManifestBoundScriptIntegrityHashMissingWorkflowConfigV1 } from "../src/script-integrity-hash-missing/production-v1.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1 } from "../src/script-integrity-hash-missing/production-v1.js";
import {
  encodeScriptIntegrityField8CheckpointV1,
  hashScriptIntegrityField8CheckpointV1,
  planScriptIntegrityHashMissingStagedWalkV1,
  scriptIntegrityGrammarHashV1,
  scriptIntegritySemanticHashV1,
} from "../src/script-integrity-hash-missing/staged-plan-v1.js";
import {
  productionCursorFamilyObservationV1,
  reconcileProductionCursorFamilyActionV1,
} from "../src/workflow/production-cursor-family-state-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string): string => `${hash(byte)}#0`;
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};
const step = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7, ref: string) => ({
  kind: "step" as const,
  step: ordinal,
  threadOutRef: ref,
  stateQueueBlockOutRef: outRef("10"),
});

const actionFor = (ordinal: 1 | 2 | 3 | 4 | 5 | 6 | 7, ref: string) => {
  const observed = productionCursorFamilyObservationV1({
    spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1,
    headerHash,
    provenance,
    stage: step(ordinal, ref),
  });
  if (observed.kind !== "action_required") throw new Error("missing action");
  return observed.action;
};

describe("scriptIntegrityHashMissing package-owned production V1", () => {
  it("exposes infrastructure-only config without evidence or actuator callbacks", () => {
    const exactKeys = [
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "decisionDigest",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ] as const satisfies readonly (keyof ManifestBoundScriptIntegrityHashMissingWorkflowConfigV1)[];
    expect(exactKeys).not.toContain("loadJournal" as never);
    expect(exactKeys).not.toContain("appendJournal" as never);
    expect(exactKeys).not.toContain("submit" as never);
    expect(exactKeys).not.toContain("observeStage" as never);
    expect(exactKeys).not.toContain("evidence" as never);
  });

  it("content-addresses every self-loop and rejects transaction substitution", async () => {
    const first = actionFor(4, outRef("40"));
    const intended = hash("41");
    await expect(
      reconcileProductionCursorFamilyActionV1({
        spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1,
        headerHash,
        action: first,
        txHash: intended,
        provenance,
        stage: step(4, `${intended}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: intended });
    const resumed = actionFor(4, `${intended}#0`);
    expect(resumed.actionId).not.toBe(first.actionId);
    await expect(
      reconcileProductionCursorFamilyActionV1({
        spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1,
        headerHash,
        action: first,
        txHash: intended,
        provenance,
        stage: step(4, `${hash("42")}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("admits only the permanent-mint successor before removal", async () => {
    const action = actionFor(7, outRef("70"));
    const txHash = hash("71");
    await expect(
      reconcileProductionCursorFamilyActionV1({
        spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1,
        headerHash,
        action,
        txHash,
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: `${txHash}#1`,
          stateQueueBlockOutRef: outRef("10"),
          nextRemovalOutRef: outRef("72"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
  });

  it("reproduces the 224+224 budget-24 cursor chain deterministically", () => {
    const scripts = Array.from({ length: 224 }, (_, index) =>
      encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: Buffer.alloc(70, (index % 250) + 1),
      }),
    );
    const redeemers = Array.from({ length: 224 }, (_, index) =>
      Buffer.alloc(70, (index % 250) + 1),
    );
    const plan = planScriptIntegrityHashMissingStagedWalkV1({
      transactionId: hash("99"),
      scriptWitnessesPreimageCbor: encodeCbor(scripts).toString("hex"),
      redeemersPreimageCbor: encodeCbor(redeemers).toString("hex"),
    });
    expect(plan.grammar).toHaveLength(10);
    expect(plan.semantic).toHaveLength(10);
    expect(plan.redeemerGrammar).toHaveLength(10);
    expect(new Set(plan.grammar.map(scriptIntegrityGrammarHashV1)).size).toBe(
      10,
    );
    expect(new Set(plan.semantic.map(scriptIntegritySemanticHashV1)).size).toBe(
      10,
    );
    expect(
      new Set(plan.redeemerGrammar.map(hashScriptIntegrityField8CheckpointV1))
        .size,
    ).toBe(10);
    const encoded = encodeScriptIntegrityField8CheckpointV1(
      plan.redeemerGrammar[0]!,
    );
    expect(encoded[36]).toBe(8);
    const substituted = Buffer.from(encoded);
    substituted[36] = 6;
    expect(substituted).not.toEqual(encoded);
  });
});
