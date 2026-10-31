import { expect, it } from "vitest";

import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "../src/field-preimage-length-mismatch/workflow-spec.js";
import {
  cursorFamilyObservation,
  reconcileCursorFamilyAction,
} from "../src/workflow/cursor-family-state.js";
import { terminal } from "./cursor-family-adapter.terminal.js";
const hash = (byte: string) => byte.repeat(32);
const outRef = (byte: string) => `${hash(byte)}#0`;
const headerHash = "ab".repeat(28);
const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "canonical-descendant-receipt",
  grade: "security",
} as const;

it("confirms an included descendant peel after another caller removes the target", async () => {
  const spec = FIELD_PREIMAGE_LENGTH_CURSOR_SPEC;
  const initial = cursorFamilyObservation({
    spec,
    headerHash,
    provenance,
    stage: {
      kind: "proof_token",
      stateQueueBlockOutRef: outRef("10"),
      nextRemovalOutRef: outRef("33"),
      fraudProofOutRef: outRef("22"),
    },
  });
  if (initial.kind !== "action_required") throw new Error("missing peel");
  const requests: unknown[] = [];
  const result = await reconcileCursorFamilyAction({
    spec,
    headerHash,
    provenance,
    action: initial.action,
    txHash: hash("55"),
    stage: {
      kind: "removed",
      terminal: {
        ...terminal({ removalTxHash: hash("66"), removedOutRef: outRef("55") }),
        category: spec.category,
      },
    },
    transactionConfirmed: async (txHash, removal) => {
      requests.push({ txHash, removal });
      return true;
    },
  });
  expect(requests).toEqual([
    {
      txHash: hash("55"),
      removal: {
        inputOutRef: outRef("33"),
        targetOutRef: outRef("10"),
        proofOutRef: outRef("22"),
      },
    },
  ]);
  expect(result).toEqual({ kind: "confirmed", txHash: hash("55") });
});
it.each([
  [
    "unchanged target",
    outRef("10"),
    outRef("55"),
    outRef("22"),
    outRef("33"),
    true,
    "conflict",
  ],
  [
    "recreated target",
    outRef("55"),
    outRef("55"),
    outRef("22"),
    outRef("33"),
    true,
    "confirmed",
  ],
  [
    "surviving second child",
    outRef("55"),
    outRef("44"),
    outRef("22"),
    outRef("33"),
    true,
    "confirmed",
  ],
  [
    "unconfirmed receipt",
    outRef("10"),
    outRef("55"),
    outRef("22"),
    outRef("33"),
    false,
    "pending",
  ],
  [
    "foreign continuation",
    outRef("10"),
    outRef("66"),
    outRef("22"),
    outRef("33"),
    true,
    "conflict",
  ],
  [
    "substituted proof",
    outRef("10"),
    outRef("55"),
    outRef("23"),
    outRef("33"),
    true,
    "conflict",
  ],
  [
    "substituted target",
    outRef("66"),
    outRef("55"),
    outRef("22"),
    outRef("33"),
    true,
    "conflict",
  ],
  [
    "substituted peeled input",
    outRef("10"),
    outRef("55"),
    outRef("22"),
    outRef("34"),
    true,
    "pending",
  ],
] as const)(
  "reconciles field descendant peel with %s",
  async (_name, target, tip, proof, peeledInput, confirmed, expected) => {
    const spec = FIELD_PREIMAGE_LENGTH_CURSOR_SPEC;
    const initial = cursorFamilyObservation({
      spec,
      headerHash,
      provenance,
      stage: {
        kind: "proof_token",
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: peeledInput,
        fraudProofOutRef: outRef("22"),
      },
    });
    if (initial.kind !== "action_required")
      throw new Error("missing descendant action");
    const requests: unknown[] = [];
    const result = await reconcileCursorFamilyAction({
      spec,
      headerHash,
      provenance,
      action: initial.action,
      txHash: hash("55"),
      stage: {
        kind: "proof_token",
        stateQueueBlockOutRef: target,
        nextRemovalOutRef: tip,
        fraudProofOutRef: proof,
      },
      transactionConfirmed: async (txHash, removal) => {
        requests.push({ txHash, removal });
        return (
          confirmed &&
          removal?.inputOutRef === outRef("33") &&
          removal.proofOutRef === outRef("22") &&
          removal.targetOutRef === outRef("10")
        );
      },
    });
    expect(result.kind).toBe(expected);
    expect(requests).toEqual([
      {
        txHash: hash("55"),
        removal: {
          inputOutRef: peeledInput,
          targetOutRef: outRef("10"),
          proofOutRef: outRef("22"),
          continuation: { targetOutRef: target, nextRemovalOutRef: tip },
        },
      },
    ]);
  },
);
