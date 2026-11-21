/**
 * A wrongful forced rejection whose reason names a coordinate the forced
 * transaction lacks is recorded as durable proof material through the owning
 * family's own preparer, and that material survives a JSON round trip.
 */
import { prepareMintItemNonCanonicalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/mint-item-non-canonical";
import { prepareResolvedOutputNonCanonicalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/resolved-output-non-canonical";
import { prepareTransactionOutputNonCanonicalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/transaction-output-non-canonical";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "@al-ft/midgard-fault-proofs/workflow/artifact-codec";
import type * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import {
  BIG,
  classifyFull,
  outcome,
  reason,
} from "./forced-claim.test-support.js";

/** Durable proof material for a decided claim, through each family's own
 * preparer, as the watcher records it before proving. */
const MATERIAL: readonly (readonly [
  string,
  SDK.RejectionReason,
  string,
  (
    full: Awaited<ReturnType<typeof classifyFull>>,
    detectionId: string,
  ) => Promise<unknown>,
])[] = [
  [
    "malformed mint item past 2^53",
    reason("MintItemMalformed", { policy_index: BIG }),
    "mintItemNonCanonical",
    (full, id) =>
      prepareMintItemNonCanonicalRecoveryMaterial(full.evidence!, id),
  ],
  [
    "non-canonical output past 2^53",
    reason("OutputNonCanonical", { output_index: BIG }),
    "transactionOutputNonCanonical",
    (full, id) =>
      prepareTransactionOutputNonCanonicalRecoveryMaterial(full.evidence!, id),
  ],
  [
    "non-canonical output at a negative index",
    reason("OutputNonCanonical", { output_index: -1n }),
    "transactionOutputNonCanonical",
    (full, id) =>
      prepareTransactionOutputNonCanonicalRecoveryMaterial(full.evidence!, id),
  ],
  [
    "non-canonical spent output of a spend input past 2^53",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 0n,
      input_index: BIG,
    }),
    "resolvedOutputNonCanonical",
    (full, id) =>
      prepareResolvedOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
        full.predecessor!,
      ),
  ],
  [
    "non-canonical spent output of an input kind past 2^53",
    reason("InputSpentOutputNonCanonical", {
      source_kind: BIG,
      input_index: 0n,
    }),
    "resolvedOutputNonCanonical",
    (full, id) =>
      prepareResolvedOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
        full.predecessor!,
      ),
  ],
];

describe("recording proof material for a wrongful forced rejection whose reason names a coordinate the transaction lacks", () => {
  it.each(MATERIAL)(
    "%s",
    async (_label, claimed, category, prepare) => {
      const full = await classifyFull(claimed);
      if (full.decision.decision !== "fault_detected")
        throw new Error(`expected a fault, got ${outcome(full.decision)}`);
      expect(full.decision.category).toBe(category);
      const { detectionId } = full.decision;
      const material = await prepare(full, detectionId);
      expect(material).toMatchObject({ category, detectionId });
      const recorded = JSON.parse(
        JSON.stringify(encodeWorkflowArtifact(material)),
      );
      const fresh = await prepare(full, detectionId);
      expect(requireWorkflowArtifactMatches(recorded, fresh)).toBe(fresh);
    },
    120_000,
  );
});
