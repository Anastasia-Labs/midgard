/**
 * The installed receive-purpose-language replay beside the input-existence
 * replay: an accepted redeemer-bearing transaction that spends or references
 * an input absent from the pre-state is decided by the input families, and
 * the receive-purpose-language member leaves it alone instead of aborting the
 * block's decision.
 */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  createCompleteCanonicalReplayUnion,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildMissingRedeemerBlock,
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
} from "./support/missing-redeemer-emulator.subject.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/receive-purpose-language-replay",
  grade: "security",
} as const;

const UNION = createCompleteCanonicalReplayUnion([
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
]);

/** The union's classification of the block committing `shape` as an
 * accepted transaction over the predecessor `ledger`. */
const decide = async (
  shape: MissingRedeemerSubjectShape,
  ledger: readonly LedgerEntry[],
) => {
  const built = await buildMissingRedeemerBlock({
    shape,
    source: { kind: "normal" },
    ledger,
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, ledger),
      currentEvidence: evidence,
    }),
  };
  const decision = await UNION.replay(evidence, context);
  return await classifyCanonicalBlockViolations({
    evidence,
    detections: requireCompleteCanonicalReplayDecision({
      evidence,
      replayer: UNION,
      decision,
      context,
    }),
  });
};

/** A signed spend plus a PlutusV3 receive whose source a reference input
 * carries and a redeemer reaches. */
const redeemedReceive = (): MissingRedeemerSubjectShape => ({
  ...honestForcedObserverShape(5),
  observers: [],
  receives: [{ script: plutusScript(8), witness: false, redeemer: true }],
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(8)]),
});

describe("receive-purpose-language replay over an input with no predecessor descriptor", () => {
  it.each([
    ["spent", "spend", "nonExistentInput"],
    ["referenced", "reference", "noReferenceInput"],
  ] as const)(
    "leaves an accepted redeemer-bearing transaction with an absent %s input to the input families",
    async (_label, field, category) => {
      const shape = redeemedReceive();
      const resolved = [...(shape.spend ?? []), ...(shape.reference ?? [])];
      const absent = missingRedeemerLedgerEntries(0x5a, [null]);
      await expect(
        decide(
          { ...shape, [field]: [...(shape[field] ?? []), ...absent] },
          resolved,
        ),
      ).resolves.toMatchObject({ decision: "fault_detected", category });
    },
  );
});
