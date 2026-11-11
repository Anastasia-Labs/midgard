/** Claim preparation and artifact validation reuse the pre-state ledger the
 * validation-trace detector walked to for the block: after detection,
 * preparing and validating a claim walks none of the block's steps again,
 * however large the predecessor ledger. Each claim still matches the one
 * built from its own one-off pre-state, for every failing event of the block.
 * Work is counted as in `helpers/trie-leaves.ts`. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { describe, expect, it } from "vitest";

import { ledgerBeforeTransactionEvent } from "../src/input-existence/event-ledger.js";
import { encodeValidationTraceDisputeArtifact } from "../src/validation-trace-dispute/artifact.js";
import { buildValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import {
  prepareValidationTraceArtifact,
  validateValidationTraceArtifact,
} from "../src/validation-trace-dispute/detected-claim.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";
import { filler, trieLeaves } from "./helpers/trie-leaves.js";

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;

const detect = async (block: Block) => {
  const replayer =
    createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);
  const decision = await replayer.replay(block.evidence, block.context);
  const classification = await classifyCanonicalBlockViolations({
    evidence: block.evidence,
    detections: requireCompleteCanonicalReplayDecision({
      evidence: block.evidence,
      replayer,
      decision,
      context: block.context,
    }),
  });
  if (classification.decision !== "fault_detected")
    throw new Error(`the block classified ${classification.decision}`);
  return classification;
};

/** Trie leaves preparing and then validating each failing event's claim
 * cost, after detection. */
const claimLeaves = async (block: Block) => {
  const classification = await detect(block);
  const claim = {
    evidence: block.evidence,
    context: block.context,
    l1SlotConfig,
  };
  return await trieLeaves(async () => {
    for (const selected of classification.detections) {
      const artifact = await prepareValidationTraceArtifact({
        ...claim,
        classification: { ...classification, selected },
      });
      await validateValidationTraceArtifact({
        ...claim,
        classification: { ...classification, selected },
        artifact,
      });
    }
  });
};

/** Failing events at steps 1 and 3: a walk to them applies 1 and 3 steps. */
const SPENDS = ["passing", "failing", "passing", "failing"] as const;
const FILLER = 300;

describe("validation-trace dispute claim: the detector's pre-state ledger is reused", () => {
  it("walks no step of the block again to prepare and validate each claim", async () => {
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(SPENDS),
      plutusExecutionEventsBlockFixture(SPENDS, {
        predecessorFiller: filler(FILLER),
      }),
    ]);
    const [{ leaves: smallLeaves }, { leaves: largeLeaves }] = [
      await claimLeaves(small),
      await claimLeaves(large),
    ];
    // Ledger-sized tries built across both failing events' claims.
    expect((largeLeaves - smallLeaves) / FILLER).toBe(0);
  });

  it("prepares each failing event's claim from exactly its own pre-state", async () => {
    const block = await plutusExecutionEventsBlockFixture(SPENDS, {
      predecessorFiller: filler(8),
    });
    const classification = await detect(block);
    expect(classification.detections).toHaveLength(2);
    const predecessor = completeCanonicalReplayPredecessorLedger({
      evidence: block.evidence,
      context: block.context,
    })!;
    for (const selected of classification.detections) {
      const event = block.evidence.reconstruction.sourceEvents.find(
        ({ fingerprint }) => fingerprint === selected.subjectEventKeyCbors[0],
      )!;
      if (event.phase !== "L2Transaction") throw new Error("not a transaction");
      const opening =
        block.evidence.reconstruction.eventToStepByFingerprint.get(
          event.fingerprint,
        )!;
      const oneOff = await ledgerBeforeTransactionEvent({
        block: block.evidence,
        eventKey: event.eventKey,
        stepIndex: opening.value.step_index,
        predecessor,
      });
      expect(
        await prepareValidationTraceArtifact({
          evidence: block.evidence,
          context: block.context,
          classification: { ...classification, selected },
          l1SlotConfig,
        }),
      ).toStrictEqual(
        encodeValidationTraceDisputeArtifact({
          headerHash: block.evidence.headerHash,
          material: await buildValidationTraceClaimMaterial({
            evidence: block.evidence,
            event,
            ledger: oneOff,
            l1SlotConfig,
          }),
        }),
      );
    }
  });

  it("still refuses a claim prepared without the predecessor ledger the detector walked from", async () => {
    const block = await plutusExecutionEventsBlockFixture(SPENDS, {
      predecessorFiller: filler(8),
    });
    const classification = await detect(block);
    for (const selected of classification.detections)
      await expect(
        prepareValidationTraceArtifact({
          evidence: block.evidence,
          context: undefined,
          classification: { ...classification, selected },
          l1SlotConfig,
        }),
      ).rejects.toThrow("the block starts from a non-empty ledger");
  });
});
