/** The validation-trace dispute's detector re-executes each accepted normal
 * transaction with redeemers from DA replay. A committed block whose
 * transaction's PlutusV3 script fails is one fault routed to
 * `validationTraceDispute`; the same block over a passing script, a forced
 * transaction (rule R-DESC), a failing script stamped `TxIsInvalid`, a
 * redeemer transaction rejected for a missing signer and a key spend with no
 * redeemers are none. */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { replayValidationVerdictEvent } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import {
  plutusExecutionBlockFixture,
  type PlutusExecutionScenario,
} from "./helpers/plutus-execution-block-fixture.js";
import { claimMaterialOf } from "./support/validation-trace-claim-material.js";

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
const replayer =
  createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);

const detect = async (scenario: PlutusExecutionScenario) => {
  const fixture = await plutusExecutionBlockFixture(scenario);
  const decision = await replayer.replay(fixture.evidence, fixture.context);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence: fixture.evidence,
    replayer,
    decision,
    context: fixture.context,
  });
  return { fixture, detections };
};

/** The scenario's own verdict over its pre-state, so a "none" row is about
 * the detector and not about a transaction that never ran its script. */
const honestVerdict = async (scenario: PlutusExecutionScenario) => {
  const fixture = await plutusExecutionBlockFixture(scenario);
  const header = fixture.evidence.header;
  return await Effect.runPromise(
    replayValidationVerdictEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      canonicalTransactionCbor: fixture.tx.canonicalCbor,
      ledgerWitnessEntries: fixture.predecessorEntries,
      blockTimeMs: Number(header.endTime),
      expectedNetworkId: header.expectedNetworkId,
      minFeeA: header.minFeeA,
      minFeeB: header.minFeeB,
      blockSlot: header.blockSlot,
      l1SlotConfig,
      sourceKind: "normal",
    }),
  );
};

describe("validation-trace dispute detection from DA replay", () => {
  it("detects one failed script run and routes it to validationTraceDispute", async () => {
    const { fixture, detections } = await detect("failing");
    expect(detections).toHaveLength(1);
    const [detection] = detections;
    expect(detection).toMatchObject({
      violationId: "validation-trace",
      headerHash: fixture.evidence.headerHash,
      position: 0n,
    });
    // S is the index of the trace's Terminal Rejected leaf.
    const material = await claimMaterialOf(fixture);
    expect(material.terminalLeafProof.stateIndex).toBe(
      Number(material.stepCount),
    );
    expect(detection!.diagnostic).toBe(
      `accepted transaction ${fixture.tx.txId} fails Plutus execution; its honest trace has ${material.stepCount.toString()} steps`,
    );
    const classification = await classifyCanonicalBlockViolations({
      evidence: fixture.evidence,
      detections,
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "validationTraceDispute",
    });
  });

  it("detects nothing when the same transaction's script passes", async () => {
    expect((await honestVerdict("passing")).verdict).toBe("accepted");
    expect((await detect("passing")).detections).toStrictEqual([]);
  });

  it("detects nothing for a forced transaction with a failing script (R-DESC)", async () => {
    const failing = await honestVerdict("forced_failing");
    expect(failing.rejection?.subject?.arm).toBe("PlutusExecutionFailed");
    expect((await detect("forced_failing")).detections).toStrictEqual([]);
  });

  it("detects nothing for a failing-script transaction stamped TxIsInvalid", async () => {
    // Its spend, stamped TxIsValid, is the "failing" row's detection.
    expect((await honestVerdict("failing")).rejection?.subject?.arm).toBe(
      "PlutusExecutionFailed",
    );
    expect((await honestVerdict("failing_stamped_invalid")).verdict).toBe(
      "rejected",
    );
    expect((await detect("failing_stamped_invalid")).detections).toStrictEqual(
      [],
    );
  });

  it("detects nothing for a redeemer transaction rejected for a non-Plutus reason", async () => {
    const rejected = await honestVerdict("unsigned_required_signer");
    expect(rejected.verdict).toBe("rejected");
    expect(rejected.rejection?.subject?.arm).toBeDefined();
    expect(rejected.rejection?.subject?.arm).not.toBe("PlutusExecutionFailed");
    expect((await detect("unsigned_required_signer")).detections).toStrictEqual(
      [],
    );
  });

  it("detects nothing for a normal transaction without redeemers", async () => {
    expect((await honestVerdict("no_redeemers")).verdict).toBe("accepted");
    expect((await detect("no_redeemers")).detections).toStrictEqual([]);
  });
});
