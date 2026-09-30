import { describe, expect, it } from "vitest";

import {
  advanceUnusedScriptWitnessPurposes,
  UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
} from "../src/unused-script-witness/checkpoint.js";
import { prepareUnusedScriptWitnessArtifact } from "../src/unused-script-witness/replay.js";
import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  buildUnusedScriptWitnessFixture,
  readUnusedScanState,
  submitUnusedStep05Raw,
} from "./support/unused-script-witness-emulator.js";
import {
  coverage,
  type Harness,
  progress,
} from "./unused-script-witness-lifecycle.make-harness.js";
import {
  type Artifact,
  purposeOpenings,
} from "./unused-script-witness-lifecycle.refuse-every-step02-seam.js";

/** Every step-05 seam against a thread that just entered the reverse match. */
export const refuseEveryStep05Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const state = await readUnusedScanState(h.common(threadOutRef, 4), 4);
  expect(state.purpose_cursor).toBe(0n);
  expect(state.alternate_cursor).toBe(state.witness.bound.script_index);
  const budget = UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH;
  const legit = purposeOpenings(artifact, 0, budget);
  expect(legit.length).toBe(budget);
  const next = advanceUnusedScriptWitnessPurposes({
    state,
    evidence: artifact.evidence,
    itemBudget: budget,
  });
  expect(next.used).toBe(false);
  const attempt = async (
    seam: string,
    input: Partial<Parameters<typeof submitUnusedStep05Raw>[0]>,
  ) => {
    progress(`step-05 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep05Raw({
          ...h.common(threadOutRef, 4),
          openings: legit,
          itemBudget: BigInt(budget),
          next: { kind: "scan", state: next },
          ...input,
        }),
    );
    coverage.seams.add(seam);
  };
  await attempt("purpose_item", {
    openings: [
      { ...legit[0]!, script_hash: artifact.evidence.targetScriptHashHex },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_kind", {
    openings: [
      { ...legit[0]!, purpose_kind: (legit[0]!.purpose_kind + 1n) % 4n },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_membership", {
    openings: [
      {
        ...legit[0]!,
        siblings: ["66".repeat(32), ...legit[0]!.siblings.slice(1)],
      },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_order", {
    openings: [legit[1]!, legit[0]!, ...legit.slice(2)],
  });
  await attempt("scan_checkpoint", {
    next: {
      kind: "scan",
      state: { ...next, checkpoint_hash: "55".repeat(32) },
    },
  });
  await attempt("scan_batch_short", {
    openings: legit.slice(0, budget - 1),
    next: {
      kind: "scan",
      state: advanceUnusedScriptWitnessPurposes({
        state,
        evidence: artifact.evidence,
        itemBudget: budget - 1,
      }),
    },
  });
  await attempt("scan_budget_over_bound", {
    openings: purposeOpenings(artifact, 0, budget + 1),
    itemBudget: BigInt(budget + 1),
    next: {
      kind: "scan",
      state: { ...next, purpose_cursor: BigInt(budget + 1) },
    },
  });
};

describe("unusedScriptWitness retained lifecycle material", () => {
  const vkey = "11".repeat(28);
  it("selects the first unused inline coordinate from the retained stage-11 audit", async () => {
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "accepted",
      claimedVerdict: "accepted",
      accusedUnused: true,
      sourceCount: 3,
      inputByte: 0x61,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock("ab".repeat(32)),
    );
    expect(artifact.evidence.finding.scriptIndex).toBe(2);
    expect(artifact.evidence.unused).toBe(true);
    expect(artifact.evidence.sources).toHaveLength(3);
    expect(artifact.evidence.purposes).toHaveLength(2);
    expect(artifact.acceptedInclusion).toBeDefined();
  });

  it("contradicts a forced rejection only through the retained stage-12 terminal", async () => {
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: false,
      sourceCount: 3,
      inputByte: 0x62,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock("ac".repeat(32)),
    );
    expect(artifact.evidence.unused).toBe(false);
    expect(artifact.evidence.matchedPurposeIndex).toBe(2);
    expect(artifact.forcedMembership).toBeDefined();
    const honest = await buildUnusedScriptWitnessFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: true,
      sourceCount: 3,
      inputByte: 0x63,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    await expect(
      prepareUnusedScriptWitnessArtifact(
        honest.canonicalBlock("ad".repeat(32)),
      ),
    ).rejects.toThrow(/no contradiction/u);
  });
});
