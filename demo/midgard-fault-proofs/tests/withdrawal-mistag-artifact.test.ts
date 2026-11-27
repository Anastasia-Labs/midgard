import { describe, expect, it } from "vitest";

import {
  admitWithdrawalMistagWorkflowArtifact,
  prepareWithdrawalMistagWorkflowArtifact,
} from "../src/withdrawal-mistag/artifact.js";
import { prepareWithdrawalMistagReplay } from "../src/withdrawal-mistag/replay.js";
import { withdrawalMistagStepPayloadCbor } from "../src/withdrawal-mistag/submit-withdrawal-mistag-steps.step-args.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import type { WorkflowJsonObject } from "../src/workflow/identity.js";
import { testLedgerEntries } from "./support/ledger-store-conformance.js";
import {
  droppedWitnesses,
  expectWitnessRefused,
  predecessorWitnessOf,
  tamperedWitnesses,
  withPredecessorWitness,
} from "./support/predecessor-witness-artifact.js";
import { buildWithdrawalMistagEvidenceMaterial } from "./support/withdrawal-mistag-emulator.build-withdrawal-mistag-evidence-material.js";
import {
  withdrawalMistagOutput,
  withdrawalMistagRetainedFixture,
} from "./support/withdrawal-mistag-retained.js";

type Fixture = Awaited<ReturnType<typeof withdrawalMistagRetainedFixture>>;

/** `count` outputs no withdrawal of these tests spends. */
const filler = (count: number) =>
  testLedgerEntries(2000, count).map(({ outRef, output }) => ({
    key: outRef,
    value: output,
  }));

/** The artifact the workflow writes for the fixture's one detection. */
const artifactOf = async (fixture: Fixture) => {
  const { detections } =
    await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
      fixture.evidence,
      fixture.context,
    );
  const classification = await classifyCanonicalBlockViolations({
    evidence: fixture.evidence,
    detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected")
    throw new Error("the fixture has no detection");
  const artifact = await prepareWithdrawalMistagWorkflowArtifact({
    evidence: fixture.evidence,
    classification,
    replayContext: fixture.context,
  });
  // What the workflow store holds: JSON written and read back.
  return JSON.parse(JSON.stringify(artifact)) as WorkflowJsonObject;
};

/** The preparation over the full predecessor ledger. */
const preparedOverFullLedger = async (fixture: Fixture) =>
  await prepareWithdrawalMistagReplay({
    current: fixture.evidence.reconstruction,
    predecessor: completeCanonicalReplayPredecessorStoreLedger({
      evidence: fixture.evidence,
      context: fixture.context,
    }),
    index: 0,
  });

describe("withdrawalMistag artifact predecessor witness", () => {
  it.each([0, 40])(
    "admits and prepares what the full ledger prepares, over %s other outputs",
    async (others) => {
      const fixture = await withdrawalMistagRetainedFixture({
        direction: "valid-marked-invalid",
        predecessorExtra: filler(others),
      });
      const artifact = await artifactOf(fixture);
      const admitted = await admitWithdrawalMistagWorkflowArtifact(artifact);
      const expected = await preparedOverFullLedger(fixture);
      expect(expected).not.toBeNull();
      expect(admitted).toEqual(expected);
      for (const step of [0, 1, 2, 3] as const)
        expect(withdrawalMistagStepPayloadCbor(admitted, step)).toBe(
          withdrawalMistagStepPayloadCbor(expected!, step),
        );
    },
  );

  it("refuses every tampered and every dropped node", async () => {
    const fixture = await withdrawalMistagRetainedFixture({
      direction: "invalid-marked-valid",
      predecessorExtra: filler(40),
    });
    const artifact = await artifactOf(fixture);
    const witness = predecessorWitnessOf(artifact);
    expect(witness.nodes.length).toBeGreaterThan(1);
    for (const [index, tampered] of tamperedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () =>
          await admitWithdrawalMistagWorkflowArtifact(
            withPredecessorWitness(artifact, tampered),
          ),
        `tampered node ${index.toString()}`,
      );
    for (const [index, dropped] of droppedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () =>
          await admitWithdrawalMistagWorkflowArtifact(
            withPredecessorWitness(artifact, dropped),
          ),
        `dropped node ${index.toString()}`,
      );
  });

  it("refuses the witness a sibling block over the same predecessor wrote", async () => {
    const own = await buildWithdrawalMistagEvidenceMaterial(
      "valid-marked-invalid",
    );
    const sibling = await buildWithdrawalMistagEvidenceMaterial(
      "invalid-marked-valid",
    );
    const others = filler(40);
    const ownFixture = await withdrawalMistagRetainedFixture({
      direction: "valid-marked-invalid",
      material: own,
      predecessorExtra: [withdrawalMistagOutput(sibling), ...others],
    });
    const siblingFixture = await withdrawalMistagRetainedFixture({
      direction: "invalid-marked-valid",
      material: sibling,
      predecessorExtra: [withdrawalMistagOutput(own), ...others],
    });
    // Both blocks start from the same ledger.
    expect(siblingFixture.evidence.header.prevUtxosRoot).toBe(
      ownFixture.evidence.header.prevUtxosRoot,
    );
    const artifact = await artifactOf(ownFixture);
    const siblingWitness = predecessorWitnessOf(
      await artifactOf(siblingFixture),
    );
    await admitWithdrawalMistagWorkflowArtifact(artifact);
    await expectWitnessRefused(
      async () =>
        await admitWithdrawalMistagWorkflowArtifact(
          withPredecessorWitness(artifact, siblingWitness),
        ),
      "sibling witness",
    );
  });

  it("refuses the previous schema version", async () => {
    const fixture = await withdrawalMistagRetainedFixture({
      direction: "valid-marked-invalid",
    });
    const { predecessorWitness: _, ...artifact } = await artifactOf(fixture);
    const previous = {
      ...artifact,
      schemaVersion: "midgard-withdrawal-mistag-workflow-artifact-v2",
      // The previous shape: the predecessor's entries as out-ref and output
      // CBOR hex pairs.
      predecessorLedger: [
        [
          "825820" + "55".repeat(32) + "00",
          "a200581d70" + "aa".repeat(28) + "018200a0",
        ],
      ],
    } as unknown as WorkflowJsonObject;
    await expect(
      admitWithdrawalMistagWorkflowArtifact(previous),
    ).rejects.toThrow("withdrawalMistag artifact shape changed");
  });

  it("refuses a null witness for a block that starts from a non-empty ledger", async () => {
    const artifact = await artifactOf(
      await withdrawalMistagRetainedFixture({
        direction: "valid-marked-invalid",
      }),
    );
    await expect(
      admitWithdrawalMistagWorkflowArtifact({
        ...artifact,
        predecessorWitness: null,
      }),
    ).rejects.toThrow("the block starts from a non-empty ledger");
  });

  it("writes a witness that does not grow with the predecessor ledger", async () => {
    const nodes = async (others: number) =>
      predecessorWitnessOf(
        await artifactOf(
          await withdrawalMistagRetainedFixture({
            direction: "valid-marked-invalid",
            predecessorExtra: filler(others),
          }),
        ),
      ).nodes.length;
    const small = await nodes(64);
    const large = await nodes(1024);
    // Sixteen times the outputs add one trie level to each path the
    // preparation reads and proves: the witness grows by that level, a whole
    // ledger in the artifact would grow sixteenfold.
    expect(large).toBeLessThan(small * 4);
  });
});
