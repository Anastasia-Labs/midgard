/**
 * `distinctAssetAccumulationLimit` emulator verticals from a genuine Init. An
 * accepted transaction, normal or forced, whose running asset entry count
 * passes the bound in its inputs, outputs or mint convicts, and the normal
 * one's block is removed. A forced rejection that records this arm at
 * another coordinate, or for a transaction within the bound, convicts, as
 * does one recording a value reason ranked after the crossing.
 */
import {
  outRefLabel,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  distinctAssetAccumulationLimitDetection,
  DistinctAssetAccumulationLimitHonestLeaf,
  requireConvictingDistinctAssetAccumulationLimitProof,
} from "../src/distinct-asset-accumulation-limit/index.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS,
  type DistinctAssetAccumulationLimitHarness,
  driveDistinctAssetAccumulationLimitThread,
  makeDistinctAssetAccumulationLimitHarness,
  publishDistinctAssetAccumulationLimitRoles,
  stepTable,
} from "./support/distinct-asset-accumulation-limit-emulator.js";
import {
  buildDistinctAssetBlock,
  buildDistinctAssetSubject,
  type DistinctAssetSource,
  type DistinctAssetSubjectShape,
  machineForcedVerdict,
  rejectedFor,
  setupDistinctAssetScenario,
} from "./support/distinct-asset-accumulation-limit-emulator.subject.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const SPENT = Array.from({ length: 10 }, () => 1_536);

/** 15,360 entries in ten spends and 1,536 more in the eleventh, which two
 * reference inputs precede in the schedule: the 16,385th entry is the
 * eleventh spend's 1,025th, at schedule position 12. */
const INPUT_CROSSING: DistinctAssetSubjectShape = {
  inputs: [...SPENT, 1_536],
  references: 2,
  outputs: [[1]],
  mint: [],
};

/** 15,616 entries before the second output, whose fourth policy holds the
 * 16,385th entry at its 769th asset. */
const OUTPUT_CROSSING: DistinctAssetSubjectShape = {
  inputs: SPENT,
  outputs: [[256], [256, 256, 256, 256]],
  mint: [],
};

/** 16,384 entries before the mint's third policy, whose first asset (the
 * mint's 513th) crosses. */
const MINT_CROSSING: DistinctAssetSubjectShape = {
  inputs: SPENT,
  outputs: [[256, 256]],
  mint: [256, 256, 10],
};

/** Exactly 16,384 entries across inputs, outputs and mint. */
const AT_BOUND: DistinctAssetSubjectShape = {
  inputs: SPENT,
  outputs: [[256, 256]],
  mint: [256, 256],
};

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.bytes);
  }
};

/** A forced subject L1 admission accepts: key spends signed by their one
 * address witness, no redeemer and no script. */
const admissible = (
  shape: DistinctAssetSubjectShape,
): DistinctAssetSubjectShape => {
  expect(screenMidgardForcedTxAdmission(buildDistinctAssetSubject(shape))).toBe(
    null,
  );
  return shape;
};

const crossingOf = async (shape: DistinctAssetSubjectShape) => {
  const built = await buildDistinctAssetBlock({
    shape,
    source: { kind: "normal" },
  });
  return distinctAssetAccumulationLimitDetection(built.material)?.crossing;
};

const removeBlock = async (
  harness: DistinctAssetAccumulationLimitHarness,
  headerHash: string,
) => {
  const removalReferenceScripts = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const now = BigInt(harness.emulator.now());
  return submitRemoveFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferenceScripts.published },
    ),
    network,
    signer: harness.proverSigner,
    fraudCategory: "distinctAssetAccumulationLimit",
    fraudulentHeaderHash: headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};

/** Drives a convicting thread to its terminal and checks the token. */
const convictAndCheck = async (
  shape: DistinctAssetSubjectShape,
  source: DistinctAssetSource,
  label: string,
) => {
  const harness = await makeDistinctAssetAccumulationLimitHarness();
  const scenario = await setupDistinctAssetScenario({
    harness,
    shape,
    source,
  });
  const references = await publishDistinctAssetAccumulationLimitRoles(harness);
  const run = await driveDistinctAssetAccumulationLimitThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`DAAL ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  const token = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.distinctAssetAccumulationLimit.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
  expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
    fraud_prover: harness.proverSigner.paymentKeyHash,
  });
  for (const step of harness.distinctAssetAccumulationLimit.steps)
    await expect(
      harness.proverLucid.utxosAtWithUnit(step.spendingScriptAddress, run.unit),
    ).resolves.toHaveLength(0);
  return { harness, scenario, run };
};

describe("distinct-asset-accumulation-limit crossings", () => {
  it("names each arm's first refused entry the way the node's classifier does", async () => {
    expect(await crossingOf(INPUT_CROSSING)).toStrictEqual({
      kind: "input",
      inputIndex: 12n,
      assetIndex: 1_024n,
    });
    expect(await crossingOf(OUTPUT_CROSSING)).toStrictEqual({
      kind: "output",
      outputIndex: 1n,
      assetIndex: 768n,
    });
    expect(await crossingOf(MINT_CROSSING)).toStrictEqual({
      kind: "mint",
      mintIndex: 512n,
    });
    expect(await crossingOf(AT_BOUND)).toBeUndefined();
    expect(
      await machineForcedVerdict(admissible(INPUT_CROSSING)),
    ).toStrictEqual({
      ForcedTxInvalid: {
        reason: {
          InputAssetAccumulationLimit: {
            input_index: 12n,
            asset_index: 1_024n,
          },
        },
      },
    });
  }, 120_000);
});

describe("distinct-asset-accumulation-limit genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction crossing in its inputs and removes its block", async () => {
    const { harness, scenario, run } = await convictAndCheck(
      INPUT_CROSSING,
      { kind: "normal" },
      "A-N input",
    );
    expect(run.prepared.crossing).toStrictEqual({
      kind: "input",
      inputIndex: 12n,
      assetIndex: 1_024n,
    });
    const removal = await removeBlock(harness, scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("distinctAssetAccumulationLimit");
    expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
      "remove-target",
    ]);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_800_000);

  it("A-N: convicts an accepted transaction crossing in an output's later policy", async () => {
    const { run } = await convictAndCheck(
      OUTPUT_CROSSING,
      { kind: "normal" },
      "A-N output",
    );
    expect(run.prepared.crossing).toStrictEqual({
      kind: "output",
      outputIndex: 1n,
      assetIndex: 768n,
    });
  }, 1_800_000);

  it("A-N: convicts an accepted transaction crossing in its mint", async () => {
    const { run } = await convictAndCheck(
      MINT_CROSSING,
      { kind: "normal" },
      "A-N mint",
    );
    expect(run.prepared.crossing).toStrictEqual({
      kind: "mint",
      mintIndex: 512n,
    });
  }, 1_800_000);

  it("A-F: convicts a forced acceptance crossing in its inputs", async () => {
    await convictAndCheck(
      admissible(INPUT_CROSSING),
      { kind: "forced", verdict: "ForcedTxValid" },
      "A-F",
    );
  }, 1_800_000);

  it("R-F: convicts a forced rejection recording this arm for a transaction within the bound", async () => {
    const { run } = await convictAndCheck(
      admissible(AT_BOUND),
      rejectedFor({
        InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n },
      }),
      "R-F false",
    );
    expect(run.prepared.crossing).toBe(null);
  }, 1_800_000);

  it.each([
    { name: "its field ordinal", input_index: 10n },
    { name: "the unset sentinel", input_index: -1n },
  ])(
    "R-F: convicts a forced rejection recording the input crossing at $name",
    async ({ input_index }) => {
      await convictAndCheck(
        admissible(INPUT_CROSSING),
        rejectedFor({
          InputAssetAccumulationLimit: { input_index, asset_index: 1_024n },
        }),
        "R-F coordinate",
      );
    },
    1_800_000,
  );

  it("R-F: convicts a forced rejection recording a value reason ranked after the crossing", async () => {
    const { run } = await convictAndCheck(
      admissible(MINT_CROSSING),
      rejectedFor("ValueNotPreserved"),
      "R-F later reason",
    );
    expect(run.prepared.crossing).toStrictEqual({
      kind: "mint",
      mintIndex: 512n,
    });
  }, 1_800_000);

  it.each([
    {
      name: "an accepted transaction at the bound",
      shape: AT_BOUND,
      source: async (): Promise<DistinctAssetSource> => ({ kind: "normal" }),
    },
    {
      name: "an honest forced InputAssetAccumulationLimit rejection",
      shape: INPUT_CROSSING,
      source: async (): Promise<DistinctAssetSource> => ({
        kind: "forced",
        verdict: await machineForcedVerdict(admissible(INPUT_CROSSING)),
      }),
    },
    {
      name: "a forced rejection recording a value reason ranked before the crossing",
      shape: MINT_CROSSING,
      source: async () =>
        rejectedFor({ OutputBelowMinAda: { output_index: 0n } }),
    },
  ])(
    "refuses $name: the terminal returns false",
    async (row) => {
      const harness = await makeDistinctAssetAccumulationLimitHarness();
      const scenario = await setupDistinctAssetScenario({
        harness,
        shape: row.shape,
        source: await row.source(),
      });
      await expect(
        requireConvictingDistinctAssetAccumulationLimitProof({
          material: scenario.material,
          contracts: harness.distinctAssetAccumulationLimit,
          prover: harness.proverSigner.paymentKeyHash,
        }),
      ).rejects.toThrow(DistinctAssetAccumulationLimitHonestLeaf);
      const references =
        await publishDistinctAssetAccumulationLimitRoles(harness);
      const run = await driveDistinctAssetAccumulationLimitThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(false);
      expectWithinBounds(run.steps);
      await expectOnchainRefusal(() => run.terminal(false), {
        refusedBy: "fraud_proofs/distinct_asset_accumulation_limit/terminal",
        check: /^Validator returned false$/u,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.distinctAssetAccumulationLimit.steps.at(-1)!
            .spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(1);
    },
    1_800_000,
  );
});
