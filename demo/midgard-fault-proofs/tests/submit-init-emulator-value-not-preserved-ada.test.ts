/**
 * `value-not-preserved` emulator lifecycle, tier-2 ADA claim: a real fault
 * whose committed outputs field is too large for tier-1 carriage convicts
 * end to end.
 *
 * The committed transaction sources 10 ADA from its two authenticated spent
 * inputs, pays out 19.5 ADA across eleven outputs, and owes a 1 ADA fee —
 * the completed fold is `10 − 19.5 − 1 = −10.5` ADA, inflation in the
 * distinguished unit. Ten of the outputs carry large inline datums, pushing
 * the §5.1 outputs preimage past the 14,336-byte tier-1 cap into the
 * (14,336, 15,148] single-publication window: SIZE ALONE forces the step-03
 * outputs carriage onto §8.4 tier 2 (`RawUtxo`) — the submitter publishes
 * the preimage as a nothing-but-bytes inline datum first and the door reads
 * it back through a positional reference-input index, re-hashing it against
 * the committed field hash. No override flag exists anywhere.
 *
 * An ADA claim carries NO mint carriage (ADA is structurally unmintable),
 * and subtracts the root-committed fee instead. The journey ends with the
 * removal leg: state-queue node NFT burned, operator slashed, fraud-proof
 * token retained.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectProofFit,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixture,
  makeValueNotPreservedEmulatorHarness,
  publishValueNotPreservedReferenceScripts,
  runValueNotPreservedThread,
  setupValueNotPreservedScenario,
  vnpOutput,
  vnpOutRef,
  vnpValue,
} from "./support/value-not-preserved-emulator.js";

/**
 * Ten datum-padded outputs: three at 23 chunks and seven at 21 push the
 * outputs preimage to ~14.8KB — inside (14,336, 15,148] with margin on both
 * edges, so the §8.4 partition lands squarely on tier 2.
 */
const paddedOutputs = () =>
  [23, 23, 23, 21, 21, 21, 21, 21, 21, 21].map((datumChunks, index) =>
    vnpOutput({
      value: vnpValue(1_000_000n),
      datumChunks,
      seed: index * 29,
    }),
  );

describe("value-not-preserved emulator lifecycle (tier-2 ADA claim)", () => {
  it("proves inflated ADA through a size-forced RawUtxo outputs carriage, mints the permanent token, and removes the fraudulent commitment", async () => {
    const harness = await makeValueNotPreservedEmulatorHarness();
    const { emulator, funderLucid, proverLucid, proverSigner, family } =
      harness;

    // Sourced 10 ADA (6 + 4), paid out 9.5 + 10×1 = 19.5 ADA, fee 1 ADA:
    // `10 − 19.5 − 1 = −10.5` ADA.
    const fixture = await buildValueNotPreservedFixture({
      spentInputs: [
        { input: vnpOutRef("31", 0), spentValue: vnpValue(6_000_000n) },
        { input: vnpOutRef("42", 1), spentValue: vnpValue(4_000_000n) },
      ],
      outputs: [vnpOutput({ value: vnpValue(9_500_000n) }), ...paddedOutputs()],
      fee: 1_000_000n,
    });
    // THE tier-2 assertion: the committed outputs preimage exceeds the
    // 14,336-byte tier-1 cap and fits the single-publication RawUtxo window
    // (≤ 15,148) — the §8.4 partition is decided by these bytes alone.
    expect(fixture.outputsPreimageCbor.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(fixture.outputsPreimageCbor.length).toBeLessThanOrEqual(
      MIDGARD_CHUNK_BYTES_K,
    );

    const { setup } = await setupValueNotPreservedScenario({
      harness,
      fixture,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.anchorHeaderHash, setup.headerHash],
    });
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const refs = await publishValueNotPreservedReferenceScripts({
      lucid: funderLucid,
      contracts: family,
    });

    const run = await runValueNotPreservedThread({
      harness,
      fixture,
      setup,
      refs,
      claimedAsset: "AdaAsset",
      claimedDirection: "ClaimedAssetInflated",
      through: "step04",
    });
    expect(run.finish.outflowState.claimed_delta).toBe(10_000_000n);
    if (run.step03 === undefined || run.step04 === undefined) {
      throw new Error("lifecycle run stopped early");
    }
    // Size forced tier 2 for the outputs field; the ADA claim carries no
    // mint carriage at all.
    expect(run.step03.outputsCarriageTier).toBe("RawUtxo");
    expect(run.step03.mintCarriageTier).toBeNull();
    expect(run.step03.carriageUtxos).toHaveLength(1);
    expect(run.step03.outputsPreimageBytes).toBe(
      fixture.outputsPreimageCbor.length,
    );
    expect(run.step03.completedState.final_delta).toBe(-10_500_000n);
    expect(run.step04.completedState.final_delta).toBe(-10_500_000n);
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    for (const [stage, measurement] of Object.entries(run.measurements)) {
      expectProofFit({ stage, measurement, maxTxExMem, maxTxExSteps });
    }

    // Thread burned, permanent token minted.
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          run.init.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      run.step04.fraudProofAddress,
      run.step04.fraudProofUnit,
    );
    expect(outRefLabel(fraudProofUtxo)).toBe(run.step04.fraudProofOutRef);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg.
    const removalDeploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferenceScriptPublications.published },
    );
    const removeNow = BigInt(emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: removalDeploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "valueNotPreserved",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("valueNotPreserved");
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error("Removal did not preserve the state-queue root");
    }
    const finalRoot = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        finalRootUtxo,
        harness.contracts.stateQueue.policyId,
      ),
    );
    // The honest anchor block (the ledger-root carrier) survives: only the
    // fraudulent successor was removed.
    expect(finalRoot.datum.next).toEqual({
      Key: { key: setup.anchorHeaderHash },
    });
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.anchorBlockUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.scheduler.spendingScriptAddress,
      toUnit(harness.contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Removal did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SDK.SchedulerDatum)).toBe(
      "NoActiveOperators",
    );

    // The permanent evidence survives at the same out-ref.
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      run.step04.fraudProofAddress,
      run.step04.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(run.step04.fraudProofOutRef);
  }, 600_000);
});
