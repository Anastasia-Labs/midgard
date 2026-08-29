/**
 * `value-not-preserved` emulator lifecycle, tier-1 token claim: a real fault
 * convicts end to end.
 *
 * The committed transaction sources 40 of the claimed token from its two
 * authenticated spent inputs, pays out 50, and mints nothing — the completed
 * fold is `40 − 50 = −10`, inflation. The journey runs the full thread —
 * init → step-01 bind → per-input step-02 folds → finish → step-03
 * outputs/mint completion → step-04 finalization mint — then the removal
 * leg: the fraudulent commitment's state-queue node NFT burns, the
 * committing operator is slashed, and the permanent fraud-proof token
 * survives untouched.
 *
 * Field sizing is realistic for tier 1: every §5.1 preimage here is a few
 * hundred bytes, far under the 14,336-byte tier-1 cap, so both step-03
 * carriages ride `Inline` — selected by size alone.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
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
  expectProofFitV1,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixtureV1,
  makeValueNotPreservedEmulatorHarnessV1,
  publishValueNotPreservedReferenceScriptsV1,
  runValueNotPreservedThreadV1,
  setupValueNotPreservedScenarioV1,
  vnpOutputV1,
  vnpOutRefV1,
  vnpValueV1,
} from "./support/value-not-preserved-emulator-v1.js";

const POLICY_ID_HEX = "ab".repeat(28);
const ASSET_NAME_HEX = "746f6b"; // "tok"

describe("value-not-preserved emulator lifecycle (tier-1 token claim)", () => {
  it("proves an inflated token end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const { emulator, funderLucid, proverLucid, proverSigner, family } =
      harness;

    // Sourced 40 (25 + 15 across two authenticated pre-state outputs), paid
    // out 50, minted nothing: `40 − 50 = −10`.
    const token = (quantity: bigint) => [
      { policyIdHex: POLICY_ID_HEX, assetNameHex: ASSET_NAME_HEX, quantity },
    ];
    const fixture = await buildValueNotPreservedFixtureV1({
      spentInputs: [
        {
          input: vnpOutRefV1("11", 0),
          spentValue: vnpValueV1(10_000_000n, token(25n)),
        },
        {
          input: vnpOutRefV1("22", 1),
          spentValue: vnpValueV1(8_000_000n, token(15n)),
        },
      ],
      outputs: [
        vnpOutputV1({ value: vnpValueV1(2_000_000n, token(30n)) }),
        vnpOutputV1({ value: vnpValueV1(2_000_000n, token(20n)) }),
      ],
      mintItems: [],
    });
    // Genuinely small tier-1 data: nowhere near the 14,336-byte cap.
    expect(fixture.outputsPreimageCbor.length).toBeLessThan(512);
    expect(fixture.outputsPreimageCbor.length).toBeLessThanOrEqual(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );

    const { setup } = await setupValueNotPreservedScenarioV1({
      harness,
      fixture,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.anchorHeaderHash, setup.headerHash],
    });
    // Published only after setup: the harness's one-shot nonce is the
    // funder's first UTxO. Removal sources its validators from reference
    // inputs; the family steps are reference scripts per the standing
    // deployment ruling.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const refs = await publishValueNotPreservedReferenceScriptsV1({
      lucid: funderLucid,
      contracts: family,
    });

    const run = await runValueNotPreservedThreadV1({
      harness,
      fixture,
      setup,
      refs,
      claimedAsset: {
        TokenAsset: { policy_id: POLICY_ID_HEX, asset_name: ASSET_NAME_HEX },
      },
      claimedDirection: "ClaimedAssetInflated",
      through: "step04",
    });
    expect(run.init.computationThreadAssetName).toBe(
      `${harness.category.categoryId}${setup.headerHash}`,
    );
    expect(run.step01.foldState.committed_fee).toBe(1_000_000n);
    expect(run.step01.foldState.prev_utxos_root).toBe(fixture.ledger.rootHex);
    expect(run.finish.outflowState.claimed_delta).toBe(40n);
    if (run.step03 === undefined || run.step04 === undefined) {
      throw new Error("lifecycle run stopped early");
    }
    // Size selected tier 1 for both fields — no override exists.
    expect(run.step03.outputsCarriageTier).toBe("Inline");
    expect(run.step03.mintCarriageTier).toBe("Inline");
    expect(run.step03.carriageUtxos).toHaveLength(0);
    expect(run.step03.outputsPreimageBytes).toBe(
      fixture.outputsPreimageCbor.length,
    );
    expect(run.step03.completedState.final_delta).toBe(-10n);
    expect(run.step04.completedState.final_delta).toBe(-10n);
    expect(run.step04.fraudProofAssetName).toBe(
      run.init.computationThreadAssetName,
    );
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    for (const [stage, measurement] of Object.entries(run.measurements)) {
      expectProofFitV1({ stage, measurement, maxTxExMem, maxTxExSteps });
    }

    // The permanent token is minted and the thread NFT is burned: no step
    // address still holds it.
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
    expect(fraudProofUtxo.assets[run.step04.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg: the minted token is the standing evidence that takes
    // the fraudulent state commitment off the queue. The state-queue node
    // NFT carrying the fraudulent commitment burns and the committing
    // operator is slashed in the same transaction; the fraud-proof token has
    // no burn path and survives as permanent evidence.
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
    expect(removal.fraudCategoryId).toBe(harness.category.categoryId);
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]!.kind).toBe("remove-target");
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    // The fraudulent commitment is gone: its state-queue node NFT is burned
    // and the root no longer links to anything.
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

    // The committing operator (the funder signed the header) is slashed out
    // of the active set, and the scheduler rewinds to the no-operator state.
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

    // The fraud-proof token survives removal untouched at the same out-ref:
    // permanent evidence, not a burnable receipt.
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      run.step04.fraudProofAddress,
      run.step04.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(run.step04.fraudProofOutRef);
    expect(retainedFraudProof.assets[run.step04.fraudProofUnit]).toBe(1n);

    // A second removal claim finds nothing left to remove.
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "valueNotPreserved",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 600_000);
});
