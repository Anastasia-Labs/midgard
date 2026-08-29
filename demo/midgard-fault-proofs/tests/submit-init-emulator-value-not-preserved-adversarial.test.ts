/**
 * `value-not-preserved` adversarial emulator scenarios: a prover attacking an
 * HONEST commitment is refused by the validators at the exact soundness
 * check, in both carriage tiers.
 *
 * 1. Balanced commitment (tier 1): the committed transaction genuinely
 *    conserves the claimed token (sourced 40, paid 40, minted nothing). The
 *    thread folds honestly all the way through step-03 — every intermediate
 *    step is sound — but the completed `final_delta` is 0, so step-04's
 *    `value_not_preserved_fault_is_established_v1` refuses the finalize
 *    both locally (the submitter's conviction twin) and on-chain (the raw
 *    finalize builder). No fraud-proof token can ever exist for a balanced
 *    leaf; a wrong-direction claim dies at the same check.
 *
 * 2. Tampered tier-2 publication: the outputs field genuinely needs RawUtxo
 *    carriage, and the attacker pre-publishes a §8.5 publication whose bytes
 *    differ from the committed preimage by one flipped byte. The honest
 *    builder can never even NAME that UTxO (`resolveChunkReferenceIndicesV1`
 *    matches publications by exact content), so the test injects it
 *    positionally through the submitter's test-only escape hatch — and the
 *    §8.8 door refuses at its `field_commitment(preimage) == expected_hash`
 *    re-hash inside `native_tx_field_access_v1`. The SAME thread then
 *    convicts honestly, proving the refusal was the tamper and nothing else.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { submitValueNotPreservedStep03 } from "../src/value-not-preserved/submit-value-not-preserved-step-03-v1.js";
import { submitValueNotPreservedStep04 } from "../src/value-not-preserved/submit-value-not-preserved-step-04-v1.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixtureV1,
  expectOnchainRefusalV1,
  makeValueNotPreservedEmulatorHarnessV1,
  publishTamperedFieldPreimagePublicationV1,
  publishValueNotPreservedReferenceScriptsV1,
  runValueNotPreservedThreadV1,
  setupValueNotPreservedScenarioV1,
  submitRawValueNotPreservedFinalizeV1,
  vnpOutputV1,
  vnpOutRefV1,
  vnpValueV1,
} from "./support/value-not-preserved-emulator-v1.js";

const POLICY_ID_HEX = "cd".repeat(28);
const ASSET_NAME_HEX = "746f6b32"; // "tok2"

describe("value-not-preserved adversarial scenarios", () => {
  it("never finalizes against a balanced honest commitment: step-04 refuses the zero delta locally and on-chain", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const { proverLucid, proverSigner, family } = harness;

    // An HONEST leaf: sourced 40 (25 + 15), paid 40 (30 + 10), minted
    // nothing. Every fold step is individually provable — only the final
    // conviction has nothing to convict.
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
        vnpOutputV1({ value: vnpValueV1(2_000_000n, token(10n)) }),
      ],
      mintItems: [],
    });

    const { setup } = await setupValueNotPreservedScenarioV1({
      harness,
      fixture,
    });
    const refs = await publishValueNotPreservedReferenceScriptsV1({
      lucid: harness.funderLucid,
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
      through: "step03",
    });
    if (run.step03 === undefined) {
      throw new Error("thread run stopped before step-03");
    }
    // Every step landed; the completed fold is exactly balanced.
    expect(run.finish.outflowState.claimed_delta).toBe(40n);
    expect(run.step03.completedState.final_delta).toBe(0n);

    // The honest submitter's conviction twin refuses before spending
    // anything.
    await expect(
      submitValueNotPreservedStep04({
        lucid: proverLucid,
        contracts: family,
        categoryId: harness.category.categoryId,
        signer: proverSigner,
        threadOutRef: run.step03.nextThreadOutRef,
        referenceScriptUtxo: refs[3],
      }),
    ).rejects.toThrow(/never finalizes/u);

    // The raw finalize (no local twin) reaches the validator itself, which
    // refuses at `value_not_preserved_fault_is_established_v1`: Inflated
    // demands `final_delta < 0` and the fold produced exactly 0.
    await expectOnchainRefusalV1(async () =>
      submitRawValueNotPreservedFinalizeV1({
        harness,
        threadOutRef: run.step03!.nextThreadOutRef,
        referenceScriptUtxo: refs[3],
      }),
    );

    // The thread survives, parked at step-04, still unconvictable.
    await expect(
      proverLucid.utxosAtWithUnit(
        family.steps[3].spendingScriptAddress,
        run.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 600_000);

  it("refuses a tampered tier-2 preimage publication at the door's field-commitment re-hash, then convicts honestly on the same thread", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const { proverLucid, proverSigner, family } = harness;

    // The same size-forced tier-2 shape the ADA lifecycle proves: sourced
    // 10 ADA, paid 19.5 ADA + 1 ADA fee across eleven outputs whose datums
    // push the outputs preimage past the tier-1 cap.
    const fixture = await buildValueNotPreservedFixtureV1({
      spentInputs: [
        { input: vnpOutRefV1("31", 0), spentValue: vnpValueV1(6_000_000n) },
        { input: vnpOutRefV1("42", 1), spentValue: vnpValueV1(4_000_000n) },
      ],
      outputs: [
        vnpOutputV1({ value: vnpValueV1(9_500_000n) }),
        ...[23, 23, 23, 21, 21, 21, 21, 21, 21, 21].map((datumChunks, index) =>
          vnpOutputV1({
            value: vnpValueV1(1_000_000n),
            datumChunks,
            seed: index * 29,
          }),
        ),
      ],
      fee: 1_000_000n,
    });
    expect(fixture.outputsPreimageCbor.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );

    const { setup } = await setupValueNotPreservedScenarioV1({
      harness,
      fixture,
    });
    const refs = await publishValueNotPreservedReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: family,
    });

    const run = await runValueNotPreservedThreadV1({
      harness,
      fixture,
      setup,
      refs,
      claimedAsset: "AdaAsset",
      claimedDirection: "ClaimedAssetInflated",
      through: "finish",
    });
    expect(run.finish.outflowState.claimed_delta).toBe(10_000_000n);

    // One flipped byte mid-preimage: same length, same §8.5 datum shape,
    // wrong content.
    const tamperedBytes = Buffer.from(fixture.outputsPreimageCbor);
    tamperedBytes[5_000] = (tamperedBytes[5_000]! + 1) & 0xff;
    const tamperedUtxo = await publishTamperedFieldPreimagePublicationV1({
      harness,
      bytes: tamperedBytes,
    });

    // The honest content-addressed resolution can never select this UTxO,
    // so it is injected positionally via the submitter's test-only option.
    // The §8.8 door decodes the publication datum, re-hashes the bytes, and
    // refuses: `field_commitment(preimage) == expected_hash` fails in
    // `native_tx_field_access_v1`'s whole-field view.
    await expectOnchainRefusalV1(async () =>
      submitValueNotPreservedStep03({
        lucid: proverLucid,
        contracts: family,
        categoryId: harness.category.categoryId,
        signer: proverSigner,
        threadOutRef: run.finish.nextThreadOutRef,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        outputs: fixture.outputs,
        mintItems: null,
        referenceScriptUtxo: refs[2],
        unsafeSpendFieldRawUtxoForTest: tamperedUtxo,
      }),
    );
    // The thread is untouched, still parked at step-03.
    await expect(
      proverLucid.utxosAtWithUnit(
        family.steps[2].spendingScriptAddress,
        run.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);

    // The SAME thread convicts honestly — the earlier refusal was the
    // tamper, nothing else. The honest path publishes the genuine preimage
    // and resolves it content-addressed.
    const honestStep03 = await submitValueNotPreservedStep03({
      lucid: proverLucid,
      contracts: family,
      categoryId: harness.category.categoryId,
      signer: proverSigner,
      threadOutRef: run.finish.nextThreadOutRef,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      outputs: fixture.outputs,
      mintItems: null,
      referenceScriptUtxo: refs[2],
    });
    expect(honestStep03.outputsCarriageTier).toBe("RawUtxo");
    expect(honestStep03.completedState.final_delta).toBe(-10_500_000n);
    const step04 = await submitValueNotPreservedStep04({
      lucid: proverLucid,
      contracts: family,
      categoryId: harness.category.categoryId,
      signer: proverSigner,
      threadOutRef: honestStep03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
    });
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04.fraudProofAddress,
      step04.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step04.fraudProofUnit]).toBe(1n);
  }, 600_000);
});
