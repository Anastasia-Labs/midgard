/**
 * `value-not-preserved` negative emulator scenarios: attacks on the thread's
 * EARLY gates — the step-01 domain boundary and the step-02 authentication
 * walk — are refused at the exact on-chain check.
 *
 * 1. Rejected committed leaf: the operator honestly recorded the transaction
 *    as a no-op (`TxIsInvalid`). The honest step-01 submitter's §1.4
 *    acceptance-gate twin refuses locally, and the raw bind builder (no
 *    local twin) reaches the validator's own
 *    `expect bad_tx_view.tx_compact.validity_code == 0` refusal. The thread
 *    never leaves step-01.
 *
 * 2. Forged step-02 value witnesses against an otherwise-real fault: (a) a
 *    witness whose descriptor and membership proof belong to the WRONG spent
 *    input dies at `verify_ledger_membership` — the proof cannot open the
 *    committed `prev_utxos_root` at the cursor input's outpoint key; (b) a
 *    genuine witness with one asset-leaf quantity inflated dies at
 *    `verify_asset_membership` — the tampered leaf hash no longer folds to
 *    the descriptor's asset frontier commitment. The honest fold submitter
 *    performs neither check locally (it just reads the witness), so both
 *    refusals are the validator's. The SAME thread then folds honestly,
 *    proving it was the forgery that was refused.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import { describe, expect, it } from "vitest";

import {
  buildSpentInputValueWitnessV1,
  spendInputsOpeningV1,
} from "../src/value-not-preserved/evidence-v1.js";
import { submitValueNotPreservedInit } from "../src/value-not-preserved/submit-value-not-preserved-init-v1.js";
import { submitValueNotPreservedStep01 } from "../src/value-not-preserved/submit-value-not-preserved-step-01-v1.js";
import { submitValueNotPreservedStep02Fold } from "../src/value-not-preserved/submit-value-not-preserved-step-02-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixtureV1,
  expectOnchainRefusalV1,
  makeValueNotPreservedEmulatorHarnessV1,
  publishValueNotPreservedReferenceScriptsV1,
  setupValueNotPreservedScenarioV1,
  submitRawValueNotPreservedBindV1,
  type ValueNotPreservedHarnessV1,
  vnpOutputV1,
  vnpOutRefV1,
  vnpValueV1,
} from "./support/value-not-preserved-emulator-v1.js";

const POLICY_ID_HEX = "ef".repeat(28);
const ASSET_NAME_HEX = "746f6b33"; // "tok3"

/** Init the thread against the committed scenario, harness-shaped. */
const initThread = async (
  harness: ValueNotPreservedHarnessV1,
  fraudulentBlockOutRef: string,
) =>
  submitValueNotPreservedInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.family,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef,
  });

describe("value-not-preserved negative scenarios", () => {
  it("refuses to bind an honestly-rejected committed leaf: the no-op recording is outside the family's domain", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const { proverLucid, proverSigner, family } = harness;

    // The leaf LOOKS like an inflation (sourced 6, paid 20) — but the
    // operator recorded it as invalid, so nothing was applied and there is
    // no fault to prove.
    const token = (quantity: bigint) => [
      { policyIdHex: POLICY_ID_HEX, assetNameHex: ASSET_NAME_HEX, quantity },
    ];
    const fixture = await buildValueNotPreservedFixtureV1({
      spentInputs: [
        {
          input: vnpOutRefV1("11", 0),
          spentValue: vnpValueV1(10_000_000n, token(6n)),
        },
      ],
      outputs: [vnpOutputV1({ value: vnpValueV1(2_000_000n, token(20n)) })],
      mintItems: [],
      validity: "TxIsInvalid",
    });
    expect(fixture.txInclusion.nativeTx.validity_code).not.toBe(0n);

    const { setup } = await setupValueNotPreservedScenarioV1({
      harness,
      fixture,
    });
    const refs = await publishValueNotPreservedReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: family,
    });
    const init = await initThread(harness, setup.fraudulentBlockOutRef);

    const claimedAsset = {
      TokenAsset: { policy_id: POLICY_ID_HEX, asset_name: ASSET_NAME_HEX },
    } as const;
    // The honest submitter's §1.4 acceptance-gate twin refuses locally.
    await expect(
      submitValueNotPreservedStep01({
        lucid: proverLucid,
        blueprint: harness.realBlueprint,
        contracts: family,
        categoryId: harness.category.categoryId,
        network,
        signer: proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        claimedAsset,
        claimedDirection: "ClaimedAssetInflated",
        prevUtxosRoot: fixture.ledger.rootHex,
        referenceScriptUtxo: refs[0],
      }),
    ).rejects.toThrow(/validity code/u);

    // The raw bind (no local twin) reaches step-01's own acceptance gate:
    // `expect bad_tx_view.tx_compact.validity_code == 0`.
    await expectOnchainRefusalV1(async () =>
      submitRawValueNotPreservedBindV1({
        harness,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        claimedAsset,
        claimedDirection: "ClaimedAssetInflated",
        prevUtxosRoot: fixture.ledger.rootHex,
        referenceScriptUtxo: refs[0],
      }),
    );

    // The thread never left step-01 and step-02 never saw it.
    await expect(
      proverLucid.utxosAtWithUnit(
        family.steps[0].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        family.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);

  it("refuses forged step-02 value witnesses at the authentication walk, then folds honestly on the same thread", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const { proverLucid, proverSigner, family } = harness;

    // A REAL token inflation (sourced 40, paid 50) — the attack is on the
    // step-02 evidence, not the leaf.
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
      outputs: [vnpOutputV1({ value: vnpValueV1(2_000_000n, token(50n)) })],
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
    const init = await initThread(harness, setup.fraudulentBlockOutRef);

    const claimedAsset = {
      TokenAsset: { policy_id: POLICY_ID_HEX, asset_name: ASSET_NAME_HEX },
    } as const;
    const step01 = await submitValueNotPreservedStep01({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      contracts: family,
      categoryId: harness.category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      claimedAsset,
      claimedDirection: "ClaimedAssetInflated",
      prevUtxosRoot: fixture.ledger.rootHex,
      referenceScriptUtxo: refs[0],
    });
    const spendInputsOpening = spendInputsOpeningV1({
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputsPreimageCbor: fixture.spendInputsPreimageCbor,
    });
    const [firstSpent, secondSpent] = fixture.ledger.spentInputs;
    if (firstSpent === undefined || secondSpent === undefined) {
      throw new Error("fixture lost its spent inputs");
    }

    // (a) A witness for the WRONG input: descriptor and membership proof
    // genuinely open the pre-state ledger — but at input B's outpoint key,
    // while the thread's cursor is at input A. `verify_ledger_membership`
    // refuses: the proof cannot open `prev_utxos_root` at
    // `encode_midgard_tx_input(input_at_cursor)`.
    const wrongInputWitness = await buildSpentInputValueWitnessV1({
      claim: claimedAsset,
      descriptorCbor: secondSpent.descriptorCbor,
      spentValue: secondSpent.spentValue,
      trie: fixture.ledger.trie,
      input: secondSpent.input,
      prevUtxosRootHex: fixture.ledger.rootHex,
    });
    await expectOnchainRefusalV1(async () =>
      submitValueNotPreservedStep02Fold({
        lucid: proverLucid,
        contracts: family,
        categoryId: harness.category.categoryId,
        signer: proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        spendInputsOpening,
        valueWitness: wrongInputWitness,
        referenceScriptUtxo: refs[1],
      }),
    );

    // (b) The RIGHT input's genuine witness with one asset-leaf quantity
    // inflated by 100. The leaf hash changes, so its membership siblings no
    // longer fold to the descriptor's asset frontier commitment:
    // `verify_asset_membership` refuses.
    const genuineWitness = await buildSpentInputValueWitnessV1({
      claim: claimedAsset,
      descriptorCbor: firstSpent.descriptorCbor,
      spentValue: firstSpent.spentValue,
      trie: fixture.ledger.trie,
      input: firstSpent.input,
      prevUtxosRootHex: fixture.ledger.rootHex,
    });
    const inflatedWitness = {
      ...genuineWitness,
      asset_openings: genuineWitness.asset_openings.map((opening) => ({
        ...opening,
        quantity: opening.quantity + 100n,
      })),
    };
    await expectOnchainRefusalV1(async () =>
      submitValueNotPreservedStep02Fold({
        lucid: proverLucid,
        contracts: family,
        categoryId: harness.category.categoryId,
        signer: proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        spendInputsOpening,
        valueWitness: inflatedWitness,
        referenceScriptUtxo: refs[1],
      }),
    );

    // The SAME thread accepts the honest witness: it was the forgery that
    // was refused, not the thread.
    const honestFold = await submitValueNotPreservedStep02Fold({
      lucid: proverLucid,
      contracts: family,
      categoryId: harness.category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      spendInputsOpening,
      valueWitness: genuineWitness,
      referenceScriptUtxo: refs[1],
    });
    expect(honestFold.foldState.input_cursor).toBe(1n);
    expect(honestFold.foldState.claimed_delta).toBe(25n);
  }, 600_000);
});
