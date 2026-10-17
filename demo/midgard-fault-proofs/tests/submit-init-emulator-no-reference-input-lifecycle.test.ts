/**
 * `no-reference-input` (Q18) emulator lifecycle, both polarities.
 *
 * Three journeys against the real Aiken validators, driven by the production
 * submitters:
 *
 *  1. **Real fault, end to end.** A committed transaction names a reference
 *     input whose producing transaction is nowhere — not in the block's
 *     `prev_utxos_root`, not among the transactions the block committed.
 *     init → step-01 bind → step-02 §2.5 field-1 opening → step-03 ledger
 *     exclusion → step-04 transactions exclusion + finalize → permanent
 *     fraud-proof token → fraudulent-block removal.
 *  2. **Tier-2 carriage, selected by size alone.** The same fault with the
 *     challenged reference input buried in a 365-item field-1 list whose §5.1
 *     preimage (14,603 bytes) exceeds §8.4's 14,336-byte tier-1 redeemer
 *     bound, so the plan is `RawUtxo` and the prover must publish the preimage
 *     before the step can reference it. Nothing forces the tier; no flag
 *     exists to force it.
 *  3. **Adversarial: an honest commitment.** The committed transaction's
 *     reference input was produced *in-block*, by the companion transaction
 *     the same block committed — so the block is honest and there is no fault
 *     to prove. steps 01–03 still go through (the out-ref really is absent
 *     from the block's prev ledger, which is what step-03 claims), and the
 *     conviction has to die at step-04's exclusion of the producing
 *     transaction id from `blocks_transactions_root`. That is the check the
 *     family's soundness rests on, and it is exercised with the strongest
 *     material an adversary has: the *genuine membership witness* for that id.
 *
 * Every step reads its validator from a published reference script (the
 * standing deployment ruling), which also puts each step's own reference-input
 * set through `resolveChunkReferenceIndicesV1`'s canonical sort — the
 * partial-set hazard fixed in nsd `fc635c8f`.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/field-opening.js";
import "../src/index.js";
import "../src/ne-proofs.js";
import "./support/legacy-submit-emulator.js";
import "./support/no-reference-input-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./submit-init-emulator-no-reference-input-lifecycle.drive-no-reference-input-to-step04.js";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.js";
import {
  submitNoReferenceInputStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { buildNonMembershipProof } from "../src/ne-proofs.js";
import {
  driveNoReferenceInputToStep04,
  makeNoReferenceInputHarness,
  TIER2_BAD_REFERENCE_INPUT_INDEX,
  TIER2_REFERENCE_INPUT_COUNT,
  tier2ReferenceInputs,
} from "./submit-init-emulator-no-reference-input-lifecycle.drive-no-reference-input-to-step04.js";
import {
  buildNoReferenceInputFixture,
  NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
  noReferenceInputOutRef,
  requireNoReferenceInputTxsMembershipProof,
  requireNoReferenceInputTxsNonMembershipProof,
} from "./support/no-reference-input-emulator.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  expectOnchainRefusal,
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

describe("no-reference-input emulator lifecycle", () => {
  it("convicts a reference input that never existed, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeNoReferenceInputHarness();
    const { realBlueprint, emulator, funderLucid, proverLucid, proverSigner } =
      harness;
    // Two reference inputs; the challenged one is NOT first, so the step-02
    // index really is what selects it. Its producing transaction id is neither
    // the disputed transaction's nor the companion's, so it is absent from the
    // block's transactions trie as well as from its empty prev ledger.
    const fixture = await buildNoReferenceInputFixture({
      buildReferenceInputs: () => [
        noReferenceInputOutRef("bb".repeat(32), 1),
        noReferenceInputOutRef(NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID, 0),
      ],
      badReferenceInputIndex: 1,
    });
    expect(fixture.missingProducerIsCommitted).toBe(false);
    // Tier selection is the data's, not a caller's: this field-1 preimage is
    // far inside the tier-1 bound, so the opening is carried inline.
    expect(
      planFaultProofFieldOpening({
        anchorSourceKind: 0n,
        fieldIndex: SDK.MIDGARD_FIELD_INDEX.referenceInputs,
        anchorTxId: fixture.subjectTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: fixture.referenceInputItemCbors,
        owner: proverSigner.paymentKeyHash,
        label: "no-reference-input tier-1 field 1",
      }).plan.tier,
    ).toBe("Inline");
    expect(fixture.fieldPreimage.length).toBeLessThanOrEqual(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );

    const journey = await driveNoReferenceInputToStep04({
      harness,
      fixture,
      publishRemoval: true,
    });
    const { setup, deploymentInfo, stepReferences, init, step03 } = journey;

    const step04 = await submitNoReferenceInputStep04({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      txsNonMembershipProofCbor:
        requireNoReferenceInputTxsNonMembershipProof(fixture),
      referenceScriptUtxo: stepReferences[3],
      awaitConfirmation: true,
    });
    expect(step04.missingReferenceInputTxId).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
    );
    expect(step04.fraudProofAssetName).toBe(init.computationThreadAssetName);

    // The thread NFT is burned: no step address still holds it.
    for (const step of harness.contracts.fraudProofContracts.noReferenceInput
      .steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          init.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04.fraudProofAddress,
      step04.fraudProofUnit,
    );
    expect(outRefLabel(fraudProofUtxo)).toBe(step04.fraudProofOutRef);
    expect(fraudProofUtxo.assets[step04.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg: the minted token is the standing evidence that takes
    // the fraudulent state commitment off the queue. The fraud-proof token
    // itself has no burn path — it survives as permanent evidence — while the
    // state-queue node NFT carrying the fraudulent commitment burns and the
    // committing operator is slashed in the same transaction.
    const removeNow = BigInt(emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "noReferenceInput",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("noReferenceInput");
    expect(removal.fraudCategoryId).toBe(
      harness.catalogue.categories.noReferenceInput.categoryId,
    );
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
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [],
    });
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
    expect(finalRoot.datum.next).toBe("Empty");

    // The committing operator (the funder signed the header) is slashed out of
    // the active set, and the scheduler rewinds to the no-operator state.
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
      step04.fraudProofAddress,
      step04.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(step04.fraudProofOutRef);
    expect(retainedFraudProof.assets[step04.fraudProofUnit]).toBe(1n);

    // A second removal claim finds nothing left to remove.
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "noReferenceInput",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 900_000);

  it("convicts a reference input buried in a 14,603-byte field-1 list through a size-selected RawUtxo publication", async () => {
    const harness = await makeNoReferenceInputHarness();
    const { realBlueprint, proverLucid, proverSigner } = harness;
    const fixture = await buildNoReferenceInputFixture({
      buildReferenceInputs: tier2ReferenceInputs,
      badReferenceInputIndex: TIER2_BAD_REFERENCE_INPUT_INDEX,
    });
    expect(fixture.referenceInputs).toHaveLength(TIER2_REFERENCE_INPUT_COUNT);
    expect(fixture.missingReferenceInput.tx_id).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
    );
    expect(fixture.missingProducerIsCommitted).toBe(false);

    // The size, not any flag, is what selects tier 2: past the tier-1 redeemer
    // bound, within one publication.
    const preimage = encodeMidgardFieldPreimage(
      fixture.referenceInputItemCbors,
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);
    expect(
      planFaultProofFieldOpening({
        anchorSourceKind: 0n,
        fieldIndex: SDK.MIDGARD_FIELD_INDEX.referenceInputs,
        anchorTxId: fixture.subjectTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: fixture.referenceInputItemCbors,
        owner: proverSigner.paymentKeyHash,
        label: "no-reference-input tier-2 field 1",
      }).plan.tier,
    ).toBe("RawUtxo");

    const { deploymentInfo, stepReferences, init, step03 } =
      await driveNoReferenceInputToStep04({ harness, fixture });
    expect(step03.missingReferenceInputTxId).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
    );

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced by the
    // step rather than carried in its redeemer.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await proverLucid.utxosAt(proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const step04 = await submitNoReferenceInputStep04({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      txsNonMembershipProofCbor:
        requireNoReferenceInputTxsNonMembershipProof(fixture),
      referenceScriptUtxo: stepReferences[3],
      awaitConfirmation: true,
    });
    expect(step04.fraudProofAssetName).toBe(init.computationThreadAssetName);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04.fraudProofAddress,
      step04.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step04.fraudProofUnit]).toBe(1n);
  }, 900_000);

  it("refuses to convict an honest commitment whose reference input was produced in-block", async () => {
    const harness = await makeNoReferenceInputHarness();
    const { realBlueprint, funderLucid, proverLucid, proverSigner } = harness;
    // HONEST block: the disputed transaction's challenged reference input is
    // an output of the companion transaction the same block committed. The
    // reference input exists, so there is no fault.
    const fixture = await buildNoReferenceInputFixture({
      buildReferenceInputs: (companionTxId) => [
        noReferenceInputOutRef("bb".repeat(32), 1),
        noReferenceInputOutRef(companionTxId, 0),
      ],
      badReferenceInputIndex: 1,
    });
    expect(fixture.missingReferenceInput.tx_id).toBe(fixture.companionTxId);
    expect(fixture.missingProducerIsCommitted).toBe(true);
    // No honest prover can even construct the witness step-04 needs: the key
    // is in the trie, so there is nothing to exclude.
    await expect(
      buildNonMembershipProof(
        fixture.txsEntries,
        Buffer.from(fixture.companionTxId, "hex"),
      ),
    ).rejects.toThrow(
      /Cannot build a non-membership proof for a key that is present/u,
    );

    // Steps 01–03 are all satisfiable against an honest block: the challenged
    // out-ref really is absent from the block's (empty) prev ledger, which is
    // all step-03 claims. The adversary reaches step-04 with a live thread.
    const { setup, deploymentInfo, stepReferences, init, step03 } =
      await driveNoReferenceInputToStep04({ harness, fixture });
    expect(step03.missingReferenceInputTxId).toBe(fixture.companionTxId);

    const step04Reference = stepReferences[3];
    const forgeStep04 = async (txsNonMembershipProofCbor: string) =>
      await submitNoReferenceInputStep04({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: step03.nextThreadOutRef,
        txsNonMembershipProofCbor,
        referenceScriptUtxo: step04Reference,
        awaitConfirmation: true,
      });

    // The strongest material an adversary holds is the genuine MEMBERSHIP
    // witness for the producing transaction id. `pexcludes.exclusion.withdraw`
    // binds it as `mpf.insert(trie, key, "", proof)`, which asserts
    // `excluding(key, proof) == root` and fails outright for a key the trie
    // already holds: a membership witness cannot masquerade as its opposite.
    const membershipRefusal = await expectOnchainRefusal(async () =>
      forgeStep04(requireNoReferenceInputTxsMembershipProof(fixture)),
    );
    expect(membershipRefusal).toMatch(/failed script execution/u);

    // The retired fixture shape (#582) stays refused too: an empty proof is a
    // witness only for an empty trie, never for this block's populated one.
    const emptyProofCbor = await buildNonMembershipProof(
      [],
      Buffer.from(fixture.companionTxId, "hex"),
    );
    await expectOnchainRefusal(async () => forgeStep04(emptyProofCbor));

    // Both refusals were the validator's, not a spent thread's: the thread is
    // still at step 04, unspent, and no fraud-proof token was minted.
    await expectSingleUtxoWithUnit(
      proverLucid,
      step03.fourthStepAddress,
      init.computationThreadUnit,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          harness.contracts.fraudProof.policyId,
          init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
    // The honest commitment is still on the state queue.
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 900_000);
});
