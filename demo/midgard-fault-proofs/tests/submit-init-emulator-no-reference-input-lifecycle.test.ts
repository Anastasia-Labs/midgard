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
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpeningV1 } from "../src/field-opening-v1.js";
import {
  submitNoReferenceInputStep01,
  submitNoReferenceInputStep02,
  submitNoReferenceInputStep03,
  submitNoReferenceInputStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { buildNonMembershipProof } from "../src/ne-proofs.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  buildNoReferenceInputFixtureV1,
  NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1,
  type NoReferenceInputFixtureV1,
  noReferenceInputOutRefV1,
  publishNoReferenceInputReferenceScriptsV1,
  requireNoReferenceInputTxsMembershipProofV1,
  requireNoReferenceInputTxsNonMembershipProofV1,
} from "./support/no-reference-input-emulator-v1.js";
import {
  expectStateQueueHeaderOrder,
  registerPexcludesExclusionRewardAccount,
  setupFraudulentBlockV1,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectOnchainRefusalV1,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarnessV1,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const makeNoReferenceInputHarnessV1 = async () =>
  await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realNoReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
    // Steps 03 and 04 both delegate their non-membership claim to
    // `pexcludes.exclusion.withdraw`, whose reward account must exist first.
    registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
  });

type NoReferenceInputHarnessV1 = Awaited<
  ReturnType<typeof makeNoReferenceInputHarnessV1>
>;

/**
 * init → step-01 → step-02 → step-03, the segment all three journeys share.
 *
 * Everything up to and including step-03 is satisfiable for an honest block
 * too: the challenged out-ref genuinely is absent from a block's prev ledger
 * when its producer sits inside the same block. Only step-04 separates the two
 * cases, which is why the adversarial journey drives this same helper.
 */
const driveNoReferenceInputToStep04V1 = async ({
  harness,
  fixture,
  publishRemoval = false,
}: {
  readonly harness: NoReferenceInputHarnessV1;
  readonly fixture: NoReferenceInputFixtureV1;
  readonly publishRemoval?: boolean;
}) => {
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
  } = harness;
  const steps = contracts.fraudProofContracts.noReferenceInput.steps;
  // The harness's one-shot nonce is the funder's first UTxO, so nothing may
  // spend from the funder wallet before `setupFraudulentBlockV1` consumes it.
  const setup = await setupFraudulentBlockV1({
    funderLucid,
    emulator,
    contracts,
    catalogue,
    fixture,
  });
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [setup.headerHash],
  });
  // Removal must source its seven validators from reference inputs to stay
  // inside the 16,384-byte L1 envelope; only the journey that removes pays for
  // publishing them.
  const removalPublications = publishRemoval
    ? await publishRemovalReferenceScripts({ lucid: proverLucid, contracts })
    : undefined;
  const stepReferences = await publishNoReferenceInputReferenceScriptsV1({
    lucid: funderLucid,
    steps,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    ...(removalPublications === undefined
      ? {}
      : { removalReferenceScripts: removalPublications.published }),
  });

  const init = await submitInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudCategory: "noReferenceInput",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  expect(init.fraudCategoryName).toBe("noReferenceInput");
  expect(init.computationThreadAssetName).toBe(
    `${catalogue.categories.noReferenceInput.categoryId}${setup.headerHash}`,
  );
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );

  const step01 = await submitNoReferenceInputStep01({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.inclusion,
    referenceScriptUtxo: stepReferences[0],
    awaitConfirmation: true,
  });
  // The §2.5 anchor is the disputed transaction's id, and the two roots the
  // later steps open are the ones the on-chain header carries: an EMPTY prev
  // ledger and the block's RAW transactions PHAS root.
  expect(step01.badTxId).toBe(fixture.subjectTxId);
  expect(step01.blocksPrevUtxosRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  expect(step01.blocksTransactionsRoot).toBe(fixture.transactionsRoot);

  const step02 = await submitNoReferenceInputStep02({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: step01.nextThreadOutRef,
    referenceInputsPreimage: fixture.referenceInputsPreimage,
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    badReferenceInputIndex: fixture.badReferenceInputIndex,
    referenceScriptUtxo: stepReferences[1],
    awaitConfirmation: true,
  });
  expect(step02.referenceInputsItemCount).toBe(fixture.referenceInputs.length);
  expect(step02.missingReferenceInput).toStrictEqual(
    fixture.missingReferenceInput,
  );

  const step03 = await submitNoReferenceInputStep03({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: step02.nextThreadOutRef,
    ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
    referenceScriptUtxo: stepReferences[2],
    awaitConfirmation: true,
  });
  expect(step03.missingReferenceInputTxId).toBe(
    fixture.missingReferenceInput.tx_id,
  );

  return {
    setup,
    deploymentInfo,
    stepReferences,
    init,
    step01,
    step02,
    step03,
  };
};

/**
 * §8.4's tier-1 redeemer bound is 14,336 bytes. A §5.3 out-ref item is a
 * constant 38 bytes and contributes a constant 40-byte stride to the §5.1
 * preimage, so the partition sits between 358 items (14,323 bytes: tier 1) and
 * 359 (14,363: tier 2). 365 items — 14,600 bytes of stride plus §5.1's 3-byte
 * envelope = 14,603 — is a round count comfortably past the bound and still
 * inside the single-publication tier-2 window `(14,336, 15,148]`. The
 * challenged reference input is item 200 of the 365: the fault is a property
 * of one item, so the rest are free to be decoys.
 */
const TIER2_REFERENCE_INPUT_COUNT = 365;
const TIER2_BAD_REFERENCE_INPUT_INDEX = 200;

const tier2ReferenceInputsV1 = (): readonly SDK.MidgardTxInput[] => {
  const decoy = (index: number): SDK.MidgardTxInput =>
    noReferenceInputOutRefV1((index + 1).toString(16).padStart(64, "0"), 0);
  const items: SDK.MidgardTxInput[] = [];
  for (let index = 0; index < TIER2_REFERENCE_INPUT_COUNT - 1; index += 1) {
    items.push(decoy(index));
  }
  items.splice(
    TIER2_BAD_REFERENCE_INPUT_INDEX,
    0,
    noReferenceInputOutRefV1(NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1, 0),
  );
  return items;
};

describe("no-reference-input emulator lifecycle", () => {
  it("convicts a reference input that never existed, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeNoReferenceInputHarnessV1();
    const { realBlueprint, emulator, funderLucid, proverLucid, proverSigner } =
      harness;
    // Two reference inputs; the challenged one is NOT first, so the step-02
    // index really is what selects it. Its producing transaction id is neither
    // the disputed transaction's nor the companion's, so it is absent from the
    // block's transactions trie as well as from its empty prev ledger.
    const fixture = await buildNoReferenceInputFixtureV1({
      buildReferenceInputs: () => [
        noReferenceInputOutRefV1("bb".repeat(32), 1),
        noReferenceInputOutRefV1(
          NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1,
          0,
        ),
      ],
      badReferenceInputIndex: 1,
    });
    expect(fixture.missingProducerIsCommitted).toBe(false);
    // Tier selection is the data's, not a caller's: this field-1 preimage is
    // far inside the tier-1 bound, so the opening is carried inline.
    expect(
      planFaultProofFieldOpeningV1({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.referenceInputs,
        anchorTxId: fixture.subjectTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: fixture.referenceInputItemCbors,
        owner: proverSigner.paymentKeyHash,
        label: "no-reference-input tier-1 field 1",
      }).plan.tier,
    ).toBe("Inline");
    expect(fixture.fieldPreimage.length).toBeLessThanOrEqual(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );

    const journey = await driveNoReferenceInputToStep04V1({
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
        requireNoReferenceInputTxsNonMembershipProofV1(fixture),
      referenceScriptUtxo: stepReferences[3],
      awaitConfirmation: true,
    });
    expect(step04.missingReferenceInputTxId).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1,
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
    const harness = await makeNoReferenceInputHarnessV1();
    const { realBlueprint, proverLucid, proverSigner } = harness;
    const fixture = await buildNoReferenceInputFixtureV1({
      buildReferenceInputs: tier2ReferenceInputsV1,
      badReferenceInputIndex: TIER2_BAD_REFERENCE_INPUT_INDEX,
    });
    expect(fixture.referenceInputs).toHaveLength(TIER2_REFERENCE_INPUT_COUNT);
    expect(fixture.missingReferenceInput.tx_id).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1,
    );
    expect(fixture.missingProducerIsCommitted).toBe(false);

    // The size, not any flag, is what selects tier 2: past the tier-1 redeemer
    // bound, within one publication.
    const preimage = encodeMidgardFieldPreimageV1(
      fixture.referenceInputItemCbors,
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K_V1);
    expect(
      planFaultProofFieldOpeningV1({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.referenceInputs,
        anchorTxId: fixture.subjectTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: fixture.referenceInputItemCbors,
        owner: proverSigner.paymentKeyHash,
        label: "no-reference-input tier-2 field 1",
      }).plan.tier,
    ).toBe("RawUtxo");

    const { deploymentInfo, stepReferences, init, step03 } =
      await driveNoReferenceInputToStep04V1({ harness, fixture });
    expect(step03.missingReferenceInputTxId).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID_V1,
    );

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced by the
    // step rather than carried in its redeemer.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCborV1(preimage);
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
        requireNoReferenceInputTxsNonMembershipProofV1(fixture),
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
    const harness = await makeNoReferenceInputHarnessV1();
    const { realBlueprint, funderLucid, proverLucid, proverSigner } = harness;
    // HONEST block: the disputed transaction's challenged reference input is
    // an output of the companion transaction the same block committed. The
    // reference input exists, so there is no fault.
    const fixture = await buildNoReferenceInputFixtureV1({
      buildReferenceInputs: (companionTxId) => [
        noReferenceInputOutRefV1("bb".repeat(32), 1),
        noReferenceInputOutRefV1(companionTxId, 0),
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
      await driveNoReferenceInputToStep04V1({ harness, fixture });
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
    const membershipRefusal = await expectOnchainRefusalV1(async () =>
      forgeStep04(requireNoReferenceInputTxsMembershipProofV1(fixture)),
    );
    expect(membershipRefusal).toMatch(/failed script execution/u);

    // The retired fixture shape (#582) stays refused too: an empty proof is a
    // witness only for an empty trie, never for this block's populated one.
    const emptyProofCbor = await buildNonMembershipProof(
      [],
      Buffer.from(fixture.companionTxId, "hex"),
    );
    await expectOnchainRefusalV1(async () => forgeStep04(emptyProofCbor));

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
