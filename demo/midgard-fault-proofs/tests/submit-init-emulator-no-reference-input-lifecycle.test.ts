import { requireInputExistenceTerminalResult } from "./support/input-existence-execution-context.js";
import { submitRawInputExistenceStep } from "./support/raw-input-existence-step.js";
/**
 * `no-reference-input` (Q18) emulator lifecycle, both polarities.
 *
 * Three journeys against the real Aiken validators, driven by the production
 * submitters:
 *
 *  1. A selected reference input is absent from its event pre-state: source
 *     binding → selection → authenticated event root → ledger exclusion,
 *     permanent fraud-proof token and fraudulent-block removal.
 *  2. The same journey with a size-selected RawUtxo field publication.
 *  3. A retained in-block creator precedes the selected reference input, so
 *     its authenticated event pre-state contains that input. The genuine
 *     membership witness and an empty proof both fail as ledger exclusions.
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
    const { setup, deploymentInfo, init, step03 } = journey;

    const step04 = requireInputExistenceTerminalResult(
      await submitNoReferenceInputStep04({
        ...journey.stageArguments,
        threadOutRef: step03.nextThreadOutRef,
      }),
    );
    expect(step04.selectedInput?.tx_id).toBe(
      NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
    );
    expect(step04.fraudProofUnit).toBe(
      toUnit(
        harness.contracts.fraudProof.policyId,
        init.computationThreadAssetName,
      ),
    );

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
      harness.contracts.fraudProof.spendingScriptAddress,
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
      harness.contracts.fraudProof.spendingScriptAddress,
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
    const { proverLucid, proverSigner } = harness;
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

    const journey = await driveNoReferenceInputToStep04({ harness, fixture });
    const { init, step03 } = journey;
    expect(step03.selectedInput?.tx_id).toBe(
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

    const step04 = requireInputExistenceTerminalResult(
      await submitNoReferenceInputStep04({
        ...journey.stageArguments,
        threadOutRef: step03.nextThreadOutRef,
      }),
    );
    expect(step04.fraudProofUnit).toBe(
      toUnit(
        harness.contracts.fraudProof.policyId,
        init.computationThreadAssetName,
      ),
    );
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      harness.contracts.fraudProof.spendingScriptAddress,
      step04.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step04.fraudProofUnit]).toBe(1n);
  }, 900_000);

  it("refuses to convict a retained commitment whose selected reference input exists at its event", async () => {
    const harness = await makeNoReferenceInputHarness();
    const { funderLucid, proverLucid } = harness;
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
    const journey = await driveNoReferenceInputToStep04({ harness, fixture });
    const { setup, init, step03 } = journey;
    const ledger = journey.prepared.ledgerEvidence;
    if (ledger === "OutOfDomain" || !("Present" in ledger))
      throw new Error(
        "present-input control has no authenticated ledger membership",
      );
    expect(step03.selectedInput).toEqual(journey.prepared.selectedInput);
    const genuineLedgerMembershipProof = Data.to(
      ledger.Present.proof,
      SDK.Proof,
    );

    const forgeStep04 = async (txsNonMembershipProofCbor: string) =>
      submitRawInputExistenceStep({
        ...journey.stageArguments,
        contracts: {
          hubOraclePolicyId: harness.contracts.hubOracle.policyId,
          steps: harness.contracts.fraudProofContracts.noReferenceInput.steps,
          computationThread: harness.contracts.computationThread,
          fraudProof: harness.contracts.fraudProof,
        },
        categoryId: harness.catalogue.categories.noReferenceInput.categoryId,
        prepared: {
          ...journey.prepared,
          ledgerEvidence: {
            Absent: { proof: Data.from(txsNonMembershipProofCbor, SDK.Proof) },
          },
        },
        threadOutRef: step03.nextThreadOutRef,
        stepIndex: 3,
      });

    // The strongest material an adversary holds is the genuine MEMBERSHIP
    // witness for the held input under its actual event pre-root. `pexcludes.exclusion.withdraw`
    // binds it as `mpf.insert(trie, key, "", proof)`, which asserts
    // `excluding(key, proof) == root` and fails outright for a key the trie
    // already holds: a membership witness cannot masquerade as its opposite.
    const membershipRefusal = await expectOnchainRefusal(async () =>
      forgeStep04(genuineLedgerMembershipProof),
    );
    expect(membershipRefusal).toMatch(/failed script execution/u);

    // The retired fixture shape (#582) stays refused too: an empty proof is a
    // witness only for an empty trie, never for this block's populated one.
    const emptyProofCbor = Data.to([], SDK.Proof);
    await expectOnchainRefusal(async () => forgeStep04(emptyProofCbor));

    // Both refusals were the validator's, not a spent thread's: the thread is
    // still at step 04, unspent, and no fraud-proof token was minted.
    await expectSingleUtxoWithUnit(
      proverLucid,
      harness.contracts.fraudProofContracts.noReferenceInput.steps[3]!
        .spendingScriptAddress,
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
      expectedHeaderHashes: [
        journey.prepared.source.header.prevHeaderHash,
        setup.headerHash,
      ],
    });
  }, 900_000);
});
