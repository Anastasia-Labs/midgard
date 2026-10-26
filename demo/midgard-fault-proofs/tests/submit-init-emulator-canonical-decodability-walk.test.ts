/**
 * canonical-decodability step 01 across two batches: the walk's successor is
 * exactly the one the validator derives, on the first step and on a resumed
 * one, and only the thread's prover may advance it.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  CanonicalDecodabilityStep01Datum,
  type CanonicalDecodabilityStep02State,
  type CanonicalDecodabilityWalkState,
  type CommittedFieldClaim,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
  miscountedMidgardFieldPreimage,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  Lucid,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  resolveProverSigner,
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityBodyFixture,
  type CanonicalDecodabilityRawSuccessor,
  makeCanonicalDecodabilityEmulatorHarness,
  network,
  publishCanonicalDecodabilityReferenceScripts,
  setupCanonicalDecodabilityScenario,
  submitCanonicalDecodabilityStep01Raw,
} from "./support/canonical-decodability-emulator.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const B = MIDGARD_ENVELOPE_BATCH_ITEMS;

describe("canonical-decodability batched walk", () => {
  it("accepts only the derived successor at each step and only from the prover", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    const {
      realBlueprint,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      canonicalDecodability,
      category,
      witnessReferenceScripts,
    } = harness;
    const [step01Ref, step02Ref] =
      await publishCanonicalDecodabilityReferenceScripts({
        lucid: proverLucid,
        contracts: canonicalDecodability,
      });
    // B + 2 items declared, B + 1 present: the first batch reads B and owes
    // two, the resumed batch reads one and finds no header for the last.
    const committedPreimage = miscountedMidgardFieldPreimage(
      B + 2,
      Array.from({ length: B + 1 }, () => Buffer.alloc(0)),
    );
    const fixture = await buildCanonicalDecodabilityBodyFixture({
      committedPreimage,
    });
    const setup = await setupCanonicalDecodabilityScenario({
      harness,
      fixture,
    });
    const init = await submitCanonicalDecodabilityInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: canonicalDecodability,
      category,
      catalogue: {
        policyId: contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts,
    });
    const unit = init.computationThreadUnit;
    const stepAddress = (step: 0 | 1) =>
      canonicalDecodability.steps[step].spendingScriptAddress;
    const threadAt = (step: 0 | 1) =>
      expectSingleUtxoWithUnit(proverLucid, stepAddress(step), unit);
    const foreignScript = credentialToAddress(
      network,
      scriptHashToCredential("ab".repeat(28)),
    );

    const claim: CommittedFieldClaim = {
      BodyFieldClaim: {
        field_index: BigInt(fixture.fieldIndex),
        carriage: { Inline: { preimage: committedPreimage.toString("hex") } },
      },
    };
    const subject = {
      bad_tx_id: fixture.badTxId,
      field_index: BigInt(fixture.fieldIndex),
    };
    const cursor = (
      remaining: number,
      offset: number,
    ): CanonicalDecodabilityWalkState => ({
      ...subject,
      remaining: BigInt(remaining),
      offset: BigInt(offset),
    });
    const verdict = (code: number): CanonicalDecodabilityStep02State => ({
      ...subject,
      verdict: BigInt(code),
    });
    const raw = (
      threadOutRef: string,
      successor: CanonicalDecodabilityRawSuccessor,
      signer = proverSigner,
      lucid = proverLucid,
    ) =>
      submitCanonicalDecodabilityStep01Raw({
        lucid,
        blueprint: realBlueprint,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        claim,
        successor,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      });

    // First step: the derived successor is the cursor after B items.
    const firstStep = outRefLabel(await threadAt(0));
    const afterFirst = cursor(2, 3 + B);
    const firstForgeries: readonly CanonicalDecodabilityRawSuccessor[] = [
      { step: 1, state: cursor(1, 3 + B) },
      { step: 1, state: cursor(3, 3 + B) },
      { step: 1, state: cursor(2, 3 + B + 1) },
      {
        step: 1,
        state: { ...afterFirst, field_index: subject.field_index + 1n },
      },
      { step: 1, state: { ...afterFirst, bad_tx_id: "00".repeat(32) } },
      { step: 1, state: afterFirst, address: stepAddress(1) },
      { step: 1, state: afterFirst, address: foreignScript },
      { step: 2, state: verdict(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER) },
      { step: 2, state: verdict(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) },
    ];
    for (const successor of firstForgeries) {
      await expectOnchainRefusal(() => raw(firstStep, successor));
    }
    expect(outRefLabel(await threadAt(0))).toBe(firstStep);

    const first = await submitCanonicalDecodabilityStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: firstStep,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      fieldIndex: fixture.fieldIndex,
      committedPreimage,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts,
    });
    expect(first.complete).toBe(false);
    expect(first.verdict).toBeNull();
    expect(first.nextStepAddress).toBe(stepAddress(0));
    const resumed = await threadAt(0);
    expect(outRefLabel(resumed)).toBe(first.nextThreadOutRef);
    expect(Data.from(resumed.datum!, CanonicalDecodabilityStep01Datum)).toEqual(
      { fraud_prover: proverSigner.paymentKeyHash, data: afterFirst },
    );

    // Resumed step: the derived successor is the verdict at step 02.
    const convicting = verdict(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER);
    const stranger = generateEmulatorAccount({ lovelace: 0n });
    const strangerLucid = await Lucid(harness.emulator, "Custom");
    const strangerSigner = resolveProverSigner({
      network,
      walletSeedPhrase: stranger.seedPhrase,
    });
    strangerSigner.selectWallet(strangerLucid);
    const funding = await harness.funderLucid
      .newTx()
      .pay.ToAddress(strangerSigner.address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(strangerSigner.address, { lovelace: 1_000_000_000n })
      .complete();
    await harness.funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    expect(strangerSigner.paymentKeyHash).not.toBe(proverSigner.paymentKeyHash);
    await expectOnchainRefusal(() =>
      raw(
        first.nextThreadOutRef,
        { step: 2, state: convicting },
        strangerSigner,
        strangerLucid,
      ),
    );
    const resumedForgeries: readonly CanonicalDecodabilityRawSuccessor[] = [
      { step: 2, state: convicting, address: stepAddress(0) },
      { step: 2, state: convicting, address: foreignScript },
      { step: 2, state: verdict(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) },
      { step: 1, state: cursor(1, committedPreimage.length) },
      {
        step: 1,
        state: cursor(1, committedPreimage.length),
        address: stepAddress(1),
      },
    ];
    for (const successor of resumedForgeries) {
      await expectOnchainRefusal(() => raw(first.nextThreadOutRef, successor));
    }
    expect(outRefLabel(await threadAt(0))).toBe(first.nextThreadOutRef);

    // The stranger's transaction differs from this one only in who signs.
    const second = await raw(first.nextThreadOutRef, {
      step: 2,
      state: convicting,
    });
    const verdictThread = await threadAt(1);
    expect(outRefLabel(verdictThread)).toBe(second.nextThreadOutRef);
    await expect(
      proverLucid.utxosAtWithUnit(stepAddress(0), unit),
    ).resolves.toHaveLength(0);

    const step02 = await submitCanonicalDecodabilityStep02({
      lucid: proverLucid,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: second.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts,
    });
    await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
  }, 600_000);
});
