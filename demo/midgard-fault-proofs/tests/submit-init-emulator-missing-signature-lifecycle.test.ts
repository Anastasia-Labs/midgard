/** Both positive planes for the pre-registration missing-signature family. */
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  proveMissingSignatureFault,
  submitMissingSignatureInit,
  submitMissingSignatureStep01,
  submitMissingSignatureStep02,
  submitMissingSignatureStep03,
  submitMissingSignatureStep04,
} from "../src/missing-signature/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  makeMissingSignatureEmulatorHarness,
  missingSignatureFinding,
  missingSignatureProverDeps,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
} from "./support/missing-signature-emulator.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("missing-signature positive emulator lifecycle", () => {
  it("proves through the core, refuses a duplicate proof, and removes/slashes the fraudulent block", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    const scenario = await setupMissingSignatureScenario({ harness });
    const [step01, step02, step03, step04] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const finding = missingSignatureFinding(scenario);
    const events: string[] = [];
    const deps = missingSignatureProverDeps({
      harness,
      scenario,
      referenceScriptUtxos: { step01, step02, step03, step04 },
      journal: (event) => events.push(`${event.phase}:${event.message}`),
    });

    const outcome = await Effect.runPromise(
      proveMissingSignatureFault(finding, deps),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `expected proven, got ${outcome.kind}: ${outcome.reason}`,
      );
    }
    expect(outcome.txHashes).toHaveLength(5);
    expect(events).toContain("outcome:proven");

    const threadUnit = toUnit(
      harness.missingSignature.computationThread.policyId,
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
    for (const step of harness.missingSignature.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          threadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingSignature.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(outcome.fraudProofOutRef);
    expect(Data.from(proofUtxo.datum!, SDK.FraudProofTokenDatum)).toStrictEqual(
      { fraud_prover: harness.proverSigner.paymentKeyHash },
    );

    await expect(
      Effect.runPromise(proveMissingSignatureFault(finding, deps)),
    ).resolves.toMatchObject({ kind: "refused", refusal: "alreadyProven" });

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "missingSignature",
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal).toMatchObject({
      fraudCategory: "missingSignature",
      fraudCategoryId: harness.category.categoryId,
    });
    expect(removal.transactions[0]).toMatchObject({
      kind: "remove-target",
      slashingApproach: "SlashActiveOperator",
      removedOperator: scenario.block.header.operatorVkey,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [root] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueRootUnit,
    );
    expect(
      (
        await Effect.runPromise(
          SDK.utxoToStateQueueUTxO(
            root!,
            harness.contracts.stateQueue.policyId,
          ),
        )
      ).datum.next,
    ).toBe("Empty");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        scenario.setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingSignature.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(outcome.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingSignature",
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("drives every submitter directly at tier-1 inline carriage", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    const scenario = await setupMissingSignatureScenario({ harness });
    if (scenario.block.txInclusion === null) {
      throw new Error("normal subject has no counted-root inclusion");
    }
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const init = await submitMissingSignatureInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingSignature,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const one = await submitMissingSignatureStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const two = await submitMissingSignatureStep02({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: one.nextThreadOutRef,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      badRequiredSignerHashIndex: 0n,
      referenceScriptUtxo: step02Ref,
    });
    const three = await submitMissingSignatureStep03({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: two.nextThreadOutRef,
      missingRequiredSignerVkey: findingVkey(),
      referenceScriptUtxo: step03Ref,
    });
    const four = await submitMissingSignatureStep04({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: three.nextThreadOutRef,
      addrTxWits: scenario.subject.addrTxWits,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      witnessSetCompact: scenario.subject.witnessSetCompact,
      referenceScriptUtxo: step04Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(four.kind).toBe("proven");
    if (four.kind !== "proven") {
      throw new Error("one-witness step-04 unexpectedly selected a scan");
    }
    expect(four.fraudProofUnit).toBe(
      toUnit(
        harness.missingSignature.fraudProof.policyId,
        `${harness.category.categoryId}${scenario.setup.headerHash}`,
      ),
    );
  }, 600_000);

  it("proves a fat field-7 subject through tier-2 published carriage", async () => {
    // 142 decoy witnesses put field 7's §5.1 preimage at 14,628 bytes — past
    // §8.4's 14,336-byte tier-1 redeemer bound, inside the single-publication
    // tier-2 window — so the ladder itself routes step-04's opening to
    // `RawUtxo`. Nothing forces the tier; the committed data's size does. The
    // required-signer field stays one hash and rides inline at tier 1, and
    // the 142-item witness scan spans five 32-item step-04 batches.
    const harness = await makeMissingSignatureEmulatorHarness();
    const scenario = await setupMissingSignatureScenario({
      harness,
      decoyWitnessCount: TIER2_DECOY_WITNESS_COUNT,
    });
    const preimageBytes = SDK.encodeAddressWitnessPreimage(
      scenario.subject.addrTxWits,
    ).length;
    expect(preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimageBytes).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);
    const [step01, step02, step03, step04] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const deps = missingSignatureProverDeps({
      harness,
      scenario,
      referenceScriptUtxos: { step01, step02, step03, step04 },
    });
    const outcome = await Effect.runPromise(
      proveMissingSignatureFault(missingSignatureFinding(scenario), deps),
    );
    if (outcome.kind !== "proven") {
      throw new Error(
        `tier-2 proof did not finalize: ${outcome.kind} ${outcome.reason}${
          outcome.kind === "stalled" ? `; cause=${String(outcome.cause)}` : ""
        }`,
      );
    }
    expect(outcome.fraudProofUnit).toBe(
      toUnit(
        harness.missingSignature.fraudProof.policyId,
        `${harness.category.categoryId}${scenario.setup.headerHash}`,
      ),
    );
  }, 600_000);
});

/**
 * 142 decoy address witnesses (~103 preimage bytes each) make a 14,628-byte
 * field-7 preimage: past §8.4's tier-1 bound, inside the single-publication
 * tier-2 window `(14,336, 15,148]` — the size alone selects `RawUtxo`.
 */
const TIER2_DECOY_WITNESS_COUNT = 142;

const findingVkey = (): string => "11".repeat(32);
