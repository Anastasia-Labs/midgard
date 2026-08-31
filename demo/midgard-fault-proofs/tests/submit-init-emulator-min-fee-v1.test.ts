/** Real lifecycle for the registered standalone single-party min-fee proof. */
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  prepareMinFeeFromTransactions,
  submitMinFeeCancel,
  submitMinFeeInit,
  submitMinFeeStep01,
  submitMinFeeStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import type { MinFeeFieldItemCborsV1 } from "../src/submit-min-fee-step-02.js";
import { parseSubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * 365 committed spend inputs (constant §5.3 stride of 40 bytes each) make a
 * 14,603-byte field-0 preimage: past §8.4's tier-1 bound, inside the
 * single-publication tier-2 window `(14,336, 15,148]` — the size alone
 * selects `RawUtxo`.
 */
const TIER2_SPEND_INPUT_COUNT = 365;

const fieldItemCbors = (
  fields: readonly (readonly string[])[],
): MinFeeFieldItemCborsV1 => {
  if (fields.length !== 9) throw new Error("fixture requires nine fields");
  return fields.map((items) =>
    items.map((item) => Buffer.from(item, "hex")),
  ) as unknown as MinFeeFieldItemCborsV1;
};

const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realMinFee: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const minFee = harness.contracts.minFee;
  const category = harness.catalogue.categories.minFee;
  if (minFee === undefined || category === undefined) {
    throw new Error("Harness did not build the min-fee contracts/category");
  }
  expect(category.categoryId).toBe(
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minFee,
  );
  expect(category.scriptHash).toBe(minFee.steps[0].spendingScriptHash);
  expect(minFee.steps[0].spendingScriptHash).not.toBe(
    minFee.steps[1].spendingScriptHash,
  );
  return { ...harness, minFee, category };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

const catalogueOf = (harness: Harness) => ({
  policyId: harness.contracts.fraudProofCatalogue.policyId,
  spendingScriptAddress:
    harness.contracts.fraudProofCatalogue.spendingScriptAddress,
  root: harness.catalogue.root,
});

const setupScenario = async ({
  harness,
  fee,
  headerMinimum,
  spendInputs = [outRefCbor(0x31, 0n)],
}: {
  readonly harness: Harness;
  readonly fee: bigint;
  readonly headerMinimum: bigint;
  readonly spendInputs?: readonly Buffer[];
}) => {
  const tx = buildFixtureTransactionV1({
    spendInputs,
    fee,
  });
  // The header's normative transactions MPF commits
  // `Data(L2TransactionSourceV1)` per transaction id, which is the value
  // step-01 authenticates and `prepareMinFeeFromTransactions` recounts.
  const block = await buildCanonicalBlockFixtureV1({
    transactions: [tx],
  });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const start =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.HeaderV1 = {
    ...makeHeader(operatorVkey, start, block.payloadSourceTransactionsRoot, 1n),
    minFeeA: 0n,
    minFeeB: headerMinimum,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  // An honest-boundary scenario cannot pass the production prepare guard.
  // Its test-only extraction uses an under-fee schedule solely to obtain the
  // same authenticated inclusion/field bytes; step-01 reads the real schedule
  // from the on-chain header and never accepts these preparation values.
  const prepared = await prepareMinFeeFromTransactions({
    headerHash: setup.headerHash,
    transactions: [
      { nodeTxId: tx.txId, txCbor: tx.canonicalCbor.toString("hex") },
    ],
    expectedTransactionsRoot: block.payloadSourceTransactionsRoot,
    minFeeA: 0n,
    minFeeB: fee + 1n,
    categoryId: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minFee,
  });
  const refs: readonly [UTxO, UTxO] = [
    (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.minFee.steps[0].spendingScript,
        label: "min-fee step-01",
      })
    ).utxo,
    (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.minFee.steps[1].spendingScript,
        label: "min-fee step-02",
      })
    ).utxo,
  ];
  return {
    setup,
    prepared,
    txInclusion: parseSubmitStep01TxInclusion(prepared.tx.txInclusion),
    fieldItemCbors: fieldItemCbors(prepared.tx.fieldItemCbors),
    refs,
  };
};

const initThread = async (
  harness: Harness,
  scenario: Awaited<ReturnType<typeof setupScenario>>,
) =>
  await submitMinFeeInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.minFee,
    category: harness.category,
    catalogue: catalogueOf(harness),
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

const advanceStep01 = async (
  harness: Harness,
  scenario: Awaited<ReturnType<typeof setupScenario>>,
  threadOutRef: string,
) =>
  await submitMinFeeStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    contracts: harness.minFee,
    categoryId: harness.category.categoryId,
    network,
    signer: harness.proverSigner,
    threadOutRef,
    stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    txInclusion: scenario.txInclusion,
    referenceScriptUtxo: scenario.refs[0],
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

describe("min-fee emulator lifecycle", () => {
  it("cancels both steps, resumes the same thread, rejects malformed evidence, mints, and removes", async () => {
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 999n,
      headerMinimum: 1_000n,
    });
    const sharedCancel = {
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    };

    const atStep01 = await initThread(harness, scenario);
    const cancel01 = await submitMinFeeCancel({
      ...sharedCancel,
      threadOutRef: atStep01.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[0],
    });
    expect(cancel01.cancelledStepIndex).toBe(0);

    const again = await initThread(harness, scenario);
    const atStep02ForCancel = await advanceStep01(
      harness,
      scenario,
      again.nextThreadOutRef,
    );
    const cancel02 = await submitMinFeeCancel({
      ...sharedCancel,
      threadOutRef: atStep02ForCancel.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[1],
    });
    expect(cancel02.cancelledStepIndex).toBe(1);

    const wrongReference = await initThread(harness, scenario);
    await expect(
      submitMinFeeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: wrongReference.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.txInclusion,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/reference script .* hashes to/u);
    await submitMinFeeCancel({
      ...sharedCancel,
      threadOutRef: wrongReference.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[0],
    });

    const resumed = await initThread(harness, scenario);
    const bound = await advanceStep01(
      harness,
      scenario,
      resumed.nextThreadOutRef,
    );
    expect(bound.computationThreadUnit).toBe(resumed.computationThreadUnit);

    const missing = scenario.fieldItemCbors.slice(0, 8);
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: missing as unknown as MinFeeFieldItemCborsV1,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/exactly nine/u);

    const permuted = [...scenario.fieldItemCbors];
    [permuted[0], permuted[1]] = [permuted[1]!, permuted[0]!];
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: permuted as unknown as MinFeeFieldItemCborsV1,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/field 0|field 1/u);

    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: {
          ...scenario.prepared.tx.witnessSet,
          script_tx_wits_hash: "99".repeat(32),
        },
        fieldItemCbors: scenario.fieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/compact transaction commits/u);

    const wrongSigner = {
      ...harness.proverSigner,
      paymentKeyHash: "99".repeat(28),
    };
    await expect(
      submitMinFeeCancel({
        ...sharedCancel,
        signer: wrongSigner,
        threadOutRef: bound.nextThreadOutRef,
        referenceScriptUtxo: scenario.refs[1],
      }),
    ).rejects.toThrow(/only that prover may cancel/u);

    const finalized = await submitMinFeeStep02({
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      witnessSet: scenario.prepared.tx.witnessSet,
      fieldItemCbors: scenario.fieldItemCbors,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(finalized.fee).toBe(999n);
    expect(finalized.minimumFee).toBe(1_000n);
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(finalized.fraudProofOutRef);
    expect(Data.from(proofUtxo.datum!, SDK.FraudProofTokenDatum)).toStrictEqual(
      { fraud_prover: harness.proverSigner.paymentKeyHash },
    );

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
      fraudCategory: "minFee",
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudCategory).toBe("minFee");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(finalized.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: scenario.setup.headerHash,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("routes an oversized field-0 preimage through tier-2 published carriage to the conviction", async () => {
    // 365 committed spend inputs put field 0's §5.1 preimage at 14,603 bytes —
    // past §8.4's 14,336-byte tier-1 redeemer bound, inside the
    // single-publication tier-2 window — so the ladder itself routes that one
    // field to `RawUtxo` while the other eight ride inline. Nothing forces
    // the tier; the committed data's size does.
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 999n,
      headerMinimum: 1_000n,
      spendInputs: Array.from({ length: TIER2_SPEND_INPUT_COUNT }, (_, index) =>
        outRefCbor(0x31, BigInt(index)),
      ),
    });
    const preimageBytes = encodeMidgardFieldPreimageV1(
      scenario.fieldItemCbors[0],
    ).length;
    expect(preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    expect(preimageBytes).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K_V1);

    const init = await initThread(harness, scenario);
    const bound = await advanceStep01(harness, scenario, init.nextThreadOutRef);
    const finalized = await submitMinFeeStep02({
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      witnessSet: scenario.prepared.tx.witnessSet,
      fieldItemCbors: scenario.fieldItemCbors,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(finalized.fee).toBe(999n);
    expect(finalized.minimumFee).toBe(1_000n);
    expect(finalized.fieldCarriageTiers[0]).toBe("RawUtxo");
    expect(
      finalized.fieldCarriageTiers.slice(1).every((tier) => tier === "Inline"),
    ).toBe(true);
    expect(finalized.fieldPreimageLengths[0]).toBe(preimageBytes);

    // The tier-2 publication really exists at the prover's address as a
    // bytes-only inline datum, referenced rather than spent by the step.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCborV1(
      encodeMidgardFieldPreimageV1(scenario.fieldItemCbors[0]),
    );
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(finalized.fraudProofOutRef);
  }, 600_000);

  it("reaches step-02 and lets the compiled validator refuse an honest exact fee", async () => {
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 1_000n,
      headerMinimum: 1_000n,
    });
    const init = await initThread(harness, scenario);
    const bound = await advanceStep01(harness, scenario, init.nextThreadOutRef);
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: scenario.fieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
        unsafeSkipLocalViolationCheckForTest: true,
      }),
    ).rejects.toThrow();
    const fraudUnit = toUnit(
      harness.minFee.fraudProof.policyId,
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.minFee.fraudProof.spendingScriptAddress,
        fraudUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
    const cancelled = await submitMinFeeCancel({
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(1);
  }, 600_000);
});
