import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  prepareMinFeeFromTransactions,
  submitMinFeeInit,
  submitMinFeeStep01,
} from "../src/index.js";
import { parseSubmitStep01TxInclusion } from "../src/step-support.js";
import type { MinFeeFieldItemCbors } from "../src/submit-min-fee-step-02.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * 365 committed spend inputs (constant §5.3 stride of 40 bytes each) make a
 * 14,603-byte field-0 preimage: past §8.4's tier-1 bound, inside the
 * single-publication tier-2 window `(14,336, 15,148]` — the size alone
 * selects `RawUtxo`.
 */
export const TIER2_SPEND_INPUT_COUNT = 365;

const fieldItemCbors = (
  fields: readonly (readonly string[])[],
): MinFeeFieldItemCbors => {
  if (fields.length !== 9) throw new Error("fixture requires nine fields");
  return fields.map((items) =>
    items.map((item) => Buffer.from(item, "hex")),
  ) as unknown as MinFeeFieldItemCbors;
};

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
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

export const setupScenario = async ({
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
  const tx = buildFixtureTransaction({
    spendInputs,
    fee,
  });
  // The header's normative transactions MPF commits
  // `Data(L2TransactionSourceV1)` per transaction id, which is the value
  // step-01 authenticates and `prepareMinFeeFromTransactions` recounts.
  const block = await buildCanonicalBlockFixture({
    transactions: [tx],
  });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const start =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
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

export const initThread = async (
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

export const advanceStep01 = async (
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
