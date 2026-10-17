/**
 * withdrawal-mistag over a committed `l1_datum` the typed encoder cannot
 * reproduce: a constructor index at the top of the 64-bit range, which the
 * typed encoder cannot write, and a map that repeats a key, which decoding
 * collapses. The redeemers carry the committed bytes as committed.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  prepareWithdrawalMistag,
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
} from "../src/withdrawal-mistag/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  buildWithdrawalMistagEvidenceMaterial,
  driveWithdrawalMistagToFraud,
  initWithdrawalMistagThread,
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
  removeWithdrawalMistagBlock,
  setupWithdrawalMistagScenario,
  withdrawalMistagBlockUtxo,
} from "./support/withdrawal-mistag-emulator.js";

// The preparer refuses honest evidence off-chain. To reach the validator with
// it, the honest case lets the preparer label it instead of throwing; every
// authentication the preparer performs still runs.
const preparer = vi.hoisted(() => ({ labelHonest: false }));
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    withdrawalMistagDirection: (
      args: Parameters<typeof actual.withdrawalMistagDirection>[0],
    ) =>
      preparer.labelHonest && args.claimedValid === args.actualValid
        ? "valid-marked-invalid"
        : actual.withdrawalMistagDirection(args),
  };
});

/** InlineDatum carrying constructor 2^64 - 1 with no fields. */
const LARGEST_CONSTRUCTOR_DATUM = "d87b9fd866821bffffffffffffffff80ff";
/** InlineDatum carrying a map that repeats key 1. */
const REPEATED_KEY_DATUM = "d87b9fa201020103ff";

type Harness = Awaited<ReturnType<typeof makeWithdrawalMistagEmulatorHarness>>;
type Scenario = Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;

const convict = async (harness: Harness, scenario: Scenario) => {
  const published = await publishWithdrawalMistagScripts({ harness });
  const lifecycle = await driveWithdrawalMistagToFraud({
    harness,
    scenario,
    refs: published.refs,
  });
  const fraud = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.withdrawalMistag.fraudProof.spendingScriptAddress,
    lifecycle.fraud.fraudProofUnit,
  );
  expect(fraud.assets[lifecycle.fraud.fraudProofUnit]).toBe(1n);
  const removal = await removeWithdrawalMistagBlock({ harness, scenario });
  expect(removal.fraudCategory).toBe("withdrawalMistag");
};

describe("withdrawal-mistag over a committed l1_datum the typed encoder cannot reproduce", () => {
  it("convicts a valid withdrawal marked invalid whose datum has the largest constructor index", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "valid-marked-invalid",
      l1DatumCbor: LARGEST_CONSTRUCTOR_DATUM,
    });
    expect(scenario.prepared.committedWithdrawalInfoCbor).toContain(
      LARGEST_CONSTRUCTOR_DATUM,
    );
    expect(scenario.prepared).toMatchObject({
      direction: "valid-marked-invalid",
      actualValid: true,
    });
    await convict(harness, scenario);
  }, 600_000);

  it("convicts an unpayable withdrawal marked valid whose datum repeats a map key", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "invalid-marked-valid",
      l1DatumCbor: REPEATED_KEY_DATUM,
    });
    expect(scenario.prepared.committedWithdrawalInfoCbor).toContain(
      REPEATED_KEY_DATUM,
    );
    expect(scenario.prepared).toMatchObject({
      direction: "invalid-marked-valid",
      coreValid: true,
      payable: false,
    });
    await convict(harness, scenario);
  }, 600_000);

  it("refuses the decoded view's bytes at step 01", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "valid-marked-invalid",
      l1DatumCbor: REPEATED_KEY_DATUM,
    });
    // Re-encoding the decoded view keeps one map entry, so the bytes differ.
    const decodedBytes = SDK.committedWithdrawalValueBytes(
      scenario.prepared.committedWithdrawal.value,
    );
    expect(decodedBytes).not.toBe(
      scenario.prepared.committedWithdrawalInfoCbor,
    );
    const { refs } = await publishWithdrawalMistagScripts({ harness });
    const init = await initWithdrawalMistagThread({ harness, scenario });
    const blockUtxo = await withdrawalMistagBlockUtxo({ harness, scenario });
    // Step-01's membership check refuses these bytes. The refusal is not
    // pinned: step-01 built with traces is too large to publish.
    await expectOnchainRefusal(() =>
      submitWithdrawalMistagStep01({
        lucid: harness.proverLucid,
        contracts: harness.withdrawalMistag,
        signer: harness.proverSigner,
        prepared: {
          ...scenario.prepared,
          committedWithdrawalInfoCbor: decodedBytes,
        },
        threadOutRef: init.nextThreadOutRef,
        hubOracleUtxo: scenario.setup.hubOracle,
        stateQueueBlockUtxo: blockUtxo,
        referenceScriptUtxo: refs[0],
      }),
    );
  }, 600_000);

  it("refuses an honestly tagged withdrawal with the largest-constructor datum", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    preparer.labelHonest = true;
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "valid-marked-invalid",
      honest: true,
      l1DatumCbor: LARGEST_CONSTRUCTOR_DATUM,
    }).finally(() => {
      preparer.labelHonest = false;
    });
    // The unmodified preparer refuses such evidence off-chain.
    const offChain = await buildWithdrawalMistagEvidenceMaterial(
      "valid-marked-invalid",
      true,
      0,
      0,
      0,
      0,
      false,
      LARGEST_CONSTRUCTOR_DATUM,
    );
    await expect(
      prepareWithdrawalMistag({
        challengedHeaderHash: scenario.setup.headerHash,
        ...offChain.args,
      }),
    ).rejects.toThrow("withdrawal-mistag evidence is honestly tagged");
    expect(scenario.prepared).toMatchObject({
      actualValid: true,
      payable: true,
    });

    const { refs } = await publishWithdrawalMistagScripts({ harness });
    const common = {
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
    };
    const init = await initWithdrawalMistagThread({ harness, scenario });
    const step01 = await submitWithdrawalMistagStep01({
      ...common,
      threadOutRef: init.nextThreadOutRef,
      hubOracleUtxo: scenario.setup.hubOracle,
      stateQueueBlockUtxo: await withdrawalMistagBlockUtxo({
        harness,
        scenario,
      }),
      referenceScriptUtxo: refs[0],
    });
    const step02 = await submitWithdrawalMistagStep02({
      ...common,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
    });
    const step03 = await submitWithdrawalMistagStep03({
      ...common,
      threadOutRef: step02.nextThreadOutRef,
      referenceScriptUtxo: refs[2],
    });
    // Step-04 judges the committed bytes valid and payable and refuses to
    // forward a claim that agrees with that verdict.
    await expectOnchainRefusal(
      () =>
        submitWithdrawalMistagStep04({
          ...common,
          threadOutRef: step03.nextThreadOutRef,
          referenceScriptUtxo: refs[3],
        }),
      {
        refusedBy: "fraud_proofs/withdrawal_mistag/step_04",
        check: /claimed_valid != actual_valid/u,
      },
    );
    await expect(
      harness.proverLucid.utxosAt(
        harness.withdrawalMistag.fraudProof.spendingScriptAddress,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);
});
