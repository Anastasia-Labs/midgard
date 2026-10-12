/**
 * withdrawal-mistag over a block whose two orders name one L2 outref. The
 * second order is judged against its own step's pre-state, which lacks the
 * outref the first order drained: `NonExistentWithdrawalUtxo` is the honest
 * tag, and `WithdrawalIsValid` is a mistag.
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
import { setupDuplicateOutRefMistagScenario } from "./support/withdrawal-mistag-duplicate-outref.js";
import {
  driveWithdrawalMistagToFraud,
  initWithdrawalMistagThread,
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
  removeWithdrawalMistagBlock,
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
        ? "invalid-marked-valid"
        : actual.withdrawalMistagDirection(args),
  };
});

describe("withdrawal-mistag over duplicate-outref withdrawals", () => {
  it("convicts a duplicate tagged WithdrawalIsValid over the drained outref", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupDuplicateOutRefMistagScenario({
      harness,
      secondValidity: "WithdrawalIsValid",
    });
    const [step0, step1] = scenario.material.steps;
    expect(step1!.pre_utxos_root).toBe(step0!.post_utxos_root);
    expect(scenario.prepared).toMatchObject({
      outputPresent: false,
      actualValid: false,
      direction: "invalid-marked-valid",
    });

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
  }, 600_000);

  it("refuses the honest NonExistentWithdrawalUtxo duplicate on chain", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    preparer.labelHonest = true;
    const scenario = await setupDuplicateOutRefMistagScenario({
      harness,
      secondValidity: "NonExistentWithdrawalUtxo",
    }).finally(() => {
      preparer.labelHonest = false;
    });
    // The unmodified preparer refuses the same evidence off-chain.
    await expect(
      prepareWithdrawalMistag({
        challengedHeaderHash: scenario.setup.headerHash,
        ...scenario.material.args,
      }),
    ).rejects.toThrow("withdrawal-mistag evidence is honestly tagged");
    expect(scenario.prepared).toMatchObject({
      outputPresent: false,
      actualValid: false,
    });
    expect(
      SDK.withdrawalClaimsValid(scenario.prepared.committedWithdrawal.value),
    ).toBe(false);

    const { refs } = await publishWithdrawalMistagScripts({ harness });
    const blockUtxo = await withdrawalMistagBlockUtxo({ harness, scenario });
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
      stateQueueBlockUtxo: blockUtxo,
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
    // Step-04 derives the verdict from the authenticated absence and
    // refuses to forward a claim that agrees with it.
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
    // The thread stays at step-04 and no fraud token exists.
    await expect(
      harness.proverLucid.utxosAt(
        harness.withdrawalMistag.steps[3].spendingScriptAddress,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.proverLucid.utxosAt(
        harness.withdrawalMistag.fraudProof.spendingScriptAddress,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);
});
