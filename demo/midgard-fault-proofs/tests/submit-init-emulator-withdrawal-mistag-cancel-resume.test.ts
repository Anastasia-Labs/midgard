/** Cancellation, strict reference-script refusal, and out-ref-only resume. */
import { describe, expect, it } from "vitest";

import {
  submitWithdrawalMistagCancel,
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
  submitWithdrawalMistagStep05,
} from "../src/withdrawal-mistag/index.js";
import {
  initWithdrawalMistagThreadV1,
  makeWithdrawalMistagEmulatorHarnessV1,
  publishWithdrawalMistagScriptsV1,
  setupWithdrawalMistagScenarioV1,
  withdrawalMistagBlockUtxoV1,
} from "./support/withdrawal-mistag-emulator-v1.js";

describe("withdrawal-mistag cancel and resume", () => {
  it("burns a cancelled thread, re-initialises, and resumes from only its committed out-ref", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarnessV1();
    const scenario = await setupWithdrawalMistagScenarioV1({
      harness,
      direction: "valid-marked-invalid",
    });
    const published = await publishWithdrawalMistagScriptsV1({ harness });
    const refs = published.refs;
    const blockUtxo = await withdrawalMistagBlockUtxoV1({ harness, scenario });
    const advanceTo = async (targetStep: 0 | 2 | 3) => {
      const init = await initWithdrawalMistagThreadV1({ harness, scenario });
      let threadOutRef = init.nextThreadOutRef;
      if (targetStep >= 2) {
        threadOutRef = (
          await submitWithdrawalMistagStep01({
            lucid: harness.proverLucid,
            contracts: harness.withdrawalMistag,
            signer: harness.proverSigner,
            prepared: scenario.prepared,
            threadOutRef,
            hubOracleUtxo: scenario.setup.hubOracle,
            stateQueueBlockUtxo: blockUtxo,
            referenceScriptUtxo: refs[0],
          })
        ).nextThreadOutRef;
        threadOutRef = (
          await submitWithdrawalMistagStep02({
            lucid: harness.proverLucid,
            contracts: harness.withdrawalMistag,
            signer: harness.proverSigner,
            prepared: scenario.prepared,
            threadOutRef,
            referenceScriptUtxo: refs[1],
          })
        ).nextThreadOutRef;
      }
      if (targetStep >= 3) {
        threadOutRef = (
          await submitWithdrawalMistagStep03({
            lucid: harness.proverLucid,
            contracts: harness.withdrawalMistag,
            signer: harness.proverSigner,
            prepared: scenario.prepared,
            threadOutRef,
            referenceScriptUtxo: refs[2],
          })
        ).nextThreadOutRef;
      }
      return threadOutRef;
    };

    for (const targetStep of [0, 2, 3] as const) {
      const cancelled = await submitWithdrawalMistagCancel({
        lucid: harness.proverLucid,
        contracts: harness.withdrawalMistag,
        signer: harness.proverSigner,
        threadOutRef: await advanceTo(targetStep),
        referenceScriptUtxo: refs[targetStep],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      expect(cancelled.cancelledStepIndex).toBe(targetStep);
      for (const step of harness.withdrawalMistag.steps) {
        await expect(
          harness.proverLucid.utxosAtWithUnit(
            step.spendingScriptAddress,
            cancelled.computationThreadUnit,
          ),
        ).resolves.toHaveLength(0);
      }
    }

    const resumedInit = await initWithdrawalMistagThreadV1({
      harness,
      scenario,
    });
    const step01Args = {
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: resumedInit.nextThreadOutRef,
      hubOracleUtxo: scenario.setup.hubOracle,
      stateQueueBlockUtxo: blockUtxo,
    } as const;
    await expect(
      submitWithdrawalMistagStep01({
        ...step01Args,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(/expected/u);
    const step01 = await submitWithdrawalMistagStep01({
      ...step01Args,
      referenceScriptUtxo: refs[0],
    });
    const step02 = await submitWithdrawalMistagStep02({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
    });
    // JSON round-trips are process boundaries: no mutable cursor or in-memory
    // machine survives, only the authenticated live out-ref.
    const afterStep02 = JSON.parse(
      JSON.stringify({ threadOutRef: step02.nextThreadOutRef }),
    ) as { readonly threadOutRef: string };
    const step03 = await submitWithdrawalMistagStep03({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: afterStep02.threadOutRef,
      referenceScriptUtxo: refs[2],
    });
    const afterStep03 = JSON.parse(
      JSON.stringify({ threadOutRef: step03.nextThreadOutRef }),
    ) as { readonly threadOutRef: string };
    const step04 = await submitWithdrawalMistagStep04({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: afterStep03.threadOutRef,
      referenceScriptUtxo: refs[3],
    });
    const fraud = await submitWithdrawalMistagStep05({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.withdrawalMistag.fraudProof.spendingScriptAddress,
        fraud.fraudProofUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 600_000);
});
