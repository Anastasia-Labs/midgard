/** Cancellation, strict reference-script refusal, and out-ref-only resume. */
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { buildLinearFaultCancel } from "../src/linear-fault-cancel.js";
import { WITHDRAWAL_MISTAG_CATEGORY_LABEL } from "../src/withdrawal-mistag/contracts.js";
import { buildWithdrawalMistagStep05 } from "../src/withdrawal-mistag/submit-withdrawal-mistag-step-05.js";
import {
  buildWithdrawalMistagStep01,
  buildWithdrawalMistagStep02,
  buildWithdrawalMistagStep03,
  buildWithdrawalMistagStep04,
} from "../src/withdrawal-mistag/submit-withdrawal-mistag-steps.submit-withdrawal-mistag-intermediate-step.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { buildAndSubmit } from "./support/submit-steps.js";
import { makeWithdrawalMistagEmulatorHarness } from "./support/withdrawal-mistag-emulator.build-withdrawal-mistag-evidence-material.js";
import {
  initWithdrawalMistagThread,
  publishWithdrawalMistagScripts,
  setupWithdrawalMistagScenario,
  withdrawalMistagBlockUtxo,
} from "./support/withdrawal-mistag-emulator.drive-withdrawal-mistag-to-fraud.js";

describe("withdrawal-mistag cancel and resume", () => {
  it("burns a cancelled thread, re-initialises, and resumes from only its committed out-ref", async () => {
    pinStepTranscript();
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "valid-marked-invalid",
    });
    const published = await publishWithdrawalMistagScripts({ harness });
    const refs = published.refs;
    const blockUtxo = await withdrawalMistagBlockUtxo({ harness, scenario });
    const advanceTo = async (targetStep: 0 | 1 | 2 | 3 | 4) => {
      const init = await initWithdrawalMistagThread({ harness, scenario });
      let threadOutRef = init.nextThreadOutRef;
      if (targetStep >= 1) {
        threadOutRef = (
          await buildAndSubmit(buildWithdrawalMistagStep01, {
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
      }
      if (targetStep >= 2) {
        threadOutRef = (
          await buildAndSubmit(buildWithdrawalMistagStep02, {
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
          await buildAndSubmit(buildWithdrawalMistagStep03, {
            lucid: harness.proverLucid,
            contracts: harness.withdrawalMistag,
            signer: harness.proverSigner,
            prepared: scenario.prepared,
            threadOutRef,
            referenceScriptUtxo: refs[2],
          })
        ).nextThreadOutRef;
      }
      if (targetStep >= 4) {
        threadOutRef = (
          await buildAndSubmit(buildWithdrawalMistagStep04, {
            lucid: harness.proverLucid,
            contracts: harness.withdrawalMistag,
            signer: harness.proverSigner,
            prepared: scenario.prepared,
            threadOutRef,
            referenceScriptUtxo: refs[3],
          })
        ).nextThreadOutRef;
      }
      return threadOutRef;
    };

    for (const targetStep of [0, 1, 2, 3, 4] as const) {
      const cancelled = await buildAndSubmit(buildLinearFaultCancel, {
        lucid: harness.proverLucid,
        family: WITHDRAWAL_MISTAG_CATEGORY_LABEL,
        steps: harness.withdrawalMistag.steps,
        computationThread: harness.withdrawalMistag.computationThread,
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawalMistag,
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

    const resumedInit = await initWithdrawalMistagThread({
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
      buildAndSubmit(buildWithdrawalMistagStep01, {
        ...step01Args,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(/expected/u);
    const step01 = await buildAndSubmit(buildWithdrawalMistagStep01, {
      ...step01Args,
      referenceScriptUtxo: refs[0],
    });
    const step02 = await buildAndSubmit(buildWithdrawalMistagStep02, {
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
    const step03 = await buildAndSubmit(buildWithdrawalMistagStep03, {
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
    const step04 = await buildAndSubmit(buildWithdrawalMistagStep04, {
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: afterStep03.threadOutRef,
      referenceScriptUtxo: refs[3],
    });
    const fraud = await buildAndSubmit(buildWithdrawalMistagStep05, {
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
