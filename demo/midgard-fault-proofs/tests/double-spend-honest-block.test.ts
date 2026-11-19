/** Honest-commitment polarity of the double-spend proof, from a fresh key. */
import { outRefLabel } from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseSpendInputCbors } from "../src/double-spend/submit-step-03.js";
import { buildStep04 } from "../src/double-spend/submit-step-04.submit-step04.js";
import { parseSubmitStep01TxInclusion } from "../src/step-support.js";
import { submitRawDoubleSpendStep04 } from "./support/double-spend-step04-raw.js";
import { network } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { stageDoubleSpendStep04 } from "./support/submit-init/double-spend-staging.js";
import { submitTurnsUnjournaled } from "./support/submit-unjournaled.js";

describe("double-spend proof against an honest block", () => {
  it("lets a fresh key reach step 04, then refuses the proof at the final input comparison", async () => {
    const staged = await stageDoubleSpendStep04({
      honestBlock: true,
      seeds: { prover: generateSeedPhrase() },
    });
    expect(
      Object.values(SELECTED_DEPLOYMENT_PROFILE.allow_lists).flatMap(
        (keys) => keys ?? [],
      ),
    ).not.toContain(staged.proverSigner.paymentKeyHash);
    const { transactionInclusion } = staged;
    expect(transactionInclusion.tx2InputsPreimage).not.toContainEqual(
      transactionInclusion.tx1InputsPreimage[1],
    );

    // The builder refuses: tx2 does not spend the input the thread carries.
    await expect(
      submitTurnsUnjournaled(buildStep04, {
        lucid: staged.proverLucid,
        blueprint: staged.realBlueprint,
        deploymentInfo: staged.deploymentInfo,
        network,
        signer: staged.proverSigner,
        threadOutRef: outRefLabel(staged.fourthStepUtxo),
        tx2SpendInputCbors: parseSpendInputCbors(
          transactionInclusion.tx2SpendInputCbors,
          "--tx2-inputs",
        ),
        nativeTxCompactCbor: parseSubmitStep01TxInclusion(
          transactionInclusion.tx2.inclusion,
        ).nativeTxCompactCbor,
        doubleSpentInputIndex: 1n,
        referenceScriptUtxo:
          staged.doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep04"]!
            .utxo,
        witnessReferenceScripts: staged.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/does not match the double-spent input/u);

    // Built by hand past that check, step 04 dies at its final comparison for
    // every tx2 input.
    for (const index of [0n, 1n]) {
      await expectOnchainRefusal(
        () =>
          submitRawDoubleSpendStep04({ staged, doubleSpentInputIndex: index }),
        {
          refusedBy: "fraud_proofs/double_spend/step_04",
          check: /^expect double_spent_input == tx2_double_spent_input/u,
        },
      );
    }
  }, 600_000);
});
