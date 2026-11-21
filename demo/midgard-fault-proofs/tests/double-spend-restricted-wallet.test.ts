/** Double-spend steps 03 and 04 built under the watcher's restricted prover
 * wallet. Both read published reference scripts, which once made them hand
 * Lucid a preset; Lucid then takes collateral only from among the preset.
 * Each step must still spend and post as collateral only the plain coins
 * the restricted wallet offers, never one another action holds. */
import { outRefLabel } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { parseSpendInputCbors } from "../src/double-spend/submit-step-03.js";
import { buildStep04 } from "../src/double-spend/submit-step-04.submit-step04.js";
import { parseSubmitStep01TxInclusion } from "../src/step-support.js";
import { network } from "./support/emulator/blueprints.js";
import {
  fundProverCoin,
  restrictedProverWallet,
} from "./support/restricted-prover-wallet.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { stageDoubleSpendStep04 } from "./support/submit-init/double-spend-staging.js";
import { submitTurnsUnjournaled } from "./support/submit-unjournaled.js";

describe("double-spend steps under the watcher's restricted wallet", () => {
  it("funds step 03 and step 04 and posts their collateral from the coins the restricted wallet offers", async () => {
    pinStepTranscript();
    let step03Checked = () => 0;
    const staged = await stageDoubleSpendStep04({
      step03Funding: async ({ emulator, funderLucid, proverSigner }) => {
        const held = await fundProverCoin({
          emulator,
          funderLucid,
          signer: proverSigner,
        });
        const funding = restrictedProverWallet({
          emulator,
          signer: proverSigner,
          held: [held],
        });
        step03Checked = funding.checked;
        return funding;
      },
    });
    expect(step03Checked()).toBe(1);

    const funding = restrictedProverWallet({
      emulator: staged.emulator,
      signer: staged.proverSigner,
    });
    const { transactionInclusion } = staged;
    const step04 = await submitTurnsUnjournaled(buildStep04, {
      lucid: staged.proverLucid,
      blueprint: staged.realBlueprint,
      deploymentInfo: staged.deploymentInfo,
      network,
      signer: funding.signer,
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
      beforeSubmit: funding.beforeSubmit,
      awaitConfirmation: true,
    });
    expect(funding.checked()).toBe(1);
    expect(step04.fraudProofUnit).toBeDefined();
  }, 600_000);
});
