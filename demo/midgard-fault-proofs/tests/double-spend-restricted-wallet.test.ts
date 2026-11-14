/** Double-spend steps 03 and 04 built under the watcher's restricted prover
 * wallet. Both read published reference scripts, which once made them hand
 * Lucid a preset; Lucid then takes collateral only from among the preset, so
 * the reserved collateral was lost and a funding coin paid it instead. */
import { outRefLabel } from "@al-ft/midgard-core";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitStep04 } from "../src/double-spend/submit-step-04.js";
import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
} from "./support/legacy-submit-emulator.js";
import {
  fundReservedCollateral,
  restrictedProverFunding,
} from "./support/restricted-prover-funding.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { stageDoubleSpendStep04 } from "./support/submit-init/double-spend-staging.js";
import { network } from "./support/submit-init-emulator-shared.js";

describe("double-spend steps under the watcher's restricted wallet", () => {
  it("takes step 03's and step 04's collateral from the reserved collateral", async () => {
    pinStepTranscript();
    let collateral: UTxO | undefined;
    let step03Checked = () => 0;
    const staged = await stageDoubleSpendStep04({
      step03Funding: async ({ emulator, funderLucid, proverSigner }) => {
        collateral = await fundReservedCollateral({
          emulator,
          funderLucid,
          signer: proverSigner,
        });
        const funding = await restrictedProverFunding({
          emulator,
          signer: proverSigner,
          collateral,
        });
        step03Checked = funding.checked;
        return funding;
      },
    });
    expect(step03Checked()).toBe(1);

    const funding = await restrictedProverFunding({
      emulator: staged.emulator,
      signer: staged.proverSigner,
      collateral: collateral!,
    });
    const { transactionInclusion } = staged;
    const step04 = await submitStep04({
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
      preSubmitBoundary: funding.preSubmitBoundary,
      awaitConfirmation: true,
    });
    expect(funding.checked()).toBe(1);
    expect(step04.fraudProofUnit).toBeDefined();
  }, 600_000);
});
