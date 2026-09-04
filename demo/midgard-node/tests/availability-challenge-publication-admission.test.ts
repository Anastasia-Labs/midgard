import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const ONE_SHOT_OUT_REF = {
  txHash: "0".repeat(64),
  outputIndex: 0,
} as const;

describe("Q58 availability-challenge publication admission V1", () => {
  it("pins the exact applied identities and refuses both impossible reference-script bodies", async () => {
    const contracts = await loadRealMidgardContractsForTest(ONE_SHOT_OUT_REF);
    const targets = nodeRuntimeReferenceScriptTargets(contracts).filter(
      ({ name }) => name.startsWith("availability-challenge "),
    );
    const measured = targets.map(({ name, script }) => ({
      name,
      scriptHash: validatorToScriptHash(script),
      rawScriptBytes: script.script.length / 2,
    }));

    expect(measured).toEqual([
      {
        name: "availability-challenge spending",
        scriptHash: "47fa50cc3818cddba8b2d7f6d2d02e63b4efa325a5cdbe719aee4286",
        rawScriptBytes: 20_029,
      },
      {
        name: "availability-challenge minting",
        scriptHash: "47fa50cc3818cddba8b2d7f6d2d02e63b4efa325a5cdbe719aee4286",
        rawScriptBytes: 20_029,
      },
    ]);
    expect(() =>
      SDK.assertReferenceScriptRawBodiesFitL1Envelope(targets),
    ).toThrow(/availability-challenge spending raw script/u);
  });

  it("rejects publication under the Van Rossem transaction limits", async () => {
    const publisher = generateEmulatorAccount({ lovelace: 5_000_000_000n });
    const emulator = new Emulator([publisher], {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      maxTxSize: 16_384,
      maxTxExMem: 16_500_000n,
      maxTxExSteps: 10_000_000_000n,
    });
    const lucid = await Lucid(emulator, "Preprod");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const authPolicy = SDK.createReferenceScriptAuthPolicy(
      lucid,
      emulator.now(),
    );
    const contracts = await loadRealMidgardContractsForTest(
      ONE_SHOT_OUT_REF,
      authPolicy,
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts).filter(
      ({ name }) => name.startsWith("availability-challenge "),
    );
    const walletAddress = await lucid.wallet().address();
    for (const target of targets) {
      await expect(async () => {
        const selectedFundingInputs = SDK.selectReferenceScriptFundingUtxos(
          await lucid.wallet().getUtxos(),
          SDK.referenceScriptPublicationFundingTarget(1),
        );
        const { tx } = await Effect.runPromise(
          SDK.completeReferenceScriptPublicationTxProgram({
            lucid,
            selectedFundingInputs,
            walletAddress,
            referenceScriptsAddress: walletAddress,
            missingTargets: [target],
            authPolicy,
          }),
        );
        await tx.sign.withWallet().complete();
      }).rejects.toThrow(/Max transaction size of 16384 exceeded/u);
    }
  });
});
