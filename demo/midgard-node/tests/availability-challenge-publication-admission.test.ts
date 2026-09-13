import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./helpers/mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const EXPECTED_ROLES = [
  "availability-challenge spending",
  "availability-challenge minting",
  ...["bond", "open", "settle", "close", "timeout"].map(
    (action) => `availability-challenge ${action} withdrawal`,
  ),
];

describe("availability-challenge publication admission", () => {
  it("signs and submits every applied role with a 512-byte reserve, then registers every yield idempotently", async () => {
    const publisher = generateEmulatorAccount({ lovelace: 5_000_000_000n });
    const emulator = new Emulator([publisher], {
      ...MAINNET_PROTOCOL_PARAMETERS,
      maxTxSize: 16_384,
      maxTxExMem: 16_500_000n,
      maxTxExSteps: 10_000_000_000n,
    });
    const lucid = await createMainnetEmulatorLucid(emulator, "Preprod");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const authPolicy = SDK.createReferenceScriptAuthPolicy(
      lucid,
      emulator.now(),
    );
    const nonce = (await lucid.wallet().getUtxos())[0]!;
    const contracts = await loadRealMidgardContractsForTest(nonce, authPolicy);
    const targets = nodeRuntimeReferenceScriptTargets(contracts).filter(
      ({ name }) => name.startsWith("availability-challenge "),
    );
    expect(targets.map(({ name }) => name)).toEqual(EXPECTED_ROLES);
    expect(
      new Set(targets.map(({ script }) => validatorToScriptHash(script))).size,
    ).toBe(6);
    expect(() =>
      SDK.assertReferenceScriptRawBodiesFitL1Envelope(targets),
    ).not.toThrow();
    const walletAddress = await lucid.wallet().address();
    const measured = [];
    for (const target of targets) {
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
      const signed = await tx.sign.withWallet().complete();
      const cbor = signed.toCBOR();
      const signedBytes = cbor.length / 2;
      expect(signedBytes, target.name).toBeLessThanOrEqual(16_384 - 512);
      const transaction = CML.Transaction.from_cbor_hex(cbor);
      expect(transaction.witness_set().vkeywitnesses()?.len()).toBe(1);
      const txHash = await signed.submit();
      await lucid.awaitTx(txHash);
      const unit = SDK.referenceScriptAuthUnit(
        authPolicy.policyId,
        target.name,
      );
      const published = await lucid.utxoByUnit(unit);
      expect(published.txHash).toBe(txHash);
      expect(published.assets[unit]).toBe(1n);
      expect(published.scriptRef).toEqual(target.script);
      measured.push({
        role: target.name,
        rawBytes: target.script.script.length / 2,
        signedBytes,
        scriptHash: validatorToScriptHash(target.script),
      });
    }
    const registrations = await Effect.runPromise(
      ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
        lucid,
        contracts,
      ),
    );
    expect(registrations).toHaveLength(5);
    for (const registration of registrations) {
      expect(registration.txHash).not.toBeNull();
      expect(
        (await lucid.rewardAccountAt(registration.rewardAddress)).registered,
      ).toBe(true);
    }
    const repeated = await Effect.runPromise(
      ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
        lucid,
        contracts,
      ),
    );
    expect(repeated.every(({ txHash }) => txHash === null)).toBe(true);
    console.info(
      "Availability challenge signed publication measurements",
      measured,
    );
  }, 120_000);
});
