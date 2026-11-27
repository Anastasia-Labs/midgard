import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  completeReferenceScriptPublicationTxProgram,
  createReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptAuthUnit,
} from "../src/reference-scripts.js";

const setup = async () => {
  const publisher = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const other = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([publisher, other]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(publisher.seedPhrase);
  const policy = await createReferenceScriptAuthPolicy(
    lucid,
    emulator.now(),
    120_000,
  );
  const target = {
    name: "hub-oracle minting",
    script: scriptFromNative({
      type: "sig",
      keyHash: getAddressDetails(publisher.address).paymentCredential!.hash,
    }),
  };
  return { publisher, other, emulator, lucid, policy, target };
};

describe("reference-script publisher authority", () => {
  it("publishes an authenticated reference script before expiry using the publisher signature", async () => {
    const { publisher, emulator, lucid, policy, target } = await setup();
    const { tx } = await Effect.runPromise(
      completeReferenceScriptPublicationTxProgram({
        lucid,
        selectedFundingInputs: await lucid.wallet().getUtxos(),
        walletAddress: publisher.address,
        referenceScriptsAddress: publisher.address,
        missingTargets: [target],
        authPolicy: policy,
      }),
    );
    const signed = await tx.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
    const unit = referenceScriptAuthUnit(policy.policyId, target.name);
    const published = (await lucid.utxosAt(publisher.address)).filter(
      (utxo) => utxo.assets[unit] === 1n,
    );
    expect(published).toHaveLength(1);
    expect(published[0].scriptRef).toEqual(target.script);
    expect(emulator.now()).toBeLessThan(policy.expiresAtUnixTime);
    expect(
      referenceScriptAuthPolicyDeploymentInfo(policy).postTimelockAudit
        .required,
    ).toBe(false);
  });

  it("rejects another funded wallet without the publisher signature", async () => {
    const { other, lucid, policy, target } = await setup();
    lucid.selectWallet.fromSeed(other.seedPhrase);
    const tx = await lucid
      .newTx()
      .mintAssets({
        [referenceScriptAuthUnit(policy.policyId, target.name)]: 1n,
      })
      .attach.MintingPolicy(policy.mintingScript)
      .validTo(policy.expiresAtUnixTime - 1_000)
      .complete({ localUPLCEval: true });
    const signed = await tx.sign.withWallet().complete();
    await expect(signed.submit()).rejects.toThrow(
      /Missing.*(witness|signature)|Native script|native script/i,
    );
  });

  it("rejects publisher minting with a validity interval beyond the policy expiry", async () => {
    const { lucid, policy, target } = await setup();
    const tx = await lucid
      .newTx()
      .mintAssets({
        [referenceScriptAuthUnit(policy.policyId, target.name)]: 1n,
      })
      .attach.MintingPolicy(policy.mintingScript)
      .validTo(policy.expiresAtUnixTime + 60_000)
      .complete({ localUPLCEval: true });
    const signed = await tx.sign.withWallet().complete();
    await expect(signed.submit()).rejects.toThrow(
      /Native script|native script/i,
    );
  });
});
