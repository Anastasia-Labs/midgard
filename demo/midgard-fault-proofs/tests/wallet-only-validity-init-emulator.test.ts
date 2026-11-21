/**
 * A dispute init spends only wallet coins, so it carries the wallet-only
 * validity window: the ledger refuses one held past the window and accepts
 * one submitted inside it.
 */
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildInit } from "../src/submit-init.submit-init.js";
import { WALLET_ONLY_VALIDITY_MS } from "../src/wallet-only-validity.js";
import { network } from "./support/emulator/blueprints.js";
import { stageDoubleSpendStep04 } from "./support/submit-init/double-spend-staging.js";
import { expectWalletOnlyValidity } from "./support/wallet-only-validity.js";

describe("dispute init validity window", () => {
  it("is refused once its window has passed and accepted inside it", async () => {
    const staged = await stageDoubleSpendStep04({
      honestBlock: true,
      seeds: { prover: generateSeedPhrase() },
    });
    const { emulator, proverLucid: lucid, proverSigner: signer } = staged;
    const init = () =>
      buildInit({
        lucid,
        blueprint: staged.realBlueprint,
        deploymentInfo: staged.deploymentInfo,
        network,
        signer,
        fraudulentBlockOutRef: staged.fraudulentBlockOutRef,
        witnessReferenceScripts: staged.witnessReferenceScripts,
      });

    const stale = await init();
    expectWalletOnlyValidity(lucid, stale.transaction.signed.toCBOR());
    emulator.awaitSlot(WALLET_ONLY_VALIDITY_MS / 1000 + 1);
    await expect(stale.transaction.signed.submit()).rejects.toThrow(
      /Upper bound \(\d+\) not in slot range/,
    );

    const fresh = await init();
    expectWalletOnlyValidity(lucid, fresh.transaction.signed.toCBOR());
    emulator.awaitSlot(60);
    const txHash = await fresh.transaction.signed.submit();
    expect(await lucid.awaitTx(txHash)).toBe(true);
    expect(
      await lucid.utxosByOutRef([
        { txHash, outputIndex: Number(fresh.firstStepOutputIndex) },
      ]),
    ).toHaveLength(1);
  }, 600_000);
});
