import type * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { historySubmissionTransport } from "../src/transactions/event-history-submission.js";

/** Native payments exercise exact-body transport, not history-policy authority. */
const setup = async () => {
  const wallet = generateEmulatorAccount({ lovelace: 100_000_000n });
  const provider = new Emulator([wallet]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  provider.awaitSlot(100);
  const tx = await lucid
    .newTx()
    .pay.ToAddress(wallet.address, { lovelace: 10_000_000n })
    .validFrom(provider.now() - 60_000)
    .validTo(provider.now() + 120_000)
    .complete({ localUPLCEval: true });
  const attempt: SDK.EventHistorySubmissionAttempt = {
    phase: "Publication",
    txHash: tx.toHash(),
    outputIndex: 0,
    transactionCbor: tx.toCBOR(),
  };
  return { wallet, provider, lucid, tx, attempt };
};

describe("history exact-body transport", () => {
  it("resumes a completed unsigned body using a fresh Lucid instance and wallet", async () => {
    const h = await setup();
    const restarted = await Lucid(h.provider, "Custom");
    restarted.selectWallet.fromSeed(h.wallet.seedPhrase);
    const transport = historySubmissionTransport(restarted, h.wallet.address);
    expect(
      await transport.submit(
        restarted.fromTx(h.attempt.transactionCbor),
        h.attempt,
      ),
    ).toEqual({ kind: "Confirmed" });
    expect(await transport.observe(h.attempt)).toEqual({ kind: "Confirmed" });
    const [output] = await restarted.utxosByOutRef([h.attempt]);
    expect(output?.txHash).toBe(h.attempt.txHash);
  });

  it("does not treat absent, pending or failed provider status as permission to rebuild", async () => {
    const h = await setup();
    const transport = historySubmissionTransport(h.lucid, h.wallet.address);
    for (const status of ["not_found", "pending", "failed"] as const) {
      vi.spyOn(h.lucid, "transactionStatus").mockResolvedValueOnce({
        status,
        txHash: h.attempt.txHash,
      });
      expect(await transport.observe(h.attempt)).toEqual({ kind: "Pending" });
    }
    vi.spyOn(h.lucid, "transactionStatus").mockResolvedValueOnce({
      status: "confirmed",
      txHash: h.attempt.txHash,
      confirmation: { txHash: "ab".repeat(32) },
    });
    await expect(transport.observe(h.attempt)).rejects.toThrow(
      "different transaction",
    );
  });

  it("checks the actual signed body before any provider submission", async () => {
    const h = await setup();
    const other = await h.lucid
      .newTx()
      .pay.ToAddress(h.wallet.address, { lovelace: 11_000_000n })
      .complete({ localUPLCEval: true });
    vi.spyOn(h.tx.sign, "withWallet").mockReturnValue(other.sign.withWallet());
    const submit = vi.spyOn(h.provider, "submitTx");
    await expect(
      historySubmissionTransport(h.lucid, h.wallet.address).submit(
        h.tx,
        h.attempt,
      ),
    ).rejects.toThrow("Wallet changed");
    expect(submit).not.toHaveBeenCalled();
  });
});
