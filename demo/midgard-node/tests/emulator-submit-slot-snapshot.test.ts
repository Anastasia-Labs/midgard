import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { deriveEmulatorSubmitSlotSnapshot } from "./helpers/emulator-submit-slot-snapshot.js";

describe("emulator submit-slot snapshot", () => {
  it("keeps an empty wallet override authoritative until explicitly cleared", async () => {
    const account = generateEmulatorAccount({ lovelace: 50_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);

    await expect(lucid.wallet().getUtxos()).resolves.toHaveLength(1);
    lucid.overrideUTxOs([]);
    await expect(lucid.wallet().getUtxos()).resolves.toEqual([]);
    lucid.clearUTxOOverride();
    await expect(lucid.wallet().getUtxos()).resolves.toHaveLength(1);
  });

  it("advances observed time coherently with slots after Lucid creation", async () => {
    const account = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([account]);
    await Lucid(emulator, "Custom");
    const baseSlot = emulator.slot;
    const baseTimeMs = emulator.now();

    emulator.awaitSlot(16 * 60);
    const snapshot = deriveEmulatorSubmitSlotSnapshot({
      currentSlot: emulator.slot,
      observedAtMs: emulator.now(),
    });

    expect(emulator.slot).toBe(baseSlot + 16 * 60);
    expect(emulator.now()).toBe(baseTimeMs + 16 * 60 * 1_000);
    expect(snapshot.currentSlot).toBe(baseSlot + 16 * 60);
    expect(snapshot.observedAtMs).toBe(baseTimeMs + 16 * 60 * 1_000);
  });

  it("maps builder validTo to the live absolute slot after a late Lucid creation", async () => {
    const account = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([account]);
    emulator.awaitSlot(16 * 60);

    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);

    const validToMs = emulator.now() + 8 * 60 * 1_000;
    const address = await lucid.wallet().address();
    const built = await lucid
      .newTx()
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .validTo(validToMs)
      .complete();
    const ttl = CML.Transaction.from_cbor_hex(built.toCBOR()).body().ttl();

    expect(Number(ttl)).toBe(emulator.slot + 8 * 60);
  });
});
