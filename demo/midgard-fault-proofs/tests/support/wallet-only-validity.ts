/**
 * The check that a signed transaction carries the wallet-only validity
 * window.
 */
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { walletOnlyValidTo } from "../../src/wallet-only-validity.js";

/**
 * Expects the signed transaction `cbor`, built at the emulator's current
 * time, to carry the wallet-only window: an upper bound at the slot of
 * `walletOnlyValidTo(lucid)` and no lower bound.
 */
export const expectWalletOnlyValidity = (
  lucid: Pick<
    LucidEvolution,
    "currentSlot" | "slotToUnixTime" | "unixTimeToSlot"
  >,
  cbor: string,
): void => {
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  expect(body.ttl()).toBe(
    BigInt(lucid.unixTimeToSlot(walletOnlyValidTo(lucid))),
  );
  expect(body.validity_interval_start()).toBeUndefined();
};

/** Expects the signed transaction `cbor` to carry no validity bound at all. */
export const expectNoValidity = (cbor: string): void => {
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  expect(body.ttl()).toBeUndefined();
  expect(body.validity_interval_start()).toBeUndefined();
};
