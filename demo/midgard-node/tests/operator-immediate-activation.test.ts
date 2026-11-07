import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canActivateRegisteredOperatorImmediately } from "../src/transactions/register-active-operator/activation.js";

const contracts = {
  policyId: "ab".repeat(28),
  spendingScriptAddress: "active-script-address",
};
const unit = toUnit(contracts.policyId, SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME);
const root: Pick<SDK.NodeWithDatum, "utxo" | "datum"> = {
  utxo: {
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address: contracts.spendingScriptAddress,
    assets: { [unit]: 1n, lovelace: 2_000_000n },
  },
  datum: { key: "Empty", next: "Empty", data: Data.void() },
};
const registered: SDK.LinkedListNodeView = {
  key: { Key: { key: "aa" } },
  next: "Empty",
  data: Data.void(),
};

describe("immediate operator activation eligibility", () => {
  it("requires the exact authenticated empty active root and an earliest registered node", () => {
    expect(
      canActivateRegisteredOperatorImmediately(root, registered, contracts),
    ).toBe(true);
    expect(
      canActivateRegisteredOperatorImmediately(
        { ...root, utxo: { ...root.utxo, address: "other" } },
        registered,
        contracts,
      ),
    ).toBe(false);
    for (const assets of [{ lovelace: 2_000_000n }, { [unit]: 2n }]) {
      expect(
        canActivateRegisteredOperatorImmediately(
          { ...root, utxo: { ...root.utxo, assets } },
          registered,
          contracts,
        ),
      ).toBe(false);
    }
  });
  it("preserves maturity for an occupied active set, an active element or a later registration", () => {
    expect(
      canActivateRegisteredOperatorImmediately(
        { ...root, datum: { ...root.datum, next: registered.key } },
        registered,
        contracts,
      ),
    ).toBe(false);
    expect(
      canActivateRegisteredOperatorImmediately(
        { ...root, datum: { ...root.datum, key: registered.key } },
        registered,
        contracts,
      ),
    ).toBe(false);
    expect(
      canActivateRegisteredOperatorImmediately(
        root,
        { ...registered, next: registered.key },
        contracts,
      ),
    ).toBe(false);
    expect(
      canActivateRegisteredOperatorImmediately(
        root,
        { ...registered, key: "Empty" },
        contracts,
      ),
    ).toBe(false);
  });
});
