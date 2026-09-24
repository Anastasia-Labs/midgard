import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  assetsToValue,
  CML,
  credentialToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { verifyEventHistoryReferenceBody } from "../src/l1-event-history-reference.js";
import type { HistoryChainTransaction } from "../src/l1-event-history-transaction.js";

// Pure body-preimage checks. The observing transaction's source admission is a
// model precondition here; applied references are covered by the replay fixture.
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "ab".repeat(28),
});
const orderedDatum = "a202a2186400011b00200000000000010100";
const unit = "cd".repeat(28) + "ef";
const bodyFixture = (collateralReturn = false) => {
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    assetsToValue({ lovelace: 3_000_000n, [unit]: 9007199254740993n }),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(orderedDatum)),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    100n,
  );
  if (collateralReturn) body.set_collateral_return(output);
  const creatingBodyCbor = body.to_cbor_hex();
  const ref = {
    txHash: computeHash32(Buffer.from(creatingBodyCbor, "hex")).toString("hex"),
    outputIndex: collateralReturn ? 1 : 0,
  };
  const transaction: HistoryChainTransaction = {
    txHash: "ff".repeat(32),
    spends: "inputs",
    inputs: [],
    references: [ref],
    collaterals: [],
    outputs: [],
    mint: {},
    withdrawals: [],
    redeemers: [],
  };
  return { transaction, ref, creatingBodyCbor, maximumBodyBytes: 16384 };
};

describe("history referenced creating-body verification", () => {
  it.each([false, true])(
    "authenticates exact output data and large Value; collateral return=%s",
    (collateral) => {
      const input = bodyFixture(collateral);
      const output = verifyEventHistoryReferenceBody(input);
      expect(output).toEqual({
        ...input.ref,
        address,
        assets: { lovelace: 3_000_000n, [unit]: 9007199254740993n },
        datum: orderedDatum,
        hasReferenceScript: false,
      });
      expect(Object.isFrozen(output.assets)).toBe(true);
      expect(Object.isFrozen(output)).toBe(true);
    },
  );

  it("hashes a preserved noncanonical body encoding instead of canonicalizing it", () => {
    const input = bodyFixture();
    expect(input.creatingBodyCbor.slice(0, 2)).toBe("a3");
    const creatingBodyCbor = "bf" + input.creatingBodyCbor.slice(2) + "ff";
    const ref = {
      ...input.ref,
      txHash: computeHash32(Buffer.from(creatingBodyCbor, "hex")).toString(
        "hex",
      ),
    };
    expect(ref.txHash).not.toBe(input.ref.txHash);
    expect(
      verifyEventHistoryReferenceBody({
        ...input,
        creatingBodyCbor,
        ref,
        transaction: { ...input.transaction, references: [ref] },
      }).datum,
    ).toBe(orderedDatum);
    expect(() =>
      verifyEventHistoryReferenceBody({ ...input, creatingBodyCbor }),
    ).toThrow(/referenced transaction hash/);
  });

  it.each([
    "wrong hash",
    "not referenced",
    "failed observer",
    "self reference",
    "missing output",
    "negative output",
    "unsafe output",
    "over bound",
    "invalid bound",
    "nonhex",
  ])("refuses %s", (fault) => {
    const input = bodyFixture();
    if (fault === "wrong hash")
      input.creatingBodyCbor = bodyFixture(true).creatingBodyCbor;
    if (fault === "not referenced")
      input.transaction = { ...input.transaction, references: [] };
    if (fault === "failed observer")
      input.transaction = { ...input.transaction, spends: "collaterals" };
    if (fault === "self reference")
      input.transaction = { ...input.transaction, txHash: input.ref.txHash };
    if (fault === "missing output") input.ref.outputIndex = 2;
    if (fault === "negative output") input.ref.outputIndex = -1;
    if (fault === "unsafe output")
      input.ref.outputIndex = Number.MAX_SAFE_INTEGER + 1;
    if (fault === "over bound")
      input.maximumBodyBytes = input.creatingBodyCbor.length / 2 - 1;
    if (fault === "invalid bound") input.maximumBodyBytes = 0;
    if (fault === "nonhex") input.creatingBodyCbor = "invalid";
    expect(() => verifyEventHistoryReferenceBody(input)).toThrow();
  });

  it("rejects trailing body bytes even if their supplied hash matches", () => {
    const input = bodyFixture();
    const creatingBodyCbor = input.creatingBodyCbor + "00";
    const ref = {
      ...input.ref,
      txHash: computeHash32(Buffer.from(creatingBodyCbor, "hex")).toString(
        "hex",
      ),
    };
    expect(() =>
      verifyEventHistoryReferenceBody({
        ...input,
        creatingBodyCbor,
        ref,
        transaction: { ...input.transaction, references: [ref] },
      }),
    ).toThrow();
  });
});
