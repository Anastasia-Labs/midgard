import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";
import { describe, expect, it } from "vitest";

import {
  decodeHistoryChainTransaction,
  historyZeroWithdrawal,
} from "../src/l1-event-history-transaction.js";

const policy = "bb".repeat(28);
const reward = (hash: string, network = 0, key = false) =>
  CML.RewardAddress.new(
    network,
    key
      ? CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex(hash))
      : CML.Credential.new_script(CML.ScriptHash.from_hex(hash)),
  )
    .to_address()
    .to_bech32();
const account = reward(policy);
const otherAccount = reward("aa".repeat(28), 0, true);
const txHash = "ab".repeat(32);
const ref = (index: number) => ({
  transaction: { id: "cd".repeat(32) },
  index,
});
const observer = Data.to(
  { Initialize: { nonce_input_index: 1n, root_output_index: 0n } },
  SDK.EventHistoryObserve,
);
const output = {
  address: CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(policy)),
  )
    .to_address()
    .to_bech32(),
  value: { ada: { lovelace: 45000000000000001n }, [policy]: { "": 1n } },
  datum: Data.to(0n),
};
const raw = () => ({
  id: txHash,
  spends: "inputs",
  inputs: [ref(10), ref(2)],
  references: [ref(12), ref(11)],
  collaterals: [ref(99)],
  outputs: [output, { ...output, datum: undefined }],
  mint: { [policy]: { "": -1n, aa: 1n } },
  withdrawals: {
    [account]: { ada: { lovelace: 0n } },
    [otherAccount]: { ada: { lovelace: 50n } },
  },
  redeemers: [
    { validator: { purpose: "withdraw", index: 0 }, redeemer: observer },
    { validator: { purpose: "mint", index: 0 }, redeemer: Data.to(123n) },
    { validator: { purpose: "spend", index: 1 }, redeemer: Data.to(1n) },
  ],
  validityInterval: { invalidBefore: 100, invalidAfter: 200 },
});

describe("node history transaction observations", () => {
  it("preserves exact values, burns, body output order and numeric input indexes across lossless wire decoding", () => {
    const wire = JSONBig({ useNativeBigInt: true });
    const tx = decodeHistoryChainTransaction(wire.parse(wire.stringify(raw())));
    expect(tx.inputs.map((entry) => entry.outputIndex)).toEqual([2, 10]);
    expect(tx.references.map((entry) => entry.outputIndex)).toEqual([11, 12]);
    expect(
      tx.outputs.map((entry) => `${entry.txHash}#${entry.outputIndex}`),
    ).toEqual([`${txHash}#0`, `${txHash}#1`]);
    expect(tx.outputs[0]!.assets.lovelace).toBe(45000000000000001n);
    expect(tx.outputs[0]!.datum).toBe(Data.to(0n));
    expect(tx.outputs[1]!.datum).toBeUndefined();
    expect(tx.mint).toEqual({ [policy]: -1n, [policy + "aa"]: 1n });
    expect(tx.invalidBefore).toBe(100);
    expect(tx.invalidAfter).toBe(200);
    expect(Object.isFrozen(tx.outputs[0]!.assets)).toBe(true);
  });

  it("uses canonical reward-account ordering and the withdraw redeemer, never arbitrary mint data", () => {
    const tx = decodeHistoryChainTransaction(raw());
    const selected = historyZeroWithdrawal(tx, policy, 0)!;
    expect(selected.withdrawalIndex).toBe(0);
    expect(Data.from(selected.redeemer.cbor, SDK.EventHistoryObserve)).toEqual({
      Initialize: { nonce_input_index: 1n, root_output_index: 0n },
    });
    expect(tx.withdrawals[1]!.credential.kind).toBe("key");
    expect(historyZeroWithdrawal(tx, policy, 1)).toBeNull();
    const reordered = raw();
    reordered.withdrawals = Object.fromEntries(
      Object.entries(reordered.withdrawals).reverse(),
    );
    reordered.redeemers.reverse();
    expect(decodeHistoryChainTransaction(reordered)).toEqual(tx);
    expect(
      historyZeroWithdrawal(
        decodeHistoryChainTransaction({
          ...raw(),
          redeemers: raw().redeemers.filter(
            (entry) => entry.validator.purpose !== "withdraw",
          ),
        }),
        policy,
        0,
      ),
    ).toBeNull();
  });

  it("does not treat a collateral-only transaction's observers as successful ledger effects", () => {
    const tx = decodeHistoryChainTransaction({
      ...raw(),
      spends: "collaterals",
      collateralReturn: output,
    });
    expect(tx.spends).toBe("collaterals");
    expect(tx.collaterals).toEqual([
      { txHash: "cd".repeat(32), outputIndex: 99 },
    ]);
    expect(tx.collateralReturn).toMatchObject({ txHash, outputIndex: 2 });
    expect(historyZeroWithdrawal(tx, policy, 0)).toBeNull();
  });

  it.each(["inputs", "collaterals"])(
    "decodes Ogmios v7 omitted empty outputs for %s disposition",
    (spends) => {
      const wire = JSONBig({ useNativeBigInt: true });
      const value = wire.parse(
        wire.stringify({
          ...raw(),
          spends,
          outputs: undefined,
          validityInterval: undefined,
          collateralReturn: output,
        }),
      );
      expect(value).not.toHaveProperty("outputs");
      const tx = decodeHistoryChainTransaction(value);
      expect(tx.outputs).toEqual([]);
      expect(Object.isFrozen(tx.outputs)).toBe(true);
      expect(tx.collateralReturn).toMatchObject({ txHash, outputIndex: 0 });
      expect(tx.invalidBefore).toBeUndefined();
      expect(tx.invalidAfter).toBeUndefined();
      expect(tx).toEqual(
        decodeHistoryChainTransaction({ ...value, outputs: [] }),
      );
      if (spends === "collaterals")
        expect(historyZeroWithdrawal(tx, policy, 0)).toBeNull();
    },
  );

  it("orders network before credential kind and hash rather than raw address bytes", () => {
    const accounts = [
      reward("00".repeat(28), 1, true),
      otherAccount,
      account,
      reward("00".repeat(28), 1),
      reward("01".repeat(28)),
    ];
    const tx = decodeHistoryChainTransaction({
      ...raw(),
      withdrawals: Object.fromEntries(
        accounts.map((address) => [address, { ada: { lovelace: 0n } }]),
      ),
      redeemers: [
        { validator: { purpose: "withdraw", index: 1 }, redeemer: observer },
      ],
    });
    expect(
      tx.withdrawals.map((entry) => [
        entry.networkId,
        entry.credential.kind,
        entry.credential.hash,
      ]),
    ).toEqual([
      [0, "script", "01".repeat(28)],
      [0, "script", policy],
      [0, "key", "aa".repeat(28)],
      [1, "script", "00".repeat(28)],
      [1, "key", "00".repeat(28)],
    ]);
    expect(historyZeroWithdrawal(tx, policy, 0)?.withdrawalIndex).toBe(1);
  });

  it("requires an exact script account with zero withdrawal amount", () => {
    const nonzero = raw();
    nonzero.withdrawals[account] = { ada: { lovelace: 1n } };
    expect(
      historyZeroWithdrawal(decodeHistoryChainTransaction(nonzero), policy, 0),
    ).toBeNull();
    const keyAccount = reward(policy, 0, true);
    const keyed = {
      ...raw(),
      withdrawals: { [keyAccount]: { ada: { lovelace: 0n } } },
      redeemers: [
        { validator: { purpose: "withdraw", index: 0 }, redeemer: observer },
      ],
    };
    expect(
      historyZeroWithdrawal(decodeHistoryChainTransaction(keyed), policy, 0),
    ).toBeNull();
  });

  it.each([
    ["missing validity disposition", { spends: undefined }],
    ["unknown disposition", { spends: "valid" }],
    ["missing ordinary inputs", { inputs: undefined }],
    ["null references", { references: null }],
    ["null mint", { mint: null }],
    ["null withdrawals", { withdrawals: null }],
    ["null redeemers", { redeemers: null }],
    ["duplicate ordinary input", { inputs: [ref(1), ref(1)] }],
    ["duplicate reference input", { references: [ref(1), ref(1)] }],
    ["unsafe index", { inputs: [ref(9007199254740992)] }],
    ["failed without collateral", { spends: "collaterals", collaterals: [] }],
    ["null outputs", { outputs: null }],
    ["malformed mint policy", { mint: { bad: { "": 1 } } }],
    [
      "oversize mint asset name",
      { mint: { [policy]: { ["aa".repeat(33)]: 1 } } },
    ],
    ["rounded mint quantity", { mint: { [policy]: { "": 9007199254740992 } } }],
    ["string mint quantity", { mint: { [policy]: { "": "1" } } }],
    [
      "nonreward withdrawal",
      { withdrawals: { [output.address]: { ada: { lovelace: 0 } } } },
    ],
    [
      "negative withdrawal",
      { withdrawals: { [account]: { ada: { lovelace: -1 } } } },
    ],
    [
      "wrong normalized purpose",
      {
        redeemers: [
          {
            validator: { purpose: "withdrawal", index: 1 },
            redeemer: observer,
          },
        ],
      },
    ],
    [
      "duplicate redeemer pointer",
      { redeemers: [raw().redeemers[0], raw().redeemers[0]] },
    ],
    [
      "out-of-range withdraw pointer",
      {
        redeemers: [
          { validator: { purpose: "withdraw", index: 2 }, redeemer: observer },
        ],
      },
    ],
    [
      "out-of-range mint pointer",
      {
        redeemers: [
          { validator: { purpose: "mint", index: 1 }, redeemer: observer },
        ],
      },
    ],
    [
      "malformed redeemer CBOR",
      {
        redeemers: [
          { validator: { purpose: "withdraw", index: 1 }, redeemer: "ff" },
        ],
      },
    ],
    [
      "rounded interval",
      { validityInterval: { invalidAfter: 9007199254740992 } },
    ],
    [
      "ambiguous datum",
      { outputs: [{ ...output, datumHash: "aa".repeat(32) }] },
    ],
  ])(
    "refuses %s instead of normalizing away missing evidence",
    (_label, fields) => {
      expect(() =>
        decodeHistoryChainTransaction({ ...raw(), ...fields }),
      ).toThrow();
    },
  );
});
