import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { CML } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { decodeBlock } from "../src/index.js";
import { encodeBlock, encodeTxBody, type SimTx } from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  fill,
  ORIGIN,
  POLICY,
  storeAdapters,
  TRACKED,
} from "./support/small-chain.js";

/**
 * The decoder accepts every value the ledger accepts in the fields it reads:
 * each case below sits at the edge of the ledger's range for that field and
 * is built as real CBOR.
 */
const WORD64_MAX = 2n ** 64n - 1n;
const INT64_MAX = 2n ** 63n - 1n;
const INT64_MIN = -(2n ** 63n);
const WORD32_MAX = 2 ** 32 - 1;
const WORD16_MAX = 2 ** 16 - 1;
const UNTRACKED_ADDRESS = Buffer.from(`61${"11".repeat(28)}`, "hex");
const REWARD_ACCOUNT = Buffer.concat([Buffer.from([0xe1]), fill(0x44, 28)]);

const payment = (fields: Partial<SimTx>): SimTx => ({
  inputs: [{ txHash: fill(0x07), index: 0 }],
  outputs: [{ address: UNTRACKED_ADDRESS, lovelace: 2_000_000n }],
  nonce: 1,
  ...fields,
});

const blockOf = (...txs: SimTx[]) =>
  encodeBlock({
    height: ORIGIN.height + 1,
    slot: ORIGIN.point.slot + 1,
    prevHash: ORIGIN.point.hash,
    branch: 0,
    txs,
  });

describe("decodeBlock over the ledger's full validity interval range", () => {
  const slots = [
    0n,
    BigInt(Number.MAX_SAFE_INTEGER),
    BigInt(Number.MAX_SAFE_INTEGER) + 1n,
    2n ** 63n,
    WORD64_MAX,
  ];

  it.each(slots)("reads TTL %s exactly, agreeing with CML", (slot) => {
    const sim = payment({ invalidAfter: slot });
    const [tx] = decodeBlock(blockOf(sim).raw).txs;
    expect(tx?.invalidAfter).toBe(slot);
    expect(CML.TransactionBody.from_cbor_bytes(encodeTxBody(sim)).ttl()).toBe(
      slot,
    );
  });

  it.each(slots)(
    "reads validity interval start %s exactly, agreeing with CML",
    (slot) => {
      const sim = payment({ invalidBefore: slot });
      const [tx] = decodeBlock(blockOf(sim).raw).txs;
      expect(tx?.invalidBefore).toBe(slot);
      expect(
        CML.TransactionBody.from_cbor_bytes(
          encodeTxBody(sim),
        ).validity_interval_start(),
      ).toBe(slot);
    },
  );

  it("decodes a block whose untracked payment carries TTL 2^63 next to an ordinary tx", () => {
    const decoded = decodeBlock(
      blockOf(
        payment({ invalidAfter: 2n ** 63n }),
        payment({ invalidAfter: 99_999, nonce: 2 }),
      ).raw,
    );
    expect(decoded.txs.map((tx) => tx.invalidAfter)).toEqual([
      2n ** 63n,
      99_999n,
    ]);
  });
});

/** One tx with every widened or bounded field at the edge of its ledger range. */
const extreme: SimTx = {
  inputs: [{ txHash: fill(0x07), index: WORD16_MAX }],
  collaterals: [{ txHash: fill(0x08), index: WORD16_MAX }],
  referenceInputs: [{ txHash: fill(0x09), index: WORD16_MAX }],
  outputs: [
    {
      address: TRACKED,
      lovelace: WORD64_MAX,
      assets: new Map([
        [POLICY.toString("hex"), new Map([["aa", WORD64_MAX]])],
      ]),
    },
  ],
  collateralReturn: { address: UNTRACKED_ADDRESS, lovelace: WORD64_MAX },
  mint: new Map([
    [
      POLICY.toString("hex"),
      new Map([
        ["aa", INT64_MAX],
        ["bb", INT64_MIN],
      ]),
    ],
  ]),
  withdrawals: [{ rewardAccount: REWARD_ACCOUNT, amount: WORD64_MAX }],
  invalidBefore: WORD64_MAX,
  invalidAfter: WORD64_MAX,
  redeemers: [
    { purpose: "spend", index: WORD32_MAX, data: Buffer.from("d87980", "hex") },
  ],
  nonce: 3,
};

describe("decodeBlock on a ledger-extreme corpus transaction", () => {
  it("reads every field at the edge of its ledger range", () => {
    const [tx] = decodeBlock(blockOf(extreme).raw).txs;
    expect(tx?.inputs).toEqual([{ txHash: fill(0x07), index: WORD16_MAX }]);
    expect(tx?.collaterals).toEqual([
      { txHash: fill(0x08), index: WORD16_MAX },
    ]);
    expect(tx?.referenceInputs).toEqual([
      { txHash: fill(0x09), index: WORD16_MAX },
    ]);
    expect(tx?.outputs[0]?.lovelace).toBe(WORD64_MAX);
    expect(tx?.outputs[0]?.assets.get(POLICY.toString("hex"))?.get("aa")).toBe(
      WORD64_MAX,
    );
    expect(tx?.collateralReturn?.lovelace).toBe(WORD64_MAX);
    expect(tx?.mint.get(POLICY.toString("hex"))?.get("aa")).toBe(INT64_MAX);
    expect(tx?.mint.get(POLICY.toString("hex"))?.get("bb")).toBe(INT64_MIN);
    expect(tx?.withdrawals.map((w) => w.amount)).toEqual([WORD64_MAX]);
    expect(tx?.invalidBefore).toBe(WORD64_MAX);
    expect(tx?.invalidAfter).toBe(WORD64_MAX);
    expect(tx?.redeemers.map((r) => r.index)).toEqual([WORD32_MAX]);
  });

  it("is a body CML parses to the same values", () => {
    const body = CML.TransactionBody.from_cbor_bytes(encodeTxBody(extreme));
    expect(body.inputs().get(0).index()).toBe(BigInt(WORD16_MAX));
    expect(body.outputs().get(0).amount().coin()).toBe(WORD64_MAX);
    expect(
      body
        .withdrawals()
        ?.get(
          CML.RewardAddress.from_address(
            CML.Address.from_raw_bytes(REWARD_ACCOUNT),
          )!,
        ),
    ).toBe(WORD64_MAX);
    expect(body.ttl()).toBe(WORD64_MAX);
    expect(body.validity_interval_start()).toBe(WORD64_MAX);
  });
});

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-ranges-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

describe.each(storeAdapters(databases, scratch))(
  "fact store keeps ledger-extreme values exactly ($name)",
  (adapter) => {
    it("stores and reads back Word64 validity bounds and coins", async () => {
      const { store } = await adapter.open(2);
      try {
        expect(await store.start()).toMatchObject({ kind: "ready" });
        expect(await store.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
        });
        const encoded = blockOf(extreme);
        expect(await store.applyBlock(decodeBlock(encoded.raw))).toMatchObject({
          kind: "applied",
        });
        const stored = await store.txByHash(encoded.txHashes[0] as Buffer);
        expect(stored?.invalidBefore).toBe(WORD64_MAX);
        expect(stored?.invalidAfter).toBe(WORD64_MAX);
        expect(stored?.withdrawals.map((w) => w.amount)).toEqual([WORD64_MAX]);
        expect(stored?.mint.get(POLICY.toString("hex"))?.get("bb")).toBe(
          INT64_MIN,
        );
      } finally {
        await store.close();
      }
    });
  },
);
