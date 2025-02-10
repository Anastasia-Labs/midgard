import { describe, it, expect, beforeAll, afterAll } from "vitest";
import sqlite3 from "sqlite3";
import { initializeDb } from "../src/database.js";
import { CML, fromHex, UTxO } from "@lucid-evolution/lucid";
import * as blocks from "../src/database/blocks.js";
import * as mempool from "../src/database/mempool.js";
import * as immutable from "../src/database/immutable.js";
import * as confirmedLedger from "../src/database/confirmedLedger.js";
import * as latestLedger from "../src/database/latestLedger.js";
import { logAbort } from "../src/utils.js";
import { Option } from "effect";

describe("database", () => {
  let db: sqlite3.Database;
  const dbFilePath = ":memory:";
  const lucid = new MockLucid();

  beforeAll(async () => {
    db = await initializeDb(dbFilePath);
  });

  afterAll(async () => {
    db.close();
  });

  const tx1 =
    "84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000019582581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d603b493979352930f3eccbd47ae0e12ef6dcea89326211cd80f87cf85a1b00000001dcd95d400200a100d901028182582015c6708ee2da48b2c46c5ab3e001ff636fa0b0e2a48b6676dffc8e36fe02c5985840599c8a11e9888ab60c10f78c708b55f6b542c6d70fa57775b26efe519dc9459604f2e1633601e2b9e448181e694278f3a89d8fea462dcfd772a6a9248e1f730df5f6";
  const tx1Hash = lucid.fromTx(tx1).toHash();

  const tx2 =
    "84a300d90102818258207351d824f3565c936f87d67c7352e0d34f13a51e6427bfad2a86e49bbae8a2dc14019582581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60f2d5d0b7f8198330ba9410ffecdf45dcc67e774849bfbe8115a8cfcf1b0000000165a6d6800200a100d9010281825820f80e638fcfe3a3d60a785202efa9ff231b804db638896eb4a63bf6e0acceda405840567525cdfd1ee65e702c28901adbc16915a46f140dd91669c160b9b9095691205a26f7f95411ece1f2a9cd19901a6525a449256d4232eadf061db137967f0a09f5f6";
  const tx2Hash = lucid.fromTx(tx2).toHash();

  const block1Hash = "aaaaaaaaaaaaaaaaaa";
  const block2Hash = "bbbbbbbbbbbbbbbbbb";

  it("should store tx hashes in blocks db", async () => {
    await blocks.insert(db, block1Hash, [tx1Hash]);
    const result1 = await blocks.retrieve(db);
    expect(result1).toStrictEqual([[block1Hash, tx1Hash]]);

    await blocks.insert(db, block1Hash, [tx2Hash]);
    const result2 = await blocks.retrieve(db);
    expect(result2).toStrictEqual([
      [block1Hash, tx1Hash],
      [block1Hash, tx2Hash],
    ]);
  });

  it("retrieves tx hashes by block hash", async () => {
    const result1 = await blocks.retrieveTxHashesByBlockHash(db, block1Hash);
    expect(result1).toEqual([tx1Hash, tx2Hash]);

    const result2 = await blocks.retrieveTxHashesByBlockHash(db, block2Hash);
    expect(result2).toEqual([]);
  });

  it("retrieves block hash by tx hash", async () => {
    const result1 = await blocks.retrieveBlockHashByTxHash(db, tx1Hash);
    expect(result1).toEqual(Option.some(block1Hash));

    const result2 = await blocks.retrieveBlockHashByTxHash(db, block2Hash);
    expect(result2).toEqual(Option.none());
  });

  it("clears given block in the blocks db", async () => {
    const nonExistintBlockHash = "1234";
    await blocks.clearBlock(db, nonExistintBlockHash);
    const result1 = await blocks.retrieve(db);
    expect(result1.map((o) => Object.values(o))).toStrictEqual([
      [block1Hash, tx1Hash],
      [block1Hash, tx2Hash],
    ]);
    const block3Hash = "cccccccccccccccc";
    await blocks.insert(db, block3Hash, ["11", "22"]);
    await blocks.clearBlock(db, block3Hash);
    const result2 = await blocks.retrieve(db);
    expect(result2.map((o) => Object.values(o))).toStrictEqual([
      [block1Hash, tx1Hash],
      [block1Hash, tx2Hash],
    ]);
  });

  it("clears blocks db", async () => {
    await blocks.clear(db);
    const result = await blocks.retrieve(db);
    expect(result).toStrictEqual([]);
  });

  it("should store transactions in the mempool db", async () => {
    await blocks.insert(db, block1Hash, [tx1Hash]);
    await blocks.insert(db, block2Hash, [tx2Hash]);
    await mempool.insert(db, tx1Hash, tx1);
    const result1 = await mempool.retrieve(db);
    expect(result1.map((o) => Object.values(o))).toStrictEqual([
      [tx1Hash, tx1],
    ]);

    await mempool.insert(db, tx2Hash, tx2);
    const result2 = await mempool.retrieve(db);
    expect(result2.map((o) => Object.values(o))).toStrictEqual([
      [tx1Hash, tx1],
      [tx2Hash, tx2],
    ]);
  });

  it("clears the mempool db", async () => {
    const initialRows = await mempool.retrieve(db);
    expect(initialRows.length).toBe(2);
    await mempool.clear(db);
    const result = await mempool.retrieve(db);
    expect(result.length).toBe(0);
  });

  it("should store transactions in the immutable db", async () => {
    await immutable.insert(db, tx1Hash, tx1);
    const result1 = await immutable.retrieve(db);
    expect(result1).toStrictEqual([[tx1Hash, tx1]]);

    await immutable.insert(db, tx2Hash, tx2);
    const result2 = await immutable.retrieve(db);
    expect(result2.map((o) => Object.values(o))).toStrictEqual([
      [tx1Hash, tx1],
      [tx2Hash, tx2],
    ]);
  });

  it("retrieves tx by hash in the immutable db", async () => {
    const nonExistentTxHash = "1234";
    const result1 = await immutable.retrieveTxCborByHash(db, nonExistentTxHash);
    expect(result1).toEqual(Option.none());

    const result2 = await immutable.retrieveTxCborByHash(db, tx1Hash);
    expect(result2).toEqual(Option.some(tx1));
  });

  it("clears the immutable db", async () => {
    const initialRows = await immutable.retrieve(db);
    expect(initialRows.length).toBe(2);
    await immutable.clear(db);
    const result = await immutable.retrieve(db);
    expect(result.length).toBe(0);
  });

  const address =
    "addr1q8gg2r3vf9zggn48g7m8vx62rwf6warcs4k7ej8mdzmqmesj30jz7psduyk6n4n2qrud2xlv9fgj53n6ds3t8cs4fvzs05yzmz";
  const utxo1: UTxO = {
    txHash: tx1Hash,
    outputIndex: 0,
    address: address,
    assets: {
      c5334d60505f62b715b098cb5f9a391416a6ed2064c7d813e03a11c1e2fb72ac0da4e312c4a0a27b73a59eba40ee6848131f82fc62be628ab72e490e:
        BigInt(12),
      lovelace: BigInt("9223372036854779904"),
    },
    datum:
      "e100c1a248cb3e9eb91d1534b176a410312a283100345de6d7f3b7b55ea7b067b4b46a43dca4f674b0682b06ed9f",
    datumHash:
      "9eead0de42833bbd51866cbafe5d29b8448fa1b9e7430a7af7a7f0e7e9913a07",
    scriptRef: null,
  };
  const utxo2: UTxO = {
    txHash: tx2Hash,
    outputIndex: 0,
    address: address,
    assets: { lovelace: BigInt(33) },
    datum: null,
    datumHash: null,
    scriptRef: {
      type: "PlutusV3",
      script:
        "6e461fe947e14c4a53b905d0aa92f08bff98fb94129f6ed26877fcf1c8a4495192c0b379fb06aa3b",
    },
  };
  it("should store utxos in the confirmed ledger db", async () => {
    await confirmedLedger.insert(db, [utxo1]);
    const result1 = await confirmedLedger.retrieve(db);
    expect(result1).toStrictEqual([utxo1]);

    await confirmedLedger.insert(db, [utxo2]);
    const result2 = await confirmedLedger.retrieve(db);
    expect(result2).toStrictEqual([utxo1, utxo2]);
  });

  it("clears given tx in the confirmed ledger db", async () => {
    const nonExistentTxHash = "1234";
    await confirmedLedger.clearTx(db, nonExistentTxHash);
    const result1 = await confirmedLedger.retrieve(db);
    expect(result1).toStrictEqual([utxo1, utxo2]);

    await confirmedLedger.clearTx(db, tx2Hash);
    const result2 = await confirmedLedger.retrieve(db);
    expect(result2).toStrictEqual([utxo1]);
  });

  it("clears the confirmed ledger db", async () => {
    const initialRows = await confirmedLedger.retrieve(db);
    expect(initialRows.length).toBe(1);
    await confirmedLedger.clear(db);
    const result = await confirmedLedger.retrieve(db);
    expect(result.length).toBe(0);
  });

  it("should store utxos in the latest ledger db", async () => {
    await latestLedger.insert(db, [utxo1]);
    const result1 = await latestLedger.retrieve(db);
    expect(result1).toStrictEqual([utxo1]);

    await latestLedger.insert(db, [utxo2]);
    const result2 = await latestLedger.retrieve(db);
    expect(result2).toStrictEqual([utxo1, utxo2]);
  });

  it("retrieves utxos by address in the latest ledger db", async () => {
    const result1 = await latestLedger.retrieveUtxosOnAddress(
      db,
      "non-existent address"
    );
    expect(result1).toEqual([]);

    const result2 = await latestLedger.retrieveUtxosOnAddress(db, address);
    expect(result2).toEqual([utxo1, utxo2]);
  });

  it("clears given tx in the latest ledger db", async () => {
    const nonExistentTxHash = "1234";
    await latestLedger.clearTx(db, nonExistentTxHash);
    const result1 = await latestLedger.retrieve(db);
    expect(result1).toStrictEqual([utxo1, utxo2]);

    await latestLedger.clearTx(db, tx2Hash);
    const result2 = await latestLedger.retrieve(db);
    expect(result2).toStrictEqual([utxo1]);
  });

  it("clears the latest ledger db", async () => {
    const initialRows = await latestLedger.retrieve(db);
    expect(initialRows.length).toBe(1);
    await latestLedger.clear(db);
    const result = await latestLedger.retrieve(db);
    expect(result.length).toBe(0);
  });
});

class MockLucid {
  fromTx(tx: string) {
    const tx_body = CML.Transaction.from_cbor_hex(tx).body();
    return {
      toHash: () => CML.hash_transaction(tx_body).to_hex(),
    };
  }
}
