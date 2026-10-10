/**
 * The node-ledger provider (option E's NodeLedger adapter) over an
 * in-memory node: no store, no follower. Reads of one build step share one
 * acquired point; confirmation is the tx's outputs in the ledger.
 */
import {
  type ChainPoint,
  encodeCbor,
  type L1NodeTransport,
  type LedgerQuery,
  type LedgerStateSession,
  TransportRequestError,
} from "@al-ft/l1-node-transport";
import {
  credentialToAddress,
  generateSeedPhrase,
  Lucid,
  SLOT_CONFIG_NETWORK,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { blake2b256 } from "../../src/codec.js";
import {
  L1AwaitTxTimeoutError,
  L1LedgerScopeError,
  L1TxStatusUnknownError,
  LedgerProvider,
} from "../../src/provider/index.js";
import { built, type BuiltOutput, DEVNET_LEDGER } from "../support/provider.js";

const fill = (byte: number, length = 32): Buffer => Buffer.alloc(length, byte);
const KEY = "aa".repeat(28);
const keyAddress = credentialToAddress("Custom", { type: "Key", hash: KEY });
const otherAddress = credentialToAddress("Custom", {
  type: "Key",
  hash: "ab".repeat(28),
});

const head = (major: number, length: number): Buffer => {
  if (length < 24) return Buffer.from([(major << 5) | length]);
  if (length < 0x100) return Buffer.from([(major << 5) | 24, length]);
  const bytes = Buffer.alloc(3);
  bytes[0] = (major << 5) | 25;
  bytes.writeUInt16BE(length, 1);
  return bytes;
};

/** `{[txHash, index] => output}`, as local state query answers it. */
const utxoAnswer = (entries: readonly BuiltOutput[]): Buffer =>
  Buffer.concat([
    head(5, entries.length),
    ...entries.flatMap((entry) => [
      head(4, 2),
      head(2, 32),
      entry.outRef.txHash,
      head(0, entry.outRef.index),
      entry.bytes,
    ]),
  ]);

type Snapshot = {
  readonly slot: number;
  readonly hash: string;
  readonly blockNo: number;
  readonly utxos: readonly BuiltOutput[];
};

/** An in-memory node: a chain of ledger states, a mempool and a submit log. */
class StubNode {
  readonly chain: Snapshot[] = [];
  /** Points older than the volatile window: acquiring one is `too old`. */
  readonly immutable = new Set<string>();
  readonly mempool = new Set<string>();
  readonly submitted: Buffer[] = [];
  /** Every acquired state: `tip`, or `slot:hash`. */
  readonly acquired: string[] = [];

  extend(utxos: readonly BuiltOutput[]): Snapshot {
    const blockNo = this.chain.length + 1;
    const snapshot = {
      slot: blockNo * 10,
      hash: fill(blockNo).toString("hex"),
      blockNo,
      utxos,
    };
    this.chain.push(snapshot);
    return snapshot;
  }

  get tip(): Snapshot {
    return this.chain.at(-1)!;
  }

  answer(snapshot: Snapshot, query: LedgerQuery): Uint8Array {
    switch (query.query) {
      case "chain_point":
        return encodeCbor([
          BigInt(snapshot.slot),
          Buffer.from(snapshot.hash, "hex"),
        ]);
      case "chain_block_no":
        return encodeCbor([1, BigInt(snapshot.blockNo)]);
      case "protocol_params":
        return Buffer.from(DEVNET_LEDGER.lsq.protocol_params, "hex");
      case "utxo_by_address": {
        const wanted = new Set(
          query.addresses.map((address) =>
            Buffer.from(address).toString("hex"),
          ),
        );
        return utxoAnswer(
          snapshot.utxos.filter((utxo) =>
            wanted.has(utxo.summary.address.toString("hex")),
          ),
        );
      }
      case "utxo_by_txin": {
        const wanted = new Set(
          query.txIns.map((txIn) => `${txIn.txId}#${txIn.index}`),
        );
        return utxoAnswer(
          snapshot.utxos.filter((utxo) =>
            wanted.has(
              `${utxo.outRef.txHash.toString("hex")}#${utxo.outRef.index}`,
            ),
          ),
        );
      }
      case "system_start":
      case "current_era":
      case "era_history":
      case "stake_deleg_deposits":
      case "filtered_delegations_and_rewards":
        throw new Error(`the stub node does not answer ${query.query}`);
    }
  }

  transport(): L1NodeTransport {
    const withLedgerState = async <T>(
      at: ChainPoint | "tip",
      use: (session: LedgerStateSession) => Promise<T>,
    ): Promise<T> => {
      let snapshot: Snapshot | undefined;
      if (at === "tip") {
        snapshot = this.tip;
        this.acquired.push("tip");
      } else {
        const key = at.kind === "origin" ? "origin" : `${at.slot}:${at.hash}`;
        if (this.immutable.has(key))
          throw new TransportRequestError("acquire_point_too_old", key);
        snapshot = this.chain.find(
          (state) =>
            at.kind === "point" &&
            BigInt(state.slot) === at.slot &&
            state.hash === at.hash,
        );
        if (snapshot === undefined)
          throw new TransportRequestError("acquire_point_not_on_chain", key);
        this.acquired.push(key);
      }
      const state = snapshot;
      return await use({ query: async (query) => this.answer(state, query) });
    };
    return {
      withLedgerState,
      query: (query: LedgerQuery) =>
        withLedgerState("tip", (session) => session.query(query)),
      submit: async (tx: Uint8Array) => {
        this.submitted.push(Buffer.from(tx));
        return { accepted: true };
      },
      hasTx: async (txId: string) => this.mempool.has(txId),
    } as unknown as L1NodeTransport;
  }
}

const output = (txHash: Buffer, index: number, address = keyAddress) =>
  built({ txHash, index }, { address, assets: { lovelace: 5_000_000n } });

/** A transaction `[{1: outputs}, {}, true, null]` and its id. */
const transaction = (outputs: readonly BuiltOutput[]) => {
  const body = Buffer.concat([
    Buffer.from([0xa1, 0x01]),
    head(4, outputs.length),
    ...outputs.map((entry) => entry.bytes),
  ]);
  return {
    cbor: Buffer.concat([
      Buffer.from([0x84]),
      body,
      Buffer.from([0xa0, 0xf5, 0xf6]),
    ]).toString("hex"),
    txHash: blake2b256(body),
  };
};

const refs = (utxos: readonly { txHash: string; outputIndex: number }[]) =>
  utxos.map((utxo) => `${utxo.txHash.slice(0, 4)}#${utxo.outputIndex}`);

describe("LedgerProvider (the node's ledger, no store)", () => {
  it("answers one build step at one acquired point, then re-acquires the tip", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0)]);
    let now = 0;
    const provider = new LedgerProvider({
      transport: node.transport(),
      pinPointMs: 1_000,
      monotonicNowMs: () => now,
    });
    expect(refs(await provider.getUtxos(keyAddress))).toEqual(["0101#0"]);
    // The chain moves within the step: the step keeps its point.
    node.extend([output(fill(1), 0), output(fill(2), 0)]);
    now = 999;
    expect(refs(await provider.getUtxos(keyAddress))).toEqual(["0101#0"]);
    expect(
      refs(
        await provider.getUtxosByOutRef([
          { txHash: fill(2).toString("hex"), outputIndex: 0 },
        ]),
      ),
    ).toEqual([]);
    // Past the window the next read starts a new step at the tip.
    now = 1_000;
    expect(refs(await provider.getUtxos(keyAddress))).toEqual([
      "0101#0",
      "0202#0",
    ]);
    expect(node.acquired).toEqual([
      "tip",
      `10:${fill(1).toString("hex")}`,
      `10:${fill(1).toString("hex")}`,
      `10:${fill(1).toString("hex")}`,
      "tip",
      `20:${fill(2).toString("hex")}`,
    ]);
  });

  it("re-acquires once when the node no longer holds the pinned point", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0)]);
    const provider = new LedgerProvider({ transport: node.transport() });
    expect(await provider.pinnedTip()).toEqual({
      slot: 10,
      hash: fill(1).toString("hex"),
    });
    // A rollback replaced the pinned block.
    node.chain.length = 0;
    node.extend([output(fill(3), 0)]);
    node.chain[0] = { ...node.chain[0]!, hash: fill(9).toString("hex") };
    expect(refs(await provider.getUtxos(keyAddress))).toEqual(["0303#0"]);
  });

  it("answers outrefs in request order and omits the absent ones", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0), output(fill(2), 1, otherAddress)]);
    const provider = new LedgerProvider({ transport: node.transport() });
    expect(
      refs(
        await provider.getUtxosByOutRef([
          { txHash: fill(2).toString("hex"), outputIndex: 1 },
          { txHash: fill(5).toString("hex"), outputIndex: 0 },
          { txHash: fill(1).toString("hex"), outputIndex: 0 },
          { txHash: fill(2).toString("hex"), outputIndex: 1 },
        ]),
      ),
    ).toEqual(["0202#1", "0101#0"]);
  });

  it("refuses the reads local state query cannot answer, naming them", async () => {
    const node = new StubNode();
    node.extend([]);
    const provider = new LedgerProvider({ transport: node.transport() });
    await expect(
      provider.getUtxos({ type: "Key", hash: KEY }),
    ).rejects.toBeInstanceOf(L1LedgerScopeError);
    await expect(
      provider.getUtxoByUnit(`${"cc".repeat(28)}0a`),
    ).rejects.toThrow(
      "getUtxoByUnit cannot be answered from the node's ledger",
    );
    await expect(provider.getDatum("dd".repeat(32))).rejects.toThrow(
      "getDatum cannot be answered from the node's ledger",
    );
  });

  it("confirms a submitted tx once any of its outputs is in the ledger", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0)]);
    const provider = new LedgerProvider({ transport: node.transport() });
    await provider.pinnedTip();
    const tx = transaction([output(fill(0), 0), output(fill(0), 1)]);
    const txHash = await provider.submitTx(tx.cbor);
    expect(txHash).toBe(tx.txHash.toString("hex"));
    expect(node.submitted).toHaveLength(1);
    node.mempool.add(txHash);
    expect(await provider.getTransactionStatus(txHash)).toEqual({
      status: "pending",
      txHash,
    });
    const landing = provider.awaitTx(txHash, 5);
    // It lands, and its change output (index 0) is already spent: index 1
    // alone, known from the submitted body, confirms it.
    node.mempool.delete(txHash);
    node.extend([output(fill(1), 0), output(tx.txHash, 1)]);
    await expect(landing).resolves.toBe(true);
    expect(await provider.getTransactionStatus(txHash)).toEqual({
      status: "confirmed",
      txHash,
      confirmation: { txHash },
    });
    // The submission ended the build step: the next read is at the new tip.
    expect(await provider.pinnedTip()).toEqual({
      slot: 20,
      hash: fill(2).toString("hex"),
    });
  });

  it("times out cleanly on a tx that never lands", async () => {
    const node = new StubNode();
    node.extend([]);
    const provider = new LedgerProvider({
      transport: node.transport(),
      awaitTxTimeoutMs: 60,
    });
    const txHash = "ee".repeat(32);
    node.mempool.add(txHash);
    const error = await provider.awaitTx(txHash, 10).catch((e: unknown) => e);
    expect(error).toBeInstanceOf(L1AwaitTxTimeoutError);
    expect(error).toMatchObject({
      txHash,
      timeoutMs: 60,
      lastSeen: "in_mempool",
    });
    expect((error as Error).message).toContain("the node's ledger");
    node.mempool.clear();
    // Neither in the ledger nor in the mempool: unknown, never not found.
    await expect(provider.getTransactionStatus(txHash)).rejects.toMatchObject({
      name: "L1TxStatusUnknownError",
      reason: "ledger_tx_status_unknown",
      txHash,
    });
  });

  it("reports a landed tx whose every output is spent as unknown, not not_found", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0)]);
    const provider = new LedgerProvider({ transport: node.transport() });
    const tx = transaction([output(fill(0), 0)]);
    const txHash = await provider.submitTx(tx.cbor);
    // It landed (its one output is in the ledger) ...
    node.extend([output(fill(1), 0), output(tx.txHash, 0)]);
    expect(await provider.getTransactionStatus(txHash)).toMatchObject({
      status: "confirmed",
    });
    // ... and a later transaction spent that output.
    node.extend([output(fill(1), 0)]);
    const unknown = await provider
      .getTransactionStatus(txHash)
      .catch((error: unknown) => error);
    expect(unknown).toBeInstanceOf(L1TxStatusUnknownError);
    expect((unknown as Error).message).toMatch(
      /cannot tell whether transaction .* landed.*--l1 kupmios/,
    );
  });

  it("reports a tx it did not submit, with no output among the probed 32, as unknown", async () => {
    const node = new StubNode();
    node.extend([]);
    const provider = new LedgerProvider({ transport: node.transport() });
    const foreign = fill(7);
    // Only output 40 is unspent: past the 32 indices probed for a tx whose
    // output count the provider does not know.
    node.extend([output(foreign, 40)]);
    await expect(
      provider.getTransactionStatus(foreign.toString("hex")),
    ).rejects.toBeInstanceOf(L1TxStatusUnknownError);
  });

  it("ends the build step on a status read", async () => {
    const node = new StubNode();
    node.extend([output(fill(1), 0)]);
    let now = 0;
    const provider = new LedgerProvider({
      transport: node.transport(),
      pinPointMs: 1_000,
      monotonicNowMs: () => now,
    });
    expect(await provider.pinnedTip()).toMatchObject({ slot: 10 });
    node.extend([output(fill(1), 0)]);
    now = 1;
    node.mempool.add("ab".repeat(32));
    await provider.getTransactionStatus("ab".repeat(32));
    // Within the window, but the status read ended the step: a new point.
    expect(await provider.pinnedTip()).toMatchObject({ slot: 20 });
  });

  it("reads the tip with its block number and the standing of a point", async () => {
    const node = new StubNode();
    const first = node.extend([]);
    node.extend([]);
    const provider = new LedgerProvider({ transport: node.transport() });
    expect(await provider.readTip()).toEqual({
      slot: 20,
      hash: fill(2).toString("hex"),
      blockNo: 2,
    });
    expect(await provider.pointStatus(first)).toBe("on_chain");
    expect(
      await provider.pointStatus({ slot: 10, hash: fill(7).toString("hex") }),
    ).toBe("not_on_chain");
    node.immutable.add(`10:${first.hash}`);
    expect(await provider.pointStatus(first)).toBe("immutable");
  });

  it("builds, signs and submits a Lucid transaction with no store or follower", async () => {
    const seed = generateSeedPhrase();
    const wallet = walletFromSeed(seed, { network: "Custom" });
    const node = new StubNode();
    node.extend([
      built(
        { txHash: fill(4), index: 0 },
        { address: wallet.address, assets: { lovelace: 50_000_000n } },
      ),
    ]);
    const provider = new LedgerProvider({ transport: node.transport() });
    const lucid = await Lucid(provider, "Custom", {
      slotConfig: { ...SLOT_CONFIG_NETWORK.Preview },
    });
    lucid.selectWallet.fromSeed(seed);
    const signed = await (
      await lucid
        .newTx()
        .pay.ToAddress(otherAddress, { lovelace: 2_000_000n })
        .complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    expect(txHash).toBe(signed.toHash());
    expect(node.submitted).toHaveLength(1);
  });
});
