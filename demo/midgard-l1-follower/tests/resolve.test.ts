/**
 * Output resolution by outref (plan §12.3 steps 2 to 4) and the transaction
 * decoder it trusts only through the id: the step order, what makes a step
 * fall through, the hash check on content answers, the transport adapter's
 * refusal mapping and the HTTP source's answer forms.
 */
import { readFileSync } from "node:fs";

import { TransportRequestError } from "@al-ft/l1-node-transport";
import { describe, expect, it } from "vitest";

import {
  decodeBlock,
  decodeLedgerUtxos,
  decodeTransaction,
  httpTxContentSource,
  type LedgerOutputs,
  type OutRef,
  type Point,
  resolveOutputs,
  transactionOutputAt,
  transportLedgerOutputs,
  type TxContentSource,
  TxDecodeError,
} from "../src/index.js";
import * as c from "../src/testing/cbor-writer.js";
import {
  encodeTxBody,
  encodeUtxoAnswer,
  type SimTx,
  simTxHash,
} from "../src/testing/index.js";

const ADDRESS = Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 7)]);

const tx = (nonce: number, outputs = 2): SimTx => ({
  inputs: [{ txHash: Buffer.alloc(32, nonce), index: 0 }],
  outputs: Array.from({ length: outputs }, (_, i) => ({
    address: ADDRESS,
    lovelace: BigInt(1_000_000 + i),
    datum: Buffer.from([0x41, nonce, i]),
  })),
  nonce,
});

const ref = (t: SimTx, index: number): OutRef => ({
  txHash: simTxHash(t),
  index,
});

/** A whole transaction: `[body, witnesses, true, null]`. */
const whole = (t: SimTx): Buffer =>
  Buffer.concat([
    Buffer.from([0x84]),
    encodeTxBody(t),
    c.map(),
    Buffer.from([0xf5, 0xf6]),
  ]);

const PARENT: Point = { slot: 10, hash: Buffer.alloc(32, 0xaa) };

/** A ledger answering from fixed UTxO sets per point, recording its calls. */
const ledgerOf = (
  sets: Readonly<{ parent?: SimTx[] | "unavailable"; tip?: SimTx[] }>,
) => {
  const calls: string[] = [];
  const ledger: LedgerOutputs = (at, outRefs) => {
    calls.push(at === "tip" ? "tip" : at.slot.toString());
    const set = at === "tip" ? sets.tip : sets.parent;
    if (set === "unavailable")
      return Promise.resolve({
        kind: "point_unavailable" as const,
        detail: "acquire_point_too_old",
      });
    const wanted = new Set(
      outRefs.map((o) => `${o.txHash.toString("hex")}#${o.index}`),
    );
    const utxos = (set ?? []).flatMap((t) =>
      t.outputs.map((output, index) => ({ outRef: ref(t, index), output })),
    );
    return Promise.resolve(
      decodeLedgerUtxos(
        encodeUtxoAnswer(
          utxos.filter((u) =>
            wanted.has(`${u.outRef.txHash.toString("hex")}#${u.outRef.index}`),
          ),
        ),
      ),
    );
  };
  return { ledger, calls };
};

const source = (
  name: string,
  answer: (txHash: Buffer) => Uint8Array | null,
): TxContentSource & { asked: number } => {
  const s = {
    name,
    asked: 0,
    fetchTx: (txHash: Buffer) => {
      s.asked += 1;
      return Promise.resolve(answer(txHash));
    },
  };
  return s;
};

describe("decodeTransaction", () => {
  it("hashes the body of a real mainnet transaction to its id, whole or bare", () => {
    const raw = Buffer.from(
      readFileSync(
        new URL("./fixtures/conway-block.hex", import.meta.url),
        "utf8",
      ).trim(),
      "hex",
    );
    const [first] = decodeBlock(raw).txs;
    const rebuilt = Buffer.concat([
      Buffer.from([0x84]),
      first!.bodyCbor,
      first!.witnessCbor,
      Buffer.from([0xf5]),
      first!.auxCbor ?? Buffer.from([0xf6]),
    ]);
    const decoded = decodeTransaction(rebuilt);
    expect(decoded.hash).toEqual(first!.hash);
    expect(decoded.outputs).toEqual(first!.outputs);
    expect(decodeTransaction(first!.bodyCbor).hash).toEqual(first!.hash);
  });

  it("reads the three-element pre-Alonzo form and refuses trailing bytes", () => {
    const t = tx(1);
    const three = Buffer.concat([
      Buffer.from([0x83]),
      encodeTxBody(t),
      c.map(),
      Buffer.from([0xf6]),
    ]);
    expect(decodeTransaction(three).hash).toEqual(simTxHash(t));
    expect(() =>
      decodeTransaction(Buffer.concat([whole(t), Buffer.from([0x00])])),
    ).toThrow(TxDecodeError);
    expect(() => decodeTransaction(Buffer.from([0x82, 0x00, 0x00]))).toThrow(
      TxDecodeError,
    );
  });

  it("names a body output, the collateral return after them, and nothing else", () => {
    const decoded = decodeTransaction(whole(tx(2, 1)));
    expect(transactionOutputAt(decoded, 0)?.lovelace).toBe(1_000_000n);
    expect(transactionOutputAt(decoded, 1)).toBeNull();
    const withReturn = {
      ...decoded,
      collateralReturn: decoded.outputs[0]!,
    };
    expect(transactionOutputAt(withReturn, 1)).toBe(decoded.outputs[0]);
    expect(transactionOutputAt(withReturn, 2)).toBeNull();
  });
});

describe("resolveOutputs (§12.3 steps 2 to 4)", () => {
  const a = tx(3);
  const b = tx(4);
  const d = tx(5);

  it("tries the ledger at the parent, then at the tip, then the sources, each only for what is left", async () => {
    const { ledger, calls } = ledgerOf({ parent: [a], tip: [b] });
    const s = source("indexer", (h) =>
      h.equals(simTxHash(d)) ? whole(d) : null,
    );
    const outcome = await resolveOutputs({
      outRefs: [ref(a, 0), ref(b, 1), ref(d, 0), ref(d, 1)],
      parent: PARENT,
      ledger,
      sources: [s],
    });
    expect(calls).toEqual(["10", "tip"]);
    expect(
      outcome.resolved.map((r) => [r.outRef.txHash, r.step, r.source]),
    ).toEqual([
      [simTxHash(a), "ledger_at_parent", undefined],
      [simTxHash(b), "ledger_at_tip", undefined],
      [simTxHash(d), "content", "indexer"],
      [simTxHash(d), "content", "indexer"],
    ]);
    expect(outcome.resolved[0]!.output.datum).toEqual(a.outputs[0]!.datum);
    // One fetch per creating transaction, not per outref.
    expect(s.asked).toBe(1);
    expect(outcome.pending).toEqual([]);
  });

  it("skips the parent step without a parent, and stops asking once all resolved", async () => {
    const { ledger, calls } = ledgerOf({ tip: [a] });
    const s = source("never", () => {
      throw new Error("asked");
    });
    const outcome = await resolveOutputs({
      outRefs: [ref(a, 1)],
      parent: null,
      ledger,
      sources: [s],
    });
    expect(calls).toEqual(["tip"]);
    expect(outcome.resolved.map((r) => r.step)).toEqual(["ledger_at_tip"]);
    expect(s.asked).toBe(0);
  });

  it("falls through an unavailable point and a failing ledger, noting why", async () => {
    const unavailable = ledgerOf({ parent: "unavailable", tip: [a] });
    const first = await resolveOutputs({
      outRefs: [ref(a, 0)],
      parent: PARENT,
      ledger: unavailable.ledger,
    });
    expect(first.notes).toEqual(["ledger_at_parent: acquire_point_too_old"]);
    expect(first.resolved.map((r) => r.step)).toEqual(["ledger_at_tip"]);
    const failing: LedgerOutputs = () =>
      Promise.reject(new Error("node_connection_lost"));
    const second = await resolveOutputs({
      outRefs: [ref(a, 0)],
      parent: PARENT,
      ledger: failing,
    });
    expect(second.notes).toEqual([
      "ledger_at_parent: node_connection_lost",
      "ledger_at_tip: node_connection_lost",
    ]);
    expect(second.pending).toEqual([ref(a, 0)]);
  });

  it("refuses an answer whose body does not hash to the id and takes the next source's", async () => {
    const forged = { ...a, nonce: a.nonce + 1 };
    const liar = source("liar", () => whole(forged));
    const honest = source("honest", () => whole(a));
    const outcome = await resolveOutputs({
      outRefs: [ref(a, 0)],
      parent: null,
      sources: [liar, honest],
    });
    expect(outcome.notes).toEqual([
      `content liar ${simTxHash(a).toString("hex")}: refused, the body hashes to ${simTxHash(forged).toString("hex")}`,
    ]);
    expect(outcome.resolved.map((r) => r.source)).toEqual(["honest"]);
    expect(outcome.resolved[0]!.output.datum).toEqual(a.outputs[0]!.datum);
  });

  it("keeps pending what no source holds, a source that throws, undecodable bytes and a missing output", async () => {
    const outcome = await resolveOutputs({
      outRefs: [ref(a, 5)],
      parent: null,
      sources: [
        source("empty", () => null),
        source("down", () => {
          throw new Error("HTTP 503");
        }),
        source("garbage", () => Buffer.from([0xff])),
        source("short", () => whole(a)),
      ],
    });
    const hash = simTxHash(a).toString("hex");
    expect(outcome.resolved).toEqual([]);
    expect(outcome.pending).toEqual([ref(a, 5)]);
    expect(outcome.notes[0]).toBe(`content down ${hash}: HTTP 503`);
    expect(outcome.notes[1]).toMatch(
      new RegExp(
        `^content garbage ${hash}: refused, the transaction does not decode`,
        "u",
      ),
    );
    expect(outcome.notes[2]).toBe(
      `content short ${hash}: the transaction has no output 5`,
    );
  });
});

describe("transportLedgerOutputs", () => {
  const t = tx(6);
  const answer = encodeUtxoAnswer([
    { outRef: ref(t, 0), output: t.outputs[0]! },
  ]);

  it("acquires the point asked, queries by txin and decodes the answer", async () => {
    const seen: unknown[] = [];
    const ledger = transportLedgerOutputs({
      withLedgerState: (
        at: unknown,
        use: (state: { query: (q: unknown) => Promise<Buffer> }) => unknown,
      ) => {
        seen.push(at);
        return use({
          query: (q) => {
            seen.push(q);
            return Promise.resolve(answer);
          },
        });
      },
    } as never);
    const found = await ledger(PARENT, [ref(t, 0)]);
    expect(found).toEqual(decodeLedgerUtxos(answer));
    expect(seen[0]).toMatchObject({ slot: 10n, hash: "aa".repeat(32) });
    expect(seen[1]).toEqual({
      query: "utxo_by_txin",
      txIns: [{ txId: simTxHash(t).toString("hex"), index: 0 }],
    });
    expect(await ledger("tip", [])).toEqual([]);
  });

  it("maps the acquire refusals to point_unavailable and rethrows the rest", async () => {
    const failing = (code: string) =>
      transportLedgerOutputs({
        withLedgerState: () =>
          Promise.reject(new TransportRequestError(code, "refused")),
      } as never);
    for (const code of ["acquire_point_too_old", "acquire_point_not_on_chain"])
      expect(await failing(code)(PARENT, [ref(t, 0)])).toEqual({
        kind: "point_unavailable",
        detail: code,
      });
    await expect(
      failing("node_unreachable")("tip", [ref(t, 0)]),
    ).rejects.toThrow(/node_unreachable/u);
  });
});

describe("httpTxContentSource", () => {
  const t = tx(7);
  const id = simTxHash(t);
  const served = (body: string | Buffer, init: ResponseInit) => {
    const urls: string[] = [];
    const s = httpTxContentSource({
      urlTemplate: "https://indexer.example/tx/{txId}/cbor",
      fetch: (url) => {
        urls.push(String(url));
        return Promise.resolve(new Response(body, init));
      },
    });
    return { s, urls };
  };

  it("reads raw CBOR, hex text and JSON { cbor } answers", async () => {
    const raw = served(whole(t), {
      headers: { "content-type": "application/cbor" },
    });
    expect(Buffer.from((await raw.s.fetchTx(id))!)).toEqual(whole(t));
    expect(raw.urls).toEqual([
      `https://indexer.example/tx/${id.toString("hex")}/cbor`,
    ]);
    const text = served(`${whole(t).toString("hex")}\n`, {
      headers: { "content-type": "text/plain" },
    });
    expect(Buffer.from((await text.s.fetchTx(id))!)).toEqual(whole(t));
    const json = served(JSON.stringify({ cbor: whole(t).toString("hex") }), {
      headers: { "content-type": "application/json" },
    });
    expect(Buffer.from((await json.s.fetchTx(id))!)).toEqual(whole(t));
  });

  it("answers null on 404 and throws on other failures and malformed JSON", async () => {
    expect(await served("", { status: 404 }).s.fetchTx(id)).toBeNull();
    await expect(served("", { status: 500 }).s.fetchTx(id)).rejects.toThrow(
      "HTTP 500",
    );
    await expect(
      served(JSON.stringify({ tx: "00" }), {
        headers: { "content-type": "application/json" },
      }).s.fetchTx(id),
    ).rejects.toThrow(/no hex `cbor` field/u);
  });

  it("requires a {txId} placeholder", () => {
    expect(() =>
      httpTxContentSource({ urlTemplate: "https://indexer.example/tx" }),
    ).toThrow(/\{txId\}/u);
  });
});
