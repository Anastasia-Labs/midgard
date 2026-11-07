// The pre-built request corpus for the submit-ingress load harness.
//
// A corpus directory holds:
//   manifest.json  what was built: seed, fee parameters, counts per kind
//   genesis.ndjson one genesis output per line, {"outRef":hex,"output":hex}:
//                  the §5.3 outref item and the Midgard output CBOR
//   requests.bin   one record per request, in send order:
//                  u8 kind index (into manifest.kinds) ‖ u32 BE length ‖ the
//                  `/submit` body (a V1 proof submission envelope)
//
// Valid txs spend one genesis output each into two outputs. Junk kinds:
//   bad-cbor       the envelope carries tx bytes cut short: refused at ingress
//   bad-signature  the witness signs another id: Phase A refuses it
//   low-fee        one lovelace under the minimum fee: Phase A refuses it
//   missing-input  spends an outref that is not in the genesis
//   double-spend   spends a genesis output an earlier valid tx spends
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core";

import {
  buildLoadTx,
  LOAD_GENESIS_OUTPUT_LOVELACE,
  loadOutRef,
  makeLoadKey,
  makeLoadOutput,
} from "./submit-ingress-load.tx.mjs";

export const CORPUS_KINDS = Object.freeze([
  "valid",
  "bad-cbor",
  "bad-signature",
  "low-fee",
  "missing-input",
  "double-spend",
]);

export const JUNK_KINDS = Object.freeze(CORPUS_KINDS.slice(1));

const CORPUS_FORMAT_VERSION = 1;
const GENESIS_OUTPUTS_PER_TX_ID = 1_000;
const RECORD_HEADER_BYTES = 5;

/**
 * Parses `bad-cbor=0.04,low-fee=0.02`: each junk kind's share of all
 * requests. Unnamed kinds get none.
 */
export const parseJunkMix = (text) => {
  const mix = Object.fromEntries(JUNK_KINDS.map((kind) => [kind, 0]));
  if (text === undefined || text.trim() === "") return mix;
  for (const part of text.split(",")) {
    const [kind, share] = part.split("=").map((value) => value.trim());
    if (!JUNK_KINDS.includes(kind))
      throw new Error(
        `unknown junk kind ${JSON.stringify(kind)}; expected one of ${JUNK_KINDS.join(", ")}`,
      );
    const value = Number(share);
    if (!Number.isFinite(value) || value < 0 || value >= 1)
      throw new Error(`junk share for ${kind} must be in [0, 1)`);
    mix[kind] = value;
  }
  const total = Object.values(mix).reduce((sum, value) => sum + value, 0);
  if (total >= 1) throw new Error("junk shares must sum to under 1");
  return mix;
};

/** Request counts per kind for `validCount` valid txs under `junkMix`. */
export const corpusCounts = (validCount, junkMix) => {
  const junkShare = Object.values(junkMix).reduce((sum, v) => sum + v, 0);
  const total = Math.round(validCount / (1 - junkShare));
  const counts = { valid: validCount };
  for (const kind of JUNK_KINDS)
    counts[kind] = Math.round(total * (junkMix[kind] ?? 0));
  if (counts["double-spend"] > validCount)
    throw new Error("double-spend junk needs a valid tx to collide with");
  return counts;
};

// Deterministic PRNG (mulberry32) so a seed always gives the same order.
const prng = (seed) => {
  let state =
    createHash("sha256").update(`order:${seed}`).digest().readUInt32BE(0) >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4_294_967_296;
  };
};

const genesisOutRef = (seed, index) =>
  loadOutRef(
    createHash("sha256")
      .update(
        `genesis:${seed}:${Math.floor(index / GENESIS_OUTPUTS_PER_TX_ID).toString()}`,
      )
      .digest(),
    index % GENESIS_OUTPUTS_PER_TX_ID,
  );

const envelope = (txCbor) =>
  encodeMidgardProofSubmission({ transactionCbor: txCbor });

/**
 * Builds a corpus into `directory`. Valid txs and the junk kinds that spend a
 * real genesis output (bad-signature, low-fee) each take a genesis output of
 * their own; the send order is a seeded shuffle, except that each
 * double-spend follows the valid tx it collides with.
 */
export const buildCorpus = ({
  directory,
  seed = "baseline",
  validCount,
  junkMix,
  minFeeA = 1n,
  minFeeB = 2_000n,
  keyCount = 1_024,
  onProgress = () => {},
}) => {
  if (keyCount < 2) throw new Error("a corpus needs at least two keys");
  const counts = corpusCounts(validCount, junkMix);
  const keys = Array.from({ length: keyCount }, (_, index) =>
    makeLoadKey(seed, index),
  );
  const ownedGenesisCount =
    counts.valid + counts["bad-signature"] + counts["low-fee"];
  fs.mkdirSync(directory, { recursive: true });

  const genesisPath = path.join(directory, "genesis.ndjson");
  const genesisFd = fs.openSync(genesisPath, "w");
  const genesisInput = (index) => ({
    key: keys[index % keyCount],
    lovelace: LOAD_GENESIS_OUTPUT_LOVELACE,
    outRef: genesisOutRef(seed, index),
  });
  let genesisLines = [];
  for (let index = 0; index < ownedGenesisCount; index += 1) {
    const input = genesisInput(index);
    genesisLines.push(
      JSON.stringify({
        outRef: input.outRef.toString("hex"),
        output: makeLoadOutput(input.key.addressBytes, input.lovelace).toString(
          "hex",
        ),
      }),
    );
    if (genesisLines.length === 4_096) {
      fs.writeSync(genesisFd, `${genesisLines.join("\n")}\n`);
      genesisLines = [];
    }
  }
  if (genesisLines.length > 0)
    fs.writeSync(genesisFd, `${genesisLines.join("\n")}\n`);
  fs.closeSync(genesisFd);

  const outputAddresses = (index) => [
    keys[(index + 1) % keyCount].addressBytes,
    keys[(index + 2) % keyCount].addressBytes,
  ];
  const fee = { minFeeA, minFeeB };

  // Plan the send order: kinds shuffled, double-spends pinned after their
  // victim below.
  const random = prng(seed);
  const order = [];
  for (const kind of CORPUS_KINDS)
    if (kind !== "double-spend")
      for (let n = 0; n < counts[kind]; n += 1) order.push(kind);
  for (let index = order.length - 1; index > 0; index -= 1) {
    const swap = Math.floor(random() * (index + 1));
    [order[index], order[swap]] = [order[swap], order[index]];
  }

  const requestsPath = path.join(directory, "requests.bin");
  const requestsFd = fs.openSync(requestsPath, "w");
  const pending = [];
  let pendingBytes = 0;
  const flush = () => {
    if (pending.length === 0) return;
    fs.writeSync(requestsFd, Buffer.concat(pending, pendingBytes));
    pending.length = 0;
    pendingBytes = 0;
  };
  const writeRecord = (kind, body) => {
    const header = Buffer.alloc(RECORD_HEADER_BYTES);
    header.writeUInt8(CORPUS_KINDS.indexOf(kind), 0);
    header.writeUInt32BE(body.length, 1);
    pending.push(header, body);
    pendingBytes += header.length + body.length;
    if (pendingBytes > 4 * 1024 * 1024) flush();
  };

  let nextGenesis = 0;
  let validSent = 0;
  let doubleSpendsPlaced = 0;
  let junkNonce = 0;
  for (let position = 0; position < order.length; position += 1) {
    const kind = order[position];
    if (kind === "valid") {
      const genesisIndex = nextGenesis++;
      const input = genesisInput(genesisIndex);
      writeRecord(
        "valid",
        envelope(
          buildLoadTx({
            inputs: [input],
            outputAddresses: outputAddresses(genesisIndex),
            ...fee,
          }).txCbor,
        ),
      );
      validSent += 1;
      // Spread the double-spends evenly over the valid txs.
      if (
        doubleSpendsPlaced <
        Math.floor((validSent * counts["double-spend"]) / counts.valid)
      ) {
        // Same input into one output instead of two: a distinct tx id for
        // any key pool.
        writeRecord(
          "double-spend",
          envelope(
            buildLoadTx({
              inputs: [input],
              outputAddresses: outputAddresses(genesisIndex).slice(0, 1),
              ...fee,
            }).txCbor,
          ),
        );
        doubleSpendsPlaced += 1;
      }
    } else if (kind === "bad-signature") {
      const genesisIndex = nextGenesis++;
      const input = genesisInput(genesisIndex);
      // The owner's witness, over another id.
      writeRecord(
        kind,
        envelope(
          buildLoadTx({
            inputs: [input],
            outputAddresses: outputAddresses(genesisIndex),
            ...fee,
            signedTxId: createHash("sha256")
              .update(`bad-signature:${seed}:${genesisIndex.toString()}`)
              .digest(),
          }).txCbor,
        ),
      );
    } else if (kind === "low-fee") {
      const genesisIndex = nextGenesis++;
      const input = genesisInput(genesisIndex);
      const honest = buildLoadTx({
        inputs: [input],
        outputAddresses: outputAddresses(genesisIndex),
        ...fee,
      });
      writeRecord(
        kind,
        envelope(
          buildLoadTx({
            inputs: [input],
            outputAddresses: outputAddresses(genesisIndex),
            ...fee,
            feeOverride: minFeeA * BigInt(honest.txCbor.length) + minFeeB - 1n,
          }).txCbor,
        ),
      );
    } else if (kind === "missing-input") {
      const nonce = junkNonce++;
      writeRecord(
        kind,
        envelope(
          buildLoadTx({
            inputs: [
              {
                key: keys[nonce % keyCount],
                lovelace: LOAD_GENESIS_OUTPUT_LOVELACE,
                outRef: loadOutRef(
                  createHash("sha256")
                    .update(`missing:${seed}:${nonce.toString()}`)
                    .digest(),
                  0,
                ),
              },
            ],
            outputAddresses: outputAddresses(nonce),
            ...fee,
          }).txCbor,
        ),
      );
    } else if (kind === "bad-cbor") {
      const nonce = junkNonce++;
      const tx = buildLoadTx({
        inputs: [
          {
            key: keys[nonce % keyCount],
            lovelace: LOAD_GENESIS_OUTPUT_LOVELACE,
            outRef: loadOutRef(
              createHash("sha256")
                .update(`bad-cbor:${seed}:${nonce.toString()}`)
                .digest(),
              0,
            ),
          },
        ],
        outputAddresses: outputAddresses(nonce),
        ...fee,
      });
      writeRecord(kind, envelope(tx.txCbor.subarray(0, tx.txCbor.length - 7)));
    }
    if (position % 10_000 === 0) onProgress(position, order.length);
  }
  onProgress(order.length, order.length);
  flush();
  fs.closeSync(requestsFd);

  const manifest = {
    formatVersion: CORPUS_FORMAT_VERSION,
    seed,
    networkId: 0,
    minFeeA: minFeeA.toString(),
    minFeeB: minFeeB.toString(),
    genesisOutputLovelace: LOAD_GENESIS_OUTPUT_LOVELACE.toString(),
    genesisOutputs: ownedGenesisCount,
    keyCount,
    kinds: [...CORPUS_KINDS],
    counts,
    requests: Object.values(counts).reduce((sum, value) => sum + value, 0),
  };
  fs.writeFileSync(
    path.join(directory, "manifest.json"),
    `${JSON.stringify(manifest, null, 2)}\n`,
  );
  return manifest;
};

/** Reads a corpus written by `buildCorpus`. */
export const readCorpus = (directory) => {
  const manifest = JSON.parse(
    fs.readFileSync(path.join(directory, "manifest.json"), "utf8"),
  );
  if (manifest.formatVersion !== CORPUS_FORMAT_VERSION)
    throw new Error(
      `corpus format ${String(manifest.formatVersion)} is not ${CORPUS_FORMAT_VERSION.toString()}`,
    );
  const bytes = fs.readFileSync(path.join(directory, "requests.bin"));
  const requests = [];
  for (let offset = 0; offset < bytes.length; ) {
    if (offset + RECORD_HEADER_BYTES > bytes.length)
      throw new Error(`truncated request record header at byte ${offset}`);
    const kind = manifest.kinds[bytes.readUInt8(offset)];
    const length = bytes.readUInt32BE(offset + 1);
    const start = offset + RECORD_HEADER_BYTES;
    if (kind === undefined || start + length > bytes.length)
      throw new Error(`malformed request record at byte ${offset}`);
    requests.push({ kind, body: bytes.subarray(start, start + length) });
    offset = start + length;
  }
  if (requests.length !== manifest.requests)
    throw new Error(
      `corpus has ${requests.length.toString()} requests, manifest says ${String(manifest.requests)}`,
    );
  return { manifest, requests };
};

/** Streams the genesis outputs of a corpus: `{outRef, output}` Buffers. */
export function* readCorpusGenesis(directory) {
  const text = fs.readFileSync(path.join(directory, "genesis.ndjson"), "utf8");
  for (const line of text.split("\n")) {
    if (line === "") continue;
    const { outRef, output } = JSON.parse(line);
    yield {
      outRef: Buffer.from(outRef, "hex"),
      output: Buffer.from(output, "hex"),
    };
  }
}
