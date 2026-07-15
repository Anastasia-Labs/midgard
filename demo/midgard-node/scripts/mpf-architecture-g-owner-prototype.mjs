import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { once } from "node:events";
import {
  closeSync,
  createWriteStream,
  openSync,
  readSync,
  rmSync,
  statSync,
  writeFileSync,
  writeSync,
} from "node:fs";
import { createRequire } from "node:module";

import { Level } from "level";

const require = createRequire(import.meta.url);
const blake2b = require("blake2b");
await new Promise((resolve, reject) =>
  blake2b.ready((error) => (error === undefined ? resolve() : reject(error))),
);
if (blake2b.WASM_SUPPORTED !== true || blake2b.WASM_LOADED !== true) {
  throw new Error("Architecture G exporter requires canonical BLAKE2b WASM");
}
const digest = (...parts) => {
  const hash = blake2b(32);
  for (const part of parts) hash.update(part);
  return Buffer.from(hash.digest());
};

const FIXTURES = new Map([
  [
    100_000,
    {
      path: "/tmp/midgard-mpf-growth-event-flat-e-100000-level",
      marker: "106f235b1a280b1f0fb11ff799bc48e434f822c76901e8977faa50b439c7a61f",
      keyCount: 137_421,
      candidate: "f7b8dd2bf9a47f7167e72340d566cd3d9b7746b772d7dcefc9be21e395b2ca4d",
    },
  ],
  [
    1_000_000,
    {
      path: "/tmp/midgard-mpf-growth-event-flat-e-1000000-level",
      marker: "cf353a18e5d16d09b1300065da2877eb957bed7e010169ef53931b5bbc8f0670",
      keyCount: 1_345_735,
      candidate: "95ac9d0b40c0aa2af474dd09d038b8cbaa123af16570163ec86a4777dbabf95e",
    },
  ],
]);

const prefixBytes = (prefix) =>
  Buffer.from([...prefix].map((digit) => Number.parseInt(digit, 16)));
const key = (index) => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value;
};
const eventsFor = (initialUtxos, transactions) =>
  Array.from({ length: transactions }, (_, index) => [
    { type: "delete", key: key(index) },
    {
      type: "insert",
      key: key(initialUtxos + index),
      value: Buffer.alloc(64, (index + 1) % 251),
    },
  ]);

const encodeRecord = (hashHex, value) => {
  const common = 1 + 32 + 1 + value.prefix.length;
  const bytes =
    value.__kind === "Leaf"
      ? common + 2 + 4 + value.key.length / 2 + value.value.length / 2
      : common +
        8 +
        2 +
        value.children.filter((child) => child != null).length * 32;
  const output = Buffer.allocUnsafe(bytes);
  let offset = 0;
  output.writeUInt8(value.__kind === "Leaf" ? 1 : 2, offset++);
  Buffer.from(hashHex, "hex").copy(output, offset);
  offset += 32;
  output.writeUInt8(value.prefix.length, offset++);
  prefixBytes(value.prefix).copy(output, offset);
  offset += value.prefix.length;
  if (value.__kind === "Leaf") {
    const keyBytes = Buffer.from(value.key, "hex");
    const valueBytes = Buffer.from(value.value, "hex");
    output.writeUInt16LE(keyBytes.length, offset);
    offset += 2;
    output.writeUInt32LE(valueBytes.length, offset);
    offset += 4;
    keyBytes.copy(output, offset);
    offset += keyBytes.length;
    valueBytes.copy(output, offset);
    offset += valueBytes.length;
  } else {
    output.writeBigUInt64LE(BigInt(value.size), offset);
    offset += 8;
    const bitmap = value.children.reduce(
      (bits, child, index) => bits | (child == null ? 0 : 1 << index),
      0,
    );
    output.writeUInt16LE(bitmap, offset);
    offset += 2;
    for (const child of value.children) {
      if (child == null) continue;
      Buffer.from(child, "hex").copy(output, offset);
      offset += 32;
    }
  }
  assert.equal(offset, output.length);
  return output;
};

const fullIndexHeader = (marker, recordCount) => {
  const output = Buffer.alloc(72);
  output.write("MEF6", 0);
  output.writeUInt16LE(1, 4);
  output.writeUInt16LE(0, 6);
  output.writeUInt32LE(2_000_000, 8);
  output.writeUInt32LE(100_000, 12);
  output.writeUInt32LE(400_000, 16);
  output.writeUInt32LE(536_870_912, 20);
  output.writeUInt32LE(536_870_912, 24);
  output.writeUInt32LE(recordCount, 28);
  output.writeUInt32LE(0, 32);
  output.writeUInt32LE(0, 36);
  Buffer.from(marker, "hex").copy(output, 40);
  return output;
};

const encodeEventStream = (marker, events) => {
  const ops = events.flat();
  const bytes =
    92 +
    events.length * 4 +
    ops.reduce(
      (total, op) =>
        total +
        1 +
        2 +
        4 +
        op.key.length +
        (op.type === "insert" ? op.value.length : 0),
      0,
    );
  const output = Buffer.allocUnsafe(bytes);
  let offset = 0;
  const put = (value) => {
    Buffer.from(value).copy(output, offset);
    offset += value.length;
  };
  output.write("MEGO", offset);
  offset += 4;
  output.writeUInt16LE(1, offset);
  offset += 2;
  output.writeUInt16LE(0, offset);
  offset += 2;
  for (const value of [events.length, ops.length, 100_000, 400_000, 536_870_912]) {
    output.writeUInt32LE(value, offset);
    offset += 4;
  }
  put(Buffer.from(marker, "hex"));
  const digestOffset = offset;
  put(Buffer.alloc(32));
  for (const event of events) {
    output.writeUInt32LE(event.length, offset);
    offset += 4;
    for (const op of event) {
      output.writeUInt8(op.type === "insert" ? 1 : 2, offset++);
      output.writeUInt16LE(op.key.length, offset);
      offset += 2;
      output.writeUInt32LE(op.type === "insert" ? op.value.length : 0, offset);
      offset += 4;
      put(op.key);
      if (op.type === "insert") put(op.value);
    }
  }
  assert.equal(offset, output.length);
  digest(
    Buffer.from("MIDGARD-MPF-ARCH-G-EVENTS-V1"),
    output.subarray(8, 28),
    Buffer.from(marker, "hex"),
    output.subarray(92),
  ).copy(output, digestOffset);
  return output;
};

const writeChunk = async (stream, value) => {
  if (!stream.write(value)) await once(stream, "drain");
};

const exportFixture = async ({ fixture, inputPath, eventPath, initialUtxos }) => {
  const db = new Level(fixture.path, { valueEncoding: "json" });
  await db.open();
  try {
    const marker = await db.get("__root__", { valueEncoding: "json" });
    assert.equal(marker, fixture.marker);
    const stream = createWriteStream(inputPath, { flags: "w" });
    await writeChunk(stream, fullIndexHeader(marker, fixture.keyCount - 1));
    let records = 0;
    let branches = 0;
    let leaves = 0;
    const logicalHash = blake2b(32);
    logicalHash.update(Buffer.from(marker, "hex"));
    for await (const [storageKey, value] of db.iterator()) {
      if (storageKey === "__root__") continue;
      assert.match(storageKey, /^[0-9a-f]{64}$/);
      if (value.__kind === "Leaf") leaves += 1;
      else branches += 1;
      const encoded = encodeRecord(storageKey, value);
      logicalHash.update(encoded);
      await writeChunk(stream, encoded);
      records += 1;
    }
    stream.end();
    await once(stream, "finish");
    assert.equal(records, fixture.keyCount - 1);
    assert.equal(await db.get("__root__", { valueEncoding: "json" }), marker);
    const logicalHashHex = Buffer.from(logicalHash.digest()).toString("hex");
    const eventBytes = encodeEventStream(marker, eventsFor(initialUtxos, 10_000));
    writeFileSync(eventPath, eventBytes);
    return {
      marker,
      records,
      branches,
      leaves,
      inputBytes: statSync(inputPath).size,
      eventBytes: eventBytes.length,
      logicalHash: logicalHashHex,
    };
  } finally {
    await db.close();
  }
};

const ownerBinary = new URL(
  "../native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  import.meta.url,
).pathname;

const runOwner = ({ inputPath, sidecarPath, eventPath, mode, expectFailure = false }) => {
  const run = spawnSync(
    ownerBinary,
    [
      `--input=${inputPath}`,
      `--sidecar=${sidecarPath}`,
      `--events=${eventPath}`,
      `--mode=${mode}`,
    ],
    { encoding: "utf8", maxBuffer: 1024 * 1024 },
  );
  if (expectFailure) {
    assert.notEqual(run.status, 0);
    return { rejected: true, error: run.stderr.trim() };
  }
  assert.equal(run.status, 0, run.stderr);
  return JSON.parse(run.stdout);
};

const flipByte = (path, position) => {
  const fd = openSync(path, "r+");
  try {
    const byte = Buffer.alloc(1);
    assert.equal(readSync(fd, byte, 0, 1, position), 1);
    byte[0] ^= 1;
    assert.equal(writeSync(fd, byte, 0, 1, position), 1);
  } finally {
    closeSync(fd);
  }
};

const verifyLockRelease = async (fixture) => {
  const db = new Level(fixture.path, { valueEncoding: "json" });
  await db.open();
  try {
    const marker = await db.get("__root__", { valueEncoding: "json" });
    assert.equal(marker, fixture.marker);
    const logicalHash = blake2b(32);
    logicalHash.update(Buffer.from(marker, "hex"));
    let records = 0;
    for await (const [storageKey, value] of db.iterator()) {
      if (storageKey === "__root__") continue;
      logicalHash.update(encodeRecord(storageKey, value));
      records += 1;
    }
    return {
      marker,
      records,
      logicalHash: Buffer.from(logicalHash.digest()).toString("hex"),
      reopened: true,
    };
  } finally {
    await db.close();
  }
};

const initialUtxos = Number.parseInt(
  process.argv.find((arg) => arg.startsWith("--utxos="))?.slice(8) ?? "",
  10,
);
const fixture = FIXTURES.get(initialUtxos);
if (fixture === undefined) throw new Error("Use --utxos=100000 or --utxos=1000000");
const prefix = `/tmp/midgard-architecture-g-owner-${initialUtxos.toString()}`;
const inputPath = `${prefix}.input`;
const eventPath = `${prefix}.events`;
const corruptEventPath = `${prefix}.events-corrupt`;
const sidecarPath = `${prefix}.sidecar`;
for (const path of [inputPath, eventPath, corruptEventPath, sidecarPath, `${prefix}.tmp`]) {
  rmSync(path, { force: true });
}

const exported = await exportFixture({ fixture, inputPath, eventPath, initialUtxos });
const prepared = runOwner({ inputPath, sidecarPath, eventPath, mode: "prepare" });
assert.equal(prepared.source, "level-export");
assert.equal(prepared.candidateRoot, fixture.candidate);
const sidecarBytes = statSync(sidecarPath).size;
const restarted = runOwner({ inputPath, sidecarPath, eventPath, mode: "recover" });
assert.equal(restarted.source, "sidecar");
assert.equal(restarted.replayDigest, prepared.replayDigest);

flipByte(sidecarPath, 80);
const corruptRebuild = runOwner({ inputPath, sidecarPath, eventPath, mode: "recover" });
assert.equal(corruptRebuild.source, "level-export");
assert.match(corruptRebuild.rebuildReason, /^corrupt:/);
assert.equal(corruptRebuild.replayDigest, prepared.replayDigest);

flipByte(sidecarPath, 8);
const staleRebuild = runOwner({ inputPath, sidecarPath, eventPath, mode: "recover" });
assert.equal(staleRebuild.source, "level-export");
assert.equal(staleRebuild.rebuildReason, "stale:marker");
assert.equal(staleRebuild.replayDigest, prepared.replayDigest);

const eventBytes = Buffer.alloc(exported.eventBytes);
const eventFd = openSync(eventPath, "r");
try {
  assert.equal(readSync(eventFd, eventBytes, 0, eventBytes.length, 0), eventBytes.length);
} finally {
  closeSync(eventFd);
}
eventBytes[eventBytes.length - 1] ^= 1;
writeFileSync(corruptEventPath, eventBytes);
const corruptReplay = runOwner({
  inputPath,
  sidecarPath,
  eventPath: corruptEventPath,
  mode: "recover",
  expectFailure: true,
});
assert.match(corruptReplay.error, /digest mismatch/);
const fixtureAfter = await verifyLockRelease(fixture);
assert.equal(fixtureAfter.records, exported.records);
assert.equal(fixtureAfter.logicalHash, exported.logicalHash);

for (const path of [inputPath, eventPath, corruptEventPath, sidecarPath, `${prefix}.tmp`]) {
  rmSync(path, { force: true });
}

process.stdout.write(
  `${JSON.stringify({
    initialUtxos,
    fixturePath: fixture.path,
    fixtureMarker: fixture.marker,
    expectedCandidate: fixture.candidate,
    exported,
    sidecarBytes,
    prepared,
    restarted,
    corruptRebuild,
    staleRebuild,
    corruptReplay,
    fixtureWrites: 0,
    markerUnchanged: true,
    fixtureAfter,
    cleanShutdownLockRelease: fixtureAfter.reopened,
    temporaryArtifactsRemoved: true,
  })}\n`,
);
