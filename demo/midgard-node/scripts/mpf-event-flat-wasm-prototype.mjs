import assert from "node:assert/strict";
import { createRequire } from "node:module";
import { performance } from "node:perf_hooks";

import { Level } from "level";

const require = createRequire(import.meta.url);
const blake2b = require("blake2b");
const wasm = require("../.architecture-f-wasm/midgard_mpf_event_flat_wasm.js");

const EMPTY_ROOT = Buffer.from(
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
  "hex",
);
const EXPECTED = new Map([
  [
    100_000,
    {
      fixtureRoot:
        "106f235b1a280b1f0fb11ff799bc48e434f822c76901e8977faa50b439c7a61f",
      candidateRoot:
        "f7b8dd2bf9a47f7167e72340d566cd3d9b7746b772d7dcefc9be21e395b2ca4d",
      // Maximum across the exact pair of summed worker CPU plus main-thread
      // serialization. This is deliberately above parallel wall.
      scratchRootUpperMs: 849.795243,
    },
  ],
  [
    1_000_000,
    {
      fixtureRoot:
        "cf353a18e5d16d09b1300065da2877eb957bed7e010169ef53931b5bbc8f0670",
      candidateRoot:
        "95ac9d0b40c0aa2af474dd09d038b8cbaa123af16570163ec86a4777dbabf95e",
      scratchRootUpperMs: 849.795243,
    },
  ],
]);

await new Promise((resolve, reject) =>
  blake2b.ready((error) => (error === undefined ? resolve() : reject(error))),
);
if (blake2b.WASM_SUPPORTED !== true || blake2b.WASM_LOADED !== true) {
  throw new Error("Prototype JS path digest requires canonical BLAKE2b WASM");
}

const digest = (...parts) => {
  const hash = blake2b(32);
  for (const part of parts) hash.update(part);
  return Buffer.from(hash.digest());
};
const pathOf = (key) => digest(key).toString("hex");
const prefixBytes = (prefix) =>
  Buffer.from([...prefix].map((digit) => Number.parseInt(digit, 16)));
const leafHash = (prefix, value) => {
  const odd = prefix.length % 2 === 1;
  const head = odd
    ? Buffer.from([0, Number.parseInt(prefix[0], 16)])
    : Buffer.from([255]);
  const tail = Buffer.from(odd ? prefix.slice(1) : prefix, "hex");
  return digest(head, tail, digest(value));
};
const branchHash = (prefix, children) => {
  let level = children.map((child) =>
    child == null ? Buffer.alloc(32) : Buffer.from(child, "hex"),
  );
  while (level.length > 1) {
    const next = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(digest(level[index], level[index + 1]));
    }
    level = next;
  }
  return digest(prefixBytes(prefix), level[0]);
};

const key = (index) => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value;
};

const loadRawTouchedProof = async (db, baseRoot, ops) => {
  const unique = new Map();
  for (const op of ops) {
    const path = pathOf(op.key);
    const previous = unique.get(path);
    unique.set(path, {
      path,
      deletePath: (previous?.deletePath ?? false) || op.type === "delete",
      ix: 0,
      id: previous?.id ?? unique.size,
    });
  }
  const states = [...unique.values()];
  const records = new Map();
  const resolved = new Set();
  let levelGetMs = 0;
  let levelGetManyCalls = 0;
  let maxBatchKeys = 0;
  const load = async (hashes) => {
    const startedAt = performance.now();
    const values = await db.getMany(hashes, { valueEncoding: "json" });
    levelGetMs += performance.now() - startedAt;
    levelGetManyCalls += 1;
    maxBatchKeys = Math.max(maxBatchKeys, hashes.length);
    return values;
  };
  let frontier = [];
  if (!baseRoot.equals(EMPTY_ROOT)) {
    const [root] = await load([baseRoot.toString("hex")]);
    if (root === undefined) throw new Error("Fixture is missing its root record");
    records.set(baseRoot.toString("hex"), root);
    frontier = [{ hash: baseRoot, value: root, states }];
  } else {
    for (const state of states) resolved.add(state.id);
  }
  while (frontier.length > 0) {
    const requested = new Map();
    const next = [];
    for (const node of frontier) {
      if (node.value.__kind === "Leaf") {
        for (const state of node.states) resolved.add(state.id);
        continue;
      }
      const branch = node.value;
      const groups = new Map();
      const deleteTargets = new Set();
      for (const state of node.states) {
        if (!state.path.slice(state.ix).startsWith(branch.prefix)) {
          resolved.add(state.id);
          continue;
        }
        const childIndex = Number.parseInt(
          state.path[state.ix + branch.prefix.length],
          16,
        );
        if (state.deletePath) deleteTargets.add(childIndex);
        const group = groups.get(childIndex);
        if (group === undefined) groups.set(childIndex, [state]);
        else group.push(state);
      }
      const present = branch.children.flatMap((child, childIndex) =>
        child == null ? [] : [childIndex],
      );
      const targetedPresent = [...deleteTargets].filter(
        (childIndex) => branch.children[childIndex] != null,
      ).length;
      if (targetedPresent > 0 && present.length - targetedPresent <= 1) {
        for (const childIndex of present) {
          if (!groups.has(childIndex)) groups.set(childIndex, []);
        }
      }
      for (const [childIndex, childStates] of groups) {
        const childHash = branch.children[childIndex];
        if (childHash == null) {
          for (const state of childStates) resolved.add(state.id);
          continue;
        }
        const nextStates = childStates.map((state) => ({
          ...state,
          ix: state.ix + branch.prefix.length + 1,
        }));
        const known = records.get(childHash);
        if (known !== undefined) {
          if (nextStates.length > 0) {
            next.push({
              hash: Buffer.from(childHash, "hex"),
              value: known,
              states: nextStates,
            });
          }
          continue;
        }
        const request = requested.get(childHash);
        if (request === undefined) requested.set(childHash, nextStates);
        else request.push(...nextStates);
      }
    }
    const requests = [...requested];
    for (let offset = 0; offset < requests.length; offset += 4096) {
      const batch = requests.slice(offset, offset + 4096);
      const values = await load(batch.map(([hash]) => hash));
      for (const [index, [hash, childStates]] of batch.entries()) {
        const value = values[index];
        if (value === undefined) {
          throw new Error(`Fixture is missing touched record ${hash}`);
        }
        records.set(hash, value);
        if (childStates.length > 0) {
          next.push({ hash: Buffer.from(hash, "hex"), value, states: childStates });
        }
      }
    }
    frontier = next;
  }
  if (resolved.size !== states.length) {
    throw new Error(
      `Raw touched proof is incomplete: resolved=${resolved.size},paths=${states.length}`,
    );
  }
  return { records, levelGetMs, levelGetManyCalls, maxBatchKeys };
};

const recordSize = (value) => {
  const common = 1 + 32 + 1 + value.prefix.length;
  if (value.__kind === "Leaf") {
    return common + 2 + 4 + value.key.length / 2 + value.value.length / 2;
  }
  return (
    common +
    8 +
    2 +
    value.children.filter((child) => child != null).length * 32
  );
};
const opSize = (op) =>
  1 + 2 + 4 + op.key.length + (op.type === "insert" ? op.value.length : 0);

const encodeInput = ({ baseRoot, records, events }) => {
  const ops = events.flat();
  const bytes =
    72 +
    [...records.values()].reduce((total, value) => total + recordSize(value), 0) +
    events.length * 4 +
    ops.reduce((total, op) => total + opSize(op), 0);
  const output = Buffer.allocUnsafe(bytes);
  let offset = 0;
  const put = (value) => {
    Buffer.from(value).copy(output, offset);
    offset += value.length;
  };
  const u8 = (value) => {
    output.writeUInt8(value, offset);
    offset += 1;
  };
  const u16 = (value) => {
    output.writeUInt16LE(value, offset);
    offset += 2;
  };
  const u32 = (value) => {
    output.writeUInt32LE(value, offset);
    offset += 4;
  };
  const u64 = (value) => {
    output.writeBigUInt64LE(BigInt(value), offset);
    offset += 8;
  };
  put(Buffer.from("MEF6"));
  u16(1);
  u16(0);
  u32(1_000_000);
  u32(100_000);
  u32(400_000);
  u32(536_870_912);
  u32(536_870_912);
  u32(records.size);
  u32(events.length);
  u32(ops.length);
  put(baseRoot);
  for (const [hash, value] of records) {
    u8(value.__kind === "Leaf" ? 1 : 2);
    put(Buffer.from(hash, "hex"));
    u8(value.prefix.length);
    put(prefixBytes(value.prefix));
    if (value.__kind === "Leaf") {
      const keyBytes = Buffer.from(value.key, "hex");
      const valueBytes = Buffer.from(value.value, "hex");
      u16(keyBytes.length);
      u32(valueBytes.length);
      put(keyBytes);
      put(valueBytes);
    } else {
      u64(value.size);
      const bitmap = value.children.reduce(
        (bits, child, index) => bits | (child == null ? 0 : 1 << index),
        0,
      );
      u16(bitmap);
      for (const child of value.children) {
        if (child != null) put(Buffer.from(child, "hex"));
      }
    }
  }
  for (const event of events) {
    u32(event.length);
    for (const op of event) {
      u8(op.type === "insert" ? 1 : 2);
      u16(op.key.length);
      u32(op.type === "insert" ? op.value.length : 0);
      put(op.key);
      if (op.type === "insert") put(op.value);
    }
  }
  assert.equal(offset, output.length);
  return output;
};

const encodeEventStream = ({
  baseRoot,
  events,
  maxEvents = 100_000,
  maxOps = 400_000,
}) => {
  const ops = events.flat();
  const bytes =
    92 +
    events.length * 4 +
    ops.reduce((total, op) => total + opSize(op), 0);
  const output = Buffer.allocUnsafe(bytes);
  let offset = 0;
  const put = (value) => {
    Buffer.from(value).copy(output, offset);
    offset += value.length;
  };
  const u8 = (value) => {
    output.writeUInt8(value, offset);
    offset += 1;
  };
  const u16 = (value) => {
    output.writeUInt16LE(value, offset);
    offset += 2;
  };
  const u32 = (value) => {
    output.writeUInt32LE(value, offset);
    offset += 4;
  };
  put(Buffer.from("MEGO"));
  u16(1);
  u16(0);
  u32(events.length);
  u32(ops.length);
  u32(maxEvents);
  u32(maxOps);
  u32(536_870_912);
  put(baseRoot);
  const digestOffset = offset;
  put(Buffer.alloc(32));
  for (const event of events) {
    u32(event.length);
    for (const op of event) {
      u8(op.type === "insert" ? 1 : 2);
      u16(op.key.length);
      u32(op.type === "insert" ? op.value.length : 0);
      put(op.key);
      if (op.type === "insert") put(op.value);
    }
  }
  assert.equal(offset, output.length);
  digest(
    Buffer.from("MIDGARD-MPF-ARCH-G-EVENTS-V1"),
    output.subarray(8, 28),
    baseRoot,
    output.subarray(92),
  ).copy(output, digestOffset);
  return output;
};

const decodeRootStream = (raw) => {
  const output = Buffer.from(raw);
  assert.equal(output.subarray(0, 4).toString(), "MEGR");
  assert.equal(output.readUInt16LE(4), 1);
  assert.equal(output.readUInt16LE(6), 0);
  const eventCount = output.readUInt32LE(8);
  assert.equal(output.length, 108 + eventCount * 32);
  const baseRoot = output.subarray(12, 44);
  const candidateRoot = output.subarray(44, 76);
  const rootDigest = output.subarray(76, 108);
  const rootsBytes = output.subarray(108);
  assert.deepEqual(
    rootDigest,
    digest(
      Buffer.from("MIDGARD-MPF-ARCH-G-ROOTS-V1"),
      baseRoot,
      candidateRoot,
      rootsBytes,
    ),
  );
  const eventRoots = Array.from({ length: eventCount }, (_, index) =>
    rootsBytes.subarray(index * 32, (index + 1) * 32).toString("hex"),
  );
  assert.equal(
    candidateRoot.toString("hex"),
    eventRoots.at(-1) ?? baseRoot.toString("hex"),
  );
  return {
    eventCount,
    outputBytes: output.length,
    baseRoot: baseRoot.toString("hex"),
    candidateRoot: candidateRoot.toString("hex"),
    rootDigest: rootDigest.toString("hex"),
    eventRoots,
  };
};

const decodeOutput = (raw, { includeEventRoots = true } = {}) => {
  const output = Buffer.from(raw);
  assert.equal(output.subarray(0, 4).toString(), "MEFO");
  assert.equal(output.readUInt16LE(4), 1);
  assert.equal(output.readUInt16LE(6), 0);
  const eventCount = output.readUInt32LE(8);
  const deltaRecordCount = output.readUInt32LE(12);
  const deltaOffset = output.readUInt32LE(16);
  const deltaBytes = output.readUInt32LE(20);
  assert.equal(deltaOffset, 120 + eventCount * 32);
  assert.equal(output.length, deltaOffset + deltaBytes);
  const aggregateCounts = Buffer.alloc(12);
  aggregateCounts.writeUInt32LE(eventCount, 0);
  aggregateCounts.writeUInt32LE(deltaRecordCount, 4);
  aggregateCounts.writeUInt32LE(deltaBytes, 8);
  const expectedDigest = digest(
    Buffer.from("MIDGARD-MPF-ARCH-F-DELTA-V1"),
    aggregateCounts,
    output.subarray(24, 56),
    output.subarray(56, 88),
    output.subarray(120, deltaOffset),
    output.subarray(deltaOffset),
  );
  assert.deepEqual(output.subarray(88, 120), expectedDigest);
  const records = new Map();
  const deltaKinds = { leaves: 0, branches: 0, leafBytes: 0, branchBytes: 0 };
  let recordOffset = deltaOffset;
  let previousHash;
  const take = (length) => {
    const end = recordOffset + length;
    assert.ok(end <= output.length, "delta record exceeds output");
    const value = output.subarray(recordOffset, end);
    recordOffset = end;
    return value;
  };
  for (let recordIndex = 0; recordIndex < deltaRecordCount; recordIndex += 1) {
    const recordStartedAt = recordOffset;
    const kind = take(1).readUInt8();
    const hash = take(32);
    const prefixLength = take(1).readUInt8();
    const prefix = [...take(prefixLength)]
      .map((nibble) => nibble.toString(16))
      .join("");
    let children = [];
    if (kind === 1) {
      const keyLength = take(2).readUInt16LE();
      const valueLength = take(4).readUInt32LE();
      const keyBytes = take(keyLength);
      const value = take(valueLength);
      assert.ok(pathOf(keyBytes).endsWith(prefix));
      assert.deepEqual(leafHash(prefix, value), hash);
      deltaKinds.leaves += 1;
      deltaKinds.leafBytes += recordOffset - recordStartedAt;
    } else {
      assert.equal(kind, 2);
      const size = take(8).readBigUInt64LE();
      assert.ok(size >= 2n && size <= BigInt(Number.MAX_SAFE_INTEGER));
      const bitmap = take(2).readUInt16LE();
      assert.ok(bitmap.toString(2).replaceAll("0", "").length >= 2);
      children = Array.from({ length: 16 }, (_, index) =>
        bitmap & (1 << index) ? take(32).toString("hex") : null,
      );
      assert.deepEqual(branchHash(prefix, children), hash);
      deltaKinds.branches += 1;
      deltaKinds.branchBytes += recordOffset - recordStartedAt;
    }
    const hashHex = hash.toString("hex");
    if (previousHash !== undefined) assert.ok(previousHash < hashHex);
    previousHash = hashHex;
    assert.ok(!records.has(hashHex), "delta contains a duplicate record");
    records.set(hashHex, children);
  }
  assert.equal(recordOffset, output.length);
  const candidateRoot = output.subarray(56, 88).toString("hex");
  const reachable = new Set();
  const pending = records.size === 0 ? [] : [candidateRoot];
  while (pending.length > 0) {
    const hash = pending.pop();
    if (reachable.has(hash)) continue;
    const children = records.get(hash);
    assert.notEqual(children, undefined, "delta closure is missing a dirty child");
    reachable.add(hash);
    for (const child of children) {
      if (child != null && records.has(child)) pending.push(child);
    }
  }
  assert.equal(reachable.size, records.size);
  return {
    outputBytes: output.length,
    eventCount,
    deltaRecordCount,
    deltaBytes,
    baseRoot: output.subarray(24, 56).toString("hex"),
    candidateRoot,
    deltaDigest: output.subarray(88, 120).toString("hex"),
    firstEventRoot:
      eventCount === 0 ? undefined : output.subarray(120, 152).toString("hex"),
    finalEventRoot:
      eventCount === 0
        ? output.subarray(56, 88).toString("hex")
        : output.subarray(deltaOffset - 32, deltaOffset).toString("hex"),
    eventRoots: includeEventRoots
      ? Array.from({ length: eventCount }, (_, index) =>
          output
            .subarray(120 + index * 32, 152 + index * 32)
            .toString("hex"),
        )
      : undefined,
    deltaKinds,
  };
};

const commonPrefix = (left, right) => {
  let index = 0;
  while (index < left.length && left[index] === right[index]) index += 1;
  return left.slice(0, index);
};

const forestryRoots = async (initialEntries, events) => {
  const { Trie } = await import("@aiken-lang/merkle-patricia-forestry");
  const trie =
    initialEntries.length === 0
      ? new Trie()
      : await Trie.fromList(initialEntries);
  const roots = [];
  for (const event of events) {
    for (const op of event) {
      if (op.type === "insert") await trie.insert(op.key, op.value);
      else await trie.delete(op.key);
    }
    roots.push(Buffer.from(trie.hash ?? EMPTY_ROOT).toString("hex"));
  }
  return roots;
};

const runAdversarial = async () => {
  const key1 = Buffer.from(
    "5252525252525252525252525252525252525252525252525252525200000001",
    "hex",
  );
  const key2 = Buffer.from(
    "525252525252525252525252525252525252525252525252525252520000ffff",
    "hex",
  );
  const path1 = pathOf(key1);
  const path2 = pathOf(key2);
  const prefix = commonPrefix(path1, path2);
  const value1 = Buffer.from([1]);
  const value2 = Buffer.from([0xff]);
  const leaf1 = {
    __kind: "Leaf",
    prefix: path1.slice(prefix.length + 1),
    key: key1.toString("hex"),
    value: value1.toString("hex"),
  };
  const leaf2 = {
    __kind: "Leaf",
    prefix: path2.slice(prefix.length + 1),
    key: key2.toString("hex"),
    value: value2.toString("hex"),
  };
  const leaf1Hash = leafHash(leaf1.prefix, value1);
  const leaf2Hash = leafHash(leaf2.prefix, value2);
  const children = Array(16).fill(null);
  children[Number.parseInt(path1[prefix.length], 16)] = leaf1Hash.toString("hex");
  children[Number.parseInt(path2[prefix.length], 16)] = leaf2Hash.toString("hex");
  const root = branchHash(prefix, children);
  assert.equal(
    root.toString("hex"),
    "9dfd44a583246956f207b9ce1560209cde64a8455645bce42c37f0065d8bca06",
  );
  const records = new Map([
    [root.toString("hex"), { __kind: "Branch", prefix, children, size: 2 }],
    [leaf1Hash.toString("hex"), leaf1],
    [leaf2Hash.toString("hex"), leaf2],
  ]);
  const numbered = (suffix) => {
    const result = Buffer.from(key1);
    result.writeUInt32BE(suffix, 28);
    return result;
  };
  const events = [
    [],
    [
      { type: "delete", key: key1 },
      { type: "insert", key: key1, value: Buffer.from([2]) },
    ],
    [
      { type: "delete", key: key1 },
      { type: "insert", key: numbered(2), value: Buffer.from([3]) },
    ],
    [
      { type: "delete", key: numbered(2) },
      { type: "insert", key: numbered(3), value: Buffer.from([4]) },
    ],
    [{ type: "insert", key: numbered(4), value: Buffer.from([5]) }],
  ];
  const input = encodeInput({ baseRoot: root, records, events });
  const rawResult = Buffer.from(wasm.run_architecture_f(input));
  const result = decodeOutput(rawResult);
  const expectedRoots = [
    "9dfd44a583246956f207b9ce1560209cde64a8455645bce42c37f0065d8bca06",
    "2856bdd5381b7939fc37aa0f16addb4c634370bab7255826301988137de954ae",
    "6840b783da8ad8a8c75a6be0bbcd28eed25dc9a7cff2c9e2a69e5583ebd155f4",
    "34bfe74a66fb23c5ed1a005fdf20818152b09a3c40feadf3cd3c80082bb4d205",
    "575cf1d0ff54e923a8496c9ce5de01fa2fb2b1ba03afd21f9276b0cf583300d6",
  ];
  assert.deepEqual(result.eventRoots, expectedRoots);
  assert.deepEqual(
    await forestryRoots(
      [
        { key: key1, value: value1 },
        { key: key2, value: value2 },
      ],
      events,
    ),
    expectedRoots,
  );
  const sessionSetup = encodeInput({ baseRoot: root, records, events: [] });
  const session = new wasm.ArchitectureGSession(sessionSetup);
  assert.equal(Buffer.from(session.base_root()).toString("hex"), root.toString("hex"));
  const firstHandle = session.fork_generation();
  const replayHandle = session.fork_generation();
  assert.equal(session.active_generations(), 2);
  assert.throws(() => session.fork_generation());
  const eventStream = encodeEventStream({ baseRoot: root, events });
  const firstRoots = decodeRootStream(
    session.apply_events_roots_only(firstHandle, eventStream),
  );
  const replayRoots = decodeRootStream(
    session.apply_events_roots_only(replayHandle, eventStream),
  );
  assert.deepEqual(firstRoots.eventRoots, expectedRoots);
  assert.deepEqual(replayRoots, firstRoots);
  assert.throws(() =>
    session.apply_events_roots_only(firstHandle, eventStream),
  );
  session.discard_generation(replayHandle);
  assert.throws(() => session.generation_root(replayHandle));
  session.discard_generation(firstHandle);
  assert.equal(session.active_generations(), 0);
  assert.throws(() =>
    session.apply_events_roots_only(firstHandle, eventStream),
  );
  const failureHandle = session.fork_generation();
  const invalidStream = encodeEventStream({
    baseRoot: root,
    events: [[{ type: "delete", key: Buffer.alloc(32, 0xee) }]],
  });
  assert.throws(() =>
    session.apply_events_roots_only(failureHandle, invalidStream),
  );
  assert.equal(
    Buffer.from(session.generation_root(failureHandle)).toString("hex"),
    root.toString("hex"),
  );
  const corruptedStream = Buffer.from(eventStream);
  corruptedStream[corruptedStream.length - 1] ^= 1;
  assert.throws(() =>
    session.apply_events_roots_only(failureHandle, corruptedStream),
  );
  const cappedStream = encodeEventStream({
    baseRoot: root,
    events,
    maxEvents: 0,
  });
  assert.throws(() =>
    session.apply_events_roots_only(failureHandle, cappedStream),
  );
  session.discard_generation(failureHandle);
  const corrupted = Buffer.from(input);
  corrupted[73] ^= 1;
  assert.throws(() => wasm.run_architecture_f(corrupted));
  const corruptedArtifact = Buffer.from(rawResult);
  corruptedArtifact[corruptedArtifact.length - 1] ^= 1;
  assert.throws(() => decodeOutput(corruptedArtifact));

  const byPrefix = new Map();
  let longPair;
  for (let index = 0; index < 100_000 && longPair === undefined; index += 1) {
    const candidate = key(index);
    const hashedPrefix = pathOf(candidate).slice(0, 6);
    const previous = byPrefix.get(hashedPrefix);
    if (previous === undefined) byPrefix.set(hashedPrefix, candidate);
    else longPair = [previous, candidate];
  }
  if (longPair === undefined) throw new Error("Unable to make long-prefix fixture");
  const thirdKey = Buffer.alloc(32, 0xff);
  const longEvents = [
    [
      { type: "insert", key: longPair[0], value: Buffer.alloc(16, 1) },
      { type: "insert", key: longPair[1], value: Buffer.alloc(16, 2) },
      { type: "insert", key: thirdKey, value: Buffer.alloc(16, 3) },
    ],
    [{ type: "delete", key: longPair[0] }],
    [{ type: "insert", key: longPair[0], value: Buffer.alloc(16, 4) }],
  ];
  const longInput = encodeInput({
    baseRoot: EMPTY_ROOT,
    records: new Map(),
    events: longEvents,
  });
  const longResult = decodeOutput(wasm.run_architecture_f(longInput));
  assert.deepEqual(
    longResult.eventRoots,
    await forestryRoots([], longEvents),
  );
  process.stdout.write(
    `${JSON.stringify({
      mode: "adversarial",
      ...result,
      failClosedCorruption: true,
      artifactCorruptionRejected: true,
      liveForestryDifferential: true,
      longPrefixNibbles: 6,
      longPrefixFinalRoot: longResult.candidateRoot,
      retainedSessionRootsExact: true,
      forkReplayExact: true,
      staleHandleRejected: true,
      generationCapRejected: true,
      failedMutationRolledBack: true,
      eventStreamCorruptionRejected: true,
    })}\n`,
  );
};

const makeProbeEvents = (initialUtxos, transactions) =>
  Array.from({ length: transactions }, (_, index) => [
    { type: "delete", key: key(index) },
    {
      type: "insert",
      key: key(initialUtxos + index),
      value: Buffer.alloc(64, (index + 1) % 251),
    },
  ]);

const runLevel = async ({ levelPath, initialUtxos, transactions }) => {
  const expected = EXPECTED.get(initialUtxos);
  if (expected === undefined) throw new Error("Only exact 100k/1M fixtures are accepted");
  const events = makeProbeEvents(initialUtxos, transactions);
  const ops = events.flat();
  const db = new Level(levelPath, { valueEncoding: "json" });
  await db.open();
  try {
    const marker = await db.get("__root__", { valueEncoding: "json" });
    assert.equal(marker, expected.fixtureRoot);
    const fetchStartedAt = performance.now();
    const proof = await loadRawTouchedProof(db, Buffer.from(marker, "hex"), ops);
    const fetchMs = performance.now() - fetchStartedAt;
    const encodeStartedAt = performance.now();
    const input = encodeInput({
      baseRoot: Buffer.from(marker, "hex"),
      records: proof.records,
      events,
    });
    const encodeMs = performance.now() - encodeStartedAt;
    const wasmStartedAt = performance.now();
    const result = decodeOutput(wasm.run_architecture_f(input), {
      includeEventRoots: false,
    });
    const wasmMs = performance.now() - wasmStartedAt;
    assert.equal(result.baseRoot, marker);
    assert.equal(result.candidateRoot, expected.candidateRoot);
    assert.equal(result.finalEventRoot, expected.candidateRoot);
    assert.equal(result.eventCount, transactions);
    assert.equal(await db.get("__root__", { valueEncoding: "json" }), marker);
    const architectureFMs = fetchMs + encodeMs + wasmMs;
    process.stdout.write(
      `${JSON.stringify({
        mode: "level",
        levelPath,
        initialUtxos,
        transactions,
        fixtureRoot: marker,
        rawRecords: proof.records.size,
        fetchMs,
        levelGetMs: proof.levelGetMs,
        levelGetManyCalls: proof.levelGetManyCalls,
        maxBatchKeys: proof.maxBatchKeys,
        encodeMs,
        inputBytes: input.length,
        wasmMs,
        architectureFMs,
        scratchRootUpperMs: expected.scratchRootUpperMs,
        projectedBuildMs: architectureFMs + expected.scratchRootUpperMs,
        maxRssKiB: process.resourceUsage().maxRSS,
        ...result,
      })}\n`,
    );
  } finally {
    await db.close();
  }
};

const runLevelSession = async ({ levelPath, initialUtxos, transactions }) => {
  const expected = EXPECTED.get(initialUtxos);
  if (expected === undefined) throw new Error("Only exact 100k/1M fixtures are accepted");
  const events = makeProbeEvents(initialUtxos, transactions);
  const ops = events.flat();
  const db = new Level(levelPath, { valueEncoding: "json" });
  await db.open();
  try {
    const marker = await db.get("__root__", { valueEncoding: "json" });
    assert.equal(marker, expected.fixtureRoot);
    const fetchStartedAt = performance.now();
    const proof = await loadRawTouchedProof(db, Buffer.from(marker, "hex"), ops);
    const setupFetchMs = performance.now() - fetchStartedAt;

    const baseEncodeStartedAt = performance.now();
    const baseInput = encodeInput({
      baseRoot: Buffer.from(marker, "hex"),
      records: proof.records,
      events: [],
    });
    const setupBaseEncodeMs = performance.now() - baseEncodeStartedAt;
    const sessionStartedAt = performance.now();
    const session = new wasm.ArchitectureGSession(baseInput);
    const setupSessionMs = performance.now() - sessionStartedAt;
    const forkStartedAt = performance.now();
    const handle = session.fork_generation();
    const setupForkMs = performance.now() - forkStartedAt;

    const referenceEncodeStartedAt = performance.now();
    const referenceInput = encodeInput({
      baseRoot: Buffer.from(marker, "hex"),
      records: proof.records,
      events,
    });
    const referenceEncodeMs = performance.now() - referenceEncodeStartedAt;
    const referenceStartedAt = performance.now();
    const reference = decodeOutput(wasm.run_architecture_f(referenceInput));
    const referenceMs = performance.now() - referenceStartedAt;
    assert.equal(reference.candidateRoot, expected.candidateRoot);

    const eventEncodeStartedAt = performance.now();
    const eventInput = encodeEventStream({
      baseRoot: Buffer.from(marker, "hex"),
      events,
    });
    const eventEncodeMs = performance.now() - eventEncodeStartedAt;
    const applyStartedAt = performance.now();
    const result = decodeRootStream(
      session.apply_events_roots_only(handle, eventInput),
    );
    const applyAndRootOutputMs = performance.now() - applyStartedAt;
    assert.deepEqual(result.eventRoots, reference.eventRoots);
    assert.equal(result.baseRoot, marker);
    assert.equal(result.candidateRoot, expected.candidateRoot);
    assert.equal(await db.get("__root__", { valueEncoding: "json" }), marker);
    const hotMs = eventEncodeMs + applyAndRootOutputMs;
    const projectedBuildMs = hotMs + expected.scratchRootUpperMs;
    session.discard_generation(handle);
    process.stdout.write(
      `${JSON.stringify({
        mode: "session",
        levelPath,
        initialUtxos,
        transactions,
        fixtureRoot: marker,
        setup: {
          rawRecords: proof.records.size,
          fetchMs: setupFetchMs,
          levelGetMs: proof.levelGetMs,
          levelGetManyCalls: proof.levelGetManyCalls,
          maxBatchKeys: proof.maxBatchKeys,
          baseEncodeMs: setupBaseEncodeMs,
          baseInputBytes: baseInput.length,
          authenticateSessionMs: setupSessionMs,
          forkMs: setupForkMs,
          referenceEncodeMs,
          referenceInputBytes: referenceInput.length,
          referenceMs,
        },
        timed: {
          eventEncodeMs,
          eventInputBytes: eventInput.length,
          applyAndRootOutputMs,
          rootOutputBytes: result.outputBytes,
          hotMs,
          scratchRootUpperMs: expected.scratchRootUpperMs,
          projectedBuildMs,
        },
        everyEventRootExact: true,
        markerUnchanged: true,
        baseRoot: result.baseRoot,
        candidateRoot: result.candidateRoot,
        firstEventRoot: result.eventRoots[0],
        finalEventRoot: result.eventRoots.at(-1),
        rootDigest: result.rootDigest,
        maxRssKiB: process.resourceUsage().maxRSS,
      })}\n`,
    );
  } finally {
    await db.close();
  }
};

const args = new Map(
  process.argv.slice(2).map((argument) => {
    const [name, value = "true"] = argument.split("=", 2);
    return [name, value];
  }),
);
if (args.has("--level")) {
  const run = args.has("--session") ? runLevelSession : runLevel;
  await run({
    levelPath: args.get("--level"),
    initialUtxos: Number.parseInt(args.get("--utxos"), 10),
    transactions: Number.parseInt(args.get("--txs") ?? "10000", 10),
  });
} else {
  await runAdversarial();
}
