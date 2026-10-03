import { Proof, Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfProofFoldTrace,
  encodeMidgardMpfProofFrame,
  MIDGARD_MPF_PROOF_FRAME_MAX_BYTES,
  parseMidgardMpfProofJson,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

const exactRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

describe("bounded MPF proof folding V1", () => {
  it("matches atomic proof verification across valid shared-prefix depths", () => {
    const key = Buffer.from("ledger-entry");
    const value = Buffer.from("output");
    const path = Buffer.from(blake2b(key, { dkLen: 32 })).toString("hex");
    const siblingPath = (cursor: number): string =>
      path.slice(0, cursor) +
      ((parseInt(path[cursor]!, 16) + 1) % 16).toString(16) +
      path.slice(cursor + 1);
    for (let skip = 0; skip <= 62; skip += 1) {
      const proofJson = [skip, skip + 1].map((cursor, index) => ({
        type: "leaf",
        skip: index === 0 ? skip : 0,
        neighbor: {
          key: siblingPath(cursor),
          value: Buffer.from(
            blake2b(Buffer.from([index + 1]), { dkLen: 32 }),
          ).toString("hex"),
        },
      }));
      const atomic = Proof.fromJSON(key, value, proofJson);
      const trace = buildMidgardMpfProofFoldTrace({
        key,
        value,
        steps: parseMidgardMpfProofJson(proofJson),
      });
      expect(trace.terminal.includingRoot).toEqual(atomic.verify(true));
      expect(trace.terminal.excludingRoot).toEqual(atomic.verify(false));
    }
  });

  // A smoke anchor against the real trie, not a regression pin for the
  // non-terminal Leaf arm: none of these 4,096 proofs has a non-terminal Leaf
  // step with a nonzero skip, so reverting that fix leaves this test green.
  // The shared-prefix test above is the pin.
  it(
    "matches real trie roots throughout ordinary batch insertion and deletion",
    { timeout: 30_000 },
    async () => {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      const entries = Array.from({ length: 2_048 }, (_, index) => ({
        key: Buffer.from(`ledger-entry-${index}`),
        value: Buffer.from(`output-${index}`),
      }));
      for (const inserting of [true, false]) {
        for (const { key, value } of entries) {
          const before = exactRoot(trie);
          const proof = await trie.prove(key, inserting);
          const steps = parseMidgardMpfProofJson(proof.toJSON());
          const trace = buildMidgardMpfProofFoldTrace({ key, value, steps });
          if (inserting) await trie.insert(key, value);
          else await trie.delete(key);
          expect(trace.terminal.includingRoot).toEqual(
            inserting ? exactRoot(trie) : before,
          );
          expect(trace.terminal.excludingRoot).toEqual(
            inserting ? before : exactRoot(trie),
          );
        }
      }
      expect(exactRoot(trie)).toEqual(Buffer.alloc(32));
    },
  );

  it("reconstructs deletion roots one authenticated frame at a time", async () => {
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    const entries = [
      [Buffer.from("alpha"), Buffer.from("one")],
      [Buffer.from("bravo"), Buffer.from("two")],
      [Buffer.from("charlie"), Buffer.from("three")],
    ] as const;
    for (const [key, value] of entries) {
      await trie.insert(key, value);
    }

    const [key, value] = entries[1]!;
    const preRoot = exactRoot(trie);
    const proof = await trie.prove(key, false);
    const trace = buildMidgardMpfProofFoldTrace({
      key,
      value,
      steps: parseMidgardMpfProofJson(proof.toJSON()),
    });
    await trie.delete(key);

    expect(trace.steps).not.toHaveLength(0);
    expect(
      trace.steps.every(({ membership }) =>
        verifyMidgardValidationMerkleMembership(membership),
      ),
    ).toBe(true);
    expect(
      trace.frames.every(
        (frame) =>
          encodeMidgardMpfProofFrame(frame).length <=
          MIDGARD_MPF_PROOF_FRAME_MAX_BYTES,
      ),
    ).toBe(true);
    expect(trace.terminal.includingRoot).toEqual(preRoot);
    expect(trace.terminal.excludingRoot).toEqual(exactRoot(trie));
    expect(trace.terminal).toMatchObject({
      nextFrameIndex: -1,
      expectedNextCursor: 0,
    });
  });

  it("reconstructs insertion roots, including an empty prior trie", async () => {
    for (const seeded of [false, true]) {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      if (seeded) {
        await trie.insert(Buffer.from("alpha"), Buffer.from("one"));
        await trie.insert(Buffer.from("charlie"), Buffer.from("three"));
      }
      const key = Buffer.from("bravo");
      const value = Buffer.from("two");
      const preRoot = exactRoot(trie);
      const proof = await trie.prove(key, true);
      const trace = buildMidgardMpfProofFoldTrace({
        key,
        value,
        steps: parseMidgardMpfProofJson(proof.toJSON()),
      });
      await trie.insert(key, value);

      expect(trace.terminal.excludingRoot).toEqual(preRoot);
      expect(trace.terminal.includingRoot).toEqual(exactRoot(trie));
      expect(trace.descriptor.frameCount).toBe(trace.frames.length);
      expect(trace.descriptor.terminalCursor).toBe(
        trace.frames.at(-1)?.nextCursor ?? 0,
      );
    }
  });

  it("rejects malformed proof JSON before constructing a frontier", () => {
    expect(() =>
      parseMidgardMpfProofJson([
        {
          type: "branch",
          skip: 0,
          neighbors: "00",
        },
      ]),
    ).toThrow(/exactly 128 bytes/u);
    expect(() =>
      parseMidgardMpfProofJson([
        {
          type: "fork",
          skip: 64,
          neighbor: {
            nibble: 16,
            prefix: "",
            root: "00".repeat(32),
          },
        },
      ]),
    ).toThrow(/canonical integer envelope/u);
  });
});

it("preserves the skipped shared prefix when a terminal fork is compressed", () => {
  const trace = buildMidgardMpfProofFoldTrace({
    key: Buffer.from(
      "abababababababababababababababababababababababababababab001f",
      "hex",
    ),
    value: Buffer.from("01", "hex"),
    steps: parseMidgardMpfProofJson([
      {
        type: "branch",
        skip: 0,
        neighbors:
          "bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857",
      },
      {
        type: "branch",
        skip: 0,
        neighbors:
          "42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a",
      },
      {
        type: "fork",
        skip: 1,
        neighbor: {
          nibble: 8,
          prefix: "",
          root: "2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb",
        },
      },
    ]),
  });
  expect(trace.terminal.includingRoot.toString("hex")).toBe(
    "a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7",
  );
  expect(trace.terminal.excludingRoot.toString("hex")).toBe(
    "aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b",
  );
});

// The terminal frame's neighbour is read two ways: beside the proven path in
// the including root, and as the node the path collapses into in the excluding
// root. path(04) = 6 4 2 2 ..., path(0106) = 6 a 9 5 ..., path(05) = f b 3 d ...
describe("bounded MPF proof folding V1 terminal neighbour", () => {
  const hash = (bytes: Uint8Array): Buffer =>
    Buffer.from(blake2b(bytes, { dkLen: 32 }));
  const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
  const a = Buffer.from("04", "hex");
  const b = Buffer.from("0106", "hex");
  const c = Buffer.from("05", "hex");
  const va = Buffer.from("0a", "hex");
  const vb = Buffer.from("0b", "hex");
  const vc = Buffer.from("0c", "hex");
  // The MPF suffix of a path from `cursor`, as a leaf hash commits it.
  const suffix = (path: Buffer, cursor: number): Buffer =>
    cursor % 2 === 0
      ? Buffer.concat([Buffer.from([0xff]), path.subarray(cursor / 2)])
      : Buffer.concat([
          Buffer.from([0x10, path[(cursor - 1) / 2]! % 16]),
          path.subarray((cursor + 1) / 2),
        ]);
  const leaf = (key: Buffer, value: Buffer, skip: number) => ({
    type: "leaf",
    skip,
    neighbor: { key: hex(key), value: hex(hash(value)) },
  });
  const fork = (
    skip: number,
    nibble: number,
    prefix: Buffer,
    root: Buffer,
  ) => ({
    type: "fork",
    skip,
    neighbor: { nibble, prefix: hex(prefix), root: hex(root) },
  });
  const fold = (proofJson: unknown) =>
    buildMidgardMpfProofFoldTrace({
      key: a,
      value: va,
      steps: parseMidgardMpfProofJson(proofJson),
    });
  const foldDeletion = (proofJson: unknown) =>
    buildMidgardMpfProofFoldTrace({
      key: a,
      value: va,
      steps: parseMidgardMpfProofJson(proofJson),
      deletionOpening: Buffer.alloc(0),
    });
  const trieOf = async (entries: readonly (readonly [Buffer, Buffer])[]) => {
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    for (const [key, value] of entries) await trie.insert(key, value);
    return trie;
  };

  it("folds honest terminal leaves to the real trie roots", async () => {
    const cases = [
      [await trieOf([[b, vb]]), 1],
      [
        await trieOf([
          [b, vb],
          [c, vc],
        ]),
        0,
      ],
    ] as const;
    for (const [trie, terminalSkip] of cases) {
      const before = exactRoot(trie);
      const proofJson = (await trie.prove(a, true)).toJSON() as readonly {
        readonly type: string;
        readonly skip: number;
      }[];
      expect(proofJson.at(-1)).toMatchObject({
        type: "leaf",
        skip: terminalSkip,
      });
      const trace = fold(proofJson);
      await trie.insert(a, va);
      expect(trace.terminal.includingRoot).toEqual(exactRoot(trie));
      expect(trace.terminal.excludingRoot).toEqual(before);
    }
  });

  it("refuses a terminal leaf whose key leaves the skipped prefix", () => {
    const tampered = Buffer.concat([Buffer.from([0x7a]), hash(b).subarray(1)]);
    expect(() => fold([leaf(hash(b), vb, 1)])).not.toThrow();
    expect(() => fold([leaf(tampered, vb, 1)])).toThrow(
      /terminal leaf neighbor does not share the skipped path prefix/u,
    );
  });

  it("refuses a terminal leaf whose skip passes the true divergence", () => {
    expect(() => fold([leaf(hash(b), vb, 2)])).toThrow(
      /terminal leaf neighbor does not share the skipped path prefix/u,
    );
  });

  it("refuses a leaf re-read as a terminal fork on deletion and insertion", async () => {
    // Leaf and branch node preimages are disjoint, so a leaf re-read as a
    // terminal Fork gives roots the honest trie does not have. On a deletion
    // the Fork neighbour must also open like a branch preimage.
    const onlyB = exactRoot(await trieOf([[b, vb]]));
    const bAndC = exactRoot(
      await trieOf([
        [b, vb],
        [c, vc],
      ]),
    );
    const aBAndC = exactRoot(
      await trieOf([
        [a, va],
        [b, vb],
        [c, vc],
      ]),
    );
    const even = [fork(1, 10, suffix(hash(b), 2), hash(vb))];
    const odd = [
      leaf(hash(c), vc, 0),
      fork(0, 0, suffix(hash(b), 1).subarray(1), hash(vb)),
    ];
    expect(() => foldDeletion(even)).toThrow(
      /terminal fork neighbor of a deletion does not open like a branch/u,
    );
    expect(fold(even).terminal.excludingRoot).not.toEqual(onlyB);
    expect(foldDeletion(odd).terminal.includingRoot).not.toEqual(aBAndC);
    expect(fold(odd).terminal.excludingRoot).not.toEqual(bAndC);
  });

  it("bounds a terminal fork prefix by the key path", () => {
    const neighborNibble = ((hash(a)[15]! % 16) + 1) % 16;
    const deepFork = (prefixLength: number) =>
      fork(31, neighborNibble, Buffer.alloc(prefixLength), hash(vc));
    expect(() => fold([deepFork(31)])).not.toThrow();
    expect(() => fold([deepFork(32)])).toThrow(
      /terminal fork neighbor prefix does not end inside the key path/u,
    );
  });
});
