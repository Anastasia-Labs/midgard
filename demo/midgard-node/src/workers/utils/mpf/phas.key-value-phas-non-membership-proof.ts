import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import {
  getMpfScratchBuild,
  MidgardMpf,
  MpfError,
} from "../../../mpf/index.js";

export type KeyValuePhasEntry = {
  readonly key: Buffer;
  readonly value: Buffer;
};

export type KeyValuePhasRoot = {
  readonly root: string;
  readonly count: bigint;
  readonly entries: readonly KeyValuePhasEntry[];
};

const toPhasTrieItem = (keyCbor: Buffer, valueCbor: Buffer) => ({
  key: keyCbor,
  value: valueCbor,
});

const compareBuffer = (left: Buffer, right: Buffer): number =>
  Buffer.compare(left, right);

const createPhasScratch = (
  trieName: string,
  entries: readonly KeyValuePhasEntry[],
): Effect.Effect<MidgardMpf, MpfError> =>
  getMpfScratchBuild() === "fromlist"
    ? MidgardMpf.createScratchFromList(trieName, entries)
    : Effect.gen(function* () {
        const mpf = yield* MidgardMpf.createScratch(trieName);
        yield* mpf.applyBatch(
          entries.map((item) => ({
            type: "insert" as const,
            key: item.key,
            value: item.value,
          })),
        );
        return mpf;
      });

const canonicalizeKeyValuePhasEntriesSync = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): readonly KeyValuePhasEntry[] => {
  if (keys.length !== values.length) {
    throw new Error(
      `Cannot build PHAS root for ${keys.length} keys and ${values.length} values`,
    );
  }

  const entries = keys
    .map((key, index) => toPhasTrieItem(key, values[index]!))
    .sort((left, right) => compareBuffer(left.key, right.key));
  const seen = new Set<string>();
  for (const entry of entries) {
    const keyHex = entry.key.toString("hex");
    if (seen.has(keyHex)) {
      throw new Error(`Cannot build PHAS root with duplicate key ${keyHex}`);
    }
    seen.add(keyHex);
  }
  return entries;
};

export const canonicalizeKeyValuePhasEntries = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<readonly KeyValuePhasEntry[], MpfError, never> =>
  Effect.try({
    try: () => canonicalizeKeyValuePhasEntriesSync(keys, values),
    catch: (e) => MpfError.phasRoot(e),
  });

export const keyValuePhasRootWithCount = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<KeyValuePhasRoot, MpfError, never> =>
  Effect.gen(function* () {
    const entries = yield* canonicalizeKeyValuePhasEntries(keys, values);
    if (entries.length === 0) {
      return {
        root: SDK.EMPTY_MERKLE_TREE_ROOT,
        count: 0n,
        entries,
      };
    }
    const root =
      getMpfScratchBuild() === "fromlist"
        ? yield* Effect.tryPromise({
            try: async () => {
              const trie = await Trie.fromList(entries);
              return Buffer.from(trie.hash).toString("hex");
            },
            catch: (cause) => MpfError.phasRoot(cause),
          })
        : yield* Effect.gen(function* () {
            const mpf = yield* createPhasScratch("phas-root", entries);
            return yield* mpf.rootHex();
          });
    return {
      root,
      count: BigInt(entries.length),
      entries,
    };
  });

export const keyValuePhasRoot = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<string, MpfError, never> =>
  keyValuePhasRootWithCount(keys, values).pipe(
    Effect.map((result) => result.root),
  );

export const keyValuePhasProof = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
  key: Buffer,
): Effect.Effect<SDK.Proof, MpfError, never> =>
  Effect.gen(function* () {
    const entries = yield* canonicalizeKeyValuePhasEntries(keys, values);
    if (entries.length === 0) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error("Cannot build a PHAS membership proof for an empty tree"),
        ),
      );
    }
    const mpf = yield* createPhasScratch("phas-proof", entries);
    const proof = yield* mpf.prove(key);
    const root = yield* mpf.rootHex();
    const verifiedRoot = yield* mpf
      .verify(proof, true)
      .pipe(Effect.map((verified) => verified.toString("hex")));
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Generated PHAS membership proof does not open committed root: root=${root},verified=${verifiedRoot}`,
          ),
        ),
      );
    }
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          proof.cbor.toString("hex"),
          SDK.Proof as never,
        ) as SDK.Proof,
      catch: (e) => MpfError.phasRoot(e),
    });
  });

export const NON_MEMBERSHIP_DUMMY_VALUE = Buffer.alloc(0);

export const keyValuePhasNonMembershipProof = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
  key: Buffer,
): Effect.Effect<SDK.Proof, MpfError, never> =>
  Effect.gen(function* () {
    const entries = yield* canonicalizeKeyValuePhasEntries(keys, values);
    const keyHex = key.toString("hex");
    if (entries.some((entry) => entry.key.equals(key))) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build a PHAS non-membership proof for present key ${keyHex}`,
          ),
        ),
      );
    }
    const mpf = yield* createPhasScratch("phas-non-membership-proof", entries);
    const root = yield* mpf.rootHex();
    yield* mpf.insert(key, NON_MEMBERSHIP_DUMMY_VALUE);
    const proof = yield* mpf.prove(key);
    const verifiedRoot = yield* mpf
      .verify(proof, false)
      .pipe(Effect.map((verified) => verified.toString("hex")));
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Generated PHAS non-membership proof does not open committed root: root=${root},verified=${verifiedRoot}`,
          ),
        ),
      );
    }
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          proof.cbor.toString("hex"),
          SDK.Proof as never,
        ) as SDK.Proof,
      catch: (e) => MpfError.phasRoot(e),
    });
  });

export const MPF_NULL_ROOT = Buffer.alloc(32);

const MIDGARD_EMPTY_ROOT = Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex");

export const digest = (bytes: Buffer): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

export const normalizeVerifiedPhasRoot = (root: Buffer): Buffer =>
  root.equals(MPF_NULL_ROOT) ? MIDGARD_EMPTY_ROOT : root;

export const nibbles = (hexDigits: string): Buffer =>
  Buffer.from(
    [...hexDigits].map((digit) => {
      const nibble = Number.parseInt(digit, 16);
      if (!Number.isInteger(nibble) || nibble < 0 || nibble > 15) {
        throw new Error(`Invalid MPF path nibble ${digit}`);
      }
      return nibble;
    }),
  );

export const computeLeafHash = (
  prefix: string,
  valueDigest: Buffer,
): Buffer => {
  const head =
    prefix.length % 2 > 0
      ? Buffer.concat([Buffer.from([0]), nibbles(prefix.slice(0, 1))])
      : Buffer.from([255]);
  const tail = Buffer.from(
    prefix.length % 2 > 0 ? prefix.slice(1) : prefix,
    "hex",
  );
  return digest(Buffer.concat([head, tail, valueDigest]));
};

export const computeBranchHash = (prefix: string, root: Buffer): Buffer =>
  digest(Buffer.concat([nibbles(prefix), root]));

const hashPair = (left: Buffer, right: Buffer): Buffer =>
  digest(Buffer.concat([left, right]));

export const branchRootFromNeighbors = (
  nibble: number,
  root: Buffer,
  neighbors: Buffer,
): Buffer => {
  if (neighbors.length !== 128) {
    throw new Error(
      `Branch proof neighbors must be 128 bytes, got ${neighbors.length}`,
    );
  }
  const siblings = [
    neighbors.subarray(96, 128),
    neighbors.subarray(64, 96),
    neighbors.subarray(32, 64),
    neighbors.subarray(0, 32),
  ];
  return siblings.reduce(
    (current, sibling, level) =>
      ((nibble >> level) & 1) === 0
        ? hashPair(current, sibling)
        : hashPair(sibling, current),
    root,
  );
};

export const parseProofInteger = (
  value: bigint | number,
  label: string,
): number => {
  const parsed = typeof value === "bigint" ? Number(value) : value;
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return parsed;
};

export const proofBytes = (value: string, label: string): Buffer => {
  if (!/^[0-9a-fA-F]*$/.test(value) || value.length % 2 !== 0) {
    throw new Error(`${label} must be even-length hex bytes`);
  }
  return Buffer.from(value, "hex");
};

export type PhasProofTraversalMode =
  | { readonly kind: "including"; readonly valueDigest: Buffer }
  | { readonly kind: "excluding" };

export const merkleRoot16 = (nodesByNibble: Record<number, Buffer>): Buffer => {
  let nodes = Array.from(
    { length: 16 },
    (_, index) => nodesByNibble[index] ?? MPF_NULL_ROOT,
  );
  while (nodes.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < nodes.length; index += 2) {
      next.push(hashPair(nodes[index]!, nodes[index + 1]!));
    }
    nodes = next;
  }
  return nodes[0]!;
};
