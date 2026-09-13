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

const NON_MEMBERSHIP_DUMMY_VALUE = Buffer.alloc(0);

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

const MPF_NULL_ROOT = Buffer.alloc(32);
const MIDGARD_EMPTY_ROOT = Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex");

const digest = (bytes: Buffer): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const normalizeVerifiedPhasRoot = (root: Buffer): Buffer =>
  root.equals(MPF_NULL_ROOT) ? MIDGARD_EMPTY_ROOT : root;

const nibbles = (hexDigits: string): Buffer =>
  Buffer.from(
    [...hexDigits].map((digit) => {
      const nibble = Number.parseInt(digit, 16);
      if (!Number.isInteger(nibble) || nibble < 0 || nibble > 15) {
        throw new Error(`Invalid MPF path nibble ${digit}`);
      }
      return nibble;
    }),
  );

const computeLeafHash = (prefix: string, valueDigest: Buffer): Buffer => {
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

const computeBranchHash = (prefix: string, root: Buffer): Buffer =>
  digest(Buffer.concat([nibbles(prefix), root]));

const hashPair = (left: Buffer, right: Buffer): Buffer =>
  digest(Buffer.concat([left, right]));

const branchRootFromNeighbors = (
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

const parseProofInteger = (value: bigint | number, label: string): number => {
  const parsed = typeof value === "bigint" ? Number(value) : value;
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return parsed;
};

const proofBytes = (value: string, label: string): Buffer => {
  if (!/^[0-9a-fA-F]*$/.test(value) || value.length % 2 !== 0) {
    throw new Error(`${label} must be even-length hex bytes`);
  }
  return Buffer.from(value, "hex");
};

type PhasProofTraversalMode =
  | { readonly kind: "including"; readonly valueDigest: Buffer }
  | { readonly kind: "excluding" };

const traversePhasProof = (
  mode: PhasProofTraversalMode,
  path: string,
  cursor: number,
  proof: readonly SDK.ProofStep[],
  index: number,
): Buffer => {
  const step = proof[index];
  if (step === undefined) {
    return mode.kind === "including"
      ? computeLeafHash(path.slice(cursor), mode.valueDigest)
      : MPF_NULL_ROOT;
  }
  if ("Branch" in step) {
    const skip = parseProofInteger(step.Branch.skip, "branch skip");
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    const prefix = path.slice(cursor, nextCursor - 1);
    return computeBranchHash(
      prefix,
      branchRootFromNeighbors(
        thisNibble,
        root,
        proofBytes(step.Branch.neighbors, "branch neighbors"),
      ),
    );
  }
  if ("Fork" in step) {
    const skip = parseProofInteger(step.Fork.skip, "fork skip");
    const neighbor = step.Fork.neighbor;
    if (mode.kind === "excluding" && proof[index + 1] === undefined) {
      const prefixParts =
        skip === 0
          ? [
              Buffer.from([
                parseProofInteger(neighbor.nibble, "fork neighbor nibble"),
              ]),
              proofBytes(neighbor.prefix, "fork neighbor prefix"),
            ]
          : [
              nibbles(path.slice(cursor, cursor + skip)),
              Buffer.from([
                parseProofInteger(neighbor.nibble, "fork neighbor nibble"),
              ]),
              proofBytes(neighbor.prefix, "fork neighbor prefix"),
            ];
      return digest(
        Buffer.concat([
          ...prefixParts,
          proofBytes(neighbor.root, "fork neighbor root"),
        ]),
      );
    }
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    const neighborNibble = parseProofInteger(
      neighbor.nibble,
      mode.kind === "including" ? "fork nibble" : "fork neighbor nibble",
    );
    if (neighborNibble === thisNibble) {
      throw new Error("Fork proof neighbor uses the proven path nibble");
    }
    const nodes: Record<number, Buffer> = {
      [thisNibble]: root,
      [neighborNibble]: digest(
        Buffer.concat([
          proofBytes(neighbor.prefix, "fork neighbor prefix"),
          proofBytes(neighbor.root, "fork neighbor root"),
        ]),
      ),
    };
    return computeBranchHash(
      path.slice(cursor, nextCursor - 1),
      merkleRoot16(nodes),
    );
  }
  if ("Leaf" in step) {
    const neighborPath = proofBytes(
      step.Leaf.key,
      "leaf neighbor key",
    ).toString("hex");
    if (mode.kind === "excluding" && proof[index + 1] === undefined) {
      return computeLeafHash(
        neighborPath.slice(cursor),
        proofBytes(step.Leaf.value, "leaf neighbor value"),
      );
    }
    const skip = parseProofInteger(step.Leaf.skip, "leaf skip");
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    if (
      mode.kind === "including" &&
      neighborPath.slice(0, cursor) !== path.slice(0, cursor)
    ) {
      throw new Error("Leaf proof neighbor is not under the expected prefix");
    }
    const neighborNibble =
      mode.kind === "including"
        ? Number.parseInt(neighborPath[nextCursor - 1]!, 16)
        : Number.parseInt(neighborPath[cursor]!, 16);
    if (neighborNibble === thisNibble) {
      throw new Error("Leaf proof neighbor uses the proven path nibble");
    }
    const nodes: Record<number, Buffer> = {
      [thisNibble]: root,
      [neighborNibble]: computeLeafHash(
        neighborPath.slice(nextCursor),
        proofBytes(step.Leaf.value, "leaf neighbor value"),
      ),
    };
    return computeBranchHash(
      path.slice(cursor, nextCursor - 1),
      merkleRoot16(nodes),
    );
  }
  throw new Error("Unknown PHAS proof step");
};

const merkleRoot16 = (nodesByNibble: Record<number, Buffer>): Buffer => {
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

export const rootFromPhasProof = ({
  key,
  value,
  proof,
  includingItem,
}: {
  readonly key: Buffer;
  readonly value?: Buffer;
  readonly proof: SDK.Proof;
  readonly includingItem: boolean;
}): Effect.Effect<string, MpfError, never> =>
  Effect.try({
    try: () => {
      if (includingItem && value === undefined) {
        throw new Error("PHAS membership proof verification requires a value");
      }
      const path = digest(key).toString("hex");
      const root = includingItem
        ? traversePhasProof(
            { kind: "including", valueDigest: digest(value!) },
            path,
            0,
            proof,
            0,
          )
        : traversePhasProof({ kind: "excluding" }, path, 0, proof, 0);
      return normalizeVerifiedPhasRoot(root).toString("hex");
    },
    catch: (e) => MpfError.phasRoot(e),
  });

export const verifyKeyValuePhasMembershipProof = ({
  root,
  key,
  value,
  proof,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly value: Buffer;
  readonly proof: SDK.Proof;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    const verifiedRoot = yield* rootFromPhasProof({
      key,
      value,
      proof,
      includingItem: true,
    });
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS membership proof root mismatch: expected=${root},actual=${verifiedRoot}`,
          ),
        ),
      );
    }
  });

export const verifyKeyValuePhasNonMembershipProof = ({
  root,
  key,
  proof,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly proof: SDK.Proof;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    const verifiedRoot = yield* rootFromPhasProof({
      key,
      proof,
      includingItem: false,
    });
    yield* rootFromPhasProof({
      key,
      value: NON_MEMBERSHIP_DUMMY_VALUE,
      proof,
      includingItem: true,
    });
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS non-membership proof root mismatch: expected=${root},actual=${verifiedRoot}`,
          ),
        ),
      );
    }
  });
