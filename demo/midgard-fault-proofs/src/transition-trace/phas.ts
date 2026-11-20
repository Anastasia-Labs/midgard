import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { transitionTraceError } from "./errors.js";

export type KeyValuePhasEntry = {
  readonly key: Buffer;
  readonly value: Buffer;
};

export type KeyValuePhasRoot = {
  readonly root: string;
  readonly count: bigint;
  readonly entries: readonly KeyValuePhasEntry[];
};

export type CountedRoot = KeyValuePhasRoot & {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
};

const MIDGARD_EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;
const NON_MEMBERSHIP_DUMMY_VALUE = Buffer.alloc(0);

const normalizeRoot = (root: Uint8Array | null | undefined): string => {
  if (root === null || root === undefined) {
    return MIDGARD_EMPTY_ROOT;
  }
  const hex = Buffer.from(root).toString("hex");
  return hex === "00".repeat(32) ? MIDGARD_EMPTY_ROOT : hex;
};

const compareBuffer = (left: Buffer, right: Buffer): number =>
  Buffer.compare(left, right);

export const canonicalizeKeyValuePhasEntries = (
  entries: readonly KeyValuePhasEntry[],
): readonly KeyValuePhasEntry[] => {
  const sorted = entries
    .map((entry) => ({
      key: Buffer.from(entry.key),
      value: Buffer.from(entry.value),
    }))
    .sort((left, right) => compareBuffer(left.key, right.key));
  const seen = new Set<string>();
  for (const entry of sorted) {
    const keyHex = entry.key.toString("hex");
    if (seen.has(keyHex)) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `Cannot build PHAS root with duplicate key ${keyHex}`,
      );
    }
    seen.add(keyHex);
  }
  return sorted;
};

const trieFromEntries = async (
  entries: readonly KeyValuePhasEntry[],
): Promise<Trie> =>
  await Trie.fromList(
    entries.map((entry) => ({
      key: Buffer.from(entry.key),
      value: Buffer.from(entry.value),
    })),
  );

export const keyValuePhasRootWithCount = async (
  entries: readonly KeyValuePhasEntry[],
): Promise<KeyValuePhasRoot> => {
  const canonical = canonicalizeKeyValuePhasEntries(entries);
  if (canonical.length === 0) {
    return {
      root: MIDGARD_EMPTY_ROOT,
      count: 0n,
      entries: canonical,
    };
  }
  const trie = await trieFromEntries(canonical);
  return {
    root: normalizeRoot(trie.hash),
    count: BigInt(canonical.length),
    entries: canonical,
  };
};

export const keyValuePhasRoot = async (
  entries: readonly KeyValuePhasEntry[],
): Promise<string> => (await keyValuePhasRootWithCount(entries)).root;

const sdkProofFromCbor = (proofCbor: Uint8Array): SDK.Proof => {
  try {
    return Data.from(
      Buffer.from(proofCbor).toString("hex"),
      SDK.Proof as never,
    ) as SDK.Proof;
  } catch (cause) {
    throw transitionTraceError(
      "proofConstructionFailed",
      "Generated PHAS proof cannot be decoded as the SDK/Aiken proof schema.",
      cause,
    );
  }
};

export const keyValuePhasProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
  value: Buffer,
): Promise<SDK.Proof> => {
  if (root.entries.length === 0) {
    throw transitionTraceError(
      "missingWitnessData",
      "Cannot build a PHAS membership proof for an empty tree.",
    );
  }
  const trie = await trieFromEntries(root.entries);
  const proof = await trie.prove(Buffer.from(key));
  const verifiedRoot = normalizeRoot(proof.verify(true));
  if (verifiedRoot !== root.root) {
    throw transitionTraceError(
      "proofConstructionFailed",
      `Generated PHAS membership proof does not open committed root: expected=${root.root},actual=${verifiedRoot}`,
    );
  }
  const present = root.entries.some(
    (entry) => entry.key.equals(key) && entry.value.equals(value),
  );
  if (!present) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build a PHAS membership proof for absent key/value ${key.toString(
        "hex",
      )}.`,
    );
  }
  return sdkProofFromCbor(proof.toCBOR());
};

export const keyValuePhasNonMembershipProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
): Promise<SDK.Proof> => {
  const keyHex = key.toString("hex");
  if (root.entries.some((entry) => entry.key.equals(key))) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build a PHAS non-membership proof for present key ${keyHex}.`,
    );
  }
  const trie = await trieFromEntries(root.entries);
  await trie.insert(Buffer.from(key), NON_MEMBERSHIP_DUMMY_VALUE);
  const proof = await trie.prove(Buffer.from(key));
  const verifiedRoot = normalizeRoot(proof.verify(false));
  if (verifiedRoot !== root.root) {
    throw transitionTraceError(
      "proofConstructionFailed",
      `Generated PHAS non-membership proof does not open committed root: expected=${root.root},actual=${verifiedRoot}`,
    );
  }
  return sdkProofFromCbor(proof.toCBOR());
};

export const commitCountedRoot = async ({
  domain,
  phasRoot,
  count,
}: {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
  readonly count: bigint;
}): Promise<string> =>
  await Effect.runPromise(
    SDK.commitCountedRootProgram({ domain, phasRoot, count }),
  );

export const buildCountedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly KeyValuePhasEntry[],
): Promise<CountedRoot> => {
  const phas = await keyValuePhasRootWithCount(entries);
  const root = await commitCountedRoot({
    domain,
    phasRoot: phas.root,
    count: phas.count,
  });
  return {
    ...phas,
    root,
    phasRoot: phas.root,
    domain,
  };
};
