import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  canonicalizeKeyValuePhasEntries,
  type KeyValuePhasEntry,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
  MpfError,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "../../mpf/index.js";
import {
  buildAuthenticatedMpfRootInWorker,
  shouldBuildMpfRootInWorker,
} from "../utils/mpf-root-pool.js";

export type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type EncodedRootEntry = KeyValuePhasEntry;

export type BuiltAuthenticatedRoot = KeyValuePhasRoot & {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
};

export type TypedRootEntry<K, V> = {
  readonly key: K;
  readonly value: V;
};

export type BuiltTypedAuthenticatedRoot<K, V> = BuiltAuthenticatedRoot & {
  readonly typedEntries: readonly TypedRootEntry<K, V>[];
};

export type RootProofVerificationOptions = {
  readonly expectedDomain: SDK.RootDomain;
  readonly expectedRoot?: string;
  readonly expectedCount?: bigint;
};

export const encodeData = <A>(
  value: A,
  schema: DataSchema,
  label: string,
): Effect.Effect<Buffer, MpfError, never> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(value as never, schema as never), "hex"),
    catch: (cause) =>
      MpfError.phasRoot(
        new Error(`Failed to encode ${label} as canonical Plutus data`, {
          cause,
        }),
      ),
  });

const rootEntryVectors = (entries: readonly EncodedRootEntry[]) => ({
  keys: entries.map((entry) => entry.key),
  values: entries.map((entry) => entry.value),
});

export const buildAuthenticatedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly EncodedRootEntry[],
): Effect.Effect<BuiltAuthenticatedRoot, MpfError, never> =>
  Effect.gen(function* () {
    const { keys, values } = rootEntryVectors(entries);
    if (shouldBuildMpfRootInWorker(entries.length)) {
      const canonicalEntries = yield* canonicalizeKeyValuePhasEntries(
        keys,
        values,
      );
      const built = yield* Effect.tryPromise({
        try: () => buildAuthenticatedMpfRootInWorker(domain, canonicalEntries),
        catch: (cause) => MpfError.rootBuild("parallel event root", cause),
      });
      return {
        root: built.rootHex,
        phasRoot: built.phasRoot,
        count: built.count,
        entries: canonicalEntries,
        domain,
      };
    }
    const phas = yield* keyValuePhasRootWithCount(keys, values);
    const root = yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.phasRoot(
          new Error("Failed to commit authenticated root count", { cause }),
        ),
      ),
    );
    return {
      root,
      phasRoot: phas.root,
      count: phas.count,
      entries: phas.entries,
      domain,
    };
  });

export const buildAuthenticatedRootFromDataEntries = <K, V>({
  domain,
  entries,
  keySchema,
  valueSchema,
}: {
  readonly domain: SDK.RootDomain;
  readonly entries: readonly TypedRootEntry<K, V>[];
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): Effect.Effect<BuiltTypedAuthenticatedRoot<K, V>, MpfError, never> =>
  Effect.gen(function* () {
    const encodedEntries = yield* Effect.forEach(entries, (entry, index) =>
      Effect.gen(function* () {
        const key = yield* encodeData(
          entry.key,
          keySchema,
          `root key at index ${index.toString()}`,
        );
        const value = yield* encodeData(
          entry.value,
          valueSchema,
          `root value at index ${index.toString()}`,
        );
        return { key, value };
      }),
    );
    const built = yield* buildAuthenticatedRootFromEncodedEntries(
      domain,
      encodedEntries,
    );
    return {
      ...built,
      typedEntries: entries,
    };
  });

export const validateCountProof = (
  proof: SDK.RootCountProof,
  options: RootProofVerificationOptions,
): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    if (proof.domain !== options.expectedDomain) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof domain mismatch: expected=${options.expectedDomain},actual=${proof.domain}`,
          ),
        ),
      );
    }
    if (proof.count < 0n) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(`Root proof count must be non-negative: ${proof.count}`),
        ),
      );
    }
    if (
      (proof.phas_root === SDK.EMPTY_MERKLE_TREE_ROOT && proof.count !== 0n) ||
      (proof.phas_root !== SDK.EMPTY_MERKLE_TREE_ROOT && proof.count === 0n)
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS root/count emptiness mismatch: phas_root=${proof.phas_root},count=${proof.count.toString()}`,
          ),
        ),
      );
    }
    const committedRoot = yield* SDK.commitCountedRootProgram({
      domain: proof.domain,
      phasRoot: proof.phas_root,
      count: proof.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.phasRoot(
          new Error("Failed to verify authenticated root count", { cause }),
        ),
      ),
    );
    if (proof.root !== committedRoot) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Counted root commitment mismatch: root=${proof.root},committed=${committedRoot},phas_root=${proof.phas_root},count=${proof.count.toString()}`,
          ),
        ),
      );
    }
    if (
      options.expectedRoot !== undefined &&
      proof.root !== options.expectedRoot
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof root mismatch: expected=${options.expectedRoot},actual=${proof.root}`,
          ),
        ),
      );
    }
    if (
      options.expectedCount !== undefined &&
      proof.count !== options.expectedCount
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof count mismatch: expected=${options.expectedCount.toString()},actual=${proof.count.toString()}`,
          ),
        ),
      );
    }
  });

export const verifyRootCountProof = (
  proof: SDK.RootCountProof,
  options: RootProofVerificationOptions,
): Effect.Effect<void, MpfError, never> => validateCountProof(proof, options);

export const buildRootMembershipProof = <K, V>({
  root,
  key,
  value,
  keySchema,
  valueSchema,
}: {
  readonly root: BuiltTypedAuthenticatedRoot<K, V>;
  readonly key: K;
  readonly value: V;
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): Effect.Effect<SDK.RootMembershipProof<K, V>, MpfError, never> =>
  Effect.gen(function* () {
    const keyBytes = yield* encodeData(key, keySchema, "membership proof key");
    const valueBytes = yield* encodeData(
      value,
      valueSchema,
      "membership proof value",
    );
    const { keys, values } = rootEntryVectors(root.entries);
    const proof = yield* keyValuePhasProof(keys, values, keyBytes);
    yield* verifyKeyValuePhasMembershipProof({
      root: root.phasRoot,
      key: keyBytes,
      value: valueBytes,
      proof,
    });
    return {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      value,
      proof,
    };
  });

export const buildRootNonMembershipProof = <K>({
  root,
  key,
  keySchema,
}: {
  readonly root: BuiltAuthenticatedRoot;
  readonly key: K;
  readonly keySchema: DataSchema;
}): Effect.Effect<SDK.RootNonMembershipProof<K>, MpfError, never> =>
  Effect.gen(function* () {
    const keyBytes = yield* encodeData(
      key,
      keySchema,
      "non-membership proof key",
    );
    const { keys, values } = rootEntryVectors(root.entries);
    const proof = yield* keyValuePhasNonMembershipProof(keys, values, keyBytes);
    yield* verifyKeyValuePhasNonMembershipProof({
      root: root.phasRoot,
      key: keyBytes,
      proof,
    });
    return {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      proof,
    };
  });
