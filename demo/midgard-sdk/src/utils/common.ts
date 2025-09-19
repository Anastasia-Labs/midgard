import { Data, Effect } from "effect";
import {
  Address,
  Assets,
  Credential,
  LucidEvolution,
  PolicyId,
  UTxO,
  fromHex,
  fromUnit,
  toHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2b";

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

export const getSingleAssetApartFromAda = (
  assets: Assets,
): Effect.Effect<[PolicyId, string, bigint], AssetError> =>
  Effect.gen(function* () {
    const flattenedAssets: [string, bigint][] = Object.entries(assets);
    const woLovelace: [string, bigint][] = flattenedAssets.filter(
      ([unit, _qty]) => !(unit === "" || unit === "lovelace"),
    );
    if (woLovelace.length === 1) {
      const explodedUnit = fromUnit(woLovelace[0][0]);
      return [
        explodedUnit.policyId,
        explodedUnit.assetName ?? "",
        woLovelace[0][1],
      ];
    } else {
      return yield* Effect.fail(
        new AssetError({
          message: "Failed to get single asset apart from ADA",
          cause: "Expected exactly 1 additional asset apart from ADA",
        }),
      );
    }
  });

export const utxosAtByNFTPolicyId = (
  lucid: LucidEvolution,
  addressOrCred: Address | Credential,
  policyId: PolicyId,
): Effect.Effect<UTxO[], LucidError | AssetError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(addressOrCred),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch UTxOs at: ${addressOrCred}`,
          cause: e,
        });
      },
    });
    const nftEffects: Effect.Effect<UTxO, AssetError>[] = allUTxOs.map(
      (u: UTxO) => {
        const nftsEffect = getSingleAssetApartFromAda(u.assets);
        return Effect.andThen(
          nftsEffect,
          ([sym, _tn, qty]): Effect.Effect<UTxO, AssetError> => {
            if (sym === policyId && qty === 1n) {
              return Effect.succeed(u);
            } else {
              return Effect.fail(
                new AssetError({
                  message: "Failed to get assets from fetched UTxOs",
                  cause:
                    "UTxO doesn't have the expected NFT policy ID, or its quantity is not exactly 1",
                }),
              );
            }
          },
        );
      },
    );
    const authenticUTxOs = yield* Effect.allSuccesses(nftEffects);
    return authenticUTxOs;
  });

const blake2bHelper = (
  hash: string,
  dkLen: number,
  functionName: string,
): Effect.Effect<string, HashingError> => {
  const errorMessage = `Failed to hash using ${functionName} function`;
  if (isHexString(hash)) {
    try {
      return Effect.succeed(toHex(blake2b(fromHex(hash), { dkLen })));
    } catch (e) {
      return Effect.fail(
        new HashingError({
          message: errorMessage,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new HashingError({
        message: errorMessage,
        cause: `Invalid hash provided`,
      }),
    );
  }
};

export const hashHexWithBlake2b224 = (
  hash: string,
): Effect.Effect<string, HashingError> => blake2bHelper(hash, 28, "Blake2b224");

export const hashHexWithBlake2b256 = (
  hash: string,
): Effect.Effect<string, HashingError> => blake2bHelper(hash, 32, "Blake2b256");

export type GenericErrorFields = {
  readonly message: string;
  readonly cause: unknown;
};

export class HubOracleError extends Data.TaggedError(
  "HubOracleError",
)<GenericErrorFields> {}

export class StateQueueError extends Data.TaggedError(
  "StateQueueError",
)<GenericErrorFields> {}

export class MptError extends Data.TaggedError("MptError")<{
  readonly message: string;
  readonly cause: unknown;
}> {
  static get(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie get operation`,
      cause,
    });
  }
  static put(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie put operation`,
      cause,
    });
  }
  static batch(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie batch operation`,
      cause,
    });
  }
  static trieDelete(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on whole ${trie} trie delete`,
      cause,
    });
  }
  static trieCreate(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie create`,
      cause,
    });
  }
}

// General errors that don't have specific domains
export class CmlUnexpectedError extends Data.TaggedError(
  "CmlUnexpectedError",
)<GenericErrorFields> {}

export class CmlDeserializationError extends Data.TaggedError(
  "CmlDeserializationError",
)<GenericErrorFields> {}

export class CborSerializationError extends Data.TaggedError(
  "CborSerializationError",
)<GenericErrorFields> {}

export class CborDeserializationError extends Data.TaggedError(
  "CborDeserializationError",
)<GenericErrorFields> {}

export class LucidError extends Data.TaggedError(
  "LucidError",
)<GenericErrorFields> {}

export class HashingError extends Data.TaggedError(
  "HashingError",
)<GenericErrorFields> {}

export class AssetError extends Data.TaggedError(
  "AssetError",
)<GenericErrorFields> {}