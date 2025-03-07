import { Effect } from "effect";
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
): Effect.Effect<[PolicyId, string, bigint], Error> =>
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
        new Error("Expected exactly 1 additional asset apart from ADA"),
      );
    }
  });

export const utxosAtByNFTPolicyId = (
  lucid: LucidEvolution,
  addressOrCred: Address | Credential,
  policyId: PolicyId,
): Effect.Effect<UTxO[], Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(addressOrCred),
      catch: (e) => {
        return new Error(
          `Failed to fetch UTxOs at: ${addressOrCred} -- Cause: ${e}`,
        );
      },
    });
    const nftEffects: Effect.Effect<UTxO, Error>[] = allUTxOs.map((u: UTxO) => {
      const nftsEffect = getSingleAssetApartFromAda(u.assets);
      return Effect.andThen(
        nftsEffect,
        ([sym, _tn, qty]): Effect.Effect<UTxO, Error> => {
          if (sym === policyId && qty === 1n) {
            return Effect.succeed(u);
          } else {
            return Effect.fail(
              new Error(
                "UTxO doesn't have the expected NFT policy ID, or its quantity is not exactly 1",
              ),
            );
          }
        },
      );
    });
    const authenticUTxOs = yield* Effect.allSuccesses(nftEffects);
    return authenticUTxOs;
  });

const blake2bHelper = (
  hash: string,
  dkLen: number,
  functionName: string,
): Effect.Effect<string, Error> => {
  if (isHexString(hash)) {
    try {
      return Effect.succeed(toHex(blake2b(fromHex(hash), { dkLen })));
    } catch (e) {
      return Effect.fail(new Error(`${e}`));
    }
  } else {
    return Effect.fail(
      new Error(`Invalid hash provided for ${functionName} function`),
    );
  }
};

export const hashHexWithBlake2b224 = (
  hash: string,
): Effect.Effect<string, Error> => blake2bHelper(hash, 28, "Blake2b224");

export const hashHexWithBlake2b256 = (
  hash: string,
): Effect.Effect<string, Error> => blake2bHelper(hash, 32, "Blake2b256");
