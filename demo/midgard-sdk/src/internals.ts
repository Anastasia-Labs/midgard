import { type LucidDataSchema } from "@al-ft/midgard-core/lucid-data";
import {
  Address,
  Assets as LucidAssets,
  Data,
  fromUnit,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AssetError,
  DataCoercionError,
  LucidError,
  UnauthenticUtxoError,
} from "./errors.js";

const getSingleAssetApartFromAda = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string, bigint], AssetError> =>
  Effect.gen(function* () {
    const woLovelace = Object.entries(assets).filter(
      ([unit, _qty]) => !(unit === "" || unit === "lovelace"),
    );
    if (woLovelace.length === 1) {
      const explodedUnit = fromUnit(woLovelace[0][0]);
      return [
        explodedUnit.policyId,
        explodedUnit.assetName ?? "",
        woLovelace[0][1],
      ];
    }

    return yield* Effect.fail(
      new AssetError({
        message: "Failed to get single asset apart from ADA",
        cause: "Expected exactly 1 additional asset apart from ADA",
      }),
    );
  });

/**
 * Similar to `getSingleAssetApartFromAda`, with the additional requirement for
 * the quantity to be exactly 1.
 */
export const getStateToken = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string], UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to get the beacon token from assets";
    const [policyId, assetName, qty] = yield* getSingleAssetApartFromAda(
      assets,
    ).pipe(
      Effect.mapError(
        (e) =>
          new UnauthenticUtxoError({
            message: errorMessage,
            cause: e,
          }),
      ),
    );

    if (qty !== 1n) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: errorMessage,
          cause: `The quantity of the beacon token was expected to be exactly 1, but it was ${qty.toString()}`,
        }),
      );
    }

    return [policyId, assetName];
  });

export type AuthenticUTxO<TDatum, TExtra = undefined> = {
  utxo: UTxO;
  datum: TDatum;
  assetName: string;
} & ([TExtra] extends [undefined] ? Record<never, never> : TExtra);

export const getDatumFromUTxO = <TDatum>(
  nodeUTxO: UTxO,
  schema: LucidDataSchema,
): Effect.Effect<TDatum, DataCoercionError> =>
  Effect.gen(function* () {
    const datumCBOR = nodeUTxO.datum;
    if (!datumCBOR) {
      return yield* Effect.fail(
        new DataCoercionError({
          message: `Datum coercion failed`,
          cause: `No datum found`,
        }),
      );
    }

    return yield* Effect.try({
      try: () => Data.from(datumCBOR, schema) as TDatum,
      catch: (e) =>
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to the expected datum type`,
          cause: e,
        }),
    });
  });

type AuthenticUTxOBase<TDatum> = {
  utxo: UTxO;
  datum: TDatum;
  assetName: string;
};

const utxoToAuthenticUTxOBase = <TDatum>(
  utxo: UTxO,
  nftPolicy: string,
  schema: LucidDataSchema,
): Effect.Effect<
  AuthenticUTxOBase<TDatum>,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getDatumFromUTxO<TDatum>(utxo, schema);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: `Failed to authenticate UTxO`,
          cause: `UTxO's NFT policy ID is not the same as the expected policy ID`,
        }),
      );
    }

    return { utxo, datum, assetName };
  });

export const authenticateUTxO: {
  <TDatum>(
    utxo: UTxO,
    nftPolicy: string,
    schema: LucidDataSchema,
  ): Effect.Effect<
    AuthenticUTxO<TDatum>,
    DataCoercionError | UnauthenticUtxoError
  >;
  <TDatum, TExtra>(
    utxo: UTxO,
    nftPolicy: string,
    schema: LucidDataSchema,
    extraFields: (datum: TDatum, utxo: UTxO) => TExtra,
  ): Effect.Effect<
    AuthenticUTxO<TDatum, TExtra>,
    DataCoercionError | UnauthenticUtxoError
  >;
} = <TDatum, TExtra>(
  utxo: UTxO,
  nftPolicy: string,
  schema: LucidDataSchema,
  extraFields?: (datum: TDatum, utxo: UTxO) => TExtra,
) =>
  Effect.gen(function* () {
    const base = yield* utxoToAuthenticUTxOBase<TDatum>(
      utxo,
      nftPolicy,
      schema,
    );
    if (extraFields === undefined) {
      return base as AuthenticUTxO<TDatum>;
    }
    const extra = yield* Effect.try({
      try: () => extraFields(base.datum, base.utxo),
      catch: (cause) =>
        new DataCoercionError({
          message: "Failed to derive authenticated UTxO extra fields",
          cause,
        }),
    });
    return {
      ...base,
      ...extra,
    } as AuthenticUTxO<TDatum, TExtra>;
  });

/**
 * Silently drops invalid UTxOs.
 */
export const authenticateUTxOs: {
  <TDatum>(
    utxos: UTxO[],
    nftPolicy: string,
    schema: LucidDataSchema,
  ): Effect.Effect<AuthenticUTxO<TDatum>[]>;
  <TDatum, TExtra>(
    utxos: UTxO[],
    nftPolicy: string,
    schema: LucidDataSchema,
    extraFields: (datum: TDatum, utxo: UTxO) => TExtra,
  ): Effect.Effect<AuthenticUTxO<TDatum, TExtra>[]>;
} = <TDatum, TExtra>(
  utxos: UTxO[],
  nftPolicy: string,
  schema: LucidDataSchema,
  extraFields?: (datum: TDatum, utxo: UTxO) => TExtra,
) => {
  const effects = utxos.map((utxo) =>
    extraFields === undefined
      ? authenticateUTxO<TDatum>(utxo, nftPolicy, schema)
      : authenticateUTxO<TDatum, TExtra>(utxo, nftPolicy, schema, extraFields),
  );
  return Effect.allSuccesses(effects);
};

/** Why a single-UTxO read did not find exactly one authentic UTxO, with the
 * counts behind it. `none-found` is what a provider still re-applying blocks
 * after a rollback also reports, so another read can clear it;
 * `several-found` means the singleton's token is duplicated, which no read
 * clears. `policyHolderCount` counts the UTxOs at the address holding any
 * token under the policy, authentic or not. */
export type UnexpectedAuthenticUTxOCount = {
  readonly reason: "none-found" | "several-found";
  readonly authenticCount: number;
  readonly rawCount: number;
  readonly policyHolderCount: number;
};

/** Optional fields a singleton's error carries when the count was wrong;
 * `retryable` is what a provider retry policy reads. */
export type UnexpectedAuthenticUTxOCountFields = {
  readonly unexpectedCount?: UnexpectedAuthenticUTxOCount;
  readonly retryable?: boolean;
};

/** The `cause`, count and retryability for a singleton's count error. */
export const unexpectedAuthenticUTxOCountFields = (
  utxoLabel: string,
  count: UnexpectedAuthenticUTxOCount,
): {
  readonly cause: string;
  readonly unexpectedCount: UnexpectedAuthenticUTxOCount;
  readonly retryable: boolean;
} => ({
  cause:
    count.reason === "none-found"
      ? `Exactly one ${utxoLabel} UTxO was expected, but no authentic ${utxoLabel} UTxO was found (${count.rawCount.toString()} UTxOs at the address, ${count.policyHolderCount.toString()} hold a token under the policy)`
      : `Exactly one ${utxoLabel} UTxO was expected, but ${count.authenticCount.toString()} authentic ${utxoLabel} UTxOs were found`,
  unexpectedCount: count,
  retryable: count.reason === "none-found",
});

export type FetchSingleAuthenticUTxOConfig<
  TAuthenticUTxO,
  TConversionError,
  TError,
> = {
  address: Address;
  policyId: PolicyId;
  utxoLabel: string;
  conversionFunction: (
    utxos: UTxO[],
    nftPolicy: PolicyId,
  ) => Effect.Effect<TAuthenticUTxO[], TConversionError>;
  onUnexpectedAuthenticUTxOCount: (
    count: UnexpectedAuthenticUTxOCount,
  ) => TError;
};

export const fetchSingleAuthenticUTxOProgram = <
  TAuthenticUTxO,
  TConversionError,
  TError,
>(
  lucid: LucidEvolution,
  config: FetchSingleAuthenticUTxOConfig<
    TAuthenticUTxO,
    TConversionError,
    TError
  >,
): Effect.Effect<TAuthenticUTxO, LucidError | TConversionError | TError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.address),
      catch: (e) =>
        new LucidError({
          message: `Failed to fetch the ${config.utxoLabel} UTxO at: ${config.address}`,
          cause: e,
        }),
    });

    const authenticUTxOs = yield* config.conversionFunction(
      allUTxOs,
      config.policyId,
    );

    if (authenticUTxOs.length === 1) {
      return authenticUTxOs[0];
    }

    return yield* Effect.fail(
      config.onUnexpectedAuthenticUTxOCount({
        reason: authenticUTxOs.length === 0 ? "none-found" : "several-found",
        authenticCount: authenticUTxOs.length,
        rawCount: allUTxOs.length,
        policyHolderCount: allUTxOs.filter((utxo) =>
          Object.keys(utxo.assets).some((unit) =>
            unit.startsWith(config.policyId),
          ),
        ).length,
      }),
    );
  });
