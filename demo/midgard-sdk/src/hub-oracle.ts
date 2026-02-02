import { Effect, Data as EffectData } from "effect";
import {
  GenericErrorFields,
  LucidError,
  AddressSchema,
  PolicyIdSchema,
  makeReturn,
  AuthenticatedValidator,
  DataCoercionError,
  UnauthenticUtxoError,
  getStateToken,
} from "@/common.js";
import {
  Address,
  fromHex,
  LucidEvolution,
  PolicyId,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

export type HubOracleConfig = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

// TODO: This should ideally come from Aiken env directory.
export const hubOracleAssetName = "";

export const HubOracleDatumSchema = Data.Object({
  registeredOperators: PolicyIdSchema,
  activeOperators: PolicyIdSchema,
  retiredOperators: PolicyIdSchema,
  scheduler: PolicyIdSchema,
  stateQueue: PolicyIdSchema,
  fraudProofCatalogue: PolicyIdSchema,
  fraudProof: PolicyIdSchema,
  registeredOperatorsAddr: AddressSchema,
  activeOperatorsAddr: AddressSchema,
  retiredOperatorsAddr: AddressSchema,
  schedulerAddr: AddressSchema,
  stateQueueAddr: AddressSchema,
  fraudProofCatalogueAddr: AddressSchema,
  fraudProofAddr: AddressSchema,
});
export type HubOracleDatum = Data.Static<typeof HubOracleDatumSchema>;
export const HubOracleDatum = HubOracleDatumSchema as unknown as HubOracleDatum;

export type HubOracleUTxO = {
  utxo: UTxO;
  datum: HubOracleDatum;
};

/**
 * Parameters for the init transaction.
 */
export type HubOracleInitParams = {
  validator: AuthenticatedValidator;
  datum: HubOracleDatum;
};

/**
 * Parameters for the burn transaction.
 */
export type HubOracleBurnParams = {};

export const getHubOracleDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<HubOracleDatum, DataCoercionError> =>
  Effect.gen(function* () {
    const datumCBOR = nodeUTxO.datum;
    if (!datumCBOR) {
      return yield* Effect.fail(
        new DataCoercionError({
          message: `HubOracle datum coercion failed`,
          cause: `No datum found`,
        }),
      );
    }
    const hubOracleDatum: HubOracleDatum = yield* Effect.try({
      try: () => Data.from(datumCBOR, HubOracleDatum),
      catch: (e) =>
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a hub oracle datum`,
          cause: e,
        }),
    });

    return hubOracleDatum;
  });
/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToHubOracleUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<HubOracleUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getHubOracleDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `HubOracleUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the hub oracle's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToHubOracleUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<HubOracleUTxO[]> => {
  const effects = utxos.map((u) => utxoToHubOracleUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

/**
 * Creates a init transaction builder.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {InitParams} params - The parameters for the init transaction.
 * @returns {TxBuilder} The transaction builder.
 */
export const incompleteHubOracleInitTxProgram = (
  lucid: LucidEvolution,
  params: HubOracleInitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Creates a burn transaction builder.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {BurnParams} params - The parameters for the burn transaction.
 * @returns {TxBuilder} The transaction builder.
 */
export const incompleteHubOracleBurnTxProgram = (
  lucid: LucidEvolution,
  params: HubOracleBurnParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

export class HubOracleError extends EffectData.TaggedError(
  "HubOracleError",
)<GenericErrorFields> {}

export const fetchHubOracleUTxOProgram = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
): Effect.Effect<
  { utxo: UTxO; datum: HubOracleDatum },
  HubOracleError | LucidError
> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to fetch the hub oracle UTxO";
    const hubOracleUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: async () => {
        return await lucid.utxosAtWithUnit(
          config.hubOracleAddress,
          toUnit(config.hubOraclePolicyId, hubOracleAssetName),
        );
      },
      catch: (e) =>
        new LucidError({
          message: errorMessage,
          cause: e,
        }),
    });
    if (hubOracleUTxOs.length === 1) {
      const utxo = hubOracleUTxOs[0];
      const datum = yield* Effect.try({
        try: () => {
          if (utxo.datum) {
            const coerced = Data.from(utxo.datum, HubOracleDatum);
            return coerced;
          } else {
            throw new HubOracleError({
              message: errorMessage,
              cause: "Hub oracle UTxO datum is missing",
            });
          }
        },
        catch: (e) => {
          return new HubOracleError({
            message: errorMessage,
            cause: `Failed to parse the hub oracle datum: ${e}`,
          });
        },
      });
      return { utxo, datum };
    } else {
      return yield* Effect.fail(
        new HubOracleError({
          message: errorMessage,
          cause:
            "Exactly one hub oracle UTxO was expected, but none or more were found",
        }),
      );
    }
  });

/**
 * Attempts fetching the hub oracle UTxO.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic hub oracle UTxO.
 */
export const fetchHubOracleUTxO = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
) => makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
