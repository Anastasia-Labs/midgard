import { Effect, Data as EffectData } from "effect";
import {
  GenericErrorFields,
  LucidError,
  AddressSchema,
  PolicyIdSchema,
  makeReturn,
  AuthenticatedValidator,
  MintingValidatorInfo,
  addressDataFromBech32,
  Bech32DeserializationError,
  MidgardValidators,
} from "@/common.js";
import {
  Address,
  LucidEvolution,
  PolicyId,
  toUnit,
  TxBuilder,
  UTxO,
  Data,
  Assets,
  fromText,
} from "@lucid-evolution/lucid";
import { HUB_ORACLE_ASSET_NAME } from "@/constants.js";

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

export type HubOracleInitParams = {};

/**
 * Validators needed to construct the hub oracle datum.
 * This is the "wiring" that connects all Midgard contracts together.
 */
export type HubOracleValidators = {
  registeredOperators: AuthenticatedValidator;
  activeOperators: AuthenticatedValidator;
  retiredOperators: AuthenticatedValidator;
  scheduler: AuthenticatedValidator;
  stateQueue: AuthenticatedValidator;
  fraudProofCatalogue: AuthenticatedValidator;
  fraudProof: AuthenticatedValidator;
};

/**
 * Constructs HubOracleDatum from validators.
 * This is the "glue" that wires all Midgard contracts together.
 *
 * @param {HubOracleValidators} validators - All validators that need to be registered in the hub oracle
 * @returns {HubOracleDatum} Effect that produces the complete hub oracle datum
 */
export const makeHubOracleDatum = (
  validators: MidgardValidators,
): Effect.Effect<HubOracleDatum, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const [
      registeredOperatorsAddr,
      activeOperatorsAddr,
      retiredOperatorsAddr,
      schedulerAddr,
      stateQueueAddr,
      fraudProofCatalogueAddr,
      fraudProofAddr,
    ] = yield* Effect.all([
      addressDataFromBech32(
        validators.registeredOperatorsAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.activeOperatorsAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.retiredOperatorsAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.schedulerAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.stateQueueAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.fraudProofCatalogueAuthValidator.spendScriptAddress,
      ),
      addressDataFromBech32(
        validators.fraudProofAuthValidator.mintScriptAddress,
      ),
    ]);

    return {
      registeredOperators: validators.registeredOperatorsAuthValidator.policyId,
      activeOperators: validators.activeOperatorsAuthValidator.policyId,
      retiredOperators: validators.retiredOperatorsAuthValidator.policyId,
      scheduler: validators.schedulerAuthValidator.policyId,
      stateQueue: validators.stateQueueAuthValidator.policyId,
      fraudProofCatalogue: validators.fraudProofCatalogueAuthValidator.policyId,
      fraudProof: validators.fraudProofAuthValidator.policyId,
      registeredOperatorsAddr,
      activeOperatorsAddr,
      retiredOperatorsAddr,
      schedulerAddr,
      stateQueueAddr,
      fraudProofCatalogueAddr,
      fraudProofAddr,
    };
  });

/**
 * Creates a hub oracle init transaction builder.
 * Handles datum construction internally from validators.
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {MidgardValidators} validators - All validators that need to be registered in the hub oracle
 * @returns {TxBuilder} Effect that produces a transaction builder.
 */
export const incompleteHubOracleInitTxProgram = (
  lucid: LucidEvolution,
  validators: MidgardValidators,
): Effect.Effect<TxBuilder, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const datum = yield* makeHubOracleDatum(validators);
    const encodedDatum = Data.to<HubOracleDatum>(datum, HubOracleDatum);

    const assets: Assets = {
      [toUnit(
        validators.hubOracleMintValidator.policyId,
        HUB_ORACLE_ASSET_NAME,
      )]: 1n,
    };

    return lucid
      .newTx()
      .mintAssets(assets, Data.void())
      .pay.ToAddressWithData(
        validators.hubOracleMintValidator.mintScriptAddress,
        { kind: "inline", value: encodedDatum },
        assets,
      )
      .attach.MintingPolicy(validators.hubOracleMintValidator.mintScript);
  });

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
