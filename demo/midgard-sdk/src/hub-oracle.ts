import { Effect, Data as EffectData } from "effect";
import {
  GenericErrorFields,
  LucidError,
  AddressSchema,
  PolicyIdSchema,
  makeReturn,
  addressDataFromBech32,
  Bech32DeserializationError,
  MidgardValidators,
  AuthenticatedValidator,
  ScriptHashSchema,
  MintingValidator,
  UnspecifiedNetworkError,
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
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { HUB_ORACLE_ASSET_NAME } from "@/constants.js";

/**
 * Hub-oracle helpers for bootstrapping and reading the Midgard validator
 * registry.
 *
 * The hub oracle anchors the policy ids and script addresses for the rest of
 * the protocol. Off-chain code therefore treats its datum as the canonical
 * source of validator wiring.
 */
export type HubOracleConfig = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

export const hubOracleAssetName = HUB_ORACLE_ASSET_NAME;

/**
 * Datum stored at the unique hub-oracle UTxO.
 */
export const HubOracleDatumSchema = Data.Object({
  registeredOperators: PolicyIdSchema,
  activeOperators: PolicyIdSchema,
  retiredOperators: PolicyIdSchema,
  scheduler: PolicyIdSchema,
  stateQueue: PolicyIdSchema,
  fraudProofCatalogue: PolicyIdSchema,
  fraudProof: PolicyIdSchema,
  deposit: PolicyIdSchema,
  withdrawal: PolicyIdSchema,
  txOrder: PolicyIdSchema,
  settlement: PolicyIdSchema,
  payout: PolicyIdSchema,
  registeredOperatorsAddr: AddressSchema,
  activeOperatorsAddr: AddressSchema,
  retiredOperatorsAddr: AddressSchema,
  schedulerAddr: AddressSchema,
  stateQueueAddr: AddressSchema,
  fraudProofCatalogueAddr: AddressSchema,
  fraudProofAddr: AddressSchema,
  depositAddr: AddressSchema,
  withdrawalAddr: AddressSchema,
  txOrderAddr: AddressSchema,
  settlementAddr: AddressSchema,
  payoutAddr: AddressSchema,
  reserveAddr: AddressSchema,
  reserveObserver: ScriptHashSchema,
});
export type HubOracleDatum = Data.Static<typeof HubOracleDatumSchema>;
export const HubOracleDatum = HubOracleDatumSchema as unknown as HubOracleDatum;

/**
 * Parameters required to mint and initialize the hub oracle.
 */
export type HubOracleInitParams = {
  hubOracleMintValidator: MintingValidator;
  validators: HubOracleValidators;
  oneShotNonceUTxO: UTxO;
};

/**
 * Validators recorded in the hub oracle datum.
 */
export type HubOracleValidators = Omit<
  MidgardValidators,
  "hubOracle" | "fraudProofs"
>;

/**
 * Builds the hub-oracle datum from the active validator bundle.
 */
export const makeHubOracleDatum = (
  validators: HubOracleValidators,
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
      depositAddr,
      withdrawalAddr,
      txOrderAddr,
      settlementAddr,
      payoutAddr,
    ] = yield* Effect.all(
      [
        validators.registeredOperators,
        validators.activeOperators,
        validators.retiredOperators,
        validators.scheduler,
        validators.stateQueue,
        validators.fraudProofCatalogue,
        validators.fraudProof,
        validators.deposit,
        validators.withdrawal,
        validators.txOrder,
        validators.settlement,
        validators.payout,
      ].map((authVal: AuthenticatedValidator) =>
        addressDataFromBech32(authVal.spendingScriptAddress),
      ),
      { concurrency: "unbounded" },
    );

    const reserveAddr = yield* addressDataFromBech32(
      validators.reserve.spendingScriptAddress,
    );

    return {
      registeredOperators: validators.registeredOperators.policyId,
      activeOperators: validators.activeOperators.policyId,
      retiredOperators: validators.retiredOperators.policyId,
      scheduler: validators.scheduler.policyId,
      stateQueue: validators.stateQueue.policyId,
      fraudProofCatalogue: validators.fraudProofCatalogue.policyId,
      fraudProof: validators.fraudProof.policyId,
      deposit: validators.deposit.policyId,
      withdrawal: validators.withdrawal.policyId,
      txOrder: validators.txOrder.policyId,
      settlement: validators.settlement.policyId,
      payout: validators.payout.policyId,
      registeredOperatorsAddr,
      activeOperatorsAddr,
      retiredOperatorsAddr,
      schedulerAddr,
      stateQueueAddr,
      fraudProofCatalogueAddr,
      fraudProofAddr,
      depositAddr,
      withdrawalAddr,
      txOrderAddr,
      settlementAddr,
      payoutAddr,
      reserveAddr,
      reserveObserver: validators.reserve.withdrawalScriptHash,
    };
  });

/**
 * Creates the transaction fragment that mints and places the hub-oracle UTxO.
 *
 * Datum construction is handled internally so callers only need to supply the
 * validator bundle and one-shot nonce input.
 */
export const incompleteHubOracleInitTxProgram = (
  lucid: LucidEvolution,
  params: HubOracleInitParams,
): Effect.Effect<
  TxBuilder,
  Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network) {
      const datum = yield* makeHubOracleDatum(params.validators);
      const encodedDatum = Data.to<HubOracleDatum>(datum, HubOracleDatum);

      const assets: Assets = {
        [toUnit(params.hubOracleMintValidator.policyId, HUB_ORACLE_ASSET_NAME)]:
          1n,
      };

      return lucid
        .newTx()
        .collectFrom([params.oneShotNonceUTxO])
        .mintAssets(assets, Data.void())
        .pay.ToAddressWithData(
          credentialToAddress(
            network,
            scriptHashToCredential(params.hubOracleMintValidator.policyId),
          ),
          { kind: "inline", value: encodedDatum },
          assets,
        )
        .attach.MintingPolicy(params.hubOracleMintValidator.mintingScript);
    } else {
      return yield* new UnspecifiedNetworkError({
        message: "",
        cause: "Cardano network not found",
      });
    }
  });

export class HubOracleError extends EffectData.TaggedError(
  "HubOracleError",
)<GenericErrorFields> {}

/**
 * Fetches and decodes the unique hub-oracle UTxO authenticated by the hub
 * oracle NFT.
 */
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
 * Promise-style wrapper around `fetchHubOracleUTxOProgram`.
 */
export const fetchHubOracleUTxO = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
) => makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
