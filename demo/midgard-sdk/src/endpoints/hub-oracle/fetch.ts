import { Effect } from "effect";
import {
  Address,
  Data,
  LucidEvolution,
  PolicyId,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import { makeReturn } from "@/core.js";
import { HubOracle } from "@/tx-builder/index.js";
import { LucidError } from "@/utils/common.js";
import { HubOracleError } from "@/utils/hub-oracle.js";

export type Config = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

// TODO: This should ideally come from Aiken env directory.
export const hubOracleAssetName = "";

export const fetchHubOracleUTxOProgram = (
  lucid: LucidEvolution,
  config: Config,
): Effect.Effect<
  { utxo: UTxO; datum: HubOracle.Datum },
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
            const coerced = Data.from(utxo.datum, HubOracle.Datum);
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
export const fetchHubOracleUTxO = (lucid: LucidEvolution, config: Config) =>
  makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
