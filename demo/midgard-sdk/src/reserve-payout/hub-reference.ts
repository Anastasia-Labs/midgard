import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { fail, ReservePayoutTxError } from "./errors.js";
import * as SDK from "./primitives.js";

const validateHubOracleReferenceProgram = (
  contracts: SDK.MidgardValidators,
  actual: UTxO,
): Effect.Effect<UTxO, ReservePayoutTxError | SDK.Bech32DeserializationError> =>
  Effect.gen(function* () {
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    if ((actual.assets[hubOracleUnit] ?? 0n) !== 1n) {
      return yield* fail("Hub oracle reference UTxO is not authenticated", {
        hubOracleRefInput: outRefLabel(actual),
        unit: hubOracleUnit,
        quantity: (actual.assets[hubOracleUnit] ?? 0n).toString(),
      });
    }
    if (actual.address !== contracts.hubOracle.spendingScriptAddress)
      return yield* fail(
        "Hub oracle reference is at the wrong deployment address",
        { hubOracleRefInput: outRefLabel(actual) },
      );
    if (actual.datum === undefined) {
      return yield* fail("Hub oracle reference UTxO has no inline datum", {
        hubOracleRefInput: outRefLabel(actual),
      });
    }
    const expectedDatum = yield* SDK.makeHubOracleDatum(contracts);
    const actualDatum = yield* Effect.try({
      try: () => Data.from(actual.datum!, SDK.HubOracleDatum),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to decode hub oracle reference datum",
          cause,
        }),
    });
    const actualDatumCbor = Data.to(actualDatum, SDK.HubOracleDatum);
    const expectedDatumCbor = Data.to(expectedDatum, SDK.HubOracleDatum);
    if (actualDatumCbor !== expectedDatumCbor) {
      return yield* fail(
        "On-chain hub oracle deployment does not match the locally configured contracts",
        {
          expectedDatumCbor,
          actualDatumCbor,
        },
      );
    }
    return actual;
  });

export const fetchHubOracleReferenceProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  explicit: UTxO | undefined,
): Effect.Effect<
  UTxO,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
> =>
  Effect.gen(function* () {
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    if (explicit !== undefined)
      yield* validateHubOracleReferenceProgram(contracts, explicit);
    const hubOracleAddress = contracts.hubOracle.spendingScriptAddress;
    const hubOracleUtxos = yield* Effect.tryPromise({
      try: () =>
        explicit === undefined
          ? lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit)
          : lucid.utxosByOutRef([explicit]),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch hub oracle reference UTxO",
          cause,
        }),
    });
    if (hubOracleUtxos.length !== 1) {
      return yield* fail("Failed to fetch the hub oracle reference UTxO", {
        address: hubOracleAddress,
        unit: hubOracleUnit,
        found: hubOracleUtxos.map(outRefLabel),
      });
    }
    const actual = hubOracleUtxos[0]!;
    return yield* validateHubOracleReferenceProgram(contracts, actual);
  });
