import { describe, it } from "vitest";
import dotenv from "dotenv";
import { Effect } from "effect";
import { Data } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import * as Services from "@/services/index.js";

dotenv.config();

const decodeRegisteredData = (value: unknown): unknown => {
  try {
    return Data.from(value as string, SDK.RegisteredOperatorDatum);
  } catch (error) {
    return {
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

describe("debug scheduler live", () => {
  it(
    "prints live scheduler and operator-set witness state",
    async () => {
      const { lucid, contracts } = await Effect.runPromise(
        Effect.gen(function* () {
          const lucidService = yield* Services.Lucid;
          const contracts = yield* Services.MidgardContracts;
          return {
            lucid: lucidService.api,
            contracts,
          };
        }).pipe(
          Effect.provide(Services.NodeConfig.layer),
          Effect.provide(Services.MidgardContracts.Default),
          Effect.provide(Services.Lucid.Default),
        ),
      );

      const [
        registeredUtxos,
        activeUtxos,
        schedulerUtxos,
        schedulerAddressUtxos,
      ] = await Promise.all([
        SDK.utxosAtByNFTPolicyId(
          lucid,
          contracts.registeredOperators.spendingScriptAddress,
          contracts.registeredOperators.policyId,
        ).pipe(Effect.runPromise),
        SDK.utxosAtByNFTPolicyId(
          lucid,
          contracts.activeOperators.spendingScriptAddress,
          contracts.activeOperators.policyId,
        ).pipe(Effect.runPromise),
        SDK.utxosAtByNFTPolicyId(
          lucid,
          contracts.scheduler.spendingScriptAddress,
          contracts.scheduler.policyId,
        ).pipe(Effect.runPromise),
        lucid.utxosAt(contracts.scheduler.spendingScriptAddress),
      ]);

      const registered = await Promise.all(
        registeredUtxos.map(async (utxo) => {
          const node = await Effect.runPromise(SDK.getNodeDatumFromUTxO(utxo));
          return {
            outRef: `${utxo.txHash}#${utxo.outputIndex}`,
            assetNames: Object.keys(utxo.assets),
            key: node.key,
            next: node.next,
            rawData: node.data,
            decodedRegisteredData: decodeRegisteredData(node.data),
          };
        }),
      );

      const active = await Promise.all(
        activeUtxos.map(async (utxo) => {
          const node = await Effect.runPromise(SDK.getNodeDatumFromUTxO(utxo));
          return {
            outRef: `${utxo.txHash}#${utxo.outputIndex}`,
            assetNames: Object.keys(utxo.assets),
            key: node.key,
            next: node.next,
            rawData: node.data,
          };
        }),
      );

      const scheduler = schedulerUtxos.map((utxo) => ({
        outRef: `${utxo.txHash}#${utxo.outputIndex}`,
        datum:
          utxo.datum == null ? null : Data.from(utxo.datum, SDK.SchedulerDatum),
        assets: utxo.assets,
      }));

      console.log(
        JSON.stringify(
          {
            registered,
            active,
            scheduler,
            schedulerAddressUtxos: schedulerAddressUtxos.map((utxo) => ({
              outRef: `${utxo.txHash}#${utxo.outputIndex}`,
              assets: utxo.assets,
              datum: utxo.datum,
            })),
          },
          (_, value) => (typeof value === "bigint" ? value.toString() : value),
          2,
        ),
      );
    },
    180_000,
  );
});
