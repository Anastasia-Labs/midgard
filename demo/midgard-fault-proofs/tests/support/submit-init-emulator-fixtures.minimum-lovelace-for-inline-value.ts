import { outRefLabel } from "@al-ft/midgard-core";
import {
  ActiveOperatorDatum,
  getLinkedListNodeViewFromUTxO,
} from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  Data,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { workflowTransactionReferenceInputOutRefs } from "../../src/index.js";
import {
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
} from "./submit-init-emulator-shared.js";

export const requireCurrentUnitUtxo = async ({
  lucid,
  address,
  unit,
  label,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  if (utxos.length !== 1 || utxos[0] === undefined) {
    throw new Error(
      `${label} expected exactly one UTxO carrying ${unit}, found ${utxos.length.toString()}.`,
    );
  }
  return utxos[0];
};

export const activeOperatorDatumFromUtxo = async (
  utxo: UTxO,
): Promise<ActiveOperatorDatum> => {
  const node = await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo));
  return Data.castFrom(
    node.data as never,
    ActiveOperatorDatum as never,
  ) as ActiveOperatorDatum;
};

export const cleanAdaOnlyWalletFeeInput = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const feeInput = (await lucid.wallet().getUtxos()).find(
    (utxo) =>
      utxo.datum === undefined &&
      utxo.datumHash === undefined &&
      utxo.scriptRef === undefined &&
      Object.entries(utxo.assets).every(
        ([unit, amount]) => unit === "lovelace" && amount > 0n,
      ),
  );
  if (feeInput === undefined) {
    throw new Error(`Expected a clean ADA-only wallet UTxO for ${label}.`);
  }
  return feeInput;
};

export const expectReferenceScriptOnlyTransaction = ({
  signed,
  measurement,
  expectedReferenceInputs,
}: {
  readonly signed: Parameters<
    typeof workflowTransactionReferenceInputOutRefs
  >[0];
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly expectedReferenceInputs: readonly UTxO[];
}): void => {
  expect(measurement.plutusV1ScriptCount).toBe(0);
  expect(measurement.plutusV2ScriptCount).toBe(0);
  expect(measurement.plutusV3ScriptCount).toBe(0);
  expect(measurement.nativeScriptCount).toBe(0);
  const referenceInputs = workflowTransactionReferenceInputOutRefs(signed);
  expect(referenceInputs).toHaveLength(expectedReferenceInputs.length);
  expect(referenceInputs).toEqual(
    expect.arrayContaining(expectedReferenceInputs.map(outRefLabel)),
  );
};

export const minimumLovelaceForInlineValue = ({
  address,
  datum,
  assets,
}: {
  readonly address: string;
  readonly datum: string;
  readonly assets: Readonly<Record<string, bigint>>;
}): bigint => {
  let lovelace = assets.lovelace ?? 0n;
  for (let attempt = 0; attempt < 16; attempt += 1) {
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      assetsToValue({ ...assets, lovelace }),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
      undefined,
    );
    const required = CML.min_ada_required(
      output,
      EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error("Failed to stabilize linked-list inline-datum min-ADA.");
};
