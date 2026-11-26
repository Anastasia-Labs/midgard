/**
 * Canonical spend-input witness decoding, plus the min-Ada helpers the
 * publication builders share. Field preimages are published by
 * `publishFaultProofFieldCarriageV1` in `field-opening.ts`.
 */

import {
  decodeMidgardSpendInputItem,
  encodeMidgardSpendInputItem,
  type MidgardTxInput as MidgardTxInputV1,
} from "@al-ft/midgard-core/codec";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type MidgardTxInput } from "@al-ft/midgard-sdk/core";
import {
  CML,
  type LucidEvolution,
  type ProtocolParameters,
} from "@lucid-evolution/lucid";

const MIN_ADA_STABILIZATION_LIMIT = 8;

export type SpendInputsWitness = {
  readonly inputs: readonly MidgardTxInput[];
};

export const resolveProtocolParameters = async (
  lucid: LucidEvolution,
): Promise<ProtocolParameters> => {
  const config = lucid.config();
  if (config.protocolParameters !== undefined) {
    return config.protocolParameters;
  }
  if (config.provider === undefined) {
    throw new Error("Lucid provider is not configured.");
  }
  return await config.provider.getProtocolParameters();
};

const inlineDatumOutput = ({
  address,
  datum,
  lovelace,
}: {
  readonly address: string;
  readonly datum: string;
  readonly lovelace: bigint;
}): CML.TransactionOutput =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    CML.Value.from_coin(lovelace),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    undefined,
  );

export const minimumLovelaceForInlineDatumOutput = ({
  address,
  datum,
  coinsPerUtxoByte,
}: {
  readonly address: string;
  readonly datum: string;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      inlineDatumOutput({ address, datum, lovelace }),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize inline-datum witness min-ADA calculation.",
  );
};

/**
 * The one canonical byte form is the §5.3 field-0/1 item —
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes — the bytes on-chain
 * `ledger_outref_key` / `encode_midgard_tx_input` derive and
 * `decode_midgard_tx_input_cbor` accepts. CML's minimal-index
 * `TransactionInput` CBOR is deliberately rejected.
 */
const requireCanonicalInputCbor = (
  inputCborHex: string,
  label: string,
): MidgardTxInputV1 => {
  let input: MidgardTxInputV1;
  const inputCbor = Buffer.from(inputCborHex, "hex");
  try {
    input = decodeMidgardSpendInputItem(inputCbor);
  } catch (cause) {
    throw new Error(
      `${label} is not valid Midgard §5.3 TxOutRef CBOR: ${formatUnknownError(cause)}`,
    );
  }
  const canonical = encodeMidgardSpendInputItem(input);
  if (!canonical.equals(inputCbor)) {
    throw new Error(`${label} must be canonical Midgard §5.3 TxOutRef CBOR.`);
  }
  return input;
};

export const spendInputsWitnessFromCbors = (
  inputCbors: readonly string[],
  label: string,
): SpendInputsWitness => {
  const inputs = inputCbors.map((inputCbor, index) => {
    // The §5.3 fixed uint16 output index bounds `output_index` to 0..65,535 by
    // construction, so the on-chain 16-bit range needs no separate check here.
    const input = requireCanonicalInputCbor(
      inputCbor,
      `${label}[${index.toString()}]`,
    );
    return {
      tx_id: Buffer.from(input.txId).toString("hex"),
      output_index: BigInt(input.outputIndex),
    };
  });

  return { inputs };
};
