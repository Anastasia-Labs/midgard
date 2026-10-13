/**
 * Withdrawal payability: the exact twin of withdrawal-mistag step-04's
 * `payout_is_exactly_payable_with_rate_v1`, over the withdrawal body's CBOR.
 * The node's withdrawal classifier and the withdrawal-mistag prover both
 * judge payability with `withdrawalMistagPayable`.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

import { AddressData } from "../common.js";

/** `ledger_output_commitment_v1.max_cardano_value_cbor_bytes`. */
export const WITHDRAWAL_MISTAG_MAXIMUM_VALUE_CBOR_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxOutputValueCborBytes;
/** `env.coins_per_utxo_byte`, the compiled rate step-04 judges min-ADA by. */
export const WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE = BigInt(
  MIDGARD_CONSENSUS_LIMITS.coinsPerUtxoByte,
);

const cborHeadLength = (length: bigint): bigint => {
  if (length < 24n) return 1n;
  if (length <= 255n) return 2n;
  if (length <= 65_535n) return 3n;
  if (length <= 4_294_967_295n) return 5n;
  return 9n;
};

const pointerWordLength = (value: bigint): bigint => {
  if (value < 0n)
    throw new Error("withdrawal pointer component must be non-negative");
  return value < 128n ? 1n : 1n + pointerWordLength(value / 128n);
};

export const withdrawalMistagAddressPayloadBytes = (
  address: AddressData,
): bigint => {
  const stake = address.stakeCredential;
  if (stake === null) return 29n;
  if ("Inline" in stake) return 57n;
  const pointer = stake.Pointer;
  return (
    29n +
    pointerWordLength(pointer.slotNumber) +
    pointerWordLength(pointer.transactionIndex) +
    pointerWordLength(pointer.certificateIndex)
  );
};

/** The two body fields step-04 sizes the payout output by. */
type WithdrawalPayoutTarget = Readonly<{
  addressPayloadBytes: bigint;
  /** `undefined` when step-04's own `expect` refuses the datum (a datum
   * hash that is not 32 bytes): no payout output exists for it. */
  datumOptionBytes: bigint | undefined;
}>;

const L1_ADDRESS_FIELD = 3;
const L1_DATUM_FIELD = 4;

/**
 * Reads the payout target from the withdrawal body's own CBOR. The inline
 * datum is measured on its bytes, re-serialised as Aiken's `cbor.serialise`
 * does, never through a decoded value: a decoded Data map loses repeated
 * keys, and with them bytes step-04 counts.
 */
const withdrawalPayoutTarget = (bodyCbor: string): WithdrawalPayoutTarget => {
  const addressPayloadBytes = withdrawalMistagAddressPayloadBytes(
    Data.from(plutusConstrFieldCbor(bodyCbor, [L1_ADDRESS_FIELD]), AddressData),
  );
  const datum = aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(bodyCbor, [L1_DATUM_FIELD]),
  );
  const decoded = Data.from(datum);
  if (!(decoded instanceof Constr) || decoded.index > 2)
    throw new Error("withdrawal l1_datum is not a Cardano datum option");
  if (decoded.index === 0) return { addressPayloadBytes, datumOptionBytes: 0n };
  const field = plutusConstrFieldCbor(datum, [0]);
  if (decoded.index === 1)
    return {
      addressPayloadBytes,
      // key 2 + [0, bytes(32)]
      datumOptionBytes: field.length === 2 * 34 ? 37n : undefined,
    };
  const dataLength = BigInt(field.length / 2);
  // key 2 + [1, tag(24), bytes(serialiseData)]
  return {
    addressPayloadBytes,
    datumOptionBytes: 5n + cborHeadLength(dataLength) + dataLength,
  };
};

const exactPayoutOutputBytes = (
  target: WithdrawalPayoutTarget,
  cardanoValueSize: bigint,
): bigint | undefined =>
  target.datumOptionBytes === undefined
    ? undefined
    : 3n +
      cborHeadLength(target.addressPayloadBytes) +
      target.addressPayloadBytes +
      cardanoValueSize +
      target.datumOptionBytes;

/** Exact twin of `step_04.exact_payout_output_bytes_v1`, over the
 * withdrawal body's CBOR. Throws where step-04's `expect` fails. */
export const withdrawalMistagExactPayoutOutputBytes = ({
  bodyCbor,
  cardanoValueSize,
}: {
  readonly bodyCbor: string;
  readonly cardanoValueSize: bigint;
}): bigint => {
  if (cardanoValueSize < 0n)
    throw new Error("Cardano value size must be non-negative");
  const bytes = exactPayoutOutputBytes(
    withdrawalPayoutTarget(bodyCbor),
    cardanoValueSize,
  );
  if (bytes === undefined)
    throw new Error("withdrawal datum hash must be 32 bytes");
  return bytes;
};

/** `vm_shared.min_ada_lovelace_v1` at the payout output's exact size. */
export const withdrawalMistagMinimumLovelace = ({
  bodyCbor,
  cardanoValueSize,
  coinsPerUtxoByte = WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE,
}: {
  readonly bodyCbor: string;
  readonly cardanoValueSize: bigint;
  readonly coinsPerUtxoByte?: bigint;
}): bigint =>
  coinsPerUtxoByte *
  (160n +
    withdrawalMistagExactPayoutOutputBytes({ bodyCbor, cardanoValueSize }));

/**
 * Exact twin of `step_04.payout_is_exactly_payable_with_rate_v1`: the
 * withdrawn value fits one L1 output and its lovelace meets that output's
 * min-ADA. The node's withdrawal classifier and the withdrawal-mistag
 * prover both judge payability with this function, so the node never tags a
 * withdrawal the step-04 predicate judges otherwise.
 *
 * `lovelace` is the lovelace of the withdrawn value; once step-03's value
 * check holds it equals the ledger output's.
 */
export const withdrawalMistagPayable = ({
  bodyCbor,
  lovelace,
  cardanoValueSize,
  coinsPerUtxoByte = WITHDRAWAL_MISTAG_COINS_PER_UTXO_BYTE,
}: {
  readonly bodyCbor: string;
  readonly lovelace: bigint;
  readonly cardanoValueSize: bigint;
  readonly coinsPerUtxoByte?: bigint;
}): boolean => {
  if (
    cardanoValueSize <= 0n ||
    cardanoValueSize > BigInt(WITHDRAWAL_MISTAG_MAXIMUM_VALUE_CBOR_BYTES)
  )
    return false;
  const bytes = exactPayoutOutputBytes(
    withdrawalPayoutTarget(bodyCbor),
    cardanoValueSize,
  );
  return bytes !== undefined && lovelace >= coinsPerUtxoByte * (160n + bytes);
};
