import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  credentialToAddress,
  Data,
  type Native,
  type Network,
  scriptFromNative,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { H32Schema, PubKeyHashSchema } from "../common.js";
import { HeaderHashSchema } from "../ledger-state.js";

/**
 * `midgard/dispute_bond_v1.DisputeBondV1`: the inline datum VerifySource
 * gives the validation-case challenger bond at `dispute_bond_v1`.
 */
export const DisputeBondSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  fraud_prover: PubKeyHashSchema,
  /** The case NFT name of the case this bond backs. */
  case_id: H32Schema,
});

export type DisputeBond = Data.Static<typeof DisputeBondSchema>;

export const DisputeBond = asDataType<DisputeBond>(DisputeBondSchema);

// One Aiken constructor, `SettleWithCase`: Lucid unwraps a one-member
// Data.Enum, so its fields are modelled as a record with the same
// constructor-0 wire shape.
/**
 * `midgard/dispute_bond_v1.SpendRedeemer`: the bond is spent only by its own
 * case's close, the case input at `case_input_index`.
 */
export const DisputeBondSpendRedeemerSchema = Data.Object({
  case_input_index: Data.Integer(),
});

export type DisputeBondSpendRedeemer = Data.Static<
  typeof DisputeBondSpendRedeemerSchema
>;

export const DisputeBondSpendRedeemer = asDataType<DisputeBondSpendRedeemer>(
  DisputeBondSpendRedeemerSchema,
);

/**
 * The protocol treasury's script hash, blake2b_224(0x00 || CBOR) of the
 * profile's `protocol_treasury_native_script`. The on-chain env derives the
 * same hash from the same script.
 */
export const protocolTreasuryScriptHash = (script: Native): string =>
  validatorToScriptHash(scriptFromNative(script));

/** Where a forfeited bond goes: the treasury script, with no stake credential. */
export const protocolTreasuryAddress = (
  network: Network,
  script: Native,
): string =>
  credentialToAddress(network, {
    type: "Script",
    hash: protocolTreasuryScriptHash(script),
  });
