import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  credentialToAddress,
  Data,
  type Native,
  type Network,
  scriptFromNative,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { PubKeyHashSchema } from "../common.js";
import { HeaderHashSchema } from "../ledger-state.js";

/**
 * `midgard/dispute_bond_v1.DisputeBondV1`: the inline datum VerifySource
 * gives the validation-dispute bond at `dispute_bond_v1`.
 */
export const DisputeBondSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  fraud_prover: PubKeyHashSchema,
  forfeit_after: Data.Integer(),
});

export type DisputeBond = Data.Static<typeof DisputeBondSchema>;

export const DisputeBond = asDataType<DisputeBond>(DisputeBondSchema);

/**
 * `midgard/dispute_bond_v1.SpendRedeemer`. Refund needs the fraud-proof token
 * of the challenged block by reference; Forfeit needs a validity lower bound
 * after `forfeit_after`. Each pays the output at `output_index`, tagged with
 * `InlineDatum(bond out ref)`.
 */
export const DisputeBondSpendRedeemerSchema = Data.Enum([
  Data.Object({
    Refund: Data.Object({
      fraud_proof_ref_input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Forfeit: Data.Object({ output_index: Data.Integer() }),
  }),
]);

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
