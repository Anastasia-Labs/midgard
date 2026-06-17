import { Data, fromText, toUnit } from "@lucid-evolution/lucid";

import type { AuthenticatedValidator } from "@/common.js";
import { HeaderHashSchema } from "@/ledger-state.js";

export const DA_PARAMS_ASSET_NAME = fromText("MIDGARD_DA_PARAMS");
export const DA_ATTESTATION_ASSET_NAME_PREFIX = fromText("DAAT");
export const EMPTY_ATTESTED_SIGNER_BITMAP =
  "0000000000000000000000000000000000000000000000000000000000000000";
export const DA_ATTESTATION_MESSAGE_PREFIX = "MidgardDAAttestationV1";

export const DaParamsDatumSchema = Data.Object({
  committee: Data.Bytes(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  da_threshold: Data.Integer(),
  owners: Data.Array(Data.Bytes({ minLength: 28, maxLength: 28 })),
  update_threshold: Data.Integer(),
});
export type DaParamsDatum = Data.Static<typeof DaParamsDatumSchema>;
export const DaParamsDatum =
  DaParamsDatumSchema as unknown as DaParamsDatum;

export const DaAttestationDatumSchema = Data.Object({
  header_hash: HeaderHashSchema,
  da_threshold: Data.Integer(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  attested_signers: Data.Bytes({ minLength: 32, maxLength: 32 }),
  attestation_count: Data.Integer(),
});
export type DaAttestationDatum = Data.Static<typeof DaAttestationDatumSchema>;
export const DaAttestationDatum =
  DaAttestationDatumSchema as unknown as DaAttestationDatum;

export const DaAttestationMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      state_queue_ref_input_index: Data.Integer(),
      state_queue_mint_ref_script_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ApplyToStateQueue: Data.Object({
      da_attestation_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      state_queue_mint_ref_script_input_index: Data.Integer(),
    }),
  }),
]);
export type DaAttestationMintRedeemer = Data.Static<
  typeof DaAttestationMintRedeemerSchema
>;
export const DaAttestationMintRedeemer =
  DaAttestationMintRedeemerSchema as unknown as DaAttestationMintRedeemer;

export const DaAttestationSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AddSignatures: Data.Object({
      output_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      signatures: Data.Bytes(),
    }),
  }),
  Data.Object({
    BurnForStateQueue: Data.Object({
      mint_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type DaAttestationSpendRedeemer = Data.Static<
  typeof DaAttestationSpendRedeemerSchema
>;
export const DaAttestationSpendRedeemer =
  DaAttestationSpendRedeemerSchema as unknown as DaAttestationSpendRedeemer;

export const daParamsUnit = (
  daParamsGovernor: AuthenticatedValidator,
): string => toUnit(daParamsGovernor.policyId, DA_PARAMS_ASSET_NAME);

export const daAttestationAssetName = (headerHash: string): string =>
  DA_ATTESTATION_ASSET_NAME_PREFIX + headerHash;

export const daAttestationUnit = (
  daAttestation: AuthenticatedValidator,
  headerHash: string,
): string => toUnit(daAttestation.policyId, daAttestationAssetName(headerHash));

export const daAttestationMessage = (headerHash: string): Buffer =>
  Buffer.concat([
    Buffer.from(DA_ATTESTATION_MESSAGE_PREFIX, "utf8"),
    Buffer.from(headerHash, "hex"),
  ]);

export const prefixAttestedSignerBitmap = (signatureCount: number): string => {
  const bitmap = Buffer.alloc(32);
  for (let signerIndex = 0; signerIndex < signatureCount; signerIndex += 1) {
    const byteIndex = Math.floor(signerIndex / 8);
    const bitInByte = signerIndex % 8;
    bitmap[byteIndex] |= 1 << (7 - bitInByte);
  }
  return bitmap.toString("hex");
};
