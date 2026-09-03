/**
 * Q35 `network-id` fault-proof wire types.
 *
 * Constructor order is consensus wire format. `TransactionNetwork` is Constr
 * 0 and `OutputNetwork` is Constr 1, matching the Aiken family modules. The
 * former convicts only an explicit transaction-body mismatch (native scalar
 * 255 is Cardano's absent value); the latter names one output-address item.
 *
 * Catalogue registration is append-only at category id `0000001c`; the shared
 * contract loader applies the deployment network id to step 01.
 */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  NonMembershipCarriageSchema,
} from "./native.js";

export const NetworkIdFaultSchema = Data.Enum([
  Data.Literal("TransactionNetwork"),
  Data.Object({ OutputNetwork: Data.Object({ output_index: Data.Integer() }) }),
  Data.Object({
    OutputNetworkUtxo: Data.Object({
      observed_network_id: Data.Integer(),
    }),
  }),
  Data.Literal("ForcedNetworkIdMismatch"),
]);
export type NetworkIdFault = Data.Static<typeof NetworkIdFaultSchema>;
export const NetworkIdFault = NetworkIdFaultSchema as unknown as NetworkIdFault;

export const NetworkIdStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export const NetworkIdPostUtxoPredecessorSchema = Data.Enum([
  Data.Literal("Introduced"),
  Data.Object({
    NetworkChanged: Data.Object({
      previous_descriptor_cbor: Data.Bytes(),
    }),
  }),
]);
export type NetworkIdPostUtxoPredecessor = Data.Static<
  typeof NetworkIdPostUtxoPredecessorSchema
>;
export const NetworkIdPostUtxoPredecessor =
  NetworkIdPostUtxoPredecessorSchema as unknown as NetworkIdPostUtxoPredecessor;
export const NetworkIdPostUtxoMembershipSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  out_ref: OutputReferenceSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
  predecessor: NetworkIdPostUtxoPredecessorSchema,
});
export type NetworkIdPostUtxoMembership = Data.Static<
  typeof NetworkIdPostUtxoMembershipSchema
>;
export const NetworkIdPostUtxoMembership =
  NetworkIdPostUtxoMembershipSchema as unknown as NetworkIdPostUtxoMembership;
export const NetworkIdStep01ArgsSchema = Data.Object({
  tx_inclusion: Data.Nullable(NativeTxInclusionCarriageSchema),
  post_utxo_membership: Data.Nullable(NetworkIdPostUtxoMembershipSchema),
  forced_source: Data.Nullable(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  ),
  fault: NetworkIdFaultSchema,
});
export const NetworkIdStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NetworkIdStep01ArgsSchema,
);

export const NetworkIdForcedStepArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});
export const NetworkIdForcedStepSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NetworkIdForcedStepArgsSchema);

export const NetworkIdStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  committed_tx_network_id: Data.Integer(),
  expected_network_id: Data.Integer(),
  fault: NetworkIdFaultSchema,
  post_utxo: Data.Nullable(
    Data.Object({
      out_ref: OutputReferenceSchema,
      descriptor_cbor: Data.Bytes(),
      prev_utxos_root: H32Schema,
      predecessor: NetworkIdPostUtxoPredecessorSchema,
    }),
  ),
  forced_source_key: Data.Nullable(Data.Bytes()),
});
export type NetworkIdStep02State = Data.Static<
  typeof NetworkIdStep02StateSchema
>;
export const NetworkIdStep02State =
  NetworkIdStep02StateSchema as unknown as NetworkIdStep02State;

export const NetworkIdStep02DatumSchema = faultProofStepDatumSchema(
  NetworkIdStep02StateSchema,
);
export type NetworkIdStep02Datum = Data.Static<
  typeof NetworkIdStep02DatumSchema
>;
export const NetworkIdStep02Datum =
  NetworkIdStep02DatumSchema as unknown as NetworkIdStep02Datum;
export const NetworkIdStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_opening: Data.Nullable(FieldOpeningSchema),
  predecessor_carriage: Data.Nullable(
    Data.Enum([
      Data.Object({
        IntroducedPredecessor: Data.Tuple([NonMembershipCarriageSchema]),
      }),
      Data.Object({
        NetworkChangedPredecessor: Data.Tuple([MembershipCarriageSchema]),
      }),
    ]),
  ),
});
export type NetworkIdStep02Args = Data.Static<typeof NetworkIdStep02ArgsSchema>;
export const NetworkIdStep02Args =
  NetworkIdStep02ArgsSchema as unknown as NetworkIdStep02Args;
export const NetworkIdStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NetworkIdStep02ArgsSchema,
);

/** Pure twin of the final transaction-body predicate. */
export const isExplicitTransactionNetworkMismatch = ({
  committedNetworkId,
  expectedNetworkId,
}: {
  readonly committedNetworkId: bigint;
  readonly expectedNetworkId: 0n | 1n;
}): boolean =>
  committedNetworkId !== 255n && committedNetworkId !== expectedNetworkId;

/** Complete twin used for wrongful forced-rejection contradiction. */
export const isAnyNetworkIdMismatch = ({
  committedNetworkId,
  outputNetworkIds,
  expectedNetworkId,
}: {
  readonly committedNetworkId: bigint;
  readonly outputNetworkIds: readonly bigint[];
  readonly expectedNetworkId: 0n | 1n;
}): boolean =>
  isExplicitTransactionNetworkMismatch({
    committedNetworkId,
    expectedNetworkId,
  }) || outputNetworkIds.some((networkId) => networkId !== expectedNetworkId);
