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
import { asDataType } from "@al-ft/midgard-core/lucid-data";
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
export const NetworkIdFault = asDataType<NetworkIdFault>(NetworkIdFaultSchema);

export const NetworkIdStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
/**
 * Datum locked at the forced door between step 01 and step 02. The shared
 * `continue` helper admits only a populated successor state, so step 01 hands
 * over exactly the `ForcedNetworkIdMismatch` marker and the forced step
 * re-authenticates it before binding the forced leaf.
 */
export const NetworkIdForcedStepDatumSchema =
  faultProofStepDatumSchema(NetworkIdFaultSchema);
export type NetworkIdForcedStepDatum = Data.Static<
  typeof NetworkIdForcedStepDatumSchema
>;
export const NetworkIdForcedStepDatum = asDataType<NetworkIdForcedStepDatum>(
  NetworkIdForcedStepDatumSchema,
);
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
  asDataType<NetworkIdPostUtxoPredecessor>(NetworkIdPostUtxoPredecessorSchema);
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
  asDataType<NetworkIdPostUtxoMembership>(NetworkIdPostUtxoMembershipSchema);
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
export const NetworkIdStep02State = asDataType<NetworkIdStep02State>(
  NetworkIdStep02StateSchema,
);

export const NetworkIdStep02DatumSchema = faultProofStepDatumSchema(
  NetworkIdStep02StateSchema,
);
export type NetworkIdStep02Datum = Data.Static<
  typeof NetworkIdStep02DatumSchema
>;
export const NetworkIdStep02Datum = asDataType<NetworkIdStep02Datum>(
  NetworkIdStep02DatumSchema,
);
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
export const NetworkIdStep02Args = asDataType<NetworkIdStep02Args>(
  NetworkIdStep02ArgsSchema,
);
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

/**
 * ## Forced outputs scan (`fraud_proofs/network_id/forced_scan`)
 *
 * The wrongful-rejection direction has to prove that *no* output of the
 * rejected forced transaction names a foreign network. A single transaction
 * cannot decode the raw-carriage bound (≈352 minimal outputs) inside the
 * execution reserve, so the forced door hands the thread to a self-looping
 * scan validator that walks the outputs field in batches and only then writes
 * step 02's terminal state.
 *
 * Constructor order below is consensus wire format and mirrors
 * `onchain/aiken/lib/midgard/fraud-proofs/network-id/forced-scan.ak` exactly.
 */
export const NetworkIdForcedScanBoundSchema = Data.Object({
  bad_tx_id: H32Schema,
  committed_tx_network_id: Data.Integer(),
  expected_network_id: Data.Integer(),
  forced_source_key: Data.Bytes(),
});
export type NetworkIdForcedScanBound = Data.Static<
  typeof NetworkIdForcedScanBoundSchema
>;
export const NetworkIdForcedScanBound = asDataType<NetworkIdForcedScanBound>(
  NetworkIdForcedScanBoundSchema,
);

/**
 * `Ready` is what the forced door writes, `Grammar` is a tier-3 envelope
 * certification in progress, and `Scanning` is the semantic walk. Only
 * `Scanning` can complete into step 02.
 */
export const NetworkIdForcedScanStateSchema = Data.Enum([
  Data.Object({
    Ready: Data.Object({ bound: NetworkIdForcedScanBoundSchema }),
  }),
  Data.Object({
    Grammar: Data.Object({
      bound: NetworkIdForcedScanBoundSchema,
      checkpoint_hash: H32Schema,
    }),
  }),
  Data.Object({
    Scanning: Data.Object({
      bound: NetworkIdForcedScanBoundSchema,
      checkpoint_hash: H32Schema,
    }),
  }),
]);
export type NetworkIdForcedScanState = Data.Static<
  typeof NetworkIdForcedScanStateSchema
>;
export const NetworkIdForcedScanState = asDataType<NetworkIdForcedScanState>(
  NetworkIdForcedScanStateSchema,
);

export const NetworkIdForcedScanDatumSchema = faultProofStepDatumSchema(
  NetworkIdForcedScanStateSchema,
);
export type NetworkIdForcedScanDatum = Data.Static<
  typeof NetworkIdForcedScanDatumSchema
>;
export const NetworkIdForcedScanDatum = asDataType<NetworkIdForcedScanDatum>(
  NetworkIdForcedScanDatumSchema,
);

export const NetworkIdForcedScanActionSchema = Data.Enum([
  Data.Object({
    Open: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    ResumeGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinishGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
    }),
  }),
  Data.Object({
    Advance: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export type NetworkIdForcedScanAction = Data.Static<
  typeof NetworkIdForcedScanActionSchema
>;
export const NetworkIdForcedScanAction = asDataType<NetworkIdForcedScanAction>(
  NetworkIdForcedScanActionSchema,
);

export const NetworkIdForcedScanSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NetworkIdForcedScanActionSchema);

/**
 * Largest semantic batch one `Advance` may fold, and the largest envelope
 * batch one grammar action may certify. Both are the exact `scan_batch` /
 * `grammar_batch` constants the validator enforces, so a builder that plans
 * against these numbers never has a batch refused for its size alone.
 */
export const NETWORK_ID_FORCED_SCAN_BATCH = 64n;
export const NETWORK_ID_FORCED_GRAMMAR_BATCH = 128n;
