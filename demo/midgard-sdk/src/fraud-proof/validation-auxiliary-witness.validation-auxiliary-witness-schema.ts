import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import { BoundedItemChunkProofSchema } from "../ledger-state.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";
import {
  CekContextPartsControlSchema,
  CekFinalContextControlSchema,
  CekRedeemerContextControlSchema,
  CekTxInfoAssemblyControlSchema,
  ValueAssetMutationWitnessSchema,
} from "./validation-auxiliary-witness.cek-redeemer-context-control-schema.js";
import { CoreStepEvidenceSchema } from "./validation-auxiliary-witness.core-step-witness-schema.js";
import {
  ByteArrayListSchema,
  DataSequenceSummarySchema,
  FrontierSchema,
} from "./validation-auxiliary-witness.data-node-schema.js";
import {
  LedgerDeltaOperationProofSchema,
  LedgerOutputProofWitnessSchema,
  NativeScriptFrameSchema,
  ProofFrameSchema,
  RedeemerItemProofControlSchema,
  RedeemerItemProofWitnessSchema,
  SignerSetProofSchema,
} from "./validation-auxiliary-witness.ledger-output-proof-witness-schema.js";

export const ValidationAuxiliaryWitnessSchema = Data.Enum([
  Data.Literal("NoAuxiliaryWitness"),
  Data.Object({
    /**
     * One item of one committed field, reached through §8's door. `field_index`
     * rides the wire because §4 removed field-index domain separation and two
     * phases read more than one slot (`CanonicalDecode`, all nine from its own
     * control, and `InputSets`, fields 0 and 1); `item_index` rides it because
     * two sites let the prover choose the item order and pin it in the claimed
     * successor. Neither is a proof — the door authenticates the whole preimage
     * once against the flat §4 commitment and the item is then a slice.
     */
    TransactionFieldChunkWitness: Data.Object({
      field_index: Data.Integer(),
      item_index: Data.Integer(),
      carriage: FieldCarriageSchema,
    }),
  }),
  Data.Object({
    /**
     * A field-4 required-signer item plus the signer-set membership evidence
     * the step decides on. No `field_index`/`item_index`: the field is 4 by
     * construction and the item index is `control.required_seen`.
     */
    RequiredSignerItemWitness: Data.Object({
      carriage: FieldCarriageSchema,
      signer_proof: SignerSetProofSchema,
    }),
  }),
  Data.Object({
    NativeScriptTokenWitness: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
      signer_proof: SignerSetProofSchema,
    }),
  }),
  Data.Object({
    NativeScriptFrameWitness: Data.Object({
      frame: NativeScriptFrameSchema,
    }),
  }),
  Data.Object({
    ScheduledLedgerMembershipWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      value: Data.Bytes(),
      proof: ProofSchema,
      signer_proof: SignerSetProofSchema,
    }),
  }),
  Data.Object({
    ScheduledLedgerNonMembershipWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      proof: ProofSchema,
    }),
  }),
  Data.Object({
    ResolvedInputReplayWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
  Data.Object({
    ScriptPurposeScanWitness: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
      script_hash: Data.Bytes(),
      subject: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    ScriptSourceScanWitness: Data.Object({
      source_index: Data.Integer(),
      origin_kind: Data.Integer(),
      source_key: Data.Bytes(),
      script_language_tag: Data.Integer(),
      script_hash: Data.Bytes(),
      script_total_length: Data.Integer(),
      script_item_commitment: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    RedeemerScanBeginWitness: Data.Object({
      item_index: Data.Integer(),
      item_count: Data.Integer(),
      total_length: Data.Integer(),
      item_commitment: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    NativeExecutionScanWitness: Data.Object({
      execution_index: Data.Integer(),
      language_tag: Data.Integer(),
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
      script_hash: Data.Bytes(),
      subject: Data.Bytes(),
      purpose_siblings: ByteArrayListSchema,
      source_index: Data.Integer(),
      origin_kind: Data.Integer(),
      source_key: Data.Bytes(),
      script_total_length: Data.Integer(),
      script_item_commitment: Data.Bytes(),
      source_siblings: ByteArrayListSchema,
      redeemer_leaf: Data.Bytes(),
      execution_siblings: ByteArrayListSchema,
      first_chunk_proof: BoundedItemChunkProofSchema,
    }),
  }),
  Data.Object({
    CekCoreStepWitness: Data.Object({
      step: CoreStepEvidenceSchema,
    }),
  }),
  Data.Object({
    CekResolvedContextItemWitness: Data.Object({
      source_kind: Data.Integer(),
      item_index: Data.Integer(),
      key: Data.Bytes(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    CekOutputContextItemWitness: Data.Object({
      output_index: Data.Integer(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    CekSignerContextItemWitness: Data.Object({
      peaks: FrontierSchema,
      signer_index: Data.Integer(),
      signer_hash: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    CekMintContextItemWitness: Data.Object({
      mint_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      siblings: ByteArrayListSchema,
      previous: Data.Nullable(
        Data.Object({
          asset_name: Data.Bytes(),
          quantity: Data.Integer(),
          tail: DataSequenceSummarySchema,
        }),
      ),
    }),
  }),
  Data.Object({
    CekRedeemerContextSelectWitness: Data.Object({
      control: CekRedeemerContextControlSchema,
      item_index: Data.Integer(),
      item_count: Data.Integer(),
      total_length: Data.Integer(),
      item_commitment: Data.Bytes(),
      redeemer_siblings: ByteArrayListSchema,
      purpose_frontier_index: Data.Integer(),
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
      script_hash: Data.Bytes(),
      subject: Data.Bytes(),
      purpose_siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    RedeemerItemStepWitness: Data.Object({
      redeemer_control: Data.Nullable(CekRedeemerContextControlSchema),
      control: RedeemerItemProofControlSchema,
      witness: RedeemerItemProofWitnessSchema,
    }),
  }),
  Data.Object({
    CekContextFinalizeWitness: Data.Object({
      redeemer_control: CekRedeemerContextControlSchema,
    }),
  }),
  Data.Object({
    CekContextFinalizeSpendWitness: Data.Object({
      redeemer_control: CekRedeemerContextControlSchema,
      item_index: Data.Integer(),
      key: Data.Bytes(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    CekContextAssembleWitness: Data.Object({
      control: CekContextPartsControlSchema,
    }),
  }),
  Data.Object({
    CekTxInfoFinalizeWitness: Data.Object({
      control: CekTxInfoAssemblyControlSchema,
    }),
  }),
  Data.Object({
    CekContextSeedWitness: Data.Object({
      control: CekFinalContextControlSchema,
    }),
  }),
  Data.Object({
    ValueInputAssetWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      descriptor_cbor: Data.Bytes(),
      asset_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      asset_peaks: FrontierSchema,
      asset_siblings: ByteArrayListSchema,
      mutation: ValueAssetMutationWitnessSchema,
    }),
  }),
  Data.Object({
    ValueOutputAssetWitness: Data.Object({
      output_index: Data.Integer(),
      descriptor_cbor: Data.Bytes(),
      asset_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      asset_peaks: FrontierSchema,
      asset_siblings: ByteArrayListSchema,
      mutation: ValueAssetMutationWitnessSchema,
    }),
  }),
  Data.Object({
    ValueMintAssetWitness: Data.Object({
      mint_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      siblings: ByteArrayListSchema,
      mutation: ValueAssetMutationWitnessSchema,
    }),
  }),
  Data.Object({
    LedgerDeltaReplayWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
  Data.Object({
    LedgerDeltaOutputWitness: Data.Object({
      output_index: Data.Integer(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    /**
     * `ScriptSources` stage 1 (field 8, one redeemer item) and stage 4 (field 2,
     * one output item). Both stages need the item's length and its
     * `bounded_item_v1` commitment and never look at its bytes, so the door's
     * derived commitment is all the carriage has to yield; field index and item
     * index are fixed by the stage and its cursor.
     */
    TransactionRedeemerItemBeginWitness: Data.Object({
      carriage: FieldCarriageSchema,
    }),
  }),
  Data.Object({
    /**
     * `CanonicalDecode`'s complete-item step: one item read whole rather than
     * chunk by chunk. Field index and item index come from the phase's control,
     * so the carriage is the entire wire surface. The item bytes that used to
     * ride here as `item_cbor` are read out of the authenticated preimage now.
     */
    TransactionFieldItemWitness: Data.Object({
      carriage: FieldCarriageSchema,
    }),
  }),
  Data.Object({
    LedgerOutputProofBeginWitness: Data.Object({
      output_index: Data.Integer(),
      total_length: Data.Integer(),
      item_commitment: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    LedgerOutputProofStepWitness: Data.Object({
      witness: LedgerOutputProofWitnessSchema,
    }),
  }),
  Data.Object({
    LedgerOutputProofFinalizeWitness: Data.Object({
      descriptor_cbor: Data.Bytes(),
      signer_proof: SignerSetProofSchema,
    }),
  }),
  Data.Object({
    LedgerDeltaProofFrameWitness: Data.Object({
      frame: ProofFrameSchema,
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    LedgerDeltaOperationWitness: Data.Object({
      operation_kind: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
      operation_proof: LedgerDeltaOperationProofSchema,
    }),
  }),
  Data.Object({
    ScriptSourceHashBlockWitness: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Object({
    NativeExecutionDescriptorWitness: Data.Object({
      execution_index: Data.Integer(),
      language_tag: Data.Integer(),
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
      script_hash: Data.Bytes(),
      subject: Data.Bytes(),
      purpose_siblings: ByteArrayListSchema,
      source_index: Data.Integer(),
      origin_kind: Data.Integer(),
      source_key: Data.Bytes(),
      script_total_length: Data.Integer(),
      script_item_commitment: Data.Bytes(),
      source_siblings: ByteArrayListSchema,
      redeemer_leaf: Data.Bytes(),
      execution_siblings: ByteArrayListSchema,
      first_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
      signer_peaks: FrontierSchema,
    }),
  }),
  Data.Object({
    ValueOutputDescriptorWitness: Data.Object({
      output_index: Data.Integer(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    MintFoldAssetWitness: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
]);
