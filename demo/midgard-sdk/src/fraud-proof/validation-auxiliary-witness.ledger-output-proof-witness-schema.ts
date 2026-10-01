import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofStepSchema } from "../common.js";
import { BoundedItemChunkProofSchema } from "../ledger-state.js";
import { CekBlobFrontierSchema } from "./validation-auxiliary-witness.core-step-witness-schema.js";
import {
  ByteArrayListSchema,
  DataSequenceSummarySchema,
  DataSummarySchema,
  FrontierSchema,
} from "./validation-auxiliary-witness.data-node-schema.js";

const Blake2b256TraceControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  cursor: Data.Integer(),
  total_length: Data.Integer(),
  chaining_value: Data.Bytes(),
  active_block: Data.Bytes(),
  active_block_length: Data.Integer(),
  working_value: Data.Bytes(),
  round: Data.Integer(),
});

const CekSourceBlobControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  frontier: CekBlobFrontierSchema,
  active_hash: Data.Nullable(Blake2b256TraceControlSchema),
});

const CekDataIntegerControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  memory: Data.Integer(),
  blob: Data.Nullable(CekSourceBlobControlSchema),
});

const CekDataBytesControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  bytes_length: Data.Integer(),
  blob: Data.Nullable(CekSourceBlobControlSchema),
});

const DataFrameSchema = Data.Object({
  kind: Data.Integer(),
  constructor: Data.Integer(),
  constructor_cbor_root: Data.Bytes(),
  constructor_cbor_length: Data.Integer(),
  constructor_memory: Data.Integer(),
  tail: Data.Bytes(),
  expected_children: Data.Integer(),
  child_count: Data.Integer(),
  child_peaks: FrontierSchema,
  fold_cursor: Data.Integer(),
  sequence: DataSequenceSummarySchema,
});

const DataTraverseControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  offset: Data.Integer(),
  frame_root: Data.Bytes(),
  integer: Data.Nullable(CekDataIntegerControlSchema),
  bytes: Data.Nullable(CekDataBytesControlSchema),
  result: Data.Nullable(DataSummarySchema),
});

const DataTraverseActionSchema = Data.Enum([
  Data.Literal("NoAction"),
  Data.Literal("HeadScalar"),
  Data.Literal("HeadSequence"),
  Data.Literal("HeadMap"),
  Data.Literal("HeadLargeConstructor"),
  Data.Object({
    AttachScalar: Data.Object({
      parent: Data.Nullable(DataFrameSchema),
    }),
  }),
  Data.Object({
    FoldList: Data.Object({
      frame: DataFrameSchema,
      child_index: Data.Integer(),
      child: DataSummarySchema,
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    FoldMap: Data.Object({
      frame: DataFrameSchema,
      pair_index: Data.Integer(),
      key: DataSummarySchema,
      value: DataSummarySchema,
      key_siblings: ByteArrayListSchema,
      value_siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    FinalizeFrame: Data.Object({
      frame: DataFrameSchema,
      parent: Data.Nullable(DataFrameSchema),
    }),
  }),
]);

export const RedeemerItemProofControlSchema = Data.Object({
  version: Data.Integer(),
  mode: Data.Integer(),
  stage: Data.Integer(),
  item_index: Data.Integer(),
  item_count: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  expected_purpose_tag: Data.Integer(),
  expected_pointer_index: Data.Integer(),
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  data_offset: Data.Integer(),
  data_length: Data.Integer(),
  execution_memory: Data.Integer(),
  execution_steps: Data.Integer(),
  traversal: Data.Nullable(DataTraverseControlSchema),
});

const RedeemerItemProofActionSchema = Data.Enum([
  Data.Literal("RedeemerItemOpenHeader"),
  Data.Literal("RedeemerItemOpenTail"),
  Data.Object({
    RedeemerItemTraverseData: Data.Object({
      action: DataTraverseActionSchema,
    }),
  }),
  Data.Literal("RedeemerItemFinishData"),
]);

export const RedeemerItemProofWitnessSchema = Data.Object({
  action: RedeemerItemProofActionSchema,
  chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
});

/**
 * Twin of `midgard/native_script_scan_v1.NativeScriptFrame` — exported so
 * the native-script-decoding family's `Scan` redeemer carries the same wire
 * identity rather than declaring a second one.
 */
export const NativeScriptFrameSchema = Data.Object({
  tail: Data.Bytes(),
  kind: Data.Integer(),
  child_count: Data.Integer(),
  remaining: Data.Integer(),
  valid_count: Data.Integer(),
  required: Data.Integer(),
});

export type NativeScriptFrame = Data.Static<typeof NativeScriptFrameSchema>;

export const NativeScriptFrame = asDataType<NativeScriptFrame>(
  NativeScriptFrameSchema,
);

/**
 * Twin of `midgard/native_tx_script_pushdown_v1.NativeScriptFrame`.
 * This semantic-evaluation frame is intentionally distinct from the
 * six-field structure-scan frame above.
 */
export const NativeScriptPushdownFrameSchema = Data.Object({
  kind: Data.Integer(),
  remaining: Data.Integer(),
  satisfied: Data.Integer(),
  required: Data.Integer(),
});

export type NativeScriptPushdownFrame = Data.Static<
  typeof NativeScriptPushdownFrameSchema
>;

export const NativeScriptPushdownFrame = asDataType<NativeScriptPushdownFrame>(
  NativeScriptPushdownFrameSchema,
);

export const SignerSetProofSchema = Data.Enum([
  Data.Literal("NoSignerSetProof"),
  Data.Object({
    SignerMembershipProof: Data.Object({
      peaks: FrontierSchema,
      signer_index: Data.Integer(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    EmptySignerSetProof: Data.Object({
      peaks: FrontierSchema,
    }),
  }),
  Data.Object({
    SignerBelowFirstProof: Data.Object({
      peaks: FrontierSchema,
      first_signer_hash: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    SignerAboveLastProof: Data.Object({
      peaks: FrontierSchema,
      last_signer_hash: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    SignerBetweenProof: Data.Object({
      peaks: FrontierSchema,
      lower_index: Data.Integer(),
      lower_signer_hash: Data.Bytes(),
      lower_siblings: ByteArrayListSchema,
      upper_signer_hash: Data.Bytes(),
      upper_siblings: ByteArrayListSchema,
    }),
  }),
]);

export type SignerSetProof = Data.Static<typeof SignerSetProofSchema>;

export const SignerSetProof = asDataType<SignerSetProof>(SignerSetProofSchema);

export const LedgerOutputProofWitnessSchema = Data.Enum([
  Data.Literal("LedgerOutputProofNoWitness"),
  Data.Object({
    LedgerOutputProofChunks: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Object({
    LedgerOutputProofValue: Data.Object({
      asset_index: Data.Integer(),
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
    LedgerOutputProofDatum: Data.Object({
      action: DataTraverseActionSchema,
      window: Data.Nullable(Data.Bytes()),
    }),
  }),
  Data.Object({
    LedgerOutputProofNativeFrame: Data.Object({
      frame: NativeScriptFrameSchema,
    }),
  }),
  Data.Object({
    LedgerOutputProofSpanAttach: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Object({
    LedgerOutputProofWindow: Data.Object({
      bytes: Data.Bytes(),
    }),
  }),
]);

export const ProofFrameSchema = Data.Object({
  version: Data.Integer(),
  frame_index: Data.Integer(),
  cursor: Data.Integer(),
  next_cursor: Data.Integer(),
  step: ProofStepSchema,
});

const ProofDescriptorSchema = Data.Object({
  version: Data.Integer(),
  frame_count: Data.Integer(),
  terminal_cursor: Data.Integer(),
  peaks: FrontierSchema,
});

export const LedgerDeltaOperationProofSchema = Data.Object({
  descriptor: ProofDescriptorSchema,
  operation_count: Data.Integer(),
  operation_peaks: FrontierSchema,
  operation_index: Data.Integer(),
  operation_siblings: ByteArrayListSchema,
});
