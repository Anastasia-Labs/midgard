import { Data } from "@lucid-evolution/lucid";

import { ProofSchema, ProofStepSchema } from "../common.js";
import { BoundedItemChunkProofSchema } from "../ledger-state.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";

type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

const ByteArrayListSchema = Data.Array(Data.Bytes());

export const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});
export type FrontierPeak = Data.Static<typeof FrontierPeakSchema>;
export const FrontierPeak = FrontierPeakSchema as unknown as FrontierPeak;

const FrontierSchema = Data.Array(FrontierPeakSchema);

const DataSummarySchema = Data.Object({
  root: Data.Bytes(),
  cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const DataSequenceSummarySchema = Data.Object({
  root: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const ConstantWitnessSchema = Data.Object({
  type_cbor: Data.Bytes(),
  payload_cbor: Data.Bytes(),
});

const DataNodeSchema = Data.Enum([
  Data.Object({
    ConstrSmallData: Data.Object({
      constructor: Data.Integer(),
      fields_count: Data.Integer(),
      fields_root: Data.Bytes(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    ConstrLargeData: Data.Object({
      constructor_cbor_root: Data.Bytes(),
      constructor_cbor_length: Data.Integer(),
      constructor_memory: Data.Integer(),
      fields_count: Data.Integer(),
      fields_root: Data.Bytes(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    MapData: Data.Object({
      entries_count: Data.Integer(),
      entries_root: Data.Bytes(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    ListData: Data.Object({
      items_count: Data.Integer(),
      items_root: Data.Bytes(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    IntegerData: Data.Object({
      cbor_root: Data.Bytes(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    BytesData: Data.Object({
      bytes_root: Data.Bytes(),
      bytes_length: Data.Integer(),
      cbor_length: Data.Integer(),
      memory: Data.Integer(),
    }),
  }),
]);

const DataListNodeSchema = Data.Object({
  head: Data.Bytes(),
  head_cbor_length: Data.Integer(),
  head_memory: Data.Integer(),
  tail: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const DataPairNodeSchema = Data.Object({
  key: Data.Bytes(),
  key_cbor_length: Data.Integer(),
  key_memory: Data.Integer(),
  value: Data.Bytes(),
  value_cbor_length: Data.Integer(),
  value_memory: Data.Integer(),
  tail: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const SemanticBuiltinWitnessSchema = Data.Object({
  data_nodes: Data.Array(DataNodeSchema),
  list_nodes: Data.Array(DataListNodeSchema),
  pair_nodes: Data.Array(DataPairNodeSchema),
  scalar_preimages: ByteArrayListSchema,
});

const DirectValueWitnessSchema = Data.Enum([
  Data.Object({
    ConstantValue: Data.Tuple([ConstantWitnessSchema]),
  }),
  Data.Object({
    SemanticConstantValue: Data.Object({
      type_cbor: Data.Bytes(),
      payload: DataSummarySchema,
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    OpaqueValue: Data.Tuple([Data.Bytes()]),
  }),
  Data.Object({
    BlsMillerLoopValue: Data.Tuple([Data.Bytes()]),
  }),
]);

const RuntimeValueWitnessSchema = Data.Enum([
  Data.Object({
    RuntimeConstantValue: Data.Tuple([ConstantWitnessSchema]),
  }),
  Data.Object({
    RuntimeSemanticConstantValue: Data.Object({
      type_cbor: Data.Bytes(),
      payload: DataSummarySchema,
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    RuntimeLambdaValue: Data.Object({
      body: Data.Bytes(),
      environment: Data.Bytes(),
    }),
  }),
  Data.Object({
    RuntimeDelayValue: Data.Object({
      body: Data.Bytes(),
      environment: Data.Bytes(),
    }),
  }),
  Data.Object({
    RuntimeConstrValue: Data.Object({
      tag: Data.Integer(),
      values_count: Data.Integer(),
      values_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    RuntimeBuiltinValue: Data.Object({
      tag: Data.Integer(),
      forces_remaining: Data.Integer(),
      arguments_count: Data.Integer(),
      arguments_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    RuntimeBlsMillerLoopValue: Data.Object({
      expression_root: Data.Bytes(),
    }),
  }),
]);

/*
 * Lucid 0.6 does not expose recursive Data schemas. The validator accepts at
 * most ten levels for either side of ExecuteBuiltinBlsFinal, so an exact
 * finite expansion covers every value the on-chain transition can accept.
 */
const blsExpressionWitnessSchema = (depth: number): PlutusDataSchema => {
  const millerLoop = Data.Object({
    BlsMillerLoopExpression: Data.Object({
      g1: ConstantWitnessSchema,
      g2: ConstantWitnessSchema,
    }),
  });
  if (depth === 1) {
    return Data.Enum([millerLoop]);
  }
  const child = blsExpressionWitnessSchema(depth - 1);
  return Data.Enum([
    millerLoop,
    Data.Object({
      BlsMultiplyExpression: Data.Object({
        left: child,
        right: child,
      }),
    }),
  ]);
};

const BlsExpressionWitnessSchema = blsExpressionWitnessSchema(10);

const CekMachineStateSchema = Data.Object({
  mode: Data.Integer(),
  execution_index: Data.Integer(),
  focus_root: Data.Bytes(),
  environment_root: Data.Bytes(),
  continuation_root: Data.Bytes(),
  auxiliary: Data.Integer(),
  cpu: Data.Integer(),
  memory: Data.Integer(),
});

const EnvironmentSummarySchema = Data.Enum([
  Data.Literal("EmptyEnvironmentSummary"),
  Data.Object({
    NonEmptyEnvironmentSummary: Data.Object({
      value: Data.Bytes(),
      tail: Data.Bytes(),
      length: Data.Integer(),
    }),
  }),
]);

const MachineValueWitnessSchema = Data.Enum([
  Data.Object({
    ConstantValue: Data.Object({
      type_root: Data.Bytes(),
      payload_root: Data.Bytes(),
      payload_length: Data.Integer(),
      semantic_root: Data.Bytes(),
      memory: Data.Integer(),
    }),
  }),
  Data.Object({
    LambdaValue: Data.Object({
      body: Data.Bytes(),
      environment: Data.Bytes(),
    }),
  }),
  Data.Object({
    DelayValue: Data.Object({
      body: Data.Bytes(),
      environment: Data.Bytes(),
    }),
  }),
  Data.Object({
    ConstrValue: Data.Object({
      tag: Data.Integer(),
      values_count: Data.Integer(),
      values_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    BuiltinValue: Data.Object({
      tag: Data.Integer(),
      forces_remaining: Data.Integer(),
      arguments_count: Data.Integer(),
      arguments_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    BlsMillerLoopValue: Data.Object({
      expression_root: Data.Bytes(),
    }),
  }),
]);

const MapConversionControlSchema = Data.Object({
  tag: Data.Integer(),
  result_root: Data.Bytes(),
  source_root: Data.Bytes(),
  source_remaining: Data.Integer(),
  source_payload_cbor_length: Data.Integer(),
  source_memory: Data.Integer(),
  destination_root: Data.Bytes(),
  destination_remaining: Data.Integer(),
  destination_payload_cbor_length: Data.Integer(),
  destination_memory: Data.Integer(),
  budget_cpu: Data.Integer(),
  budget_memory: Data.Integer(),
});

const MapConversionStartWitnessSchema = Data.Object({
  source_node: DataNodeSchema,
  source_list: Data.Nullable(DataListNodeSchema),
  source_pairs: Data.Nullable(DataPairNodeSchema),
  result_node: DataNodeSchema,
  result_list: Data.Nullable(DataListNodeSchema),
  result_pairs: Data.Nullable(DataPairNodeSchema),
});

const CoreStepWitnessSchema = Data.Enum([
  Data.Object({
    ComputeVariable: Data.Object({ index: Data.Integer() }),
  }),
  Data.Object({
    ComputeConstant: Data.Object({ value: ConstantWitnessSchema }),
  }),
  Data.Object({
    ComputeLambda: Data.Object({ body: Data.Bytes() }),
  }),
  Data.Object({
    ComputeDelay: Data.Object({ body: Data.Bytes() }),
  }),
  Data.Object({
    ComputeApplication: Data.Object({
      function: Data.Bytes(),
      argument: Data.Bytes(),
    }),
  }),
  Data.Object({
    ComputeForce: Data.Object({ term: Data.Bytes() }),
  }),
  Data.Literal("ComputeError"),
  Data.Object({
    ComputeBuiltin: Data.Object({ tag: Data.Integer() }),
  }),
  Data.Object({
    ComputeConstrEmpty: Data.Object({ tag: Data.Integer() }),
  }),
  Data.Object({
    ComputeConstrNonEmpty: Data.Object({
      tag: Data.Integer(),
      terms_count: Data.Integer(),
      first_term: Data.Bytes(),
      remaining_terms_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    ComputeCase: Data.Object({
      scrutinee: Data.Bytes(),
      branches_count: Data.Integer(),
      branches_root: Data.Bytes(),
    }),
  }),
  Data.Object({
    LookupEnvironment: Data.Object({
      value: Data.Bytes(),
      tail: Data.Bytes(),
      length: Data.Integer(),
    }),
  }),
  Data.Literal("LookupEmptyEnvironment"),
  Data.Object({
    ReturnEmptyContinuation: Data.Object({
      value: MachineValueWitnessSchema,
    }),
  }),
  Data.Object({
    ReturnApplyArgument: Data.Object({
      argument: Data.Bytes(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyLambda: Data.Object({
      body: Data.Bytes(),
      closure_environment: Data.Bytes(),
      closure_summary: EnvironmentSummarySchema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyBuiltin: Data.Object({
      tag: Data.Integer(),
      forces_remaining: Data.Integer(),
      arguments_count: Data.Integer(),
      arguments_root: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyInvalid: Data.Object({
      function: MachineValueWitnessSchema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyValueLambda: Data.Object({
      argument: Data.Bytes(),
      body: Data.Bytes(),
      closure_environment: Data.Bytes(),
      closure_summary: EnvironmentSummarySchema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyValueBuiltin: Data.Object({
      argument: Data.Bytes(),
      tag: Data.Integer(),
      forces_remaining: Data.Integer(),
      arguments_count: Data.Integer(),
      arguments_root: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyValueInvalid: Data.Object({
      argument: Data.Bytes(),
      function: MachineValueWitnessSchema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnForceDelay: Data.Object({
      body: Data.Bytes(),
      closure_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnForceBuiltin: Data.Object({
      tag: Data.Integer(),
      forces_remaining: Data.Integer(),
      arguments_count: Data.Integer(),
      arguments_root: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnForceInvalid: Data.Object({
      value: MachineValueWitnessSchema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnConstrNext: Data.Object({
      tag: Data.Integer(),
      remaining_terms_count: Data.Integer(),
      next_term: Data.Bytes(),
      remaining_terms_tail: Data.Bytes(),
      values_count: Data.Integer(),
      values_root: Data.Bytes(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnConstrDone: Data.Object({
      tag: Data.Integer(),
      values_count: Data.Integer(),
      values_root: Data.Bytes(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnCaseConstr: Data.Object({
      tag: Data.Integer(),
      values_count: Data.Integer(),
      values_root: Data.Bytes(),
      branches_count: Data.Integer(),
      branches_root: Data.Bytes(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnCaseInvalid: Data.Object({
      value: MachineValueWitnessSchema,
      branches_count: Data.Integer(),
      branches_root: Data.Bytes(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    SelectCaseBranch: Data.Object({
      branch: Data.Bytes(),
      remaining_branches_root: Data.Bytes(),
      length: Data.Integer(),
      captured_environment: Data.Bytes(),
      tail: Data.Bytes(),
      values_count: Data.Integer(),
    }),
  }),
  Data.Object({
    ApplyCaseValue: Data.Object({
      value: Data.Bytes(),
      remaining_values_root: Data.Bytes(),
      length: Data.Integer(),
      captured_environment: Data.Bytes(),
      built_continuation: Data.Bytes(),
    }),
  }),
  Data.Object({
    ExecuteBuiltinDirect: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessSchema),
      result: DirectValueWitnessSchema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinSemantic: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessSchema),
      result: DirectValueWitnessSchema,
      material: SemanticBuiltinWitnessSchema,
    }),
  }),
  Data.Object({
    StartBuiltinMapConversion: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessSchema),
      result: DirectValueWitnessSchema,
      material: MapConversionStartWitnessSchema,
    }),
  }),
  Data.Object({
    StepBuiltinListToMap: Data.Object({
      control: MapConversionControlSchema,
      source: DataListNodeSchema,
      pair: DataNodeSchema,
      first: DataListNodeSchema,
      second: DataListNodeSchema,
      key: DataNodeSchema,
      value: DataNodeSchema,
      destination: DataPairNodeSchema,
    }),
  }),
  Data.Object({
    StepBuiltinMapToList: Data.Object({
      control: MapConversionControlSchema,
      source: DataPairNodeSchema,
      destination: DataListNodeSchema,
      pair: DataNodeSchema,
      first: DataListNodeSchema,
      second: DataListNodeSchema,
      key: DataNodeSchema,
      value: DataNodeSchema,
    }),
  }),
  Data.Object({
    FinishBuiltinMapConversion: Data.Object({
      control: MapConversionControlSchema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinSemanticFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessSchema),
      material: SemanticBuiltinWitnessSchema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinBlsFinal: Data.Object({
      left_root: Data.Bytes(),
      right_root: Data.Bytes(),
      left: BlsExpressionWitnessSchema,
      right: BlsExpressionWitnessSchema,
      result: DirectValueWitnessSchema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessSchema),
    }),
  }),
  Data.Object({
    ExecuteBuiltinTypeFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(RuntimeValueWitnessSchema),
    }),
  }),
  Data.Object({
    ComputeContextConstant: Data.Object({
      value_root: Data.Bytes(),
    }),
  }),
]);

const CoreStepEvidenceSchema = Data.Object({
  pre: CekMachineStateSchema,
  post: CekMachineStateSchema,
  witness: CoreStepWitnessSchema,
});

const CekBlobFrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  root: Data.Bytes(),
  byte_length: Data.Integer(),
});

const CekBlobFrontierSchema = Data.Object({
  count: Data.Integer(),
  byte_length: Data.Integer(),
  peaks: Data.Array(CekBlobFrontierPeakSchema),
});

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
  pending_large_expected_children: Data.Nullable(Data.Integer()),
  integer: Data.Nullable(CekDataIntegerControlSchema),
  bytes: Data.Nullable(CekDataBytesControlSchema),
  result: Data.Nullable(DataSummarySchema),
});

const DataTraverseActionSchema = Data.Enum([
  Data.Literal("NoAction"),
  Data.Object({
    HeadScalar: Data.Object({ item_length: Data.Integer() }),
  }),
  Data.Object({
    HeadSequence: Data.Object({ expected_children: Data.Integer() }),
  }),
  Data.Literal("HeadMap"),
  Data.Object({
    HeadLargeConstructor: Data.Object({
      constructor_cbor_length: Data.Integer(),
      expected_children: Data.Integer(),
    }),
  }),
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

const RedeemerItemProofControlSchema = Data.Object({
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

const RedeemerItemProofWitnessSchema = Data.Object({
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
export const NativeScriptFrame =
  NativeScriptFrameSchema as unknown as NativeScriptFrame;

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
export const NativeScriptPushdownFrame =
  NativeScriptPushdownFrameSchema as unknown as NativeScriptPushdownFrame;

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
export const SignerSetProof = SignerSetProofSchema as unknown as SignerSetProof;

const LedgerOutputProofWitnessSchema = Data.Enum([
  Data.Literal("LedgerOutputProofNoWitness"),
  Data.Object({
    LedgerOutputProofChunks: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Object({
    LedgerOutputProofValue: Data.Object({
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    LedgerOutputProofDatum: Data.Object({
      action: DataTraverseActionSchema,
      chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Object({
    LedgerOutputProofNativeFrame: Data.Object({
      frame: NativeScriptFrameSchema,
    }),
  }),
]);

const ProofFrameSchema = Data.Object({
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

const LedgerDeltaOperationProofSchema = Data.Object({
  descriptor: ProofDescriptorSchema,
  operation_count: Data.Integer(),
  operation_peaks: FrontierSchema,
  operation_index: Data.Integer(),
  operation_siblings: ByteArrayListSchema,
});

const ValueAssetMutationWitnessSchema = Data.Object({
  delta_was_present: Data.Boolean(),
  old_delta: Data.Integer(),
  delta_proof: ProofSchema,
});

const CekRedeemerContextControlSchema = Data.Object({
  cursor: Data.Integer(),
  map_items: DataSequenceSummarySchema,
  active_scan_hash: Data.Bytes(),
  active_redeemer_leaf: Data.Bytes(),
  active_purpose: DataSummarySchema,
  current_redeemer: DataSummarySchema,
});

const CekFinalContextControlSchema = Data.Object({
  tx_info: DataSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});

const CekContextPartsControlSchema = Data.Object({
  redeemer_items: DataSequenceSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});

const CekTxInfoAssemblyControlSchema = Data.Object({
  tail_fields: DataSequenceSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});

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

export type ValidationAuxiliaryWitness = Data.Static<
  typeof ValidationAuxiliaryWitnessSchema
>;
export const ValidationAuxiliaryWitness =
  ValidationAuxiliaryWitnessSchema as unknown as ValidationAuxiliaryWitness;

/**
 * A field preimage published once, at the proof-item script address, for the
 * `CanonicalDecode` complete-item steps of one disputed transaction to reach by
 * reference input instead of re-carrying it in every step's redeemer.
 *
 * **It publishes the field's whole §5.1 preimage, not one item.** Under the
 * retired counted scheme it published one item's bytes beside an `ItemProofV1`
 * opening them against the field commitment — an opening §4 made unsatisfiable
 * (#592). Under §8 the unit that authenticates is the whole preimage, so the
 * whole preimage is what a publication carries;
 * `canonical_decode_item_observe_v1`'s `ObserveReference` arm wraps it as
 * `Inline` carriage and the door hashes it once against the committed field
 * hash. `transaction_id` and `transaction_commitment` are what stop a look-alike
 * UTxO passing a preimage off as belonging to a different dispute.
 *
 * Aiken source of truth:
 * `onchain/aiken/lib/midgard/validation-machine/`.
 */
export const ValidationProofItemDatumSchema = Data.Object({
  version: Data.Integer(),
  transaction_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
  transaction_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  field_preimage: Data.Bytes(),
});
export type ValidationProofItemDatum = Data.Static<
  typeof ValidationProofItemDatumSchema
>;
export const ValidationProofItemDatum =
  ValidationProofItemDatumSchema as unknown as ValidationProofItemDatum;
