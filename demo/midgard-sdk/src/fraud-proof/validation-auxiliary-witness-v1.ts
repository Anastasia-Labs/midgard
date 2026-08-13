import { Data } from "@lucid-evolution/lucid";

import { ProofSchema, ProofStepSchema } from "@/common.js";
import { BoundedItemChunkProofV1Schema } from "@/ledger-state.js";
import { FieldCarriageV1Schema } from "@/native-tx-field-access-v1.js";

type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

const ByteArrayListSchema = Data.Array(Data.Bytes());

const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});

const FrontierSchema = Data.Array(FrontierPeakSchema);

const DataSummaryV1Schema = Data.Object({
  root: Data.Bytes(),
  cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const DataSequenceSummaryV1Schema = Data.Object({
  root: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const ConstantWitnessV1Schema = Data.Object({
  type_cbor: Data.Bytes(),
  payload_cbor: Data.Bytes(),
});

const DataNodeV1Schema = Data.Enum([
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

const DataListNodeV1Schema = Data.Object({
  head: Data.Bytes(),
  head_cbor_length: Data.Integer(),
  head_memory: Data.Integer(),
  tail: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

const DataPairNodeV1Schema = Data.Object({
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

const SemanticBuiltinWitnessV1Schema = Data.Object({
  data_nodes: Data.Array(DataNodeV1Schema),
  list_nodes: Data.Array(DataListNodeV1Schema),
  pair_nodes: Data.Array(DataPairNodeV1Schema),
  scalar_preimages: ByteArrayListSchema,
});

const DirectValueWitnessV1Schema = Data.Enum([
  Data.Object({
    ConstantValue: Data.Tuple([ConstantWitnessV1Schema]),
  }),
  Data.Object({
    SemanticConstantValue: Data.Object({
      type_cbor: Data.Bytes(),
      payload: DataSummaryV1Schema,
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

const RuntimeValueWitnessV1Schema = Data.Enum([
  Data.Object({
    RuntimeConstantValue: Data.Tuple([ConstantWitnessV1Schema]),
  }),
  Data.Object({
    RuntimeSemanticConstantValue: Data.Object({
      type_cbor: Data.Bytes(),
      payload: DataSummaryV1Schema,
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
const blsExpressionWitnessV1Schema = (depth: number): PlutusDataSchema => {
  const millerLoop = Data.Object({
    BlsMillerLoopExpression: Data.Object({
      g1: ConstantWitnessV1Schema,
      g2: ConstantWitnessV1Schema,
    }),
  });
  if (depth === 1) {
    return Data.Enum([millerLoop]);
  }
  const child = blsExpressionWitnessV1Schema(depth - 1);
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

const BlsExpressionWitnessV1Schema = blsExpressionWitnessV1Schema(10);

const CekMachineStateV1Schema = Data.Object({
  mode: Data.Integer(),
  execution_index: Data.Integer(),
  focus_root: Data.Bytes(),
  environment_root: Data.Bytes(),
  continuation_root: Data.Bytes(),
  auxiliary: Data.Integer(),
  cpu: Data.Integer(),
  memory: Data.Integer(),
});

const EnvironmentSummaryV1Schema = Data.Enum([
  Data.Literal("EmptyEnvironmentSummary"),
  Data.Object({
    NonEmptyEnvironmentSummary: Data.Object({
      value: Data.Bytes(),
      tail: Data.Bytes(),
      length: Data.Integer(),
    }),
  }),
]);

const MachineValueWitnessV1Schema = Data.Enum([
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

const MapConversionControlV1Schema = Data.Object({
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

const MapConversionStartWitnessV1Schema = Data.Object({
  source_node: DataNodeV1Schema,
  source_list: Data.Nullable(DataListNodeV1Schema),
  source_pairs: Data.Nullable(DataPairNodeV1Schema),
  result_node: DataNodeV1Schema,
  result_list: Data.Nullable(DataListNodeV1Schema),
  result_pairs: Data.Nullable(DataPairNodeV1Schema),
});

const CoreStepWitnessV1Schema = Data.Enum([
  Data.Object({
    ComputeVariable: Data.Object({ index: Data.Integer() }),
  }),
  Data.Object({
    ComputeConstant: Data.Object({ value: ConstantWitnessV1Schema }),
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
      value: MachineValueWitnessV1Schema,
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
      closure_summary: EnvironmentSummaryV1Schema,
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
      function: MachineValueWitnessV1Schema,
      tail: Data.Bytes(),
    }),
  }),
  Data.Object({
    ReturnApplyValueLambda: Data.Object({
      argument: Data.Bytes(),
      body: Data.Bytes(),
      closure_environment: Data.Bytes(),
      closure_summary: EnvironmentSummaryV1Schema,
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
      function: MachineValueWitnessV1Schema,
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
      value: MachineValueWitnessV1Schema,
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
      value: MachineValueWitnessV1Schema,
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
      arguments: Data.Array(DirectValueWitnessV1Schema),
      result: DirectValueWitnessV1Schema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinSemantic: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessV1Schema),
      result: DirectValueWitnessV1Schema,
      material: SemanticBuiltinWitnessV1Schema,
    }),
  }),
  Data.Object({
    StartBuiltinMapConversion: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessV1Schema),
      result: DirectValueWitnessV1Schema,
      material: MapConversionStartWitnessV1Schema,
    }),
  }),
  Data.Object({
    StepBuiltinListToMap: Data.Object({
      control: MapConversionControlV1Schema,
      source: DataListNodeV1Schema,
      pair: DataNodeV1Schema,
      first: DataListNodeV1Schema,
      second: DataListNodeV1Schema,
      key: DataNodeV1Schema,
      value: DataNodeV1Schema,
      destination: DataPairNodeV1Schema,
    }),
  }),
  Data.Object({
    StepBuiltinMapToList: Data.Object({
      control: MapConversionControlV1Schema,
      source: DataPairNodeV1Schema,
      destination: DataListNodeV1Schema,
      pair: DataNodeV1Schema,
      first: DataListNodeV1Schema,
      second: DataListNodeV1Schema,
      key: DataNodeV1Schema,
      value: DataNodeV1Schema,
    }),
  }),
  Data.Object({
    FinishBuiltinMapConversion: Data.Object({
      control: MapConversionControlV1Schema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinSemanticFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessV1Schema),
      material: SemanticBuiltinWitnessV1Schema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinBlsFinal: Data.Object({
      left_root: Data.Bytes(),
      right_root: Data.Bytes(),
      left: BlsExpressionWitnessV1Schema,
      right: BlsExpressionWitnessV1Schema,
      result: DirectValueWitnessV1Schema,
    }),
  }),
  Data.Object({
    ExecuteBuiltinFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(DirectValueWitnessV1Schema),
    }),
  }),
  Data.Object({
    ExecuteBuiltinTypeFailure: Data.Object({
      tag: Data.Integer(),
      arguments: Data.Array(RuntimeValueWitnessV1Schema),
    }),
  }),
  Data.Object({
    ComputeContextConstant: Data.Object({
      value_root: Data.Bytes(),
    }),
  }),
]);

const CoreStepEvidenceV1Schema = Data.Object({
  pre: CekMachineStateV1Schema,
  post: CekMachineStateV1Schema,
  witness: CoreStepWitnessV1Schema,
});

const CekBlobFrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  root: Data.Bytes(),
  byte_length: Data.Integer(),
});

const CekBlobFrontierV1Schema = Data.Object({
  count: Data.Integer(),
  byte_length: Data.Integer(),
  peaks: Data.Array(CekBlobFrontierPeakV1Schema),
});

const Blake2b256TraceControlV1Schema = Data.Object({
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

const CekSourceBlobControlV1Schema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  frontier: CekBlobFrontierV1Schema,
  active_hash: Data.Nullable(Blake2b256TraceControlV1Schema),
});

const CekDataIntegerControlV1Schema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  memory: Data.Integer(),
  blob: Data.Nullable(CekSourceBlobControlV1Schema),
});

const CekDataBytesControlV1Schema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  bytes_length: Data.Integer(),
  blob: Data.Nullable(CekSourceBlobControlV1Schema),
});

const DataFrameV1Schema = Data.Object({
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
  sequence: DataSequenceSummaryV1Schema,
});

const DataTraverseControlV1Schema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  source_start: Data.Integer(),
  source_length: Data.Integer(),
  offset: Data.Integer(),
  frame_root: Data.Bytes(),
  pending_large_expected_children: Data.Nullable(Data.Integer()),
  integer: Data.Nullable(CekDataIntegerControlV1Schema),
  bytes: Data.Nullable(CekDataBytesControlV1Schema),
  result: Data.Nullable(DataSummaryV1Schema),
});

const DataTraverseActionV1Schema = Data.Enum([
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
      parent: Data.Nullable(DataFrameV1Schema),
    }),
  }),
  Data.Object({
    FoldList: Data.Object({
      frame: DataFrameV1Schema,
      child_index: Data.Integer(),
      child: DataSummaryV1Schema,
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    FoldMap: Data.Object({
      frame: DataFrameV1Schema,
      pair_index: Data.Integer(),
      key: DataSummaryV1Schema,
      value: DataSummaryV1Schema,
      key_siblings: ByteArrayListSchema,
      value_siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    FinalizeFrame: Data.Object({
      frame: DataFrameV1Schema,
      parent: Data.Nullable(DataFrameV1Schema),
    }),
  }),
]);

const RedeemerItemProofControlV1Schema = Data.Object({
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
  traversal: Data.Nullable(DataTraverseControlV1Schema),
});

const RedeemerItemProofActionV1Schema = Data.Enum([
  Data.Literal("RedeemerItemOpenHeader"),
  Data.Literal("RedeemerItemOpenTail"),
  Data.Object({
    RedeemerItemTraverseData: Data.Object({
      action: DataTraverseActionV1Schema,
    }),
  }),
  Data.Literal("RedeemerItemFinishData"),
]);

const RedeemerItemProofWitnessV1Schema = Data.Object({
  action: RedeemerItemProofActionV1Schema,
  chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
  next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
});

const NativeScriptFrameV1Schema = Data.Object({
  tail: Data.Bytes(),
  kind: Data.Integer(),
  child_count: Data.Integer(),
  remaining: Data.Integer(),
  valid_count: Data.Integer(),
  required: Data.Integer(),
});

const SignerSetProofV1Schema = Data.Enum([
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

const LedgerOutputProofWitnessV1Schema = Data.Enum([
  Data.Literal("LedgerOutputProofNoWitness"),
  Data.Object({
    LedgerOutputProofChunks: Data.Object({
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
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
      action: DataTraverseActionV1Schema,
      chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
    }),
  }),
  Data.Object({
    LedgerOutputProofNativeFrame: Data.Object({
      frame: NativeScriptFrameV1Schema,
    }),
  }),
]);

const ProofFrameV1Schema = Data.Object({
  version: Data.Integer(),
  frame_index: Data.Integer(),
  cursor: Data.Integer(),
  next_cursor: Data.Integer(),
  step: ProofStepSchema,
});

const ProofDescriptorV1Schema = Data.Object({
  version: Data.Integer(),
  frame_count: Data.Integer(),
  terminal_cursor: Data.Integer(),
  peaks: FrontierSchema,
});

const LedgerDeltaOperationProofV1Schema = Data.Object({
  descriptor: ProofDescriptorV1Schema,
  operation_count: Data.Integer(),
  operation_peaks: FrontierSchema,
  operation_index: Data.Integer(),
  operation_siblings: ByteArrayListSchema,
});

const ValueAssetMutationWitnessV1Schema = Data.Object({
  delta_was_present: Data.Boolean(),
  old_delta: Data.Integer(),
  delta_proof: ProofSchema,
});

const CekRedeemerContextControlV1Schema = Data.Object({
  cursor: Data.Integer(),
  map_items: DataSequenceSummaryV1Schema,
  active_scan_hash: Data.Bytes(),
  active_redeemer_leaf: Data.Bytes(),
  active_purpose: DataSummaryV1Schema,
  current_redeemer: DataSummaryV1Schema,
});

const CekFinalContextControlV1Schema = Data.Object({
  tx_info: DataSummaryV1Schema,
  redeemer: DataSummaryV1Schema,
  script_info: DataSummaryV1Schema,
});

const CekContextPartsControlV1Schema = Data.Object({
  redeemer_items: DataSequenceSummaryV1Schema,
  redeemer: DataSummaryV1Schema,
  script_info: DataSummaryV1Schema,
});

const CekTxInfoAssemblyControlV1Schema = Data.Object({
  tail_fields: DataSequenceSummaryV1Schema,
  redeemer: DataSummaryV1Schema,
  script_info: DataSummaryV1Schema,
});

export const ValidationAuxiliaryWitnessV1Schema = Data.Enum([
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
      carriage: FieldCarriageV1Schema,
    }),
  }),
  Data.Object({
    /**
     * A field-4 required-signer item plus the signer-set membership evidence
     * the step decides on. No `field_index`/`item_index`: the field is 4 by
     * construction and the item index is `control.required_seen`.
     */
    RequiredSignerItemWitness: Data.Object({
      carriage: FieldCarriageV1Schema,
      signer_proof: SignerSetProofV1Schema,
    }),
  }),
  Data.Object({
    NativeScriptTokenWitness: Data.Object({
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
      signer_proof: SignerSetProofV1Schema,
    }),
  }),
  Data.Object({
    NativeScriptFrameWitness: Data.Object({
      frame: NativeScriptFrameV1Schema,
    }),
  }),
  Data.Object({
    ScheduledLedgerMembershipWitness: Data.Object({
      source_kind: Data.Integer(),
      key: Data.Bytes(),
      next_schedule_hash: Data.Bytes(),
      value: Data.Bytes(),
      proof: ProofSchema,
      signer_proof: SignerSetProofV1Schema,
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
      first_chunk_proof: BoundedItemChunkProofV1Schema,
    }),
  }),
  Data.Object({
    CekCoreStepWitness: Data.Object({
      step: CoreStepEvidenceV1Schema,
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
      control: CekRedeemerContextControlV1Schema,
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
      redeemer_control: Data.Nullable(CekRedeemerContextControlV1Schema),
      control: RedeemerItemProofControlV1Schema,
      witness: RedeemerItemProofWitnessV1Schema,
    }),
  }),
  Data.Object({
    CekContextFinalizeWitness: Data.Object({
      redeemer_control: CekRedeemerContextControlV1Schema,
    }),
  }),
  Data.Object({
    CekContextFinalizeSpendWitness: Data.Object({
      redeemer_control: CekRedeemerContextControlV1Schema,
      item_index: Data.Integer(),
      key: Data.Bytes(),
      descriptor_cbor: Data.Bytes(),
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    CekContextAssembleWitness: Data.Object({
      control: CekContextPartsControlV1Schema,
    }),
  }),
  Data.Object({
    CekTxInfoFinalizeWitness: Data.Object({
      control: CekTxInfoAssemblyControlV1Schema,
    }),
  }),
  Data.Object({
    CekContextSeedWitness: Data.Object({
      control: CekFinalContextControlV1Schema,
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
      mutation: ValueAssetMutationWitnessV1Schema,
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
      mutation: ValueAssetMutationWitnessV1Schema,
    }),
  }),
  Data.Object({
    ValueMintAssetWitness: Data.Object({
      mint_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      siblings: ByteArrayListSchema,
      mutation: ValueAssetMutationWitnessV1Schema,
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
      carriage: FieldCarriageV1Schema,
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
      carriage: FieldCarriageV1Schema,
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
      witness: LedgerOutputProofWitnessV1Schema,
    }),
  }),
  Data.Object({
    LedgerOutputProofFinalizeWitness: Data.Object({
      descriptor_cbor: Data.Bytes(),
      signer_proof: SignerSetProofV1Schema,
    }),
  }),
  Data.Object({
    LedgerDeltaProofFrameWitness: Data.Object({
      frame: ProofFrameV1Schema,
      siblings: ByteArrayListSchema,
    }),
  }),
  Data.Object({
    LedgerDeltaOperationWitness: Data.Object({
      operation_kind: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
      operation_proof: LedgerDeltaOperationProofV1Schema,
    }),
  }),
  Data.Object({
    ScriptSourceHashBlockWitness: Data.Object({
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
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
      first_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
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
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
    }),
  }),
]);

export type ValidationAuxiliaryWitnessV1 = Data.Static<
  typeof ValidationAuxiliaryWitnessV1Schema
>;
export const ValidationAuxiliaryWitnessV1 =
  ValidationAuxiliaryWitnessV1Schema as unknown as ValidationAuxiliaryWitnessV1;

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
 * `canonical_decode_item_semantic_v1`'s `VerifyReference` route wraps it as
 * `Inline` carriage and the door hashes it once against the committed field
 * hash. `transaction_id` and `transaction_commitment` are what stop a look-alike
 * UTxO passing a preimage off as belonging to a different dispute.
 *
 * Aiken source of truth:
 * `onchain/aiken/lib/midgard/validation-machine-v1.ak:421`.
 */
export const ValidationProofItemDatumV1Schema = Data.Object({
  version: Data.Integer(),
  transaction_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
  transaction_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  field_preimage: Data.Bytes(),
});
export type ValidationProofItemDatumV1 = Data.Static<
  typeof ValidationProofItemDatumV1Schema
>;
export const ValidationProofItemDatumV1 =
  ValidationProofItemDatumV1Schema as unknown as ValidationProofItemDatumV1;
