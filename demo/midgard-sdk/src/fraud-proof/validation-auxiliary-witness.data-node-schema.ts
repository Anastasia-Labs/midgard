import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

export const ByteArrayListSchema = Data.Array(Data.Bytes());

export const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});

export type FrontierPeak = Data.Static<typeof FrontierPeakSchema>;

export const FrontierPeak = asDataType<FrontierPeak>(FrontierPeakSchema);

export const FrontierSchema = Data.Array(FrontierPeakSchema);

export const DataSummarySchema = Data.Object({
  root: Data.Bytes(),
  cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

export const DataSequenceSummarySchema = Data.Object({
  root: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

export const ConstantWitnessSchema = Data.Object({
  type_cbor: Data.Bytes(),
  payload_cbor: Data.Bytes(),
});

export const DataNodeSchema = Data.Enum([
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

export const DataListNodeSchema = Data.Object({
  head: Data.Bytes(),
  head_cbor_length: Data.Integer(),
  head_memory: Data.Integer(),
  tail: Data.Bytes(),
  length: Data.Integer(),
  payload_cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

export const DataPairNodeSchema = Data.Object({
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

export const SemanticBuiltinWitnessSchema = Data.Object({
  data_nodes: Data.Array(DataNodeSchema),
  list_nodes: Data.Array(DataListNodeSchema),
  pair_nodes: Data.Array(DataPairNodeSchema),
  scalar_preimages: ByteArrayListSchema,
});

export const DirectValueWitnessSchema = Data.Enum([
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

export const RuntimeValueWitnessSchema = Data.Enum([
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

export const BlsExpressionWitnessSchema = blsExpressionWitnessSchema(10);

export const CekMachineStateSchema = Data.Object({
  mode: Data.Integer(),
  execution_index: Data.Integer(),
  focus_root: Data.Bytes(),
  environment_root: Data.Bytes(),
  continuation_root: Data.Bytes(),
  auxiliary: Data.Integer(),
  cpu: Data.Integer(),
  memory: Data.Integer(),
});

export const EnvironmentSummarySchema = Data.Enum([
  Data.Literal("EmptyEnvironmentSummary"),
  Data.Object({
    NonEmptyEnvironmentSummary: Data.Object({
      value: Data.Bytes(),
      tail: Data.Bytes(),
      length: Data.Integer(),
    }),
  }),
]);

export const MachineValueWitnessSchema = Data.Enum([
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

export const MapConversionControlSchema = Data.Object({
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

export const MapConversionStartWitnessSchema = Data.Object({
  source_node: DataNodeSchema,
  source_list: Data.Nullable(DataListNodeSchema),
  source_pairs: Data.Nullable(DataPairNodeSchema),
  result_node: DataNodeSchema,
  result_list: Data.Nullable(DataListNodeSchema),
  result_pairs: Data.Nullable(DataPairNodeSchema),
});
