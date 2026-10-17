import { Data } from "@lucid-evolution/lucid";

import {
  BlsExpressionWitnessSchema,
  CekMachineStateSchema,
  ConstantWitnessSchema,
  DataListNodeSchema,
  DataNodeSchema,
  DataPairNodeSchema,
  DirectValueWitnessSchema,
  EnvironmentSummarySchema,
  MachineValueWitnessSchema,
  MapConversionControlSchema,
  MapConversionStartWitnessSchema,
  RuntimeValueWitnessSchema,
  SemanticBuiltinWitnessSchema,
} from "./validation-auxiliary-witness.data-node-schema.js";

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

export const CoreStepEvidenceSchema = Data.Object({
  pre: CekMachineStateSchema,
  post: CekMachineStateSchema,
  witness: CoreStepWitnessSchema,
});

const CekBlobFrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  root: Data.Bytes(),
  byte_length: Data.Integer(),
});

export const CekBlobFrontierSchema = Data.Object({
  count: Data.Integer(),
  byte_length: Data.Integer(),
  peaks: Data.Array(CekBlobFrontierPeakSchema),
});
