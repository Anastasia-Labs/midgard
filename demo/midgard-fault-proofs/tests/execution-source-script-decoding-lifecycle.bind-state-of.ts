import { fileURLToPath } from "node:url";

import {
  executionSourceAuthenticatedSource,
  executionSourceScriptDecodingCheckpoint,
  ExecutionSourceScriptDecodingResultClasses,
} from "../src/execution-source-script-decoding/index.js";
import { buildNativeScriptDecodingChunkProof } from "../src/native-script-decoding/evidence.js";
import {
  commitSubjectBlock,
  createMeasurementRecorder,
  type ExecutionSourceContext,
  makeExecutionSourceStages,
  publishFamilyReferences,
  type SubjectFixture,
} from "./support/execution-source-script-decoding-emulator.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "validation_trace_root",
  "machine_state",
  "execution_descriptor",
  "forced_leaf_root",
  "forced_leaf_header",
  "forced_leaf_direction",
  "forced_leaf_reason_coordinate",
  "source_item_chunk",
  "bind_result",
  "scan_window_chunk",
  "scan_checkpoint",
  "wrong_successor",
  "premature_close",
] as const;

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/execution-source-script-decoding-v1-fit-ledger.json",
    import.meta.url,
  ),
);

export const coverage = createLifecycleCoverageRecorder();

export const recorder = createMeasurementRecorder();

export const { record } = recorder;

export const progress = (message: string) =>
  console.info(`[execution-source-script-decoding-progress] ${message}`);

export const Classes = ExecutionSourceScriptDecodingResultClasses;

/** The block, references and stages one subject fixture is driven through. */
export const stand = async (
  context: ExecutionSourceContext,
  fixture: SubjectFixture,
  label: string,
) => {
  const setup = await commitSubjectBlock(context, fixture);
  const references = await publishFamilyReferences(context, recorder, label);
  return makeExecutionSourceStages({ context, setup, references });
};

/** The step-02 datum a thread bound to `fixture` carries at `executionIndex`. */
export const boundOf = (fixture: SubjectFixture, executionIndex = 0n) => ({
  subject: fixture.subject,
  validation_traces_root: fixture.header.validationTracesRoot,
  validation_trace_count: fixture.header.validationTraceCount,
  execution_index: executionIndex,
  accused_class: BigInt(fixture.evidence.finding.accusedClass),
});

/** The step-04 state step 03 opens for `fixture`, given its validators. */
export const bindStateOf = (
  context: ExecutionSourceContext,
  fixture: SubjectFixture,
) => {
  const { evidence } = fixture;
  const nextExpectedScriptHash = context.contracts.steps[3].spendingScriptHash;
  return {
    source: executionSourceAuthenticatedSource(
      boundOf(fixture),
      fixture.authentication.authentication,
    ),
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash,
    }),
    result_class: BigInt(
      evidence.initialControlCbor === ""
        ? evidence.resultClass
        : Classes.Pending,
    ),
  };
};

export const chunkOf = (fixture: SubjectFixture, chunkIndex: number) =>
  buildNativeScriptDecodingChunkProof({
    fieldIndex: 6,
    itemIndex: fixture.evidence.descriptor.sourceIndex,
    itemBytes: fixture.scriptItem,
    chunkIndex,
  });
