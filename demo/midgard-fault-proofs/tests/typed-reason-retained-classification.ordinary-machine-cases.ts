import "./typed-reason-retained-classification.cases.js";

import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { type ReasonCase } from "./typed-reason-retained-classification.reason-case.js";

export const ordinaryMachineCases = [
  {
    arm: "ExecutionNativeScriptMalformed",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptMalformed: { execution_index: 0n } },
  },
  {
    arm: "ExecutionNativeScriptNodeLimit",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptNodeLimit: { execution_index: 0n } },
  },
  {
    arm: "ExecutionNativeScriptDepthLimit",
    category: "executionSourceScriptDecoding",
    replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    reason: { ExecutionNativeScriptDepthLimit: { execution_index: 0n } },
  },
  {
    arm: "ScriptIntegrityHashMismatch",
    category: "scriptIntegrityHashMismatch",
    replayer: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    reason: "ScriptIntegrityHashMismatch",
  },
  {
    arm: "ReceivePurposePlutusV3Forbidden",
    category: "receivePurposeLanguage",
    replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
    reason: { ReceivePurposePlutusV3Forbidden: { execution_index: 0n } },
  },
  {
    arm: "OutputAssetAccumulationLimit",
    category: "distinctAssetAccumulationLimit",
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    reason: {
      OutputAssetAccumulationLimit: { output_index: 1n, asset_index: 0n },
    },
  },
  {
    arm: "MintAssetAccumulationLimit",
    category: "distinctAssetAccumulationLimit",
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    reason: { MintAssetAccumulationLimit: { mint_index: 0n } },
  },
] as const satisfies readonly Pick<
  ReasonCase,
  "arm" | "category" | "replayer" | "reason"
>[];
