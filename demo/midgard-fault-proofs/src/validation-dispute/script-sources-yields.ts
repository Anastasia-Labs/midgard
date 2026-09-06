import {
  asArray,
  asBigInt,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec/cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

export const SCRIPT_SOURCES_MIDDLE_YIELD_ROLES = [
  {
    contract: "scriptSourcesStageTwoAdvance",
    deployment: "validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw",
    role: "V1 validation-trace script-sources StageTwoAdvance yield",
  },
  {
    contract: "scriptSourcesStageThreeReplay",
    deployment: "validationTraceDisputeScriptSourcesStageThreeReplayWithdraw",
    role: "V1 validation-trace script-sources StageThreeReplay yield",
  },
  {
    contract: "scriptSourcesStageThreeFinish",
    deployment: "validationTraceDisputeScriptSourcesStageThreeFinishWithdraw",
    role: "V1 validation-trace script-sources StageThreeFinish yield",
  },
  {
    contract: "scriptSourcesStageFourBegin",
    deployment: "validationTraceDisputeScriptSourcesStageFourBeginWithdraw",
    role: "V1 validation-trace script-sources StageFourBegin yield",
  },
  {
    contract: "scriptSourcesStageFourFinish",
    deployment: "validationTraceDisputeScriptSourcesStageFourFinishWithdraw",
    role: "V1 validation-trace script-sources StageFourFinish yield",
  },
  {
    contract: "scriptSourcesStageSixBeginPolicy",
    deployment:
      "validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw",
    role: "V1 validation-trace script-sources StageSixBeginPolicy yield",
  },
  {
    contract: "scriptSourcesStageSixFoldAsset",
    deployment: "validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw",
    role: "V1 validation-trace script-sources StageSixFoldAsset yield",
  },
  {
    contract: "scriptSourcesStageSixFinish",
    deployment: "validationTraceDisputeScriptSourcesStageSixFinishWithdraw",
    role: "V1 validation-trace script-sources StageSixFinish yield",
  },
] as const;

/** The canonical predecessor and auxiliary select one fixed semantic arm. */
export const scriptSourcesMiddleYieldIndex = (
  workWitnessCbor: string,
  auxiliary: Data,
): number => {
  if (!(auxiliary instanceof Constr))
    throw new Error("ScriptSources auxiliary must be a constructor");
  const fields = asArray(
    decodeSingleCbor(Buffer.from(workWitnessCbor, "hex")),
    "ScriptSources middle control",
  );
  if (!Array.isArray(fields) || fields.length !== 30)
    throw new Error("ScriptSources middle control requires 30 fields");
  const stage = asBigInt(fields[9], "ScriptSources middle stage");
  if (stage === 2n && auxiliary.index === 0) return 0;
  if (stage === 3n && auxiliary.index === 7) return 1;
  if (stage === 3n && auxiliary.index === 0) return 2;
  if (stage === 4n && auxiliary.index === 29) return 3;
  if (stage === 4n && auxiliary.index === 0) return 4;
  if (stage === 6n && auxiliary.index === 1) return 5;
  if (stage === 6n && auxiliary.index === 39) return 6;
  if (stage === 6n && auxiliary.index === 0) return 7;
  throw new Error("ScriptSources middle auxiliary does not match its stage");
};

export const SCRIPT_SOURCES_OBSERVER_YIELD_ROLES = [
  {
    contract: "scriptSourcesObserverItem",
    deployment: "validationTraceDisputeScriptSourcesObserverItemWithdraw",
    role: "V1 validation-trace script-sources observer item yield",
  },
  {
    contract: "scriptSourcesObserverBound",
    deployment: "validationTraceDisputeScriptSourcesObserverBoundWithdraw",
    role: "V1 validation-trace script-sources observer bound yield",
  },
] as const;
