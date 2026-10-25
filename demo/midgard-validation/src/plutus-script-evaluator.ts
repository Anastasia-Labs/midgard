import {
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";

import { storedFlatProgramBytes } from "./cek-program.unwrap-canonical-cbor-byte-string.js";
import type { MidgardCekScriptRun } from "./phase-b.evaluate-cek-script.js";
import { MidgardUplcCapabilityError } from "./plutus-script-evaluator.capability-error.js";
import {
  discardUplcInstance,
  uplcInstance,
} from "./plutus-script-evaluator.uplc-instance.js";
import type {
  ScriptDecodePhase2Class,
  ScriptDecodeRecord,
} from "./script-decode.step-types.js";
import { scriptDecodeRecordOfScript } from "./script-decode.step-walk.js";
import type { LocalScriptEvalResult } from "./types.js";

export type MidgardUplcEvalProgramResult = {
  readonly ok: boolean;
  readonly cpu: bigint;
  readonly mem: bigint;
  readonly error_kind: number;
  readonly logs: readonly string[];
  free(): void;
};
export type MidgardUplcEvalProgram = (
  programFlat: Uint8Array,
  argsCbor: Uint8Array[],
  language: number,
  protocolMajorVersion: number,
  costModel: BigInt64Array,
  budgetCpu: bigint,
  budgetMemory: bigint,
  strictBudget: boolean,
) => MidgardUplcEvalProgramResult;

// Local extraction cache only. The exact stored script determines its key;
// neither request-supplied identifiers nor material can substitute its bytes.
type DecodedProgram = {
  readonly record: Extract<ScriptDecodeRecord, { readonly k: "decoded" }>;
  readonly flatBytes: Buffer;
};
const flatByScriptHash = new Map<string, DecodedProgram>();
const CACHE_ENTRIES = 128;
const storedFlat = (run: MidgardCekScriptRun): DecodedProgram => {
  const hash = hashMidgardVersionedScript({
    language: run.scriptLanguage,
    scriptBytes: Buffer.from(run.scriptBytes),
  });
  const cached = flatByScriptHash.get(hash);
  if (cached !== undefined) {
    flatByScriptHash.delete(hash);
    flatByScriptHash.set(hash, cached);
    return cached;
  }
  const record = scriptDecodeRecordOfScript(
    run.scriptLanguage === "PlutusV3" ? 3 : 128,
    run.scriptBytes,
  );
  if (record.k === "refused")
    throw new MidgardUplcCapabilityError(
      "Phase B received a carried Plutus script that failed phase-one decoding",
    );
  const flat = { record, flatBytes: storedFlatProgramBytes(run.scriptBytes) };
  flatByScriptHash.set(hash, flat);
  if (flatByScriptHash.size > CACHE_ENTRIES)
    flatByScriptHash.delete(flatByScriptHash.keys().next().value!);
  return flat;
};

/** The phase-two class of a carried Plutus script's decode record. */
export const storedMidgardPlutusScriptPhase2Class = (
  run: MidgardCekScriptRun,
): ScriptDecodePhase2Class => storedFlat(run).record.phase2Class;

/** Runs carried Flat directly with Cardano PV11 ledger budget semantics. */
export const evaluateStoredMidgardPlutusScript = (
  run: MidgardCekScriptRun,
): LocalScriptEvalResult => {
  try {
    const decoded = storedFlat(run);
    if (decoded.record.phase2Class !== 0)
      return {
        kind: "script_invalid",
        detail: `UPLC PV11 phase-two class ${decoded.record.phase2Class}`,
      };
    const UPLC = uplcInstance();
    if (
      !("eval_program_raw" in UPLC) ||
      typeof UPLC.eval_program_raw !== "function"
    ) {
      throw new MidgardUplcCapabilityError(
        "Pinned @lucid-evolution/uplc lacks eval_program_raw; the owner-approved evaluator release is required",
      );
    }
    if (PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length !== 350) {
      throw new MidgardUplcCapabilityError(
        "PV11 requires the owner-approved 350-entry PlutusV3 cost model",
      );
    }
    const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
    const budget = run.executionBudget ?? {
      cpu: BigInt(
        MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionCpuUnits,
      ),
      memory: BigInt(
        MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionMemoryUnits,
      ),
    };
    let result: MidgardUplcEvalProgramResult;
    try {
      result = evaluate(
        decoded.flatBytes,
        [run.contextCbor],
        3,
        11,
        BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
        budget.cpu,
        budget.memory,
        false,
      );
    } catch (cause) {
      // The instance may be left unusable; the next evaluation gets a new one.
      discardUplcInstance();
      throw cause;
    }
    try {
      if (
        typeof result.ok !== "boolean" ||
        typeof result.cpu !== "bigint" ||
        typeof result.mem !== "bigint" ||
        result.cpu < 0n ||
        result.mem < 0n ||
        !Number.isInteger(result.error_kind) ||
        ![0, 1, 2, 3].includes(result.error_kind) ||
        result.ok !== (result.error_kind === 0) ||
        !Array.isArray(result.logs) ||
        !result.logs.every((entry) => typeof entry === "string")
      ) {
        throw new TypeError(
          "UPLC evaluator returned an invalid execution result",
        );
      }
      return result.ok
        ? { kind: "accepted", budget: { cpu: result.cpu, memory: result.mem } }
        : {
            kind: "script_invalid",
            detail: `UPLC PV11 execution failed (${result.error_kind}): ${result.logs.join("; ")}`,
          };
    } finally {
      result.free();
    }
  } catch (cause) {
    if (cause instanceof MidgardUplcCapabilityError) throw cause;
    throw new MidgardUplcCapabilityError(
      "UPLC script evaluation is unavailable",
      {
        cause,
      },
    );
  }
};
