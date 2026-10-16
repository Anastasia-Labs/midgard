/**
 * The import checker's kills (`ImportCheckConfig`): one script run is
 * killed `floor + a * declared cpu + b * bytes` after it starts.
 */
import {
  type MidgardCekVerdictRequest,
  type MidgardCommittedScriptRun,
} from "@al-ft/midgard-validation";

import { type ImportCheckLoad } from "../database/importChecks.js";
import { type ImportCheckConfig } from "../services/config.import-check.js";

/** The bytes a run decodes: its script, its context (`contextBytes`) and
 * its sidecar. */
export const importCheckCommittedRunBytes = (
  run: MidgardCommittedScriptRun,
  contextBytes: number,
): number =>
  run.scriptBytes.byteLength +
  contextBytes +
  (run.programMaterialSidecarCbor?.byteLength ?? 0);

/** The bytes a run decodes: its script, its context and its sidecar. */
export const importCheckRunBytes = (run: MidgardCekVerdictRequest): number =>
  importCheckCommittedRunBytes(run, run.contextCbor.byteLength);

/** The kill of one run: `floor + a * declared cpu + b * bytes`. */
export const importCheckKillMs = (
  run: MidgardCekVerdictRequest,
  config: ImportCheckConfig,
): number =>
  Math.ceil(
    config.killFloorMs +
      (Number(run.executionBudget?.cpu ?? 0n) * config.killMsPerGigaCpu) / 1e9 +
      (importCheckRunBytes(run) * config.killUsPerKib) / 1024 / 1000,
  );

/** An upper bound on the kills of an entry's runs, from its load. */
export const importCheckLoadKillMs = (
  load: ImportCheckLoad,
  config: ImportCheckConfig,
): number =>
  load.runs * (config.killFloorMs + 1) +
  (load.cpu * config.killMsPerGigaCpu) / 1e9 +
  (load.bytes * config.killUsPerKib) / 1024 / 1000;
