import { join } from "node:path";

import {
  formatL1Origin,
  type L1Origin,
  parseL1Origin,
} from "@al-ft/midgard-core/l1-origin";
import { loadDeploymentRunState } from "midgard-node/e2e/run-state";

import {
  type DerivedL1Origin,
  L1OriginUndeterminedError,
  runStateL1Origin,
  sameL1Origin,
} from "../l1-origin.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import type { StackProcesses } from "./process.js";
import type { StackStep } from "./workflow.js";

/**
 * The run's L1 origin record: the origin T the deployment run state recorded
 * before the hub-oracle nonce was first signed, once proven to precede the
 * nonce's block, with that block.
 */
type OriginRecord = Partial<DerivedL1Origin>;

const originPath = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "l1-origin.json");
const statePath = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "deployment-run-state.json");

const readRecord = async (processes: StackProcesses) =>
  ((await readJsonIfPresent(originPath(processes))) ?? {}) as OriginRecord;

const recordedOrigin = (record: OriginRecord, nonceTxHash: string) =>
  record.origin !== undefined &&
  record.nonceTxHash === nonceTxHash.toLowerCase()
    ? record.origin
    : undefined;

/** Restores the recorded origin of `nonceTxHash` into the node environment. */
export async function restoreL1Origin(
  processes: StackProcesses,
  nonceTxHash: string,
) {
  const origin = recordedOrigin(await readRecord(processes), nonceTxHash);
  if (origin !== undefined) processes.env.L1_ORIGIN = formatL1Origin(origin);
}

/**
 * The run's L1 origin as the origin step restored it into `env`, or
 * `undefined` before that step; a malformed value is refused.
 */
export function runL1Origin(
  env: Readonly<Record<string, string>>,
): L1Origin | undefined {
  const text = env.L1_ORIGIN ?? "";
  if (text === "") return undefined;
  try {
    return parseL1Origin(text, "L1_ORIGIN");
  } catch (error) {
    throw new L1OriginUndeterminedError((error as Error).message);
  }
}

const requireNonce = (processes: StackProcesses) => {
  const nonceTxHash = processes.env.HUB_ORACLE_ONE_SHOT_TX_HASH;
  if (nonceTxHash === undefined || nonceTxHash === "")
    throw new L1OriginUndeterminedError(
      "the run records no hub-oracle nonce, so its L1 origin is unknown",
    );
  return nonceTxHash;
};

/**
 * Proves and records the run's L1 origin once its nonce landed: the origin T
 * the deployment run state recorded before the nonce was signed. Refuses when
 * the run state records no T or the chain does not prove it. An operator
 * `L1_ORIGIN` in the stack env file must be T itself.
 */
export function originStep(
  processes: StackProcesses,
  restoreDeployment: () => Promise<void>,
): StackStep {
  const configured = () => {
    const text = processes.configuredL1Origin;
    if (text === undefined) return undefined;
    try {
      return parseL1Origin(text, "L1_ORIGIN");
    } catch (error) {
      throw new L1OriginUndeterminedError((error as Error).message);
    }
  };
  return {
    id: "origin",
    reconcile: async () => {
      await restoreDeployment();
      const nonceTxHash = requireNonce(processes);
      const origin = recordedOrigin(await readRecord(processes), nonceTxHash);
      const runOrigin = (await loadDeploymentRunState(statePath(processes)))
        ?.identity.l1Origin;
      if (
        origin === undefined ||
        runOrigin === undefined ||
        !sameL1Origin(origin, runOrigin)
      )
        return { status: "retry" };
      const operator = configured();
      if (
        operator !== undefined &&
        formatL1Origin(operator) !== formatL1Origin(origin)
      )
        throw new L1OriginUndeterminedError(
          `L1_ORIGIN ${formatL1Origin(operator)} in ${processes.config.envFile} is not the run's recorded origin ${formatL1Origin(origin)}`,
        );
      processes.env.L1_ORIGIN = formatL1Origin(origin);
      return { status: "complete", data: { l1Origin: formatL1Origin(origin) } };
    },
    execute: async () => {
      const nonceTxHash = requireNonce(processes);
      const recorded = await runStateL1Origin(
        statePath(processes),
        nonceTxHash,
      );
      const operator = configured();
      if (operator !== undefined && !sameL1Origin(operator, recorded))
        throw new L1OriginUndeterminedError(
          `L1_ORIGIN ${formatL1Origin(operator)} in ${processes.config.envFile} is not the origin ${formatL1Origin(recorded)} the deployment run state recorded for the hub-oracle nonce ${nonceTxHash}`,
        );
      const derived = await processes.deriveL1Origin(nonceTxHash, recorded);
      await writeDurableJson(originPath(processes), derived);
      return { l1Origin: formatL1Origin(derived.origin) };
    },
  };
}
