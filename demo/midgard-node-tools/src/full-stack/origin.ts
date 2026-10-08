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
} from "../l1-origin.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import type { StackProcesses } from "./process.js";
import type { StackStep } from "./workflow.js";

/**
 * The run's L1 origin record. `startHint` is the local node's tip read before
 * the hub-oracle nonce was first signed ("genesis" at genesis): a point
 * before the nonce block, so the origin scan need not start at genesis.
 * `origin` is the point immediately before the nonce block.
 */
type OriginRecord = {
  startHint?: L1Origin | "genesis";
} & Partial<DerivedL1Origin>;

const originPath = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "l1-origin.json");
const statePath = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "deployment-run-state.json");

const readRecord = async (processes: StackProcesses) =>
  ((await readJsonIfPresent(originPath(processes))) ?? {}) as OriginRecord;

/**
 * Reads the local node's tip as the origin scan start, once, while nothing
 * has been signed: the node records its run state before it signs the nonce,
 * so no run state means the nonce block is still to come.
 */
export async function recordOriginStartHint(processes: StackProcesses) {
  const record = await readRecord(processes);
  if (record.startHint !== undefined) return;
  if ((await loadDeploymentRunState(statePath(processes))) !== null) return;
  const tip = await processes.l1NodeTip();
  await writeDurableJson(originPath(processes), {
    ...record,
    startHint: tip ?? "genesis",
  });
}

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

const requireNonce = (processes: StackProcesses) => {
  const nonceTxHash = processes.env.HUB_ORACLE_ONE_SHOT_TX_HASH;
  if (nonceTxHash === undefined || nonceTxHash === "")
    throw new L1OriginUndeterminedError(
      "the run records no hub-oracle nonce, so its L1 origin is unknown",
    );
  return nonceTxHash;
};

/**
 * Derives and records the run's L1 origin once its nonce landed, and refuses
 * when it cannot. An operator `L1_ORIGIN` in the stack env file must be that
 * exact origin; otherwise the scan starts at the recorded start hint.
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
      if (origin === undefined) return { status: "retry" };
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
      const record = await readRecord(processes);
      const operator = configured();
      const hint = record.startHint;
      if (operator === undefined && hint === undefined)
        throw new L1OriginUndeterminedError(
          `the hub-oracle nonce ${nonceTxHash} was signed before this run recorded an origin scan start; ` +
            `set L1_ORIGIN in ${processes.config.envFile} to the l1Origin that ` +
            `midgard-l1-follower find-origin --tx ${nonceTxHash} --network-magic 1 prints`,
        );
      const derived = await processes.deriveL1Origin(
        nonceTxHash,
        operator ?? (hint === "genesis" ? undefined : hint),
      );
      if (
        operator !== undefined &&
        formatL1Origin(derived.origin) !== formatL1Origin(operator)
      )
        throw new L1OriginUndeterminedError(
          `L1_ORIGIN ${formatL1Origin(operator)} in ${processes.config.envFile} is not the origin of the hub-oracle nonce ${nonceTxHash}; find-origin gives ${formatL1Origin(derived.origin)}`,
        );
      await writeDurableJson(originPath(processes), { ...record, ...derived });
      return { l1Origin: formatL1Origin(derived.origin) };
    },
  };
}
