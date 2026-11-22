import {
  type CorrectionLockDatum,
  getStateQueueNodeFromStateQueueDatum,
  NO_DA_ATTESTATION,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk/core";
import {
  DA_ATTESTATION_TIMEOUT_MS,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk/state-queue";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";
import { requireMatchingScriptHash } from "./runtime.js";

export type TimeoutCorrectionTxKind = "prune-descendant" | "remove-block";

export const headerHashOf = (node: StateQueueUTxO): string => {
  if (node.datum.key === "Empty") {
    throw new Error("Confirmed-state root does not have a block header hash.");
  }
  const headerHash = node.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (node.datum.key.Key.key !== headerHash) {
    throw new Error("State-queue node key does not match its NFT asset name.");
  }
  return headerHash;
};

export type TimeoutCorrectionPlan = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly predecessor: StateQueueUTxO;
  readonly target: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
};

/** Locate the authenticated target without discarding its unaffected prefix. */
export const planNextTimeoutCorrection = (
  queue: readonly StateQueueUTxO[],
  targetHeaderHash: string,
): TimeoutCorrectionPlan | undefined => {
  if (queue[0]?.datum.key !== "Empty")
    throw new Error(
      "Canonical state queue is missing its confirmed-state root.",
    );
  const index = queue.findIndex(
    (node, i) => i > 0 && headerHashOf(node) === targetHeaderHash,
  );
  if (index < 0) return undefined;
  const predecessor = queue[index - 1]!;
  const target = queue[index]!;
  if (
    predecessor.datum.next === "Empty" ||
    predecessor.datum.next.Key.key !== targetHeaderHash
  )
    throw new Error(
      "Timeout target is not linked from its authenticated predecessor.",
    );
  const descendant = queue[index + 1];
  if (descendant !== undefined) {
    if (
      target.datum.next === "Empty" ||
      target.datum.next.Key.key !== headerHashOf(descendant)
    )
      throw new Error(
        "Timeout target does not link to its immediate descendant.",
      );
    return {
      kind: "prune-descendant",
      predecessor,
      target,
      removed: descendant,
    };
  }
  if (target.datum.next !== "Empty")
    throw new Error("Terminal timeout target retains a descendant link.");
  return {
    kind: "remove-block",
    predecessor,
    target,
    removed: target,
  };
};

/** Resume the on-chain correction before considering a different expired block. */
export const selectTimeoutCorrectionTarget = async (
  queue: readonly StateQueueUTxO[],
  nowMs: bigint,
  lock: CorrectionLockDatum,
): Promise<{ target: StateQueueUTxO; deadline: bigint } | undefined> => {
  const lockedTarget =
    lock === "Idle" ? undefined : lock.Locked.target_header_hash;
  if (
    lock !== "Idle" &&
    lock.Locked.correction_identity !== "AttestationTimeout"
  )
    throw new Error(
      "State-queue correction lock is owned by another correction kind.",
    );
  let waiting: { target: StateQueueUTxO; deadline: bigint } | undefined;
  for (const target of queue.slice(1)) {
    if (lockedTarget !== undefined && headerHashOf(target) !== lockedTarget)
      continue;
    const node = await Effect.runPromise(
      getStateQueueNodeFromStateQueueDatum(target.datum),
    );
    if (node.da_attestation !== NO_DA_ATTESTATION) {
      if (lockedTarget !== undefined)
        throw new Error("Locked timeout target is already attested.");
      continue;
    }
    const deadline = node.block_time + DA_ATTESTATION_TIMEOUT_MS;
    if (deadline <= nowMs) return { target, deadline };
    if (lockedTarget !== undefined)
      throw new Error("Locked timeout target has not reached its deadline.");
    waiting ??= { target, deadline };
  }
  if (lockedTarget !== undefined)
    throw new Error(
      "Locked timeout target is absent from the canonical queue.",
    );
  return waiting;
};

export const hasCompetingCorrection = (
  lock: CorrectionLockDatum,
  target?: string,
): boolean =>
  lock !== "Idle" &&
  (lock.Locked.correction_identity !== "AttestationTimeout" ||
    (target !== undefined && lock.Locked.target_header_hash !== target));

export const releaseTimeoutCorrectionLeaseBeforeYield = async (
  lease: { readonly release: () => Promise<void> } | undefined,
): Promise<boolean> => {
  if (lease === undefined) {
    return false;
  }
  await lease.release();
  return true;
};

export const requireDeploymentScript = (
  deploymentInfo: ContractDeploymentInfo,
  name:
    | "correctionLockSpend"
    | "stateQueueSpend"
    | "stateQueueMint"
    | "stateQueueUnattestedTimeoutWithdraw",
): Script => {
  const entry = deploymentInfo[name];
  if (entry?.contract === undefined) {
    throw new Error(
      `Deployment info entry "${name}" is missing contract CBOR.`,
    );
  }
  const script = {
    type: entry.contract.type,
    script: entry.contract.cborHex,
  } as Script;
  requireMatchingScriptHash({
    label: `${name} script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(script),
  });
  return script;
};

export type SubmitUnattestedTimeoutCorrectionResult = {
  readonly status: "empty" | "not-ready" | "pending" | "complete";
  readonly targetHeaderHash: string | null;
  readonly deadlineMs: string | null;
  /** The step this run submitted and left in flight. */
  readonly pendingTxHash: string | null;
};
