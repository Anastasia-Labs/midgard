/** What the emulator rows of the reclaim of our own threads read back: our
 * wallet's lovelace, and a run that resolved by another record after
 * reclaiming our thread, never through a conviction of ours. */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.js";

export const walletLovelace = async (lucid: LucidEvolution) =>
  (await lucid.wallet().getUtxos()).reduce(
    (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
    0n,
  );

/**
 * The run resolved by another record after our reclaim of the thread at
 * `threadOutRef` by `reclaimTxHash`: the removal is theirs, not our cancel,
 * and no thread of ours is left standing in it. The result carries no list
 * of what was reclaimed; each caller pins the reclaim itself from the chain
 * (the one send, the wallet's lovelace, the thread spent).
 */
export const expectResolvedWithReclaim = (
  result: FraudProofWorkflowRunResult,
  threadOutRef: string,
  reclaimTxHash: string,
) => {
  expect(result).toMatchObject({ kind: "resolved_by_another_record" });
  if (result.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(result.resolution.removalTxHash).not.toBe(reclaimTxHash);
  expect(result.resolution.standingThread).toBeUndefined();
  expect(result.resolution.referencedProofTokenOutRef).not.toBe(threadOutRef);
  // Never our conviction: no terminal, no completion, no removal of ours.
  expect("terminal" in result).toBe(false);
};
