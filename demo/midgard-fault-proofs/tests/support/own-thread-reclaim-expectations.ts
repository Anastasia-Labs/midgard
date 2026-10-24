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

export const expectResolvedWithReclaim = (
  result: FraudProofWorkflowRunResult,
  thread: { readonly threadOutRef: string; readonly lovelace: string },
  reclaimTxHash: string,
) => {
  expect(result).toMatchObject({
    kind: "resolved_by_another_record",
    reclaimed: [
      {
        txHash: reclaimTxHash,
        threadOutRef: thread.threadOutRef,
        lovelace: thread.lovelace,
      },
    ],
  });
  if (result.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  // Never our conviction: no terminal, no completion, no removal of ours.
  expect("terminal" in result).toBe(false);
  const kinds = result.entries.map(({ event }) => event.kind);
  expect(kinds).not.toContain("completed");
  expect(kinds).not.toContain("terminal_included");
};
