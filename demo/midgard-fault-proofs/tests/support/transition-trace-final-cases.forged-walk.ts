/**
 * The forged datum walk case: a prover whose phase-8 witnesses restore stack
 * entries other than the ones the walk pushed.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import * as walkModule from "../../src/transition-trace/data-summary-walk.js";
import { expectSingleUtxoWithUnit } from "./submit-init-emulator-shared.js";

/** An item's memory off by one, or a parent frame swapped list/constr. */
const forged = (
  entry: walkModule.DataSummaryWalkEntry,
): walkModule.DataSummaryWalkEntry =>
  entry.kind === "item"
    ? {
        kind: "item",
        summary: { ...entry.summary, memory: entry.summary.memory + 1n },
      }
    : {
        kind: "parent",
        frame:
          entry.frame.kind === "list"
            ? { kind: "constr", constructor: 0n }
            : { kind: "list" },
      };

/**
 * Plans the output datum walk one step per transaction, so later transactions
 * restore stack entries, and forges every restore's top entry: `submitProof`
 * is refused and the thread stays at its phase-8 checkpoint without a
 * fraud-proof token. `finish` then continues from that checkpoint with the
 * honest witnesses and must prove the fault.
 */
export const proveAfterForgedWalk = async ({
  lucid,
  submitProof,
  finish,
  threadUnit,
  fraudProofAddress,
  fraudProofUnit,
}: {
  lucid: LucidEvolution;
  submitProof: () => Promise<unknown>;
  finish: (threadOutRef: string) => Promise<{ fraudProofUnit: string }>;
  threadUnit: string;
  fraudProofAddress: string;
  fraudProofUnit: string;
}) => {
  const plan = walkModule.planDataSummaryWalkTransaction;
  const oneStep = { memory: 0, cpu: 0, restoreBytes: 0 };
  const spy = vi
    .spyOn(walkModule, "planDataSummaryWalkTransaction")
    .mockImplementation((trace, from) => {
      const transaction = plan(trace, from, oneStep);
      const [top, ...rest] = transaction.restore;
      return top === undefined
        ? transaction
        : { ...transaction, restore: [forged(top), ...rest] };
    });
  try {
    await expect(submitProof()).rejects.toThrow();
  } finally {
    spy.mockRestore();
  }
  const stopped = await lucid.utxoByUnit(threadUnit);
  const state = Data.from(
    stopped.datum!,
    SDK.TransitionTraceProofCommitmentDatum,
  ).data!;
  expect(state.phase).toBe(8n);
  expect(state.datum_walk).not.toBeNull();
  await expect(
    lucid.utxosAtWithUnit(fraudProofAddress, fraudProofUnit),
  ).resolves.toHaveLength(0);
  const finished = await finish(`${stopped.txHash}#${stopped.outputIndex}`);
  expect(finished.fraudProofUnit).toBe(fraudProofUnit);
  await expectSingleUtxoWithUnit(lucid, fraudProofAddress, fraudProofUnit);
};
