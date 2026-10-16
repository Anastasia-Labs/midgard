/**
 * The honest operator's accepted root: every step of the thread replays, and
 * the terminal step is refused by its one withdrawal, the replay yield, whose
 * remaining check is that the replayed root differs from the claimed one.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  readTransitionProof,
  type TransitionProofInput,
} from "../../src/transition-trace/proof-material.js";

/** The post-state root the challenged trace step claims. */
const claimedPostRoot = (proof: TransitionProofInput) => {
  const { fault } = readTransitionProof(proof);
  if (!("InvalidOneStepTransition" in fault))
    throw new Error("expected a one-step transition proof");
  const [witness] = Object.values(fault.InvalidOneStepTransition.witness);
  return witness!.trace_proof.value.post_utxos_root;
};

export const expectRefusedAtTerminal = async ({
  lucid,
  submitProof,
  proof,
  threadUnit,
  fraudProofAddress,
  fraudProofUnit,
}: {
  lucid: LucidEvolution;
  submitProof: () => Promise<unknown>;
  proof: TransitionProofInput;
  threadUnit: string;
  fraudProofAddress: string;
  fraudProofUnit: string;
}) => {
  const refusal = await submitProof().then(
    () => undefined,
    (cause: unknown) => cause,
  );
  expect(refusal).toBeInstanceOf(Error);
  const messages: string[] = [];
  for (let cause = refusal; cause instanceof Error; cause = cause.cause)
    messages.push(cause.message);
  // The terminal step withdraws from its replay yield alone.
  expect(messages.join("\n")).toMatch(
    /failed script execution\s+Withdraw\[0\]/u,
  );
  const stopped = await lucid.utxoByUnit(threadUnit);
  const state = Data.from(
    stopped.datum!,
    SDK.TransitionTraceProofCommitmentDatum,
  ).data!;
  expect(state.phase).toBe(5n);
  expect(state.current_root).toBe(claimedPostRoot(proof));
  await expect(
    lucid.utxosAtWithUnit(fraudProofAddress, fraudProofUnit),
  ).resolves.toHaveLength(0);
};

/**
 * A forced leaf whose verdict is not ForcedTxValid: the thread binds the step,
 * then the open step refuses the leaf and no fraud-proof token is minted.
 */
export const expectRefusedAtForcedOpen = async ({
  lucid,
  submitProof,
  threadUnit,
  fraudProofAddress,
  fraudProofUnit,
}: {
  lucid: LucidEvolution;
  submitProof: () => Promise<unknown>;
  threadUnit: string;
  fraudProofAddress: string;
  fraudProofUnit: string;
}) => {
  const refusal = await submitProof().then(
    () => undefined,
    (cause: unknown) => cause,
  );
  expect(refusal).toBeInstanceOf(Error);
  const stopped = await lucid.utxoByUnit(threadUnit);
  const state = Data.from(
    stopped.datum!,
    SDK.TransitionTraceProofCommitmentDatum,
  ).data!;
  expect(state.kind).toBe(2n);
  expect(state.phase).toBe(0n);
  await expect(
    lucid.utxosAtWithUnit(fraudProofAddress, fraudProofUnit),
  ).resolves.toHaveLength(0);
};
