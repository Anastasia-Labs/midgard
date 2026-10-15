/**
 * The forged accepted-transaction step cases: a prover whose step at one
 * streamed kind-0 phase is forged so that exactly one withdraw-zero script
 * can see it. `submitProof` is refused by that script, named by its
 * withdrawal redeemer, and the thread stays at the forged step's checkpoint
 * without a fraud-proof token. `finish` then continues from that checkpoint
 * with honest steps and must prove the fault.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "../../src/transition-trace/detect.mpf-proof-from-witness.js";
import * as phasesModule from "../../src/transition-trace/phases.js";
import { type LocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";
import { expectSingleUtxoWithUnit } from "./submit-init-emulator-shared.js";

/**
 * The script each forgery is visible to:
 * - `delete`: a phase-1 step claiming a post-root its witness does not give;
 * - `outputs`: the phase-10 step that completes certification claiming one
 *   output more than the outputs field holds;
 * - `outputStep`: a whole phase-11 replay claiming a post-root its insert
 *   does not give;
 * - `descriptor`: a whole replay inserting a descriptor other than the one
 *   its output derives, consistently claimed to both the main step and the
 *   summary verifier;
 * - `summary`: a whole replay whose summaries are forged consistently in both
 *   claims, the descriptor derived from them and the insert.
 */
export type TransitionTraceForgedStep =
  | "delete"
  | "outputs"
  | "outputStep"
  | "descriptor"
  | "summary";

type TransitionTraceYieldHashes = Readonly<
  Record<
    | "l2Delete"
    | "l2Outputs"
    | "l2OutputStep"
    | "l2OutputDescriptor"
    | "l2OutputSummary",
    { readonly withdrawalScriptHash: string }
  >
>;

const REFUSING_YIELD = {
  delete: "l2Delete",
  outputs: "l2Outputs",
  outputStep: "l2OutputStep",
  descriptor: "l2OutputDescriptor",
  summary: "l2OutputSummary",
} as const satisfies Record<
  TransitionTraceForgedStep,
  keyof TransitionTraceYieldHashes
>;

const FORGED_PHASE = {
  delete: 1n,
  outputs: 10n,
  outputStep: 11n,
  descriptor: 11n,
  summary: 11n,
} as const satisfies Record<TransitionTraceForgedStep, bigint>;

/** `hex` with its first byte's low bit flipped. */
const flipped = (hex: string) =>
  `${(Number.parseInt(hex.slice(0, 2), 16) ^ 1).toString(16).padStart(2, "0")}${hex.slice(2)}`;

/** Every occurrence of the 32-byte CBOR byte string `from` replaced by `to`. */
const substituted = (hex: string, from: string, to: string) =>
  hex.split(`5820${from}`).join(`5820${to}`);

/** A whole phase-11 replay inserting `descriptor` in place of its own. */
const withInsertedDescriptor = (
  step: phasesModule.TransitionTracePhaseStep,
  descriptor: string,
  verifiers: NonNullable<phasesModule.TransitionTracePhaseStep["verifiers"]>,
): phasesModule.TransitionTracePhaseStep => {
  const redeemer = Data.from(step.redeemer, SDK.TransitionTraceOutputStep);
  const insert = { ...redeemer.insert, value: descriptor };
  const proof = mpfProofFromWitness({
    key: Buffer.from(insert.key, "hex"),
    value: Buffer.from(descriptor, "hex"),
    proof: insert.insert_proof,
    label: "forged output insert",
  });
  return {
    state: {
      ...step.state,
      current_root: normalizedMpfRoot(proof.verify(true), "forged next-root"),
    },
    redeemer: Data.to({ ...redeemer, insert }, SDK.TransitionTraceOutputStep),
    verifiers,
  };
};

const forgedStep = (
  forge: TransitionTraceForgedStep,
  step: phasesModule.TransitionTracePhaseStep,
): phasesModule.TransitionTracePhaseStep => {
  if (forge === "delete" || forge === "outputStep")
    return {
      ...step,
      state: { ...step.state, current_root: flipped(step.state.current_root) },
    };
  if (forge === "outputs")
    return {
      ...step,
      state: { ...step.state, output_count: step.state.output_count + 1n },
    };
  const verifiers = step.verifiers!;
  const claim = Data.from(
    verifiers.descriptor,
    SDK.TransitionTraceOutputDescriptorClaim,
  );
  if (forge === "descriptor") {
    const descriptor = flipped(claim.descriptor);
    return withInsertedDescriptor(step, descriptor, {
      descriptor: Data.to(
        { ...claim, descriptor },
        SDK.TransitionTraceOutputDescriptorClaim,
      ),
      summary: verifiers.summary,
    });
  }
  const root = claim.summary[0].root;
  const forgedRoot = flipped(root);
  // Without an inline datum or reference script the first two summaries
  // coincide, so both are forged together, consistently in every claim.
  expect(claim.descriptor).toContain(root);
  return withInsertedDescriptor(
    step,
    substituted(claim.descriptor, root, forgedRoot),
    {
      descriptor: substituted(verifiers.descriptor, root, forgedRoot),
      summary: substituted(verifiers.summary, root, forgedRoot),
    },
  );
};

const forges = (
  forge: TransitionTraceForgedStep,
  state: SDK.TransitionTraceFinalState,
  step: phasesModule.TransitionTracePhaseStep,
) =>
  state.kind === 0n &&
  state.phase === FORGED_PHASE[forge] &&
  (forge === "outputs"
    ? step.state.phase !== 10n
    : forge === "delete" || step.verifiers !== undefined);

/** The withdrawal redeemer index of `scriptHash` in `transaction`. */
const withdrawalIndex = (
  transaction: LocallyEvaluatedTransaction,
  scriptHash: string,
) => {
  const keys = transaction.signed.toTransaction().body().withdrawals()!.keys();
  const hashes = Array.from({ length: keys.len() }, (_, index) =>
    keys.get(index).payment().as_script()!.to_hex(),
  ).sort();
  expect(hashes).toContain(scriptHash);
  return hashes.indexOf(scriptHash);
};

export const proveAfterForgedStep = async ({
  forge,
  lucid,
  yields,
  submitProof,
  capture,
  finish,
  threadUnit,
  fraudProofAddress,
  fraudProofUnit,
}: {
  forge: TransitionTraceForgedStep;
  lucid: LucidEvolution;
  yields: TransitionTraceYieldHashes;
  submitProof: () => Promise<unknown>;
  /** The honest step from the checkpoint, built but not submitted. */
  capture: (threadOutRef: string) => Promise<LocallyEvaluatedTransaction>;
  finish: (threadOutRef: string) => Promise<{ fraudProofUnit: string }>;
  threadUnit: string;
  fraudProofAddress: string;
  fraudProofUnit: string;
}) => {
  const next = phasesModule.nextTransitionTracePhase;
  const spy = vi
    .spyOn(phasesModule, "nextTransitionTracePhase")
    .mockImplementation((input) => {
      const step = next(input);
      return forges(forge, input.state, step) ? forgedStep(forge, step) : step;
    });
  let refusal: unknown;
  try {
    refusal = await submitProof().then(
      () => undefined,
      (cause: unknown) => cause,
    );
  } finally {
    spy.mockRestore();
  }
  expect(refusal).toBeInstanceOf(Error);
  const stopped = await lucid.utxoByUnit(threadUnit);
  const state = Data.from(
    stopped.datum!,
    SDK.TransitionTraceProofCommitmentDatum,
  ).data!;
  expect(state.phase).toBe(FORGED_PHASE[forge]);
  await expect(
    lucid.utxosAtWithUnit(fraudProofAddress, fraudProofUnit),
  ).resolves.toHaveLength(0);
  const threadOutRef = `${stopped.txHash}#${stopped.outputIndex}`;
  const index = withdrawalIndex(
    await capture(threadOutRef),
    yields[REFUSING_YIELD[forge]].withdrawalScriptHash,
  );
  const messages: string[] = [];
  for (let cause = refusal; cause instanceof Error; cause = cause.cause)
    messages.push(cause.message);
  expect(messages.join("\n")).toMatch(
    new RegExp(
      `failed script execution\\s+Withdraw\\[${index.toString()}\\]`,
      "u",
    ),
  );
  const finished = await finish(threadOutRef);
  expect(finished.fraudProofUnit).toBe(fraudProofUnit);
  await expectSingleUtxoWithUnit(lucid, fraudProofAddress, fraudProofUnit);
};
