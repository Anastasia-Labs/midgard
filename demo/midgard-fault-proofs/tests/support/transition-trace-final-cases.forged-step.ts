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
import { type TransitionProofInput } from "../../src/transition-trace/proof-material.js";
import { type LocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";
import { expectSingleUtxoWithUnit } from "./submit-init-emulator-shared.js";
import {
  nextOutputKey,
  type PhaseInput,
  reinserted,
  rerouted,
  secondInputDelete,
  skippedWalk,
  withInsert,
} from "./transition-trace-final-cases.forged-step-witnesses.js";

/**
 * The script each forgery is visible to:
 * - `delete`: a phase-1 step claiming a post-root its witness does not give;
 * - `deleteKey`: a phase-1 step deleting another present ledger entry than
 *   the walked spend input, with a valid proof and the root it gives;
 * - `outputs`: the phase-10 step that completes certification claiming one
 *   output more than the outputs field holds;
 * - `outputStep`: a whole phase-11 replay claiming a post-root its insert
 *   does not give;
 * - `insertKey`: a whole replay inserting its descriptor under another
 *   output's reference, with the root that insert gives;
 * - `insertValue`: a whole replay inserting a descriptor other than the one
 *   both verifiers checked, with the root that insert gives;
 * - `walk`: a whole replay resuming the outputs walk at the next output's
 *   checkpoint, whose bytes are the same as this output's;
 * - `wholeAsStaged`: an output the phase-11 step replays whole, taken to the
 *   staged phases instead;
 * - `stagedAsWhole`: an output the phase-11 step stages, replayed whole with
 *   both verifiers' claims instead;
 * - `stagedInsert`: a staged output's phase-4 insert under another output's
 *   reference, with the root that insert gives;
 * - `descriptor`: a whole replay inserting a descriptor other than the one
 *   its output derives, consistently claimed to both the main step and the
 *   summary verifier;
 * - `summary`: a whole replay whose summaries are forged consistently in both
 *   claims, the descriptor derived from them and the insert.
 */
export type TransitionTraceForgedStep =
  | "delete"
  | "deleteKey"
  | "outputs"
  | "outputStep"
  | "insertKey"
  | "insertValue"
  | "walk"
  | "wholeAsStaged"
  | "stagedAsWhole"
  | "stagedInsert"
  | "descriptor"
  | "summary";

type TransitionTraceYieldHashes = Readonly<
  Record<
    | "l2Delete"
    | "l2Outputs"
    | "l2OutputStep"
    | "l2OutputDescriptor"
    | "l2OutputSummary"
    | "l2Replay",
    { readonly withdrawalScriptHash: string }
  >
>;

const REFUSING_YIELD = {
  delete: "l2Delete",
  deleteKey: "l2Delete",
  outputs: "l2Outputs",
  outputStep: "l2OutputStep",
  insertKey: "l2OutputStep",
  insertValue: "l2OutputStep",
  walk: "l2OutputStep",
  wholeAsStaged: "l2OutputStep",
  stagedAsWhole: "l2OutputStep",
  stagedInsert: "l2Replay",
  descriptor: "l2OutputDescriptor",
  summary: "l2OutputSummary",
} as const satisfies Record<
  TransitionTraceForgedStep,
  keyof TransitionTraceYieldHashes
>;

const FORGED_PHASE = {
  delete: 1n,
  deleteKey: 1n,
  outputs: 10n,
  outputStep: 11n,
  insertKey: 11n,
  insertValue: 11n,
  walk: 11n,
  wholeAsStaged: 11n,
  stagedAsWhole: 11n,
  stagedInsert: 4n,
  descriptor: 11n,
  summary: 11n,
} as const satisfies Record<TransitionTraceForgedStep, bigint>;

/** `hex` with its first byte's low bit flipped. */
const flipped = (hex: string) =>
  `${(Number.parseInt(hex.slice(0, 2), 16) ^ 1).toString(16).padStart(2, "0")}${hex.slice(2)}`;

/** Every occurrence of the 32-byte CBOR byte string `from` replaced by `to`. */
const substituted = (hex: string, from: string, to: string) =>
  hex.split(`5820${from}`).join(`5820${to}`);

const forgedStep = (
  forge: TransitionTraceForgedStep,
  input: PhaseInput,
  step: phasesModule.TransitionTracePhaseStep,
  deleteWitness: SDK.LedgerDeleteWitness | undefined,
): phasesModule.TransitionTracePhaseStep => {
  const preRoot = input.state.current_root;
  if (forge === "delete" || forge === "outputStep")
    return {
      ...step,
      state: { ...step.state, current_root: flipped(step.state.current_root) },
    };
  if (forge === "deleteKey") {
    const redeemer = Data.from(step.redeemer, SDK.TransitionTraceDeleteStep);
    const proof = mpfProofFromWitness({
      key: Buffer.from(deleteWitness!.key, "hex"),
      value: Buffer.from(deleteWitness!.value, "hex"),
      proof: deleteWitness!.delete_proof,
      label: "forged input delete",
    });
    expect(normalizedMpfRoot(proof.verify(true), "forged pre-root")).toBe(
      preRoot,
    );
    return {
      state: {
        ...step.state,
        current_root: normalizedMpfRoot(proof.verify(false), "forged root"),
      },
      redeemer: Data.to(
        { ...redeemer, witnesses: [deleteWitness!] },
        SDK.TransitionTraceDeleteStep,
      ),
    };
  }
  if (forge === "outputs")
    return {
      ...step,
      state: { ...step.state, output_count: step.state.output_count + 1n },
    };
  if (forge === "insertKey")
    return withInsert(
      step,
      preRoot,
      { key: nextOutputKey(input.state) },
      step.verifiers!,
    );
  if (forge === "insertValue")
    return withInsert(
      step,
      preRoot,
      {
        value: flipped(
          Data.from(step.redeemer, SDK.TransitionTraceOutputStep).insert.value,
        ),
      },
      step.verifiers!,
    );
  if (forge === "walk") return skippedWalk(input, step);
  if (forge === "wholeAsStaged" || forge === "stagedAsWhole")
    return rerouted(input, step);
  if (forge === "stagedInsert") {
    const witness = Data.from(step.redeemer, SDK.LedgerInsertWitness);
    const { insert, root } = reinserted(witness, preRoot, {
      key: nextOutputKey(input.state),
    });
    return {
      state: { ...step.state, current_root: root },
      redeemer: Data.to(insert, SDK.LedgerInsertWitness),
    };
  }
  const verifiers = step.verifiers!;
  const claim = Data.from(
    verifiers.descriptor,
    SDK.TransitionTraceOutputDescriptorClaim,
  );
  if (forge === "descriptor") {
    const descriptor = flipped(claim.descriptor);
    return withInsert(
      step,
      preRoot,
      { value: descriptor },
      {
        descriptor: Data.to(
          { ...claim, descriptor },
          SDK.TransitionTraceOutputDescriptorClaim,
        ),
        summary: verifiers.summary,
      },
    );
  }
  // Without an inline datum or reference script the first two summaries
  // coincide, so both are forged together, consistently in every claim. The
  // claims are rewritten field by field: a Data byte string longer than 64
  // bytes is encoded in chunks, which can split a root.
  const root = claim.summary[0].root;
  const forgedRoot = flipped(root);
  const descriptor = substituted(claim.descriptor, root, forgedRoot);
  expect(descriptor).not.toBe(claim.descriptor);
  expect(descriptor).not.toContain(root);
  const forgedSummary = (summary: (typeof claim.summary)[number]) =>
    summary.root === root ? { ...summary, root: forgedRoot } : summary;
  const [s1, s2, s3] = claim.summary;
  const summaryClaim = Data.from(
    verifiers.summary,
    SDK.TransitionTraceOutputSummaryClaim,
  );
  return withInsert(
    step,
    preRoot,
    { value: descriptor },
    {
      descriptor: Data.to(
        {
          ...claim,
          descriptor,
          summary: [forgedSummary(s1), forgedSummary(s2), forgedSummary(s3)],
        },
        SDK.TransitionTraceOutputDescriptorClaim,
      ),
      summary: Data.to(
        {
          ...summaryClaim,
          summary: Data.from(
            substituted(Data.to(summaryClaim.summary), root, forgedRoot),
          ),
        },
        SDK.TransitionTraceOutputSummaryClaim,
      ),
    },
  );
};

/** `state` as comparable text, independent of its fields' order. */
const stateKey = (state: SDK.TransitionTraceFinalState) =>
  JSON.stringify(state, (_key, value: unknown) =>
    typeof value === "bigint"
      ? `${value.toString()}n`
      : value !== null && typeof value === "object" && !Array.isArray(value)
        ? Object.fromEntries(
            Object.entries(value).sort(([a], [b]) =>
              a < b ? -1 : a > b ? 1 : 0,
            ),
          )
        : value,
  );

const forges = (
  forge: TransitionTraceForgedStep,
  state: SDK.TransitionTraceFinalState,
  step: phasesModule.TransitionTracePhaseStep,
) =>
  state.kind === 0n &&
  state.phase === FORGED_PHASE[forge] &&
  (forge === "outputs"
    ? step.state.phase !== 10n
    : forge === "stagedAsWhole"
      ? step.verifiers === undefined
      : FORGED_PHASE[forge] !== 11n || step.verifiers !== undefined);

/**
 * The withdrawal redeemer index of `scriptHash` in `transaction` with the
 * scripts `added` withdrawn as well and those `removed` not.
 */
const withdrawalIndex = (
  transaction: LocallyEvaluatedTransaction,
  scriptHash: string,
  { added = [], removed = [] }: { added?: string[]; removed?: string[] },
) => {
  const keys = transaction.signed.toTransaction().body().withdrawals()!.keys();
  const hashes = [
    ...Array.from({ length: keys.len() }, (_, index) =>
      keys.get(index).payment().as_script()!.to_hex(),
    ),
    ...added,
  ]
    .filter((hash) => !removed.includes(hash))
    .sort();
  expect(hashes).toContain(scriptHash);
  return hashes.indexOf(scriptHash);
};

export const proveAfterForgedStep = async ({
  forge,
  lucid,
  proof,
  yields,
  submitProof,
  refusal: pinned,
  capture,
  finish,
  threadUnit,
  fraudProofAddress,
  fraudProofUnit,
}: {
  forge: TransitionTraceForgedStep;
  lucid: LucidEvolution;
  proof: TransitionProofInput;
  yields: TransitionTraceYieldHashes;
  submitProof: () => Promise<unknown>;
  /** Asserts the forged step's refusal, given the submission it refuses. */
  refusal?: (submit: () => Promise<unknown>) => Promise<string>;
  /** The honest step from the checkpoint, built but not submitted. */
  capture: (threadOutRef: string) => Promise<LocallyEvaluatedTransaction>;
  finish: (threadOutRef: string) => Promise<{ fraudProofUnit: string }>;
  threadUnit: string;
  fraudProofAddress: string;
  fraudProofUnit: string;
}) => {
  const deleteWitness =
    forge === "deleteKey" ? await secondInputDelete(proof) : undefined;
  // Exactly one step is forged. A step built from any other checkpoint after
  // it means the chain accepted the forgery, and the submission stops there.
  // The case first asserts that the thread still stands at the forged step's
  // checkpoint, before any later step can fail for another reason.
  const next = phasesModule.nextTransitionTracePhase;
  let forgedFrom: string | undefined;
  let accepted = false;
  const spy = vi
    .spyOn(phasesModule, "nextTransitionTracePhase")
    .mockImplementation((input) => {
      const from = stateKey(input.state);
      if (forgedFrom !== undefined && from !== forgedFrom) {
        accepted = true;
        throw new Error(`the forged ${forge} step was accepted on chain`);
      }
      const step = next(input);
      if (!forges(forge, input.state, step)) return step;
      forgedFrom = from;
      return forgedStep(forge, input, step, deleteWitness);
    });
  // A rerouted step's submission withdraws the verifiers of its own route.
  const whole = phasesModule.transitionTraceOutputStepIsWhole;
  const route =
    forge === "wholeAsStaged" || forge === "stagedAsWhole"
      ? vi
          .spyOn(phasesModule, "transitionTraceOutputStepIsWhole")
          .mockImplementation(
            (state, outputs) =>
              whole(state, outputs) !==
              (state.kind === 0n && state.phase === 11n),
          )
      : undefined;
  let refusal: unknown;
  const submit = () =>
    submitProof().catch((cause: unknown) => {
      refusal = cause;
      throw cause;
    });
  let pinFailure: unknown;
  try {
    if (pinned === undefined) await submit().catch(() => undefined);
    else await pinned(submit).catch((cause: unknown) => (pinFailure = cause));
  } finally {
    spy.mockRestore();
    route?.mockRestore();
  }
  expect(forgedFrom, `no ${forge} step was forged`).toBeDefined();
  const stopped = await lucid.utxoByUnit(threadUnit).catch(() => undefined);
  const state =
    stopped?.datum === undefined || stopped.datum === null
      ? undefined
      : (Data.from(stopped.datum, SDK.TransitionTraceProofCommitmentDatum)
          .data ?? undefined);
  expect(
    !accepted && state !== undefined && stateKey(state) === forgedFrom,
    `the forged ${forge} step was accepted on chain`,
  ).toBe(true);
  if (pinFailure !== undefined) throw pinFailure;
  expect(refusal).toBeInstanceOf(Error);
  expect(state!.phase).toBe(FORGED_PHASE[forge]);
  await expect(
    lucid.utxosAtWithUnit(fraudProofAddress, fraudProofUnit),
  ).resolves.toHaveLength(0);
  const threadOutRef = `${stopped!.txHash}#${stopped!.outputIndex}`;
  // A rerouted step withdraws the output-step verifiers the honest one does
  // not, or the other way round.
  const verifiers = [
    yields.l2OutputDescriptor.withdrawalScriptHash,
    yields.l2OutputSummary.withdrawalScriptHash,
  ];
  const index = withdrawalIndex(
    await capture(threadOutRef),
    yields[REFUSING_YIELD[forge]].withdrawalScriptHash,
    forge === "wholeAsStaged"
      ? { removed: verifiers }
      : forge === "stagedAsWhole"
        ? { added: verifiers }
        : {},
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
