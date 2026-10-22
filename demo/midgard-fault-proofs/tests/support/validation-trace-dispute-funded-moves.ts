/**
 * The validation-trace dispute's thread moves under the watcher's prover
 * funding, on the emulator: a funded watcher opens our case, the scene's
 * operator defends it with a scripted trace and reveals, and the watcher
 * plays the thread. Each move the watcher builds is captured with the
 * thread (and case) it spends while they are still live, so a test can
 * walk the funding permit over the exact signed bytes it admitted.
 */
import {
  CML,
  coreToTxOutput,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import { confirmedValidationTraceStages } from "../../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  type ValidationTraceRemovalConfig,
} from "../../src/validation-trace-dispute/removal.js";
import {
  VALIDATION_TRACE_THREAD_AWAITING,
  validationTraceChallengerDeadlineMs,
  validationTraceDefenderDeadlineMs,
} from "../../src/validation-trace-dispute/threads.js";
import { readValidationDisputeMoveAuthority } from "../../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import type { ValidationDisputeMoveAuthority } from "../../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { drainProver } from "./installed-prover-wallet.js";
import { enrollSuccessor } from "./rotated-successor-operator.js";
import {
  type Defender,
  sceneOperator,
} from "./validation-trace-dispute-defend.js";
import {
  type AdapterWrap,
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import {
  caseAndBond,
  entriesOf,
  FUNDED,
} from "./validation-trace-dispute-funded-verify-source.js";
import {
  awaitLastSlotAtOrBefore,
  defenderClosesItsWin,
  defenderEntersOurTimeout,
  liveCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./validation-trace-dispute-removal-l1.js";
import {
  defendScripted,
  liveThread,
  revealAsDefender,
  type ScriptedTrace,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./validation-trace-dispute-scripted-defender.js";

/** One move the watcher built, with what it spends as it was then. */
export type CapturedMove = Readonly<{
  signed: TxSigned;
  authority: ValidationDisputeMoveAuthority;
  action: FraudProofWorkflowAction;
  thread: UTxO;
  caseInput: UTxO | undefined;
}>;

export type FundedMoves = Readonly<{
  scene: Scene;
  /** The convicted block's removal, its L1 view held from before the case. */
  removal: RecordedRemoval;
  dispute: FundedDispute;
  material: ValidationTraceClaimMaterial;
  defender: Defender;
  trace: ScriptedTrace;
  /** The case output our VerifySource left, as it was before the Defend. */
  caseOrigin: UTxO;
  /** The pending claim our Open left, which every move's authority binds. */
  opener: UTxO;
  /** The defender's thread token. */
  unit: string;
  /** Every move the watcher built, in order. */
  moves: CapturedMove[];
  /** Captures every move the watcher builds into `moves`. */
  capture: AdapterWrap;
}>;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The output at `outRef` as the scene's raw L1 recorded it, spent or not. */
export const recordedOutput = (scene: Scene, outRef: string): UTxO => {
  const { txHash, outputIndex } = parseOwnOutRef(outRef);
  const row = scene.l1.rows.find((recorded) => recorded.txHash === txHash);
  if (row === undefined) throw new Error(`${outRef} was not recorded`);
  return {
    ...coreToTxOutput(
      CML.TransactionBody.from_cbor_hex(row.bodyCbor)
        .outputs()
        .get(outputIndex),
    ),
    txHash,
    outputIndex,
  };
};

/** Records into `moves` each thread move the adapter signs, with what it
 * spends as it was then. */
export const captureMoves =
  (scene: Scene, moves: CapturedMove[]): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const signed = workflowPreflightTransaction(preflight);
      const authority =
        signed === undefined
          ? null
          : readValidationDisputeMoveAuthority(signed);
      if (authority === null) return preflight;
      const [spent, caseInput] = await scene.h.emulator.getUtxosByOutRef([
        parseOwnOutRef(authority.threadOutRef),
        ...(authority.caseOutRef === null
          ? []
          : [parseOwnOutRef(authority.caseOutRef)]),
      ]);
      moves.push({
        signed: signed!,
        authority,
        action: context.action,
        thread: spent!,
        caseInput,
      });
      return preflight;
    },
  });

/** The pending claim our confirmed Open left, as it was before VerifySource. */
export const ourOpener = (
  scene: Scene,
  result: Awaited<ReturnType<FundedDispute["turn"]>>,
): UTxO => {
  const open = confirmedValidationTraceStages(entriesOf(result)).find(
    ({ action }) => action.stage === "open",
  );
  if (open === undefined) throw new Error("no Open was confirmed");
  return recordedOutput(scene, open.outputs[0]!.outRef);
};

type FundedOptions = Pick<
  NonNullable<Parameters<typeof openFundedDispute>[1]>,
  "liveParameters"
> & {
  /** Rewrites the convicted block's removal the watcher runs with. */
  readonly rewriteRemoval?: (
    recorded: ValidationTraceRemovalConfig,
  ) => ValidationTraceRemovalConfig;
};

/** A second defender of our case: its operator, trace and thread token. */
export type SecondDefense = Readonly<{
  defender: Defender;
  trace: ScriptedTrace;
  unit: string;
}>;

/**
 * A fresh scene whose funded watcher has opened our case, which the
 * scene's operator then defends with a trace agreeing up to a third of the
 * steps. The removal's L1 view is held from before the case, so a case the
 * watcher convicts waits on it.
 */
export const openDefendedFundedCase = async (
  options: FundedOptions = {},
): Promise<FundedMoves> => (await openFundedCase(options, false)).journey;

/**
 * As `openDefendedFundedCase`, with a successor operator, enrolled before
 * the case opens, defending it on a second thread with a trace agreeing up
 * to half of the steps. `beforeOpen` runs on the scene just before the
 * watcher's first turn.
 */
export const openTwoDefendedFundedCase = async (
  options: FundedOptions & {
    readonly beforeOpen?: (scene: Scene) => void;
  } = {},
): Promise<FundedMoves & Readonly<{ second: SecondDefense }>> => {
  const { journey, second } = await openFundedCase(options, true);
  return { ...journey, second: second! };
};

const openFundedCase = async (
  {
    beforeOpen,
    rewriteRemoval,
    ...options
  }: FundedOptions & { readonly beforeOpen?: (scene: Scene) => void },
  twoDefenders: boolean,
): Promise<{ journey: FundedMoves; second: SecondDefense | undefined }> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = recordCrossBlockRawEmulator();
  const scene = await setUpScene();
  const successor = twoDefenders
    ? await enrollSuccessor(scene.h as never, scene.operator.vkey)
    : undefined;
  const removal = await recordedRemoval(scene, recorder);
  removal.holdView();
  await removal
    .l1For(scene.failing.evidence.headerHash)
    .observe({ headerHash: scene.failing.evidence.headerHash });
  await drainProver(scene.h as never, FUNDED);
  beforeOpen?.(scene);
  const dispute = await openFundedDispute(scene, {
    ...options,
    removal: rewriteRemoval?.(removal.removal) ?? removal.removal,
    removalScripts: removal.published,
  });
  const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
  if (opened.result.kind !== "pending")
    throw new Error(`the case did not open: ${opened.history.join("\n")}`);
  const { caseOutput } = await caseAndBond(scene, opened.result);
  const material = await failingMaterial(scene);
  const trace = scriptedTrace(
    material,
    Math.floor(Number(material.stepCount) / 3),
  );
  const defender = sceneOperator(scene);
  const threadOutRef = await defendScripted(scene, caseOutput, defender, trace);
  const [thread] = await scene.h.emulator.getUtxosByOutRef([
    parseOwnOutRef(threadOutRef),
  ]);
  const moves: CapturedMove[] = [];
  const capture = captureMoves(scene, moves);
  const journey: FundedMoves = {
    scene,
    removal,
    dispute,
    material,
    defender,
    trace,
    caseOrigin: caseOutput,
    opener: ourOpener(scene, opened.result),
    unit: threadUnitOf(scene, thread!),
    moves,
    capture,
  };
  if (successor === undefined) return { journey, second: undefined };
  const second: Defender = {
    vkey: successor.keyHash,
    nodeUnit: successor.activeNodeUnit,
    lucid: successor.lucid,
  };
  const secondTrace = scriptedTrace(
    material,
    Math.floor(Number(material.stepCount) / 2),
  );
  const [defendedCase] = await scene.h.emulator.getUtxosByOutRef([
    { ...parseOwnOutRef(threadOutRef), outputIndex: 0 },
  ]);
  const secondThreadOutRef = await defendScripted(
    scene,
    defendedCase!,
    second,
    secondTrace,
    [defender.vkey],
  );
  const [secondThread] = await scene.h.emulator.getUtxosByOutRef([
    parseOwnOutRef(secondThreadOutRef),
  ]);
  return {
    journey,
    second: {
      defender: second,
      trace: secondTrace,
      unit: threadUnitOf(scene, secondThread!),
    },
  };
};

/** The defender's live thread. */
export const liveFundedThread = async ({
  scene,
  unit,
}: FundedMoves): Promise<UTxO> => {
  const utxo = await liveThread(scene, unit);
  if (utxo === undefined) throw new Error("the thread is not live");
  return utxo;
};

/** Moves the emulator past the live thread's defender deadline. */
export const passDefenderDeadline = async (journey: FundedMoves) => {
  const { scene } = journey;
  const deadline = validationTraceDefenderDeadlineMs(
    threadOf(await liveFundedThread(journey)),
  );
  const lucid = scene.h.proverLucid;
  scene.h.emulator.awaitSlot(
    lucid.unixTimeToSlot(deadline) - lucid.currentSlot() + 2,
  );
};

/**
 * The defender reveals, the watcher reveals in turn, the defender goes
 * silent, and the watcher times its thread out, closes it into our case and,
 * the case's defense deadline passed, closes the case Convicted.
 */
export const playFundedMovesToClose = async (journey: FundedMoves) => {
  const { scene, dispute, defender, trace, capture } = journey;
  await revealAsDefender(
    scene,
    defender,
    await liveFundedThread(journey),
    trace,
  );
  const revealed = await turnUntil(dispute, VALIDATION_TRACE_THREAD_AWAITING, {
    wrap: capture,
  });
  if (revealed.result.kind !== "pending")
    throw new Error(`no reveal: ${revealed.history.join("\n")}`);
  await passDefenderDeadline(journey);
  const closed = await turnUntil(dispute, VALIDATION_TRACE_CASE_CONVICTED, {
    wrap: capture,
  });
  if (closed.result.kind !== "pending")
    throw new Error(`no thread close: ${closed.history.join("\n")}`);
};

/** The stages the journal confirmed, with the out-refs each left. */
export const confirmedMoves = (
  result: Awaited<ReturnType<FundedDispute["turn"]>>,
) =>
  confirmedValidationTraceStages(entriesOf(result)).map(
    ({ action, outputs }) => ({
      stage: action.stage,
      outputs: outputs.map(({ outRef }) => outRef),
    }),
  );

/** The submission intents the journal recorded for actions of `stage`. */
export const moveIntents = (
  result: Awaited<ReturnType<FundedDispute["turn"]>>,
  stage: "reveal" | "timeout" | "close-thread",
) =>
  entriesOf(result).flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionId.startsWith(`validation-trace-${stage}:`)
      ? [
          {
            actionId: event.actionId,
            attempt: event.attempt,
            txHash: event.txHash,
          },
        ]
      : [],
  );

/** The prover wallet's outputs, by out-ref. */
export const walletOutRefs = async ({ scene }: FundedMoves) =>
  new Map(
    (await scene.h.emulator.getUtxos(scene.h.proverSigner.address)).map(
      (utxo) => [outRefOf(utxo), utxo] as const,
    ),
  );

/** Our case's token. */
export const caseUnitOf = ({ scene, caseOrigin }: FundedMoves): string =>
  Object.keys(caseOrigin.assets).find(
    (unit) =>
      unit.startsWith(scene.chain.case.policyId) &&
      unit !== scene.chain.case.policyId,
  )!;

/**
 * The successor reveals and, no turn of ours answering before our clock on
 * its thread runs out, moves its thread to the timeout validator and closes
 * it into our case as its win, from its own wallet; the operator stays
 * silent past its deadline on its own thread.
 */
export const successorWinsItsThread = async (
  journey: FundedMoves & Readonly<{ second: SecondDefense }>,
): Promise<void> => {
  const { scene, dispute, second } = journey;
  const { deployment } = dispute;
  await revealAsDefender(
    scene,
    second.defender,
    (await liveThread(scene, second.unit))!,
    second.trace,
  );
  const revealed = (await liveThread(scene, second.unit))!;
  awaitLastSlotAtOrBefore(
    scene,
    validationTraceChallengerDeadlineMs(threadOf(revealed)) + 1_000,
  );
  await defenderClosesItsWin({
    deployment,
    defender: second.defender,
    thread: await defenderEntersOurTimeout({
      deployment,
      defender: second.defender,
      thread: revealed,
    }),
    unit: second.unit,
    caseUtxo: await liveCase({ scene, caseUnit: caseUnitOf(journey) }),
  });
  if (
    scene.h.emulator.now() <=
    validationTraceDefenderDeadlineMs(threadOf(await liveFundedThread(journey)))
  )
    await passDefenderDeadline(journey);
};
