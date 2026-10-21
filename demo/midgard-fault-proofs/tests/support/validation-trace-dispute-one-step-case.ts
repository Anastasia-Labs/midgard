/** Our case on the Open scene defended by two operators, with the one-step
 * resolution installed from the start: the scene's operator defends with a
 * trace that agrees with ours up to the state before a CEK compute step, and
 * an enrolled successor operator with one that agrees up to the state before
 * a CEK machine step. The runner records each transaction it signs and, when
 * asked, observes the convicted block's removal over the recorded L1. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { confirmedValidationTraceStages } from "../../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import {
  buildValidationTraceGameMove,
  validationTraceMoveWindow,
} from "../../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../../src/validation-trace-dispute/move-fee.js";
import {
  caseConfigUtxo,
  type ValidationTraceDisputeDeployment,
} from "../../src/validation-trace-dispute/transactions.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { enrollSuccessor } from "./rotated-successor-operator.js";
import {
  type Defender,
  sceneOperator,
} from "./validation-trace-dispute-defend.js";
import type { AdapterWrap } from "./validation-trace-dispute-funded-runner.js";
import {
  oneStepDeployment,
  publishOneStepReferences,
} from "./validation-trace-dispute-one-step.js";
import {
  capturePrepared,
  type PreparedMove,
  type Side,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
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
  makeRunner,
  type Runner,
  runUntil,
} from "./validation-trace-dispute-runner.js";
import {
  defendScripted,
  liveThread,
  scriptedTrace,
  threadUnitOf,
} from "./validation-trace-dispute-scripted-defender.js";

/** The last agreed state of each defender: before a CEK compute step, and
 * before a CEK machine step. */
export const COMPUTE_STEP_FROM = 116;
export const MACHINE_STEP_FROM = 118;
/** A last agreed state before a CEK script selection. */
export const SELECTION_STEP_FROM = 93;

export type TwoThreadCase = Readonly<{
  scene: Scene;
  deployment: ValidationTraceDisputeDeployment;
  runner: Runner;
  material: ValidationTraceClaimMaterial;
  caseUnit: string;
  /** The bond our VerifySource locked. */
  bondOutRef: string;
  /** The operator's thread (a compute step) and the successor's (a machine
   * step). */
  a: Side;
  b: Side;
  /** Each transaction the runner signed. */
  prepared: PreparedMove[];
  /** The raw L1 recorder and the removal, when asked for. */
  recorder: ReturnType<typeof recordCrossBlockRawEmulator> | undefined;
  removal: RecordedRemoval | undefined;
}>;

const outRef = (text: string) => ({
  txHash: text.split("#")[0]!,
  outputIndex: Number(text.split("#")[1]),
});

/** Opens our case and has both operators defend it, each on its own thread
 * on its defender's turn; the successor's trace agrees with ours up to
 * `successorFrom`. */
export const defendTwoThreads = async ({
  withRemoval,
  successorFrom = MACHINE_STEP_FROM,
  wrap,
}: {
  readonly withRemoval: boolean;
  readonly successorFrom?: number;
  /** Rewrites the adapter beneath the record of what it signs. */
  readonly wrap?: (scene: Scene) => AdapterWrap;
}): Promise<TwoThreadCase> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = withRemoval ? recordCrossBlockRawEmulator() : undefined;
  const scene = await setUpScene();
  const removal =
    recorder === undefined ? undefined : await recordedRemoval(scene, recorder);
  const successor = await enrollSuccessor(
    scene.h as never,
    scene.operator.vkey,
  );
  const deployment = oneStepDeployment(
    scene,
    await publishOneStepReferences(scene),
  );
  const prepared: PreparedMove[] = [];
  const runner = makeRunner(scene, {
    deployment,
    wrap: (adapter) =>
      capturePrepared(prepared)(
        wrap === undefined ? adapter : wrap(scene)(adapter),
      ),
    ...(removal === undefined
      ? {}
      : {
          removal: removal.removal,
          terminalVerifier: removal.terminalVerifier(
            scene.failing.evidence.headerHash,
          ),
        }),
  });
  const { result } = await runUntil(
    runner,
    scene.failing,
    VALIDATION_TRACE_CASE_OPEN,
  );
  if (!("entries" in result)) throw new Error("no workflow journal");
  const verified = confirmedValidationTraceStages(result.entries).find(
    ({ action }) => action.stage === "verify_source",
  )!;
  const material = await failingMaterial(scene);
  const operator = sceneOperator(scene);
  const second: Defender = {
    vkey: successor.keyHash,
    nodeUnit: successor.activeNodeUnit,
    lucid: successor.lucid,
  };
  const traceA = scriptedTrace(material, COMPUTE_STEP_FROM);
  const traceB = scriptedTrace(material, successorFrom);
  const lucid = scene.h.proverLucid;
  const [caseUtxo] = await lucid.utxosByOutRef([
    outRef(verified.outputs[0]!.outRef),
  ]);
  const threadA = await defendScripted(scene, caseUtxo!, operator, traceA);
  const [defendedCase] = await lucid.utxosByOutRef([
    { ...outRef(threadA), outputIndex: 0 },
  ]);
  const threadB = await defendScripted(scene, defendedCase!, second, traceB, [
    operator.vkey,
  ]);
  const unitOf = async (thread: string) =>
    threadUnitOf(scene, (await lucid.utxosByOutRef([outRef(thread)]))[0]!);
  return Object.freeze({
    scene,
    deployment,
    runner,
    material,
    caseUnit: verified.outputs[0]!.unit,
    bondOutRef: verified.outputs[1]!.outRef,
    a: { defender: operator, trace: traceA, unit: await unitOf(threadA) },
    b: { defender: second, trace: traceB, unit: await unitOf(threadB) },
    prepared,
    recorder,
    removal,
  });
};

/** Our case as it stands now. */
export const liveCase = async ({
  scene,
  caseUnit,
}: Pick<TwoThreadCase, "scene" | "caseUnit">): Promise<UTxO> => {
  const [found] = await scene.h.proverLucid.utxosAtWithUnit(
    scene.chain.case.spendingScriptAddress,
    caseUnit,
  );
  if (found === undefined) throw new Error("the case is not live");
  return found;
};

/** The live thread holding `unit` at the game or timeout validator. */
export const gameThread = async (scene: Scene, unit: string): Promise<UTxO> => {
  const found = await liveThread(scene, unit);
  if (found === undefined) throw new Error("the thread is not at the game");
  return found;
};

/** Whether the thread holding `unit` is closed: live nowhere. */
export const threadClosed = async (
  { scene, deployment }: Pick<TwoThreadCase, "scene" | "deployment">,
  unit: string,
): Promise<boolean> =>
  (await threadPlace(scene, deployment, unit)) === undefined;

/** Moves the emulator to the last slot whose time is not past `time`. */
export const awaitLastSlotAtOrBefore = (scene: Scene, time: number): void => {
  const lucid = scene.h.proverLucid;
  let slot = lucid.unixTimeToSlot(time);
  while (lucid.slotToUnixTime(slot) > time) slot -= 1;
  while (lucid.slotToUnixTime(slot + 1) <= time) slot += 1;
  scene.h.emulator.awaitSlot(slot - lucid.currentSlot());
  expect(lucid.currentSlot()).toBe(slot);
};

const signSubmit = async (
  lucid: LucidEvolution,
  unsigned: Awaited<ReturnType<typeof completeValidationTraceMove>>,
): Promise<string> => {
  const hash = await (await unsigned.sign.withWallet().complete()).submit();
  await lucid.awaitTx(hash);
  return hash;
};

/**
 * The defender of `thread`, on our turn past our clock, moves it to the
 * timeout validator, signed and submitted from its own wallet; returns the
 * thread there.
 */
export const defenderEntersOurTimeout = async ({
  deployment,
  defender,
  thread,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly defender: Defender;
  readonly thread: UTxO;
}): Promise<UTxO> => {
  const { lucid } = defender;
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const hash = await signSubmit(
    lucid,
    await completeValidationTraceMove({
      lucid,
      stage: "EnterChallengerTimeout",
      referenced: [deployment.references.game],
      build: (fee) =>
        buildValidationTraceGameMove({
          lucid,
          deployment,
          thread,
          action: { kind: "EnterChallengerTimeout" },
          successor: {
            address: deployment.chain.timeout.spendingScriptAddress,
            datum: thread.datum!,
          },
          fee,
          validFrom,
          validTo,
        }),
    }),
  );
  const [moved] = await lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }]);
  return moved!;
};

/**
 * The defender closes its timed-out thread at `thread` into the case at
 * `caseUtxo` as its win: the thread token burns, no receipt is minted, the
 * case counts one thread fewer and a defender's win, and the rest of the
 * thread, less the fee it pays, goes to the defender's key tagged with the
 * thread. Signed and submitted from the defender's wallet; returns its hash.
 */
export const defenderClosesItsWin = async ({
  deployment,
  defender,
  thread,
  unit,
  caseUtxo,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly defender: Defender;
  readonly thread: UTxO;
  readonly unit: string;
  readonly caseUtxo: UTxO;
}): Promise<string> => {
  const { lucid } = defender;
  const { references } = deployment;
  const caseConfig = await caseConfigUtxo(lucid, deployment);
  const caseDatum = Data.from(caseUtxo.datum!, SDK.Case);
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const caseRedeemer = (ctx: Parameters<typeof SDK.requireInputIndex>[0]) =>
    Data.to(
      {
        ThreadClose: {
          case_output_index: 0n,
          thread_input_index: SDK.requireInputIndex(ctx, thread, "thread"),
          outcome: "DefenderWins",
          defender_vkh: defender.vkey,
        },
      },
      SDK.CaseRedeemer,
    );
  return await signSubmit(
    lucid,
    await completeValidationTraceMove({
      lucid,
      stage: "close_thread",
      referenced: [caseConfig, references.case, references.timeout],
      build: (fee) =>
        lucid
          .newTx()
          .collectFrom([thread], (ctx) =>
            Data.to(
              {
                input_index: SDK.requireInputIndex(ctx, thread, "thread"),
                case_input_index: SDK.requireInputIndex(ctx, caseUtxo, "case"),
              },
              SDK.ValidationTimeoutSpendRedeemer,
            ),
          )
          .collectFrom([caseUtxo], caseRedeemer)
          .mintAssets({ [unit]: -1n }, caseRedeemer)
          .readFrom([caseConfig, references.case, references.timeout])
          .pay.ToContract(
            caseUtxo.address,
            {
              kind: "inline",
              value: Data.to(
                {
                  ...caseDatum,
                  open_threads: caseDatum.open_threads - 1n,
                  defender_won: true,
                },
                SDK.Case,
              ),
            },
            caseUtxo.assets,
          )
          .pay.ToAddressWithData(
            credentialToAddress(deployment.network, {
              type: "Key",
              hash: defender.vkey,
            }),
            {
              kind: "inline",
              value: Data.to(
                new Constr(0, [thread.txHash, BigInt(thread.outputIndex)]),
              ),
            },
            { lovelace: (thread.assets.lovelace ?? 0n) - fee },
          )
          .validFrom(validFrom)
          .validTo(validTo),
    }),
  );
};
