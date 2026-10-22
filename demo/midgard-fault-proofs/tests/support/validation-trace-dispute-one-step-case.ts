/** Our case on the Open scene defended by two operators, with the one-step
 * resolution installed from the start: the scene's operator defends with a
 * trace that agrees with ours up to the state before a CEK compute step, and
 * an enrolled successor operator with one that agrees up to the state before
 * a CEK machine step. The runner records each transaction it signs and, when
 * asked, observes the convicted block's removal over the recorded L1. */
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec/versioned-script";
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
  type ValidationTraceOneStepReferences,
} from "../../src/validation-trace-dispute/transactions.js";
import type { ScenarioSigners } from "../helpers/plutus-execution-block-fixture.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { enrollSuccessor } from "./rotated-successor-operator.js";
import {
  type Defender,
  sceneOperator,
} from "./validation-trace-dispute-defend.js";
import type { AdapterWrap } from "./validation-trace-dispute-funded-runner.js";
import {
  oneStepDeployment,
  publishCanonicalDecodeReferences,
  publishContextReferences,
  publishOneStepReferences,
  publishPhaseReferences,
  publishPhaseYieldReferences,
  publishScriptSourcesRedeemerReferences,
  publishSelectionReferences,
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
 * on its defender's turn; the operator's trace agrees with ours up to
 * `operatorFrom` and the successor's up to `successorFrom`. */
export const defendTwoThreads = async ({
  withRemoval,
  operatorFrom = COMPUTE_STEP_FROM,
  successorFrom = MACHINE_STEP_FROM,
  selections = false,
  contexts = false,
  canonicalDecode = false,
  phases = false,
  scriptSourcesRedeemers = false,
  failingRedeemerData,
  failingReferenceInputs,
  failingSigners,
  failingScript,
  failingExUnits,
  failingNativeSpend,
  failingNativeMint,
  wrap,
  releaseDepth,
}: {
  readonly withRemoval: boolean;
  /** The operator's last agreed state, or how to pick it from our trace. */
  readonly operatorFrom?:
    | number
    | ((material: ValidationTraceClaimMaterial) => Promise<number>);
  /** The successor's last agreed state, or how to pick it from our trace. */
  readonly successorFrom?:
    | number
    | ((material: ValidationTraceClaimMaterial) => Promise<number>);
  /** Whether the deployment drives CEK script selections. */
  readonly selections?: boolean;
  /** Whether the deployment drives CEK context steps. */
  readonly contexts?: boolean;
  /** Whether the deployment drives canonical-decode steps. */
  readonly canonicalDecode?: boolean;
  /** Whether the deployment drives the steps of the phases one-step-phase.ts
   * names. */
  readonly phases?: boolean;
  /** Whether the deployment publishes the script-sources redeemer item
   * chain past its entry. */
  readonly scriptSourcesRedeemers?: boolean;
  /** The failing transaction's redeemer, as Plutus Data CBOR. */
  readonly failingRedeemerData?: Uint8Array;
  /** How many reference inputs the failing transaction reads. */
  readonly failingReferenceInputs?: number;
  /** Who signs the failing transaction beside its owner. */
  readonly failingSigners?: ScenarioSigners;
  /** The script the failing transaction's spend runs. */
  readonly failingScript?: MidgardVersionedScript;
  /** The execution units the failing transaction's spend declares. */
  readonly failingExUnits?: readonly [bigint, bigint];
  /** Whether the failing transaction also spends an output a native script
   * holds. */
  readonly failingNativeSpend?: boolean;
  /** Whether the failing transaction also mints under a native script's
   * policy. */
  readonly failingNativeMint?: boolean;
  /** Rewrites the adapter beneath the record of what it signs. */
  readonly wrap?: (scene: Scene) => AdapterWrap;
  /** How many blocks below the tip the runner's release-final point stands;
   * by default it is the tip. */
  readonly releaseDepth?: number;
}): Promise<TwoThreadCase> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = withRemoval ? recordCrossBlockRawEmulator() : undefined;
  // Published before the scene's blocks, so the publications take none of
  // the source verification's window from the failing block's end.
  let oneStep: ValidationTraceOneStepReferences | undefined;
  const scene = await setUpScene(
    async (contracts) => {
      oneStep = {
        ...(await publishOneStepReferences(contracts)),
        ...(selections
          ? { cekSelection: await publishSelectionReferences(contracts) }
          : {}),
        ...(contexts
          ? { cekContext: await publishContextReferences(contracts) }
          : {}),
        ...(canonicalDecode
          ? {
              canonicalDecode:
                await publishCanonicalDecodeReferences(contracts),
            }
          : {}),
        ...(phases
          ? {
              phases: await publishPhaseReferences(contracts),
              phaseYields: await publishPhaseYieldReferences(contracts),
            }
          : {}),
        ...(scriptSourcesRedeemers
          ? {
              scriptSourcesRedeemers:
                await publishScriptSourcesRedeemerReferences(contracts),
            }
          : {}),
      };
    },
    {
      ...(failingRedeemerData === undefined ? {} : { failingRedeemerData }),
      ...(failingReferenceInputs === undefined
        ? {}
        : { failingReferenceInputs }),
      ...(failingSigners === undefined ? {} : { failingSigners }),
      ...(failingScript === undefined ? {} : { failingScript }),
      ...(failingExUnits === undefined ? {} : { failingExUnits }),
      ...(failingNativeSpend === undefined ? {} : { failingNativeSpend }),
      ...(failingNativeMint === undefined ? {} : { failingNativeMint }),
    },
  );
  const removal =
    recorder === undefined ? undefined : await recordedRemoval(scene, recorder);
  const successor = await enrollSuccessor(
    scene.h as never,
    scene.operator.vkey,
  );
  const deployment = oneStepDeployment(scene, oneStep!);
  const prepared: PreparedMove[] = [];
  const runner = makeRunner(scene, {
    deployment,
    wrap: (adapter) =>
      capturePrepared(prepared)(
        wrap === undefined ? adapter : wrap(scene)(adapter),
      ),
    ...(releaseDepth === undefined
      ? {}
      : {
          signedRecovery: emulatorSignedTransactionRecovery(
            scene.h.emulator,
            releaseDepth,
          ),
        }),
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
  // A picker reads a material of its own, so whatever it builds from our
  // trace is not held through the play.
  const agreedUpTo = async (from: typeof operatorFrom) =>
    typeof from === "number" ? from : await from(await failingMaterial(scene));
  const traceA = scriptedTrace(material, await agreedUpTo(operatorFrom));
  const traceB = scriptedTrace(material, await agreedUpTo(successorFrom));
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
