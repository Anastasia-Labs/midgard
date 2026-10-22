/** The validation-trace dispute runner when a thread move of ours does not
 * build, on the emulator: our case defended on two threads. A reveal, a
 * timeout or a thread's close that does not build raises the move-unbuilt
 * alarm, and an entry into the one-step resolution that does not build the
 * one-step alarm, each naming the move, the thread, the case and the
 * deadline the move must land by: the case's expiry, or for our reveal and
 * entry the end of our clock on the thread if it comes first. An entry our
 * wallet cannot fund raises the wallet's alarm instead. None moves the
 * thread; once the move builds and is funded, the runner makes it. A move
 * that does not build gives way, in the same run, to the move ranked after
 * it, and the run still raises its alarm. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { validationTraceActionId } from "../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_MOVE_UNBUILT,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  validationTraceCaseExpiryMs,
  validationTraceChallengerDeadlineMs,
  validationTraceDefenderDeadlineMs,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  gameThread,
  liveCase,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  playThreads,
  type Side,
  threadPlace,
  withLedgerParameters,
} from "./support/validation-trace-dispute-one-step-play.js";
import type { Scene } from "./support/validation-trace-dispute-open-scene.js";
import {
  isTurn,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

const ADA = 1_000_000n;

/** A transaction size limit under which none of the moves here builds: the
 * smallest, a timeout, is about 605 bytes. */
const UNBUILDABLE = 500;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const reasonOf = (result: FraudProofWorkflowRunResult) => {
  if (result.kind !== "pending") throw new Error("the run is not pending");
  return result.reason;
};

const caseExpiryOf = async (dispute: TwoThreadCase) =>
  validationTraceCaseExpiryMs(
    Data.from((await liveCase(dispute)).datum!, SDK.Case),
  );

/** The thread holding `unit`, which must be live. */
const placeOf = async (dispute: TwoThreadCase, unit: string) => {
  const place = await threadPlace(dispute.scene, dispute.deployment, unit);
  if (place === undefined) throw new Error("the thread is not live");
  return place;
};

/** Runs the runner under the unbuildable size limit, both defenders
 * silent, until it waits under `state`; returns the run. */
const runUnbuilt = async (dispute: TwoThreadCase, state: string) =>
  (
    await withLedgerParameters(
      dispute.scene,
      { maxTxSize: UNBUILDABLE },
      async () =>
        await playThreads({
          scene: dispute.scene,
          runner: dispute.runner,
          sides: [
            { ...dispute.a, silent: true },
            { ...dispute.b, silent: true },
          ],
          until: async (run) => isPendingFor(run, state),
          limit: 4,
        }),
    )
  ).result;

/** Asserts `result` raises the alarm `state` for the `stage` move on
 * `thread`, which must land by `deadline`. */
const expectAlarm = (
  result: FraudProofWorkflowRunResult,
  {
    state,
    stage,
    thread,
    caseUnit,
    deadline,
  }: {
    readonly state: string;
    readonly stage: string;
    readonly thread: UTxO;
    readonly caseUnit: string;
    readonly deadline: number;
  },
) => {
  expect(reasonOf(result)).toContain(
    `${state}: the ${stage} move on thread ${outRefOf(thread)} of case ${caseUnit} does not build, and must land by ${deadline.toString()}: `,
  );
  const runState = validationTraceRunState(result);
  expect("namedState" in runState && runState.namedState).toStrictEqual({
    name: state,
    alarm: true,
  });
};

/** Sends every plain-Ada coin of our wallet to the funder. */
const emptyProverWallet = async ({ h }: Scene) => {
  h.emulator.awaitBlock();
  const lucid = h.proverLucid;
  lucid.clearUTxOOverride();
  const plain = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.datum == null && utxo.datumHash == null && utxo.scriptRef == null,
  );
  const emptied = await lucid
    .newTx()
    .collectFrom(plain)
    .complete({
      presetWalletInputs: plain,
      coinSelection: false,
      changeAddress: await h.funderLucid.wallet().address(),
    });
  await (await emptied.sign.withWallet().complete()).submit();
  h.emulator.awaitBlock();
};

/** The funder pays our wallet `lovelace`. */
const topUpProverWallet = async ({ h }: Scene, lovelace: bigint) => {
  const topUp = await h.funderLucid
    .newTx()
    .pay.ToAddress(h.proverSigner.address, { lovelace })
    .complete();
  await (await topUp.sign.withWallet().complete()).submit();
  h.emulator.awaitBlock();
};

describe("validation-trace dispute runner: our reveal and entry into the one-step resolution, when they do not build or our wallet cannot fund them", () => {
  // The operator's defender is silent; the successor's plays.
  let dispute: TwoThreadCase;
  let sides: readonly Side[];
  // The actions whose builds run under the unbuildable size limit.
  const stuck = new Set<string>();

  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: false,
      wrap: (scene) => (adapter) => ({
        ...adapter,
        preflight: async (context) =>
          stuck.has(context.action.actionId)
            ? await withLedgerParameters(
                scene,
                { maxTxSize: UNBUILDABLE },
                async () => await adapter.preflight(context),
              )
            : await adapter.preflight(context),
      }),
    });
    sides = [{ ...dispute.a, silent: true }, dispute.b];
  }, 1_800_000);

  it("raises the move-unbuilt alarm when our reveal does not build, naming the thread, the case and the earlier of the case's expiry and our clock's end, with the thread unmoved", async () => {
    const { scene, b, caseUnit } = dispute;
    await revealAsDefender(
      scene,
      b.defender,
      await gameThread(scene, b.unit),
      b.trace,
    );
    const result = await runUnbuilt(dispute, VALIDATION_TRACE_MOVE_UNBUILT);
    const place = await placeOf(dispute, b.unit);
    expect(place.holder).toBe("game");
    expect(isTurn(threadOf(place.utxo), "AwaitingChallenger")).toBe(true);
    expectAlarm(result, {
      state: VALIDATION_TRACE_MOVE_UNBUILT,
      stage: "reveal",
      thread: place.utxo,
      caseUnit,
      deadline: Math.min(
        await caseExpiryOf(dispute),
        validationTraceChallengerDeadlineMs(threadOf(place.utxo)),
      ),
    });
    expect(
      dispute.prepared.filter(({ stage }) => stage === "reveal"),
    ).toStrictEqual([]);
  }, 600_000);

  it("raises the one-step alarm when our entry into the one-step resolution does not build, naming the thread, the case and the earlier of the case's expiry and our clock's end, with the thread unmoved", async () => {
    const { scene, runner, b, caseUnit } = dispute;
    const ready = async () => {
      const place = await placeOf(dispute, b.unit);
      return place.holder === "game" &&
        threadOf(place.utxo).turn === "ReadyForOneStep"
        ? place
        : undefined;
    };
    // The successor's thread is played out under the ledger's own limit, up
    // to ReadyForOneStep, where only our entry is left to make.
    await playThreads({
      scene,
      runner,
      sides,
      until: async () => (await ready()) !== undefined,
      limit: 80,
    });
    const place = (await ready())!;
    const result = await runUnbuilt(
      dispute,
      VALIDATION_TRACE_ONE_STEP_REQUIRED,
    );
    expect(outRefOf((await ready())!.utxo)).toBe(outRefOf(place.utxo));
    expectAlarm(result, {
      state: VALIDATION_TRACE_ONE_STEP_REQUIRED,
      stage: "enter_resolution",
      thread: place.utxo,
      caseUnit,
      deadline: Math.min(
        await caseExpiryOf(dispute),
        validationTraceChallengerDeadlineMs(threadOf(place.utxo)),
      ),
    });
    expect(
      dispute.prepared.filter(({ stage }) => stage === "enter_resolution"),
    ).toStrictEqual([]);
  }, 1_800_000);

  it("makes our reveal on the other thread in the same run when our entry, due first, does not build, and still raises the entry's alarm", async () => {
    const { scene, runner, a, b } = dispute;
    const entered = await placeOf(dispute, b.unit);
    expect(threadOf(entered.utxo).turn).toBe("ReadyForOneStep");
    // The operator's defender reveals: our reveal on its thread is due after
    // our entry on the successor's, so it is ranked after it.
    await revealAsDefender(
      scene,
      a.defender,
      await gameThread(scene, a.unit),
      a.trace,
    );
    const revealed = await placeOf(dispute, a.unit);
    expect(isTurn(threadOf(revealed.utxo), "AwaitingChallenger")).toBe(true);
    expect(
      validationTraceChallengerDeadlineMs(threadOf(entered.utxo)),
    ).toBeLessThan(
      validationTraceChallengerDeadlineMs(threadOf(revealed.utxo)),
    );
    const entry = validationTraceActionId({
      stage: "enter_resolution",
      consumes: outRefOf(entered.utxo),
    });
    const signed = dispute.prepared.length;
    stuck.add(entry);
    const result = await runner.runOnce(scene.failing).finally(() => {
      stuck.clear();
    });
    expect(dispute.prepared.slice(signed)).toStrictEqual([
      expect.objectContaining({
        stage: "reveal",
        consumes: outRefOf(revealed.utxo),
      }),
    ]);
    expect(result).toMatchObject({
      kind: "pending",
      alarms: [
        {
          actionId: entry,
          reason: expect.stringContaining(
            `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: the enter_resolution move on thread ${outRefOf(entered.utxo)} `,
          ),
        },
      ],
    });
    const runState = validationTraceRunState(result);
    expect(runState).toMatchObject({
      resumeOnObservation: true,
      namedState: { name: VALIDATION_TRACE_ONE_STEP_REQUIRED, alarm: true },
    });
    scene.h.emulator.awaitBlock();
    expect(outRefOf((await placeOf(dispute, a.unit)).utxo)).not.toBe(
      outRefOf(revealed.utxo),
    );
    expect(outRefOf((await placeOf(dispute, b.unit)).utxo)).toBe(
      outRefOf(entered.utxo),
    );
  }, 600_000);

  it("raises the wallet's alarm, not the one-step alarm, when our wallet cannot fund the entry", async () => {
    const { scene, runner, b } = dispute;
    const before = await placeOf(dispute, b.unit);
    await emptyProverWallet(scene);
    const result = await runner.runOnce(scene.failing);
    expect(reasonOf(result)).toContain(
      `${VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED}: the prover wallet cannot fund ${validationTraceActionId({ stage: "enter_resolution", consumes: outRefOf(before.utxo) })} on thread ${outRefOf(before.utxo)}`,
    );
    const runState = validationTraceRunState(result);
    expect("namedState" in runState && runState.namedState).toStrictEqual({
      name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
      alarm: true,
    });
    expect(outRefOf((await placeOf(dispute, b.unit)).utxo)).toBe(
      outRefOf(before.utxo),
    );
  }, 600_000);

  it("enters the resolution once the entry builds and our wallet is topped up", async () => {
    const { scene, runner, b } = dispute;
    await topUpProverWallet(scene, 5_000n * ADA);
    await playThreads({
      scene,
      runner,
      sides,
      until: async () => (await placeOf(dispute, b.unit)).holder !== "game",
      limit: 4,
    });
    expect(
      dispute.prepared.filter(({ stage }) => stage === "enter_resolution"),
    ).toHaveLength(1);
  }, 600_000);
});

describe("validation-trace dispute runner: our timeout and our close of a thread, when they do not build", () => {
  // Both defenders are silent.
  let dispute: TwoThreadCase;
  let sides: readonly Side[];

  beforeAll(async () => {
    dispute = await defendTwoThreads({ withRemoval: false });
    sides = [
      { ...dispute.a, silent: true },
      { ...dispute.b, silent: true },
    ];
    const deadlines = await Promise.all(
      [dispute.a, dispute.b].map(async ({ unit }) =>
        validationTraceDefenderDeadlineMs(
          threadOf((await placeOf(dispute, unit)).utxo),
        ),
      ),
    );
    const late = Math.max(...deadlines) + 1_000;
    expect(late).toBeLessThan(await caseExpiryOf(dispute));
    awaitLastSlotAtOrBefore(dispute.scene, late);
  }, 1_800_000);

  it("raises the move-unbuilt alarm when our timeout does not build, naming the thread, the case and the case's expiry, with the thread unmoved", async () => {
    const { a, b, caseUnit } = dispute;
    const threads = async () =>
      await Promise.all(
        [a, b].map(async ({ unit }) => (await placeOf(dispute, unit)).utxo),
      );
    const before = await threads();
    const result = await runUnbuilt(dispute, VALIDATION_TRACE_MOVE_UNBUILT);
    const named = before.find((utxo) =>
      reasonOf(result).includes(
        `the timeout move on thread ${outRefOf(utxo)} `,
      ),
    );
    expect(named).toBeDefined();
    // The defender whose clock ended first is timed out first.
    for (const utxo of before)
      expect(
        validationTraceDefenderDeadlineMs(threadOf(named!)),
      ).toBeLessThanOrEqual(validationTraceDefenderDeadlineMs(threadOf(utxo)));
    expectAlarm(result, {
      state: VALIDATION_TRACE_MOVE_UNBUILT,
      stage: "timeout",
      thread: named!,
      caseUnit,
      deadline: await caseExpiryOf(dispute),
    });
    expect((await threads()).map(outRefOf)).toStrictEqual(before.map(outRefOf));
    expect(
      dispute.prepared.filter(({ stage }) => stage === "timeout"),
    ).toStrictEqual([]);
  }, 600_000);

  it("raises the move-unbuilt alarm when our close of a thread we won does not build, naming the thread, the case and the case's expiry, with the thread unmoved", async () => {
    const { scene, runner, a, b, caseUnit } = dispute;
    const timedOut = async () =>
      await Promise.all(
        [a, b].map(async ({ unit }) => await placeOf(dispute, unit)),
      );
    await playThreads({
      scene,
      runner,
      sides,
      until: async () =>
        (await timedOut()).every(({ holder }) => holder === "timeout"),
      limit: 6,
    });
    const before = (await timedOut()).map(({ utxo }) => outRefOf(utxo));
    const result = await runUnbuilt(dispute, VALIDATION_TRACE_MOVE_UNBUILT);
    const named = before.find((outRef) =>
      reasonOf(result).includes(`the close_thread move on thread ${outRef} `),
    );
    expect(named).toBeDefined();
    const [thread] = await scene.h.proverLucid.utxosByOutRef([
      {
        txHash: named!.split("#")[0]!,
        outputIndex: Number(named!.split("#")[1]),
      },
    ]);
    expectAlarm(result, {
      state: VALIDATION_TRACE_MOVE_UNBUILT,
      stage: "close_thread",
      thread: thread!,
      caseUnit,
      deadline: await caseExpiryOf(dispute),
    });
    expect((await timedOut()).map(({ utxo }) => outRefOf(utxo))).toStrictEqual(
      before,
    );
    expect(
      dispute.prepared.filter(({ stage }) => stage === "close_thread"),
    ).toStrictEqual([]);
  }, 900_000);
});
