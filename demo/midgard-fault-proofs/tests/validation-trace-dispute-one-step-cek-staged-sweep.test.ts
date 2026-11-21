/** The validation-trace dispute runner's spend back of a staged builtin
 * step's staging outputs (arm 31) while its threads play on, on the
 * emulator (support/validation-trace-dispute-one-step-staged-drive.ts), with
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: both
 * threads dispute the same step and read the same staging, and one plays on
 * to its close while the other, whose moves past the core the test does not
 * let build, stands mid-chain.
 *
 * The run of a staging output the provider does not show is staged again
 * once its staging is final, with no grace for provider lag, and each
 * PrepareSelected move
 * reads the new output. A fresh process started there asks first for what
 * the process that was not stopped asks for
 * (support/validation-trace-dispute-restart-probe.ts). The output no move
 * read is spent back while a thread is held mid-chain, and each output our
 * moves read is kept, by a process restarted after our move took that
 * thread to the settle, while that move is short of release depth. The
 * case's completion waits on no grace for a staging output the provider
 * does not show and on no spend back's finality; once the provider shows
 * it, it is taken back. Once those spend backs roll back after the ending,
 * the next runs spend each output back once more, by its first spend back
 * or one new one, and complete again. */
import * as SDK from "@al-ft/midgard-sdk/da";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "../src/validation-trace-dispute/actions.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { WorkflowActionUnbuiltError } from "../src/workflow/orchestrator.unbuilt-actions.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
  threadClosed,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  type Side,
  threadPlace,
  withLedgerParameters,
} from "./support/validation-trace-dispute-one-step-play.js";
import { stagedDrive } from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import {
  expectSameNextAfterRestart,
  recordingAsks,
} from "./support/validation-trace-dispute-restart-probe.js";
import { expectSpentBackAgainAfterRollback } from "./support/validation-trace-dispute-staging-left-for-sweep.js";

/** How many blocks under the tip a transaction is final. */
const RELEASE_DEPTH = 4;

/** A transaction size limit under which no thread move builds (the
 * smallest, an arm's, is about 1,600 bytes) and a spend back of one staging
 * output (about 230 bytes) does. */
const NO_THREAD_MOVE = 1_000;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH, withRemoval: true });
/** What each observation of the process that was not stopped asks for. */
const seen: string[] = [];

/** Ends the provider's blind spot. */
let hidden: (() => void) | undefined;
/** The staging output staged last, which the provider did not show. */
let unseen: string;
/** The staging outputs the PrepareSelected moves read. */
let read: readonly string[];
/** The thread that stands mid-chain once the other has closed. */
let rest: Side;
/** While set, no move of thread b from the core builds: the runner plays
 * thread a on in the same runs, and b stands mid-chain. */
let holdAtCore = false;

const ledgerHas = (label: string) =>
  onLedger(drive.dispute().scene, parseOwnOutRef(label));

/** The moves the runner landed whose thread output pays to `holder`. */
const movesTo = (holder: ValidationTraceOneStepHolder) => {
  const { scene, deployment, prepared } = drive.dispute();
  const address = validationTraceOneStepHolder(deployment, holder).address;
  return prepared.filter(({ stage, hash, cbor }) => {
    if (stage !== "resolve" || !scene.l1.signedCbors.has(hash)) return false;
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    return Array.from(
      { length: outputs.len() },
      (_, index) => coreToTxOutput(outputs.get(index)).address,
    ).includes(address);
  });
};

/** Where `side`'s thread stands. */
const placeOf = async (side: Side) => {
  const { scene, deployment } = drive.dispute();
  return (await threadPlace(scene, deployment, side.unit))?.holder;
};

/** Where both threads stand. */
const places = async () => {
  const { scene, deployment, a, b } = drive.dispute();
  return await Promise.all(
    [a.unit, b.unit].map(
      async (unit) => (await threadPlace(scene, deployment, unit))?.holder,
    ),
  );
};

/** Plays `runs` runs while no thread move builds. */
const holdFor = async (runs: number) => {
  let played = 0;
  await withLedgerParameters(
    drive.dispute().scene,
    { maxTxSize: NO_THREAD_MOVE },
    async () => await drive.play(async () => (played += 1) >= runs, runs),
  );
};

/** The spend back of `label` to our wallet that landed. */
const expectSpentBack = async (label: string) => {
  const { scene } = drive.dispute();
  const reclaim = await drive.spenderOf(label);
  expect(reclaim, label).toBeDefined();
  const body = CML.Transaction.from_cbor_hex(
    scene.l1.signedCbors.get(reclaim!)!,
  ).body();
  expect(body.inputs().len()).toBe(1);
  expect(body.outputs().len()).toBe(1);
  expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
    scene.h.proverSigner.address,
  );
  return reclaim!;
};

describe("validation-trace dispute runner: spending back a staged step's staging outputs while its threads play on", () => {
  beforeAll(async () => {
    await drive.setUp();
    drive.restart({
      wrap: (inner) => {
        const adapter = recordingAsks(seen)(inner);
        return {
          ...adapter,
          preflight: async (context) => {
            if (
              holdAtCore &&
              context.action.input.schemaVersion ===
                VALIDATION_TRACE_DISPUTE_ACTION
            ) {
              const { scene, deployment, b } = drive.dispute();
              const { consumes } = parseValidationTraceAction(context.action);
              const [thread] =
                consumes === null
                  ? []
                  : await scene.h.proverLucid.utxosByOutRef([
                      parseOwnOutRef(consumes),
                    ]);
              if (
                thread?.assets[b.unit] === 1n &&
                thread.address ===
                  validationTraceOneStepHolder(deployment, "core").address
              )
                throw new WorkflowActionUnbuiltError(
                  context.action.actionId,
                  "thread b stays at the core",
                );
            }
            return await adapter.preflight(context);
          },
        };
      },
    });
  }, 1_800_000);

  afterAll(() => {
    hidden?.();
    drive.dispute()?.recorder?.restore();
  });

  it("stages the run of a staging output the provider does not show again once its staging is final at release depth, taking no grace for provider lag", async () => {
    const runs = await drive.runs();
    expect(runs.length).toBeGreaterThan(1);
    await drive.play(
      async () => (await drive.stagings()).length >= runs.length,
    );
    unseen = (await drive.stagings()).at(-1)!.outRef;
    hidden = hideFromProvider(drive.dispute().scene.h.proverLucid, unseen);
    // The staging is followed until it is final; the next run stages again.
    await drive.play(
      async () => (await drive.stagings()).length > runs.length,
      RELEASE_DEPTH + 3,
    );
    expect(await drive.stagings()).toHaveLength(runs.length + 1);
    expect(ledgerHas(unseen)).toBe(true);
  }, 1_200_000);

  it("plays each PrepareSelected move reading the new output", async () => {
    holdAtCore = true;
    let played: readonly string[];
    try {
      played = await drive.play(async () => {
        const at = await places();
        return !at.includes("prepare") && at.includes(undefined);
      }, 40);
    } finally {
      holdAtCore = false;
    }
    const runs = await drive.runs();
    const stagings = await drive.stagings();
    expect(
      stagings,
      [
        ...stagings.map(
          ({ outRef, datumCbor }) => `${outRef} ${datumCbor.slice(-16)}`,
        ),
        ...played,
      ].join("\n"),
    ).toHaveLength(runs.length + 1);
    const again = stagings.at(-1)!;
    expect(again.datumCbor).toBe(
      stagings.find((staging) => staging.outRef === unseen)!.datumCbor,
    );
    read = stagings
      .filter(({ outRef: label }) => label !== unseen)
      .map(({ outRef: label }) => label)
      .sort();
    const selected = movesTo("core");
    expect(selected).toHaveLength(2);
    for (const { hash } of selected) {
      expect(drive.readsOf(hash)).toEqual(expect.arrayContaining([...read]));
      expect(drive.readsOf(hash)).not.toContain(unseen);
    }
    // Thread a played on to its award and close; thread b stands at the
    // core.
    expect(await places(), played.join("\n")).toStrictEqual([
      undefined,
      "core",
    ]);
    rest = drive.dispute().b;
  }, 1_200_000);

  it("asks first, after a restart, for what the process that was not stopped asks for, once one thread closed and the other stands mid-chain", async () => {
    hidden?.();
    hidden = undefined;
    const { scene, runner } = drive.dispute();
    expect(ledgerHas(unseen)).toBe(true);
    // The process that was not stopped takes what it asks for; no thread
    // move builds, so the thread stays where it stands.
    await withLedgerParameters(
      scene,
      { maxTxSize: NO_THREAD_MOVE },
      async () =>
        await expectSameNextAfterRestart({
          processOver: drive.processOver,
          runner,
          seen,
          block: scene.failing,
          opened: true,
        }),
    );
    expect(await placeOf(rest)).toBe("core");
  }, 1_200_000);

  it("spends the staging output no move read back to our wallet while a thread is held mid-chain, keeping each output its moves read", async () => {
    hidden?.();
    hidden = undefined;
    expect(await placeOf(rest)).toBe("core");
    await holdFor(6);
    expect(ledgerHas(unseen)).toBe(false);
    await expectSpentBack(unseen);
    for (const label of read) expect(ledgerHas(label), label).toBe(true);
    expect(await placeOf(rest)).toBe("core");
  }, 1_200_000);

  it("keeps each staging output our moves read, restarted after our move took the thread to the settle, while that move is short of release depth", async () => {
    const { scene, deployment } = drive.dispute();
    // A process of ours plays the thread to the settle, and no further.
    // Only our key moves a thread past the core, so its move to the settle
    // is ours, which reads the outputs until it is final at release depth.
    const settle = validationTraceOneStepHolder(deployment, "settle").address;
    drive.restart({
      wrap: (adapter) => ({
        ...adapter,
        preflight: async (context) => {
          if (
            context.action.input.schemaVersion ===
            VALIDATION_TRACE_DISPUTE_ACTION
          ) {
            const { consumes } = parseValidationTraceAction(context.action);
            const [thread] =
              consumes === null
                ? []
                : await scene.h.proverLucid.utxosByOutRef([
                    parseOwnOutRef(consumes),
                  ]);
            if (thread?.address === settle)
              throw new WorkflowActionUnbuiltError(
                context.action.actionId,
                "the thread stays at the settle",
              );
          }
          return await adapter.preflight(context);
        },
      }),
    });
    const ours = drive.dispute().prepared.length;
    await playThreads({
      scene,
      runner: drive.dispute().runner,
      sides: [rest],
      until: async () => (await placeOf(rest)) === "settle",
      limit: 12,
    });
    expect(drive.dispute().prepared.length).toBeGreaterThan(ours);
    // The process stops; the next one knows nothing of that move but what
    // the chain shows.
    drive.restart();
    for (const label of read) expect(ledgerHas(label), label).toBe(true);

    await holdFor(RELEASE_DEPTH - 2);
    for (const label of read) expect(ledgerHas(label), label).toBe(true);
    expect(await placeOf(rest)).toBe("settle");
  }, 1_200_000);

  it("spends each back once the threads move on, and ends waiting on no grace for a staging output the provider does not show and on no spend back's finality; once those spend backs roll back after the ending, the next runs spend each back once more and complete again", async () => {
    const { scene, a, b } = drive.dispute();
    const [kept, ...others] = read;
    const outputs: readonly UTxO[] = await scene.h.proverLucid.utxosByOutRef(
      read.map(parseOwnOutRef),
    );
    expect(outputs).toHaveLength(read.length);
    // The thread stands at the settle while the provider does not show an
    // output its moves read; its move from the settle reads none.
    hidden = hideFromProvider(scene.h.proverLucid, kept!);
    await drive.play(
      async () =>
        (await threadClosed(drive.dispute(), a.unit)) &&
        (await threadClosed(drive.dispute(), b.unit)),
      40,
    );
    const awards = movesTo("award");
    expect(awards).toHaveLength(2);
    const stagings = (await drive.stagings()).map(({ outRef: label }) => label);
    for (const { hash } of awards)
      for (const label of stagings)
        expect(drive.readsOf(hash), label).not.toContain(label);
    // The case closes from its defense deadline on.
    const deadline = validationTraceDefenseDeadlineMs(
      Data.from((await liveCase(drive.dispute())).datum!, SDK.Case),
    );
    if (scene.h.emulator.now() <= deadline)
      awaitLastSlotAtOrBefore(scene, deadline + 1_000);
    // The removal dates its validity window from the application clock,
    // which on the emulator is the emulator's.
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => scene.h.emulator.now());
    try {
      const history = await drive.play(
        async (result) =>
          result.kind === "terminal_included" || result.kind === "completed",
        40,
      );
      expect(
        history.some((run) =>
          run.includes(VALIDATION_TRACE_STAGING_OUTSTANDING),
        ),
        history.join("\n"),
      ).toBe(false);
      for (const label of others) {
        expect(ledgerHas(label), label).toBe(false);
        await expectSpentBack(label);
      }
      hidden();
      hidden = undefined;
      const { runner } = drive.dispute();
      const after: string[] = [];
      for (let run = 0; run < 12 && ledgerHas(kept!); run += 1) {
        const result = await runner.runOnce(scene.failing);
        after.push(
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        );
      }
      expect(ledgerHas(kept!), after.join("\n")).toBe(false);
      // The process may have seen each spend back land final, so it may
      // follow none of them: each output may be spent back anew.
      await expectSpentBackAgainAfterRollback({
        scene,
        outputs,
        runOnce: async () => await runner.runOnce(scene.failing),
        describe: (result) =>
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        ended: (result) =>
          result.kind === "terminal_included" || result.kind === "completed",
        renewed: read,
      });
    } finally {
      clock.mockRestore();
    }
  }, 1_800_000);
});
