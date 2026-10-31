/** The validation-trace dispute runner's spend back of a staged builtin
 * step's staging outputs (arm 31) while its threads play on, on the
 * emulator (support/validation-trace-dispute-one-step-staged-drive.ts), with
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: both
 * threads dispute the same step and read the same staging, and one plays on
 * to its close while the other stands mid-chain.
 *
 * A staging output the provider does not show yet is waited on while its
 * confirmation is inside the provider's grace, and its run is staged again
 * only past it. The output no move read is spent back while a thread is held
 * mid-chain, and each output our moves read is kept while another party's
 * move holds that thread at the settle. The case's completion waits while a
 * staging output the provider does not show may still be live, and until its
 * spend back is final at release depth. */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_PROVIDER_GRACE_MS } from "../src/validation-trace-dispute/adapter.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { WorkflowActionUnbuiltError } from "../src/workflow/orchestrator.unbuilt-actions.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
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
import {
  stagedDrive,
  stagingIntents,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/** How many blocks under the tip a transaction is final. */
const RELEASE_DEPTH = 4;

/** A transaction size limit under which no thread move builds (the
 * smallest, an arm's, is about 1,600 bytes) and a spend back of one staging
 * output (about 230 bytes) does. */
const NO_THREAD_MOVE = 1_000;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH, withRemoval: true });

/** Ends the provider's blind spot. */
let hidden: (() => void) | undefined;
/** The staging output staged last, which the provider did not show. */
let unseen: string;
/** The staging outputs the PrepareSelected moves read. */
let read: readonly string[];
/** The thread that stands mid-chain once the other has closed. */
let rest: Side;

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
  beforeAll(drive.setUp, 1_800_000);

  afterAll(() => {
    hidden?.();
    drive.dispute()?.recorder?.restore();
  });

  it("waits on a staging output the provider does not show yet while its confirmation is inside the provider's grace, staging its run no second time", async () => {
    const runs = await drive.runs();
    expect(runs.length).toBeGreaterThan(1);
    await drive.play(async () => drive.stagings().length >= runs.length);
    unseen = drive.stagings().at(-1)!.outRef;
    hidden = hideFromProvider(drive.dispute().scene.h.proverLucid, unseen);
    let played = 0;
    await drive.play(async () => (played += 1) >= 5, 5);
    expect(stagingIntents(drive.entries())).toHaveLength(runs.length);
    expect(await places()).toStrictEqual(["prepare", "prepare"]);
    expect(ledgerHas(unseen)).toBe(true);
  }, 1_200_000);

  it("stages that run again once the grace has passed, each PrepareSelected move reading the new output", async () => {
    const { scene } = drive.dispute();
    scene.h.emulator.awaitSlot(VALIDATION_TRACE_PROVIDER_GRACE_MS / 1_000);
    await drive.play(async () => !(await places()).includes("prepare"), 40);
    const runs = await drive.runs();
    expect(stagingIntents(drive.entries())).toHaveLength(runs.length + 1);
    const stagings = drive.stagings();
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
    // One thread played on to its award and close; the other stands at the
    // core.
    const at = await places();
    expect([...at].sort()).toStrictEqual(["core", undefined]);
    const { a, b } = drive.dispute();
    rest = at[0] === "core" ? a : b;
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

  it("keeps each staging output our moves read while another party's move holds the thread at the settle, past release depth", async () => {
    const { scene, deployment, runner } = drive.dispute();
    // Another party with our journal so far plays the thread to the settle,
    // and no further.
    const [first] = drive.entries();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    for (const entry of await runner.journal.load(first!.workflowId))
      await journal.append(entry, entry.sequence);
    const settle = validationTraceOneStepHolder(deployment, "settle").address;
    const other = makeRunner(scene, {
      deployment,
      journal,
      signedRecovery: emulatorSignedTransactionRecovery(
        scene.h.emulator,
        RELEASE_DEPTH,
      ),
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
      runner: other,
      sides: [rest],
      until: async () => (await placeOf(rest)) === "settle",
      limit: 12,
    });
    expect(drive.dispute().prepared.length).toBe(ours);
    for (const label of read) expect(ledgerHas(label), label).toBe(true);

    await holdFor(RELEASE_DEPTH + 2);
    for (const label of read) expect(ledgerHas(label), label).toBe(true);
    expect(await placeOf(rest)).toBe("settle");
  }, 1_200_000);

  it("spends each back once the threads move on, and holds the case's completion while a staging output the provider does not show may still be live, and until its spend back is final at release depth", async () => {
    const { scene, a, b } = drive.dispute();
    const [kept, ...others] = read;
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
    const stagings = drive.stagings().map(({ outRef: label }) => label);
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
      const held = `${VALIDATION_TRACE_STAGING_OUTSTANDING}: ${kept!} is not yet visible to the provider`;
      await drive.play(async (_, result) => {
        expect(["terminal_included", "completed"]).not.toContain(result.kind);
        return result.kind === "pending" && result.reason === held;
      }, 40);
      for (const label of others) {
        expect(ledgerHas(label), label).toBe(false);
        await expectSpentBack(label);
      }
      let runs = 0;
      await drive.play(async (_, result) => {
        expect(result.kind === "pending" && result.reason).toBe(held);
        return (runs += 1) >= 3;
      }, 3);
      expect(ledgerHas(kept!)).toBe(true);
      hidden();
      hidden = undefined;

      let ended: FraudProofWorkflowRunResult | undefined;
      let endedAt = 0;
      await drive.play(async (_, result) => {
        if (result.kind !== "terminal_included" && result.kind !== "completed")
          return false;
        ended = result;
        endedAt = scene.h.emulator.blockHeight - 1;
        return true;
      }, 20);
      expect(ended).toBeDefined();
      const reclaim = await expectSpentBack(kept!);
      expect(endedAt - (await drive.blockOf(reclaim))).toBeGreaterThanOrEqual(
        RELEASE_DEPTH,
      );
    } finally {
      clock.mockRestore();
    }
  }, 1_800_000);
});
