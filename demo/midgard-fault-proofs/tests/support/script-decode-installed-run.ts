/**
 * One installed-application objective driven to conviction: the committed
 * block, the watcher's classification of it, then watcher turns until the
 * proof token is minted. A turn either pends or pauses until a fresh
 * observation; anything else fails the run.
 *
 * The calling test mocks the manifest binding and the L1 observation port
 * with `hooks` (both are hoisted into the test file); the port's signed
 * recovery is the emulator's, as the watcher's follower gives it.
 */
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION } from "../../src/output-reference-script-decoding/authenticated-workflow.js";
import { WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION } from "../../src/witness-script-decoding/workflow.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../../src/workflow/complete-replay.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { installedWorkflowEmulatorClock } from "./installed-workflow.advance-emulator-observation.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { installApplicationClock } from "./script-decode-installed-host.js";
import {
  commitScriptDecodeBlock,
  type InstalledFamily,
  installScriptDecodeApplication,
} from "./script-decode-installed-workflow.js";

const MAX_TURNS = 400;

/** The witness family (kind 0), as the watcher admits it. */
export const INSTALLED_WITNESS = {
  definition: WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION as never,
  replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  contractOptions: {
    realWitnessScriptDecoding: true,
    alwaysFraudProofCatalogue: true,
  },
  violationId: "witness-plutus-script-undecodable",
} as const;

/** The output-reference family (kind 1), as the watcher admits it. */
export const INSTALLED_OUTPUT_REFERENCE = {
  definition: OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION as never,
  replayer: OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  contractOptions: {
    realOutputReferenceScriptDecoding: true,
    alwaysFraudProofCatalogue: true,
  },
  violationId: "output-reference-plutus-script-undecodable",
} as const;

export type InstalledHooks = {
  binding: unknown;
  authority: unknown;
  recovery: unknown;
};

export type InstalledScenario = Readonly<{
  definition: InstalledFamily;
  replayer: Parameters<typeof installScriptDecodeApplication>[0]["replayer"];
  contractOptions: NonNullable<
    Parameters<typeof makeFaultProofEmulatorHarness>[0]
  >["contractOptions"];
  referenceScriptAuthTimelockMs?: number;
  nativeTx: () => Parameters<typeof commitScriptDecodeBlock>[0]["nativeTx"];
  violationId: string;
}>;

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type Application = Awaited<ReturnType<typeof installScriptDecodeApplication>>;
type TurnResult = Awaited<ReturnType<Application["turn"]>>;

/** What a test sees between two turns, and may act on. */
export type InstalledTurnContext = Readonly<{
  harness: Harness;
  application: Application;
  clock: ReturnType<typeof installApplicationClock>;
  turn: number;
  result: TurnResult;
}>;

export type InstalledRunOptions = Readonly<{
  /**
   * Confirmation polls after which the process stops: a run that spins
   * inside one turn fails instead of hanging.
   */
  maxPolls?: number;
  /**
   * Turns the run may take to convict (at most the harness budget): a run
   * that spins across turns fails fast instead of after the whole budget.
   */
  maxTurns?: number;
  /** Runs once before the first turn; may return its cleanup. */
  beforeTurns?: (
    context: Omit<InstalledTurnContext, "result" | "turn">,
  ) => Promise<(() => void) | void> | (() => void) | void;
  /** Runs after each turn that did not convict; "stop" ends the run there. */
  afterTurn?: (
    context: InstalledTurnContext,
  ) => Promise<"stop" | void> | "stop" | void;
  /**
   * A turn that throws is a stopped process: restart it (the default
   * fails the run).
   */
  onStopped?: (
    error: unknown,
    context: Omit<InstalledTurnContext, "result">,
  ) => Promise<void>;
}>;

/** The record's footprint: the UTxOs the record left on chain. */
export type InstalledRecordFootprint = Readonly<{
  /** Outputs at the script-decode validator (runs and records). */
  recordOutputs: number;
  /** Datum-bearing outputs at the prover's address (staging chunks). */
  stagingOutputs: number;
}>;

const isStaging = (utxo: UTxO): boolean =>
  utxo.scriptRef == null && (utxo.datum != null || utxo.datumHash != null);

export const runInstalled = async (
  scenario: InstalledScenario,
  hooks: InstalledHooks,
  options: InstalledRunOptions = {},
) => {
  const recorder = recordCrossBlockRawEmulator();
  let clock: ReturnType<typeof installApplicationClock> | undefined;
  let close: (() => Promise<void>) | undefined;
  let cleanup: (() => void) | undefined;
  try {
    hooks.authority = recorder.authority;
    const harness = await makeFaultProofEmulatorHarness({
      ...(scenario.referenceScriptAuthTimelockMs === undefined
        ? {}
        : {
            referenceScriptAuthTimelockMs:
              scenario.referenceScriptAuthTimelockMs,
          }),
      contractOptions: scenario.contractOptions,
    });
    hooks.recovery = emulatorSignedTransactionRecovery(harness.emulator);
    const committed = await commitScriptDecodeBlock({
      harness,
      definition: scenario.definition,
      nativeTx: scenario.nativeTx(),
    });
    const workflowClock = installedWorkflowEmulatorClock(harness.emulator);
    workflowClock.awaitReleaseDepth();
    clock = installApplicationClock(harness.emulator);
    const application = await installScriptDecodeApplication({
      harness,
      definition: scenario.definition,
      replayer: scenario.replayer,
      committed,
      recorder,
      setBinding: (binding) => {
        hooks.binding = binding;
      },
    });
    close = application.close;
    expect(application.decision).toMatchObject({
      decision: "fault_detected",
      category: scenario.definition.category,
      violationId: scenario.violationId,
    });
    const categoryId =
      harness.catalogue.categories[scenario.definition.category]!.categoryId;
    const convictions = async () =>
      await harness.proverLucid.utxosAtWithUnit(
        harness.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          harness.contracts.fraudProof.policyId,
          categoryId + committed.headerHash,
        ),
      );
    // Conviction is the proof token. Removal after it is not driven here:
    // under the funding reservation a bond-backed slash needs the finalized
    // manifest's slash authority, which this legacy-deployment harness lacks.
    clock.state.crashAfterPolls = options.maxPolls;
    cleanup =
      (await options.beforeTurns?.({ harness, application, clock })) ??
      undefined;
    const reasons: string[] = [];
    let turns = 0;
    let convicted = false;
    for (;;) {
      let result: TurnResult | undefined;
      let stopped: { readonly error: unknown } | undefined;
      try {
        result = await application.turn();
      } catch (error) {
        stopped = { error };
      }
      turns += 1;
      if ((await convictions()).length > 0) {
        convicted = true;
        break;
      }
      expect(
        turns,
        `conviction within the turn budget (last: ${reasons.at(-1) ?? "none"})`,
      ).toBeLessThan(Math.min(options.maxTurns ?? MAX_TURNS, MAX_TURNS));
      if (stopped !== undefined) {
        if (options.onStopped === undefined) throw stopped.error;
        await options.onStopped(stopped.error, {
          harness,
          application,
          clock,
          turn: turns,
        });
        continue;
      }
      // Only a run that resumes by itself on a fresh observation continues.
      expect(
        result!.kind === "pending" ||
          (result!.kind === "stalled" && result!.resumeOnObservation === true),
        JSON.stringify(result!.reason ?? result!.kind),
      ).toBe(true);
      if (result!.reason !== undefined) reasons.push(result!.reason);
      const next = await options.afterTurn?.({
        harness,
        application,
        clock,
        turn: turns,
        result: result!,
      });
      if (next === "stop") break;
      await workflowClock.advance(result!);
    }
    if (convicted) expect(await convictions()).toHaveLength(1);
    const deployment = scriptDecodeDeploymentOf(harness.emulator);
    const footprint: InstalledRecordFootprint = {
      recordOutputs: (
        await harness.proverLucid.utxosAt(
          deployment.validator.spendingScriptAddress,
        )
      ).length,
      stagingOutputs: (
        await harness.proverLucid.utxosAt(harness.proverSigner.address)
      ).filter(isStaging).length,
    };
    return {
      convicted,
      turns,
      polls: clock.state.polls,
      reasons,
      footprint,
    } as const;
  } finally {
    cleanup?.();
    clock?.restore();
    recorder.restore();
    await close?.();
  }
};

/** The prover wallet's datum-bearing staging outputs, as a test sees them. */
export const stagingOutputsOf = async (harness: Harness) =>
  (await harness.proverLucid.utxosAt(harness.proverSigner.address)).filter(
    isStaging,
  );
