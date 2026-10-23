/** The validation-trace dispute runner at a staged builtin step (arm 31,
 * semantics group 7), on the emulator: the failing transaction runs a script
 * whose trace takes one staged step, the successor's thread disputes it, and
 * the runner stages the step's operands at our staging address before the
 * PrepareSelected move, plays the thread through the binder, the group's
 * arm chain and the settle to the award
 * (validation-trace-dispute-one-step-resolution-drive.ts), and spends the
 * staging outputs back once no thread needs them. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import {
  midgardCekStagingAddress,
  type MidgardCekStagingRun,
  planMidgardCekStaging,
} from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCoreStepPlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import {
  journaledStagings,
  VALIDATION_TRACE_STAGING,
} from "../../src/validation-trace-dispute/one-step-staging-journal.js";
import type { FraudProofWorkflowJournalEntry } from "../../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  playThreads,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";
import {
  STAGED_SHA2_SCRIPT,
  stagedScript,
  type StagedWitness,
} from "./validation-trace-dispute-staged-scripts.js";

/** The arm chain of a staged bytes step. */
export const STAGED_BYTES_ARMS = [
  "semanticRoots",
  "semanticResult",
  "semanticBudget",
  "stagedBytes",
] as const;

/** The staged witness of the step from `index` of our trace, if it is one. */
const stagedWitnessAt = async (
  material: ValidationTraceClaimMaterial,
  index: number,
): Promise<StagedWitness | undefined> => {
  const auxiliary = (await material.steps(index)).witnesses[index]?.auxiliary;
  return auxiliary?.kind === "cekCoreStep" &&
    auxiliary.step.witness.kind === "executeBuiltinStaged"
    ? auxiliary.step.witness
    : undefined;
};

/** The last step of our trace that is a staged builtin step. */
export const lastStagedStep = async (
  material: ValidationTraceClaimMaterial,
): Promise<number> => {
  for (let index = Number(material.stepCount) - 1; index >= 0; index -= 1)
    if ((await stagedWitnessAt(material, index)) !== undefined) return index;
  throw new Error("our trace has no staged builtin step");
};

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The journal entries a run of the runner returned. */
export const entriesOf = (
  result: FraudProofWorkflowRunResult,
): readonly FraudProofWorkflowJournalEntry[] =>
  "entries" in result ? result.entries : [];

/** The action ids of the staging actions the journal shows submitted. */
export const stagingIntents = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): readonly string[] => [
  ...new Set(
    entries.flatMap(({ event }) =>
      event.kind === "submission_intent" &&
      event.actionInput.schemaVersion === VALIDATION_TRACE_STAGING
        ? [event.actionId]
        : [],
    ),
  ),
];

/**
 * The drive of the successor's thread through the staged step of the
 * failing script `hex`, whose arm chain is `arms`, set up with the thread
 * standing at the PrepareSelected move's holder.
 */
export const stagedDrive = ({
  hex = STAGED_SHA2_SCRIPT,
  arms = STAGED_BYTES_ARMS,
}: {
  readonly hex?: string;
  readonly arms?: readonly (typeof STAGED_BYTES_ARMS)[number][];
} = {}) => {
  const drive = resolutionDrive({
    from: lastStagedStep,
    at: "prepare",
    path: ["prepare", "core", ...arms, "settle"],
    labels: [
      "PrepareSelected core",
      "VerifyCoreStep",
      ...arms.map((holder) => `VerifyArm ${holder}`),
      "Settle core",
    ],
    installs: {
      builtins: true,
      failingScript: stagedScript(hex),
      // The operator's thread disputes the same step, so both threads read
      // the same staging.
      operatorFrom: lastStagedStep,
    },
    planAt: async (dispute): Promise<ValidationTraceCoreStepPlan> => {
      const { material } = dispute;
      const stateIndex = await lastStagedStep(material);
      const planning = await planValidationTraceOneStep({
        material,
        stateIndex,
        challengerHighHash: hashMidgardValidationMachineState(
          (await material.steps(stateIndex)).states[stateIndex + 1]!,
        ).toString("hex"),
        builtins: true,
      });
      if (planning.kind !== "plan" || planning.plan.kind !== "core")
        throw new Error("the thread's step is not a CEK core step");
      return planning.plan;
    },
  });

  /** The latest journal the runner returned. */
  let latest: readonly FraudProofWorkflowJournalEntry[] = [];

  const placeOfB = async () => {
    const { scene, deployment, b } = drive.dispute();
    return await threadPlace(scene, deployment, b.unit);
  };

  return {
    ...drive,
    entries: () => latest,
    placeOfB,

    /** The runs our staging of the step stages, in order. */
    runs: async (): Promise<readonly MidgardCekStagingRun[]> => {
      const { material } = drive.dispute();
      const witness = await stagedWitnessAt(
        material,
        await lastStagedStep(material),
      );
      return planMidgardCekStaging(witness!.stagedOperands);
    },

    /** Our staging address. */
    stagingAddress: () => {
      const { lucid, signer } = {
        lucid: drive.dispute().scene.h.proverLucid,
        signer: drive.dispute().scene.h.proverSigner,
      };
      return midgardCekStagingAddress(
        lucid.config().network!,
        signer.paymentKeyHash,
      );
    },

    /** Plays the runner one block at a time until `until` holds over the
     * journal; the history of its runs. */
    play: async (
      until: (
        entries: readonly FraudProofWorkflowJournalEntry[],
      ) => Promise<boolean>,
      limit = 40,
    ): Promise<readonly string[]> => {
      const { scene, runner, a, b } = drive.dispute();
      const { history } = await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async (result) => {
          latest = entriesOf(result);
          return await until(latest);
        },
        limit,
      });
      return history;
    },

    /** The staging outputs the journal shows, and those it shows spent
     * back. */
    stagings: () => journaledStagings(latest),

    /** The block the transaction `hash` landed in. */
    blockOf: async (hash: string): Promise<number> => {
      const status = await drive
        .dispute()
        .scene.h.emulator.getTransactionStatus(hash);
      if (
        status.status !== "confirmed" ||
        status.confirmation.blockHeight === undefined
      )
        throw new Error(`transaction ${hash} has not landed`);
      return status.confirmation.blockHeight;
    },

    /** The out-refs the landed transaction `hash` reads, in ledger order. */
    readsOf: (hash: string): readonly string[] => {
      const references = CML.Transaction.from_cbor_hex(
        drive.dispute().scene.l1.signedCbors.get(hash)!,
      )
        .body()
        .reference_inputs();
      return Array.from({ length: references?.len() ?? 0 }, (_, index) => {
        const input = references!.get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      });
    },

    /** The live output at `outRef`, if any. */
    live: async (outRef: string): Promise<UTxO | undefined> => {
      const [txHash, index] = outRef.split("#");
      const [utxo] = await drive
        .dispute()
        .scene.h.proverLucid.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(index) },
        ]);
      return utxo !== undefined && outRefOf(utxo) === outRef ? utxo : undefined;
    },
  };
};
