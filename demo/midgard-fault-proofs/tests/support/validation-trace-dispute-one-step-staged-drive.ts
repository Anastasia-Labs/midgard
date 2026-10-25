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
import {
  CML,
  coreToTxOutput,
  type Emulator,
  type UTxO,
} from "@lucid-evolution/lucid";

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

/** The arm chain of a staged integer step. */
export const STAGED_INTEGER_ARMS = [
  "semanticRoots",
  "semanticResult",
  "semanticBudget",
  "stagedInteger",
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

/** The transactions of `signedCbors` the ledger of `emulator` holds, in
 * the order they landed. */
const landed = async (
  signedCbors: ReadonlyMap<string, string>,
  emulator: Emulator,
) => {
  const found: { hash: string; body: CML.TransactionBody; block: number }[] =
    [];
  for (const [hash, cbor] of signedCbors) {
    const status = await emulator.getTransactionStatus(hash);
    if (status.status === "confirmed")
      found.push({
        hash,
        body: CML.Transaction.from_cbor_hex(cbor).body(),
        block: status.confirmation.blockHeight ?? 0,
      });
  }
  return found.sort((x, y) => x.block - y.block);
};

/** The landed transaction of `signedCbors` that spent `outRef`, by the
 * ledger of `emulator`; undefined while none has. */
export const landedSpenderOf = async (
  signedCbors: ReadonlyMap<string, string>,
  emulator: Emulator,
  outRef: string,
): Promise<string | undefined> =>
  (await landed(signedCbors, emulator)).find(({ body }) => {
    const inputs = body.inputs();
    return Array.from({ length: inputs.len() }, (_, index) =>
      inputs.get(index),
    ).some(
      (input) =>
        `${input.transaction_id().to_hex()}#${input.index().toString()}` ===
        outRef,
    );
  })?.hash;

/**
 * The drive of the successor's thread through the staged step of the
 * failing script `hex`, run under `exUnits` when given, whose arm chain is
 * `arms`, set up with the thread standing at the PrepareSelected move's
 * holder; a move is final at release depth `releaseDepth` blocks under the
 * tip when given, else at the tip; with `withRemoval`, a case the challenger
 * won removes the block and completes.
 */
export const stagedDrive = ({
  hex = STAGED_SHA2_SCRIPT,
  exUnits,
  arms = STAGED_BYTES_ARMS,
  releaseDepth,
  withRemoval = false,
}: {
  readonly hex?: string;
  readonly exUnits?: readonly [bigint, bigint];
  readonly arms?: typeof STAGED_BYTES_ARMS | typeof STAGED_INTEGER_ARMS;
  readonly releaseDepth?: number;
  readonly withRemoval?: boolean;
} = {}) => {
  const drive = resolutionDrive({
    withRemoval,
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
      ...(exUnits === undefined ? {} : { failingExUnits: exUnits }),
      ...(releaseDepth === undefined ? {} : { releaseDepth }),
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

  const witness = async (): Promise<StagedWitness> => {
    const { material } = drive.dispute();
    return (await stagedWitnessAt(material, await lastStagedStep(material)))!;
  };

  return {
    ...drive,
    entries: () => latest,
    placeOfB,

    /** The witness of the staged step. */
    witness,

    /** The runs our staging of the step stages, in order. */
    runs: async (): Promise<readonly MidgardCekStagingRun[]> =>
      planMidgardCekStaging((await witness()).stagedOperands),

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
     * journal and the run; the history of its runs. */
    play: async (
      until: (
        entries: readonly FraudProofWorkflowJournalEntry[],
        result: FraudProofWorkflowRunResult,
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
          return await until(latest, result);
        },
        limit,
      });
      return history;
    },

    /** The staging outputs the journal shows. */
    stagings: () => journaledStagings(latest).stagings,

    /** The staging outputs the landed transactions left at our staging
     * address, in the order they landed: what the ledger shows staged. */
    stagedOnL1: async (): Promise<
      readonly Readonly<{ outRef: string; datumCbor: string | undefined }>[]
    > => {
      const { scene } = drive.dispute();
      const address = midgardCekStagingAddress(
        scene.h.proverLucid.config().network!,
        scene.h.proverSigner.paymentKeyHash,
      );
      return (await landed(scene.l1.signedCbors, scene.h.emulator)).flatMap(
        ({ hash, body }) => {
          const outputs = body.outputs();
          return Array.from({ length: outputs.len() }, (_, index) => ({
            output: outputs.get(index),
            index,
          }))
            .filter(({ output }) => coreToTxOutput(output).address === address)
            .map(({ output, index }) => ({
              outRef: `${hash}#${index.toString()}`,
              datumCbor: output.datum()?.as_datum()?.to_cbor_hex(),
            }));
        },
      );
    },

    /** The landed transaction that spent `outRef`, if any. */
    spenderOf: async (outRef: string): Promise<string | undefined> => {
      const { scene } = drive.dispute();
      return await landedSpenderOf(
        scene.l1.signedCbors,
        scene.h.emulator,
        outRef,
      );
    },

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
