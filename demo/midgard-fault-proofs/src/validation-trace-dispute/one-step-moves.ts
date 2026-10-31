/** The challenger's moves through the one-step resolution of a thread of
 * ours, made now from the thread's live state: enter the resolution from the
 * game when the thread is ReadyForOneStep on our turn, move the thread on
 * from each holder to the next (one-step-builders.ts), and close the thread
 * the award holds into our case with its defender's lost-defense receipt.
 * The thread is read by the out-ref the action names and checked as ours:
 * our case's token for one of its recorded defenders, at the holder its
 * address names, with a datum that names us its prover. Each move carries the
 * funding permit's move authority and names the output it leaves. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type {
  ValidationTraceCaseHistory,
  ValidationTraceRecordedThread,
} from "./case-history.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  buildValidationTraceGameMove,
  buildValidationTraceThreadClose,
  challengerEnteredResolution,
  validationTraceMoveWindow,
} from "./move-builders.js";
import { completeValidationTraceMove } from "./move-fee.js";
import {
  captureMove,
  liveThread,
  type ValidationTraceMoveInput,
} from "./moves.js";
import {
  buildValidationTraceHop,
  validationTraceHop,
} from "./one-step-builders.js";
import { isValidationTraceContextItemChainHolder } from "./one-step-context-item-holders.js";
import {
  contextItemReads,
  redeemerItemCarried,
} from "./one-step-context-item-hops.js";
import {
  type ValidationTraceOneStepContract,
  validationTraceOneStepDrives,
  validationTraceOneStepHolder,
  validationTraceOneStepHolderAt,
} from "./one-step-holders.js";
import { phaseYieldListReads, phaseYieldReads } from "./one-step-phase.js";
import { planValidationTraceThread } from "./one-step-plan.js";
import { planned, resolvingPlan } from "./one-step-resolving-plan.js";
import {
  isScriptSourcesRedeemerOwnHolder,
  scriptSourcesRedeemerReads,
} from "./one-step-script-sources-redeemer.js";
import { validationTraceSelectionReads } from "./one-step-selection-reads.js";
import {
  stagingOutputs,
  type ValidationTraceStagingLookup,
} from "./one-step-staged.js";
import { validationTraceOneStepProver } from "./thread-observation.js";
import { isThreadMaterial } from "./threads.js";
import {
  type CapturedValidationTraceStage,
  caseConfigUtxo,
  ours,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";

type MaterialInput = ValidationTraceMoveInput & {
  readonly material: ValidationTraceClaimMaterial | undefined;
  /** Our published copy of a field too long to observe inline. */
  readonly observeCopy?: () => Promise<UTxO | undefined>;
  /** Where we staged a staged step's operands. */
  readonly staging?: ValidationTraceStagingLookup;
};

/** EnterResolution: our thread ReadyForOneStep, on our turn, now. */
export const captureValidationTraceEnterResolution = async (
  input: MaterialInput,
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment, material } = input;
  const thread = await liveThread(input, "game");
  if (thread.thread.turn !== "ReadyForOneStep")
    throw new WorkflowActionChangedError(
      `validation-trace thread ${thread.outRef} is not ReadyForOneStep`,
    );
  if (!isThreadMaterial(thread.thread, material))
    throw new Error(
      "validation-trace one step has no trace matching its thread's challenger root",
    );
  // Only a step the runner drives to its award enters the resolution.
  planned(
    await planValidationTraceThread(
      material,
      thread.thread,
      validationTraceOneStepDrives(deployment),
    ),
  );
  const boundary = validationTraceOneStepHolder(deployment, "boundary");
  signer.selectWallet(lucid);
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const next = challengerEnteredResolution({
    thread: thread.thread,
    lower: BigInt(validFrom),
    upper: BigInt(validTo - 1),
  });
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "enter_resolution",
    referenced: [deployment.references.game],
    build: (fee) =>
      buildValidationTraceGameMove({
        lucid,
        deployment,
        thread: thread.utxo,
        action: { kind: "EnterResolution" },
        successor: {
          address: boundary.address,
          datum: Data.to(
            { fraud_prover: signer.paymentKeyHash, data: next },
            SDK.CaseThreadDatum,
          ),
        },
        fee,
        validFrom,
        validTo,
        signer: signer.paymentKeyHash,
      }),
  });
  return await captureMove({
    input,
    stage: "enter_resolution",
    unsigned,
    thread,
    defenderVkh: thread.thread.header.defender_vkh,
    slots: [{ address: boundary.address, unit: thread.unit }],
  });
};

/**
 * The thread of ours at `threadOutRef` past the game: at a one-step holder
 * (the boundary only when `boundary`), carrying our case's token for one of
 * its `defenders`, with a datum that names us its prover; or changed.
 */
const resolvingThread = async (
  input: ValidationTraceMoveInput & {
    readonly defenders: readonly ValidationTraceRecordedThread[];
  },
): Promise<
  Readonly<{
    utxo: UTxO;
    unit: string;
    defenderVkh: string;
    holder: ValidationTraceOneStepContract;
    /** At the boundary, the case thread its datum carries. */
    caseThread: SDK.CaseThread | undefined;
  }>
> => {
  const { lucid, signer, deployment, threadOutRef, caseUnit } = input;
  const utxo = await ours(lucid, threadOutRef, "thread");
  const holder = validationTraceOneStepHolderAt(deployment, utxo.address);
  const recorded = input.defenders.find(({ unit }) => utxo.assets[unit] === 1n);
  // At the boundary the thread keeps its case thread, read as at the game.
  const caseThread =
    holder?.holder === "boundary"
      ? (await liveThread(input, "boundary")).thread
      : undefined;
  const prover =
    caseThread === undefined
      ? validationTraceOneStepProver(utxo)
      : signer.paymentKeyHash;
  if (
    holder === undefined ||
    recorded === undefined ||
    prover !== signer.paymentKeyHash
  )
    throw new WorkflowActionChangedError(
      `validation-trace ${threadOutRef} is not a thread of case ${caseUnit} in its one-step resolution`,
    );
  return Object.freeze({ utxo, ...recorded, holder, caseThread });
};

/** The field the observe move of our thread `utxo` reads from our published
 * copy, by our trace in `material`; none off that stage or where it fits
 * inline. */
export const validationTraceObserveCopyPreimage = async ({
  deployment,
  material,
  utxo,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly material: ValidationTraceClaimMaterial | undefined;
  readonly utxo: UTxO;
}): Promise<Buffer | undefined> => {
  const holder = validationTraceOneStepHolderAt(deployment, utxo.address);
  if (holder?.holder !== "canonicalDecodeItemObserve") return undefined;
  const plan = await resolvingPlan({
    deployment,
    material,
    utxo,
    holder: holder.holder,
    caseThread: undefined,
    carried: undefined,
  });
  return plan.kind === "canonicalDecode"
    ? plan.canonicalDecode.item?.copied
    : undefined;
};

/** A hop of our thread through its one-step resolution, now; along the
 * shared item chain, reading the state it carried into the chain's entry
 * from the thread's history in `history`. */
export const captureValidationTraceResolve = async (
  input: MaterialInput & {
    readonly defenders: readonly ValidationTraceRecordedThread[];
    readonly history: Readonly<{
      caseHistory: ValidationTraceCaseHistory;
      headerHash: string;
    }>;
  },
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment, history } = input;
  const thread = await resolvingThread(input);
  const from = thread.holder;
  if (from.holder === "award" || from.reference === undefined)
    throw new WorkflowActionChangedError(
      `validation-trace thread ${input.threadOutRef} has reached its award`,
    );
  const { cekContextItemStages, scriptSourcesStageOneRedeemerStages } =
    deployment.chain;
  const carried =
    isValidationTraceContextItemChainHolder(from.holder) ||
    isScriptSourcesRedeemerOwnHolder(from.holder)
      ? await redeemerItemCarried({
          history: history.caseHistory,
          headerHash: history.headerHash,
          unit: thread.unit,
          entries: {
            cek: cekContextItemStages.entry.spendingScriptAddress,
            scriptSources:
              scriptSourcesStageOneRedeemerStages.envelope
                .spendingScriptAddress,
          },
        })
      : undefined;
  const plan = await resolvingPlan({
    deployment,
    material: input.material,
    utxo: thread.utxo,
    holder: from.holder,
    caseThread: thread.caseThread,
    carried,
    staging: input.staging,
  });
  const hop = validationTraceHop({
    plan,
    holder: from.holder,
    utxo: thread.utxo,
    prover: signer.paymentKeyHash,
    ...(from.holder === "selection"
      ? { selection: await validationTraceSelectionReads({ input, plan }) }
      : {}),
    ...(plan.kind === "context" && plan.context.item !== undefined
      ? { item: contextItemReads(deployment, carried?.stateCbor) }
      : {}),
    ...(plan.kind === "phase" && from.holder === plan.phase.semantic
      ? {
          phaseYield: phaseYieldReads(deployment, plan.phase),
          phaseYieldList: phaseYieldListReads(deployment, plan.phase),
        }
      : {}),
    ...(plan.kind === "phase"
      ? {
          redeemerChain: scriptSourcesRedeemerReads(
            deployment,
            carried?.stateCbor,
          ),
        }
      : {}),
    ...(plan.kind === "canonicalDecode" &&
    from.holder === "canonicalDecodeItemObserve" &&
    plan.canonicalDecode.item?.copied !== undefined
      ? { copy: await input.observeCopy?.() }
      : {}),
    staging: await stagingOutputs(lucid, plan),
  });
  const to = validationTraceOneStepHolder(deployment, hop.next);
  const reference = from.reference;
  signer.selectWallet(lucid);
  const window = validationTraceMoveWindow(lucid);
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "resolve",
    referenced: [reference, ...(hop.reads?.references ?? [])],
    build: (fee) =>
      buildValidationTraceHop({
        lucid,
        thread: thread.utxo,
        unit: thread.unit,
        reference,
        hop,
        address: to.address,
        prover: signer.paymentKeyHash,
        fee,
        ...window,
      }),
  });
  return await captureMove({
    input,
    stage: "resolve",
    unsigned,
    thread,
    defenderVkh: thread.defenderVkh,
    slots: [{ address: to.address, unit: thread.unit }],
  });
};

/** CloseAwardedThread: a thread we won at the award, into our case. */
export const captureValidationTraceAwardedClose = async (
  input: ValidationTraceMoveInput & {
    readonly caseOutRef: string;
    readonly defenders: readonly ValidationTraceRecordedThread[];
  },
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment, caseUnit } = input;
  const thread = await resolvingThread(input);
  if (thread.holder.holder !== "award")
    throw new WorkflowActionChangedError(
      `validation-trace thread ${input.threadOutRef} is not at its award`,
    );
  const caseUtxo = await ours(lucid, input.caseOutRef, "case");
  if (
    caseUtxo.address !== deployment.chain.case.spendingScriptAddress ||
    caseUtxo.assets[caseUnit] !== 1n ||
    typeof caseUtxo.datum !== "string"
  )
    throw new WorkflowActionChangedError(
      `validation-trace ${input.caseOutRef} is not case ${caseUnit}`,
    );
  const caseConfig = await caseConfigUtxo(lucid, deployment);
  signer.selectWallet(lucid);
  const window = validationTraceMoveWindow(lucid);
  const close = (fee: bigint) =>
    buildValidationTraceThreadClose({
      lucid,
      deployment,
      via: "award",
      thread,
      caseId: caseUnit.slice(56),
      defenderVkh: thread.defenderVkh,
      caseUtxo,
      caseConfig,
      fee,
      ...window,
    });
  const { references } = deployment;
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "close_awarded",
    referenced: [caseConfig, references.case, references.award],
    build: (fee) => close(fee).tx,
  });
  const { receiptUnit } = close(0n);
  return await captureMove({
    input,
    stage: "close_awarded",
    unsigned,
    thread,
    defenderVkh: thread.defenderVkh,
    caseInput: caseUtxo,
    slots: [
      { address: deployment.chain.case.spendingScriptAddress, unit: caseUnit },
      {
        address: deployment.chain.award.spendingScriptAddress,
        unit: receiptUnit,
      },
    ],
  });
};
