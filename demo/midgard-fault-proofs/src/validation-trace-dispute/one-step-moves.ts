/** The challenger's moves through the one-step resolution of a thread of
 * ours, made now from the thread's live state: enter the resolution from the
 * game when the thread is ReadyForOneStep on our turn, move the thread on
 * from each holder to the next (one-step-builders.ts), and close the thread
 * the award holds into our case with its defender's lost-defense receipt.
 * The thread is read by the out-ref the action names and checked as ours:
 * our case's token for one of its recorded defenders, at the holder its
 * address names, with a datum that names us its prover. Each move carries the
 * funding permit's move authority and names the output it leaves. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { findScriptDecodeRecord } from "../script-decode/record.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { ValidationTraceRecordedThread } from "./case-history.js";
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
  type ValidationTraceSelectionReads,
} from "./one-step-builders.js";
import {
  validationTraceDrivesSelections,
  type ValidationTraceOneStepContract,
  validationTraceOneStepHolder,
  validationTraceOneStepHolderAt,
} from "./one-step-holders.js";
import {
  planValidationTraceOneStep,
  planValidationTraceThread,
  type ValidationTraceOneStepPlan,
  type ValidationTraceOneStepPlanning,
} from "./one-step-plan.js";
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
};

/** A plan, or the reason the runner does not drive the step, as an error. */
const planned = (
  planning: ValidationTraceOneStepPlanning,
): ValidationTraceOneStepPlan => {
  if (planning.kind !== "plan")
    throw new Error(`validation-trace one step: ${planning.reason}`);
  return planning.plan;
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
      validationTraceDrivesSelections(deployment),
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

/** Where each holder past the boundary keeps our successor's hash: the
 * selection resolver holds the state the core step binder would. */
const CHALLENGER_SUCCESSOR_HASH = {
  prepare: [1, 0, 3],
  core: [1, 0, 1, 3],
  selection: [1, 0, 1, 3],
  bound: [1, 0, 0, 1, 3],
} as const;

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

/**
 * The plan the thread `utxo` at `holder` resolves, from our trace in
 * `material`: at the boundary, by its case thread `caseThread`; past it, by
 * our successor's hash its holder keeps.
 */
const resolvingPlan = async ({
  deployment,
  material,
  utxo,
  holder,
  caseThread,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly material: ValidationTraceClaimMaterial | undefined;
  readonly utxo: UTxO;
  readonly holder: ValidationTraceOneStepContract["holder"];
  readonly caseThread: SDK.CaseThread | undefined;
}): Promise<ValidationTraceOneStepPlan> => {
  const decodeRecords = validationTraceDrivesSelections(deployment);
  if (caseThread !== undefined) {
    if (!isThreadMaterial(caseThread, material))
      throw new Error(
        "validation-trace one step has no trace matching its thread's challenger root",
      );
    return planned(
      await planValidationTraceThread(material, caseThread, decodeRecords),
    );
  }
  if (material === undefined)
    throw new Error("validation-trace one step has no trace of ours");
  const path =
    holder === "prepare" || holder === "core" || holder === "selection"
      ? CHALLENGER_SUCCESSOR_HASH[holder]
      : CHALLENGER_SUCCESSOR_HASH.bound;
  return planned(
    await planValidationTraceOneStep({
      material,
      challengerHighHash: Data.from<string>(
        plutusConstrFieldCbor(utxo.datum!, [...path]),
      ),
      decodeRecords,
    }),
  );
};

/**
 * What the selection resolver's hop of `plan` reads on `deployment`: the
 * selection's yields and the selected script's decode record, which the
 * script-decode prerequisite has made before the thread entered its
 * resolution.
 */
const selectionReads = async ({
  input,
  plan,
}: {
  readonly input: MaterialInput;
  readonly plan: ValidationTraceOneStepPlan;
}): Promise<ValidationTraceSelectionReads> => {
  const { lucid, deployment, material } = input;
  const references = deployment.references.oneStep?.cekSelection;
  const { scriptDecode } = deployment;
  if (
    plan.kind !== "selection" ||
    references === undefined ||
    scriptDecode === undefined ||
    material === undefined
  )
    throw new Error(
      "validation-trace selection hop has no CEK script selection to drive",
    );
  let record: UTxO | undefined;
  if (plan.script !== undefined) {
    record = await findScriptDecodeRecord({
      lucid,
      deployment: scriptDecode,
      scriptHashHex: plan.script.scriptHashHex,
    });
    if (record === undefined)
      throw new Error(
        `validation-trace selection's script ${plan.script.scriptHashHex} has no decode record`,
      );
  }
  const { cekSelectionAuthenticate, cekSelectionSuccessor } =
    deployment.chain.yields;
  return Object.freeze({
    trace: await material.trace(),
    yields: {
      references: [references.authenticate, references.successor] as const,
      scripts: [
        cekSelectionAuthenticate.withdrawalScript,
        cekSelectionSuccessor.withdrawalScript,
      ] as const,
    },
    record,
    scriptDecodePolicyId: scriptDecode.validator.policyId,
  });
};

/** A hop of our thread through its one-step resolution, now. */
export const captureValidationTraceResolve = async (
  input: MaterialInput & {
    readonly defenders: readonly ValidationTraceRecordedThread[];
  },
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment } = input;
  const thread = await resolvingThread(input);
  const from = thread.holder;
  if (from.holder === "award" || from.reference === undefined)
    throw new WorkflowActionChangedError(
      `validation-trace thread ${input.threadOutRef} has reached its award`,
    );
  const plan = await resolvingPlan({
    deployment,
    material: input.material,
    utxo: thread.utxo,
    holder: from.holder,
    caseThread: thread.caseThread,
  });
  const hop = validationTraceHop({
    plan,
    holder: from.holder,
    utxo: thread.utxo,
    prover: signer.paymentKeyHash,
    ...(from.holder === "selection"
      ? { selection: await selectionReads({ input, plan }) }
      : {}),
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
