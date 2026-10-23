/** The defender's side of the game on the Open scene: false claims opened
 * on the scene's passing block, both sides' moves built by the src
 * builders, and the expiry of a thread past its case's duration, with the
 * thread-expiry withdrawal registered and the scripts it runs published. */
import { type MidgardValidationTraceProof } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Script,
  type TxSignBuilder,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  buildValidationTraceThreadExpiry,
  type ValidationTraceExpiryHolder,
} from "../../src/validation-trace-dispute/expiry.js";
import {
  buildValidationTraceGameMove,
  buildValidationTraceOperatorReveal,
  challengerEnteredResolution,
  revealedThread,
  validationTraceMoveWindow,
} from "../../src/validation-trace-dispute/move-builders.js";
import { completeValidationTraceMove } from "../../src/validation-trace-dispute/move-fee.js";
import {
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";
import {
  type SparseTrace,
  sparseTrace,
} from "./validation-game-deep-traces.js";
import type { Defender } from "./validation-trace-dispute-defend.js";
import {
  failingMaterial,
  initAndOpen,
  type Scene,
  verifySource,
} from "./validation-trace-dispute-open-scene.js";
import { passingTrace } from "./validation-trace-dispute-passing-trace.js";
import { threadOf } from "./validation-trace-dispute-scripted-defender.js";

/** A signed move that landed. */
export type Landed = Readonly<{ hash: string; cbor: string }>;

/** A case the challenger opened on the passing block with a trace ending
 * Terminal Rejected at `stepCount`. */
export type FalseClaim = Readonly<{
  caseUnit: string;
  caseUtxo: UTxO;
  stepCount: number;
  /** The challenger's committed trace. */
  trace: SparseTrace;
}>;

/** The passing block's honest trace, and a Terminal Rejected state its
 * honest run never reaches. */
export const falseClaimMaterial = async (scene: Scene) => {
  const honest = await passingTrace(scene);
  const rejected = (await failingMaterial(scene)).terminal;
  return {
    honest,
    claimedTerminal: {
      ...honest.terminal,
      verdict: "rejected" as const,
      rejectionCodeHash: rejected.rejectionCodeHash,
    },
  };
};

/** Opens a case on the passing block whose trace holds the initial state
 * until `stepCount` and the claimed Terminal Rejected state from there; with
 * `agreeingFrom`, it holds the honest terminal state from that index on
 * until `stepCount`, so it agrees with the honest trace past the honest
 * end. */
export const openFalseClaim = async (
  scene: Scene,
  { honest, claimedTerminal }: Awaited<ReturnType<typeof falseClaimMaterial>>,
  stepCount: number,
  agreeingFrom?: number,
): Promise<FalseClaim> => {
  const trace = sparseTrace(
    [
      { from: 0, state: honest.initialState },
      ...(agreeingFrom === undefined
        ? []
        : [{ from: agreeingFrom, state: honest.terminal }]),
      { from: stepCount, state: claimedTerminal },
    ],
    stepCount,
  );
  const { opener } = await initAndOpen(scene, scene.passing, {
    claimWitness: honest.claim.claimWitness,
    challengerRoot: trace.descriptor.traceRoot.toString("hex"),
    stepCount: BigInt(stepCount),
  });
  const verification = verifySource(scene, opener, {
    terminal: claimedTerminal,
    terminalProof: trace.proofAt(stepCount),
    initialState: honest.initialState,
  });
  const signed = await (await verification.build()).sign
    .withWallet()
    .complete();
  const txHash = await signed.submit();
  await scene.h.proverLucid.awaitTx(txHash);
  const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
    { txHash, outputIndex: 0 },
  ]);
  expect(caseUtxo?.assets[verification.opening.caseUnit]).toBe(1n);
  return {
    caseUnit: verification.opening.caseUnit,
    caseUtxo: caseUtxo!,
    stepCount,
    trace,
  };
};

/** The published references an expiry reads beyond the scene's own. */
export type ExpiryReferences = Readonly<{ threadExpiry: UTxO; boundary: UTxO }>;

/** Registers the thread-expiry withdrawal's reward account, and publishes
 * its script and the boundary's. */
export const publishExpiryReferences = async (
  scene: Scene,
): Promise<ExpiryReferences> => {
  const { h, chain } = scene;
  const lucid = h.proverLucid;
  const registration = await (
    await lucid
      .newTx()
      .register.Stake(
        validatorToRewardAddress(network, chain.threadExpiry.withdrawalScript),
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await registration.submit());
  const publish = async (script: Script, label: string) =>
    (await publishPlainReferenceScriptUtxo({ lucid, script, label })).utxo;
  return {
    threadExpiry: await publish(
      chain.threadExpiry.withdrawalScript,
      "thread expiry",
    ),
    boundary: await publish(chain.boundary.spendingScript, "boundary"),
  };
};

/** Signs `unsigned` with `lucid`'s wallet and submits it; once it lands. */
export const landWith = async (
  lucid: LucidEvolution,
  unsigned: TxSignBuilder,
): Promise<Landed> => {
  const signed = await unsigned.sign.withWallet().complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  return { hash, cbor: signed.toCBOR() };
};

/** The thread output 0 of `landed` created. */
export const movedThread = async (
  lucid: LucidEvolution,
  landed: Landed,
): Promise<UTxO> => {
  const [moved] = await lucid.utxosByOutRef([
    { txHash: landed.hash, outputIndex: 0 },
  ]);
  if (moved === undefined) throw new Error("the moved thread is absent");
  return moved;
};

/** The operator's RevealOperator on `thread`, now, of its state at the
 * turn's midpoint by `proofAt`, before signing. */
export const operatorReveal = async (
  scene: Scene,
  defender: Defender,
  thread: UTxO,
  proofAt: (index: number) => MidgardValidationTraceProof,
): Promise<TxSignBuilder> => {
  const { lucid } = defender;
  const { turn } = threadOf(thread);
  if (typeof turn === "string" || !("AwaitingOperator" in turn))
    throw new Error("the thread is not on the operator's turn");
  const proof = SDK.validationTraceProofDataFromCore(
    proofAt(Number(turn.AwaitingOperator.midpoint)),
  );
  const window = validationTraceMoveWindow(lucid);
  return await completeValidationTraceMove({
    lucid,
    stage: "RevealOperator",
    referenced: [scene.references.game],
    build: (fee) =>
      buildValidationTraceOperatorReveal({
        lucid,
        deployment: scene,
        thread,
        proof,
        fee,
        ...window,
      }),
  });
};

/** The challenger's RevealChallenger on `thread`, now, from `trace`, before
 * signing. */
export const challengerReveal = async (
  scene: Scene,
  thread: UTxO,
  trace: Pick<SparseTrace, "proofAt">,
): Promise<TxSignBuilder> => {
  const lucid = scene.h.proverLucid;
  const datum = Data.from(thread.datum!, SDK.CaseThreadDatum);
  const { turn } = datum.data!;
  if (typeof turn === "string" || !("AwaitingChallenger" in turn))
    throw new Error("the thread is not on the challenger's turn");
  const proof = SDK.validationTraceProofDataFromCore(
    trace.proofAt(Number(turn.AwaitingChallenger.midpoint)),
  );
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const next = revealedThread({
    side: "challenger",
    thread: datum.data!,
    proof,
    lower: BigInt(validFrom),
    upper: BigInt(validTo - 1),
  });
  return await completeValidationTraceMove({
    lucid,
    stage: "reveal",
    referenced: [scene.references.game],
    build: (fee) =>
      buildValidationTraceGameMove({
        lucid,
        deployment: scene,
        thread,
        action: { kind: "RevealChallenger", proof },
        successor: {
          address: thread.address,
          datum: Data.to({ ...datum, data: next }, SDK.CaseThreadDatum),
        },
        fee,
        validFrom,
        validTo,
      }),
  });
};

/** The challenger's EnterResolution on `thread`, ReadyForOneStep, now,
 * signed by its fraud prover, before signing. */
export const challengerEntersResolution = async (
  scene: Scene,
  thread: UTxO,
): Promise<TxSignBuilder> => {
  const lucid = scene.h.proverLucid;
  const datum = Data.from(thread.datum!, SDK.CaseThreadDatum);
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const next = challengerEnteredResolution({
    thread: datum.data!,
    lower: BigInt(validFrom),
    upper: BigInt(validTo - 1),
  });
  return await completeValidationTraceMove({
    lucid,
    stage: "enter_resolution",
    referenced: [scene.references.game],
    build: (fee) =>
      buildValidationTraceGameMove({
        lucid,
        deployment: scene,
        thread,
        action: { kind: "EnterResolution" },
        successor: {
          address: scene.chain.boundary.spendingScriptAddress,
          datum: Data.to({ ...datum, data: next }, SDK.CaseThreadDatum),
        },
        fee,
        validFrom,
        validTo,
        signer: datum.fraud_prover,
      }),
  });
};

/** The holder's published reference, by holder. */
const holderReference = (
  scene: Scene,
  references: ExpiryReferences,
  holder: ValidationTraceExpiryHolder,
): UTxO =>
  holder === "boundary" ? references.boundary : scene.references[holder];

/** The expiry of `thread`, holding `unit` at `holder`, now, submitted from
 * `lucid`'s wallet, before signing. */
export const threadExpiry = async ({
  scene,
  references,
  lucid,
  holder,
  thread,
  unit,
}: {
  readonly scene: Scene;
  readonly references: ExpiryReferences;
  readonly lucid: LucidEvolution;
  readonly holder: ValidationTraceExpiryHolder;
  readonly thread: UTxO;
  readonly unit: string;
}): Promise<TxSignBuilder> => {
  const reference = holderReference(scene, references, holder);
  const window = validationTraceMoveWindow(lucid);
  return await completeValidationTraceMove({
    lucid,
    stage: "expiry",
    referenced: [reference, scene.references.case, references.threadExpiry],
    build: (fee) =>
      buildValidationTraceThreadExpiry({
        lucid,
        network,
        holder,
        thread,
        threadUnit: unit,
        caseConfig: scene.caseConfig,
        references: {
          holder: reference,
          case: scene.references.case,
          threadExpiry: references.threadExpiry,
        },
        threadExpiryScriptHash: scene.chain.threadExpiry.withdrawalScriptHash,
        fee,
        ...window,
      }),
  });
};
