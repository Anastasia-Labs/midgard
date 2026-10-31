/** A staged builtin step's case of ours (arm 31) on the emulator
 * (validation-trace-dispute-one-step-staged-drive.ts) whose block another
 * record removes. Another prover opens a case of its own on the block beside
 * ours and leaves it undefended. Both threads of our case play through the
 * staged step to their award and close while the test holds our spend backs
 * until our case's close is confirmed, so each staging output is still live;
 * past both cases' deadlines that prover convicts and removes the block
 * against its own record, and the removal goes past rollback recovery before
 * our run is let on. Our case then closes, and our run reads the block as
 * removed against another record.
 *
 * With `ourKey`, the other case is another run's on our key, with a journal
 * of its own: our run then reads the removal as a terminal on our key whose
 * record its journal never created, not as another prover's. With
 * `sibling`, another run on our key has Inited a thread of its own on the
 * block and stopped short of its Open, so a thread of ours stands at the
 * opener when the block is removed. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import { resolveProverSigner } from "../../src/index.js";
import {
  type ConfirmedValidationTraceStage,
  confirmedValidationTraceStages,
} from "../../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_STAGING_RECLAIM } from "../../src/validation-trace-dispute/one-step-staging-journal.js";
import { validationTraceDefenseDeadlineMs } from "../../src/validation-trace-dispute/waiting.js";
import {
  type FraudProofWorkflowJournalEntry,
  MemoryFraudProofWorkflowJournalStore,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { RELEASE_FINALITY_POLICY } from "../workflow.make-adapter.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { network } from "./submit-init-emulator-shared.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
  threadClosed,
} from "./validation-trace-dispute-one-step-case.js";
import {
  entriesOf,
  stagedDrive,
} from "./validation-trace-dispute-one-step-staged-drive.js";
import { onLedger } from "./validation-trace-dispute-one-step-staging-ledger.js";
import { recordedRemoval } from "./validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./validation-trace-dispute-runner.js";

/** How many blocks under the tip a transaction of ours is final. */
export const RELEASE_DEPTH = 4;

/** How deep a removal, or our cancel of a thread, is past rollback
 * recovery. */
const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

const SPEND_BACKS_HELD = "spend backs held by the test";
/** How many runs past our cancel's landing the chain waits on our spend
 * backs before it is let past the cancel's rollback recovery. */
const AFTER_CANCEL = 3 * RELEASE_DEPTH;
const SIBLING_OPEN_HELD = "the other run's Open is held by the test";

const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

/** The first output of the confirmed `stage` in `result`'s journal. */
const stageOutRef = (
  result: FraudProofWorkflowRunResult,
  stage: ConfirmedValidationTraceStage["action"]["stage"],
): string => {
  const found = confirmedValidationTraceStages(entriesOf(result)).find(
    ({ action }) => action.stage === stage,
  );
  if (found === undefined) throw new Error(`no ${stage} is confirmed`);
  return found.outputs[0]!.outRef;
};

/** The out-refs the signed transaction `cbor` spends. */
const inputsOf = (cbor: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** A run of ours from the block's removal on, and the tip it observed. */
export type OurRun = Readonly<{
  result: FraudProofWorkflowRunResult;
  observedAt: number;
}>;

export const stagedOtherRecord = ({
  sibling = false,
  ourKey = false,
}: { readonly sibling?: boolean; readonly ourKey?: boolean } = {}) => {
  const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH, withRemoval: true });
  let theirs: Runner;
  let theirCase: string;
  let siblingThread: string | undefined;
  let clock: { readonly mockRestore: () => void } | undefined;

  /** Holds each spend back our workflow would take, as a wait, until our
   * case's close is confirmed. */
  const holding = (
    adapter: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter => ({
    ...adapter,
    observe: async (context) => {
      const observed = await adapter.observe(context);
      return observed.kind === "action_required" &&
        !confirmedValidationTraceStages(context.entries).some(
          ({ action }) => action.stage === "close",
        ) &&
        observed.action.input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM
        ? { kind: "pending", reason: SPEND_BACKS_HELD }
        : observed;
    },
  });

  const scene = () => drive.dispute().scene;

  /** A runner of another prover, funded from ours, with its own wallet and
   * key. */
  const anotherProver = async (
    recorder: Parameters<typeof recordedRemoval>[1],
    headerHash: string,
  ): Promise<Runner> => {
    const { h } = scene();
    const signer = resolveProverSigner({
      network,
      walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
    });
    const funding = await h.proverLucid
      .newTx()
      .pay.ToAddress(signer.address, { lovelace: 15_000_000_000n })
      .pay.ToAddress(signer.address, { lovelace: 5_000_000_000n })
      .complete({ localUPLCEval: true });
    await h.proverLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    const lucid = await Lucid(h.emulator, "Custom");
    signer.selectWallet(lucid);
    const theirRemoval = await recordedRemoval(
      scene(),
      recorder,
      signer.paymentKeyHash,
    );
    return makeRunner(scene(), {
      prover: { lucid, signer },
      removal: theirRemoval.removal,
      terminalVerifier: theirRemoval.terminalVerifier(headerHash),
    });
  };

  /** Our case set up, our spend backs held; the other prover's case open;
   * with `sibling`, the other run on our key Inited. */
  const setUp = async () => {
    await drive.setUp();
    drive.restart({ wrap: holding });
    const { h, failing } = scene();
    const { deployment, removal, recorder } = drive.dispute();
    const { headerHash } = failing.evidence;
    theirs = ourKey
      ? // Another run on our key, with a journal of its own.
        makeRunner(scene(), {
          deployment,
          journal: new MemoryFraudProofWorkflowJournalStore(),
          signedRecovery: emulatorSignedTransactionRecovery(
            h.emulator,
            RELEASE_DEPTH,
          ),
          removal: removal!.removal,
          terminalVerifier: removal!.terminalVerifier(headerHash),
        })
      : await anotherProver(recorder!, headerHash);
    theirCase = stageOutRef(
      (await runUntil(theirs, failing, VALIDATION_TRACE_CASE_OPEN)).result,
      "verify_source",
    );
    if (!sibling) return;
    // Another run on our key, with a journal of its own, Inits and stops
    // short of its Open.
    const other = makeRunner(scene(), {
      deployment,
      journal: new MemoryFraudProofWorkflowJournalStore(),
      signedRecovery: emulatorSignedTransactionRecovery(
        h.emulator,
        RELEASE_DEPTH,
      ),
      removal: removal!.removal,
      terminalVerifier: removal!.terminalVerifier(headerHash),
      wrap: (adapter) => ({
        ...adapter,
        observe: async (context) => {
          const observed = await adapter.observe(context);
          return observed.kind === "action_required" &&
            observed.action.input.stage === "open"
            ? { kind: "pending", reason: SIBLING_OPEN_HELD }
            : observed;
        },
      }),
    });
    siblingThread = stageOutRef(
      (await runUntil(other, failing, SIBLING_OPEN_HELD)).result,
      "init",
    );
  };

  /** Plays both threads of our case to their award and close, our spend
   * backs held; the staging outputs, each still live. */
  const playToClose = async (): Promise<readonly string[]> => {
    const { a, b } = drive.dispute();
    await drive.play(
      async () =>
        (await threadClosed(drive.dispute(), a.unit)) &&
        (await threadClosed(drive.dispute(), b.unit)),
      60,
    );
    const stagings = (await drive.stagedOnL1()).map(({ outRef }) => outRef);
    expect(stagings.length).toBeGreaterThan(1);
    for (const outRef of stagings)
      expect(onLedger(scene(), parseOwnOutRef(outRef)), outRef).toBe(true);
    return stagings;
  };

  /** Past both cases' deadlines, the other prover convicts its case and
   * removes the block against its record, and the removal goes past
   * rollback recovery; that record. */
  const removeByTheirs = async (): Promise<string> => {
    const { h, failing } = scene();
    const [theirs_] = await h.proverLucid.utxosByOutRef([
      parseOwnOutRef(theirCase),
    ]);
    const deadline = Math.max(
      ...[(await liveCase(drive.dispute())).datum!, theirs_!.datum!].map(
        (datum) => validationTraceDefenseDeadlineMs(Data.from(datum, SDK.Case)),
      ),
    );
    if (h.emulator.now() <= deadline)
      awaitLastSlotAtOrBefore(scene(), deadline + 1_000);
    // The removal dates its validity window from the application clock,
    // which on the emulator is the emulator's.
    clock ??= vi.spyOn(Date, "now").mockImplementation(() => h.emulator.now());
    const history: string[] = [];
    for (let attempt = 0; attempt < 16; attempt += 1) {
      const outcome = await theirs.runOnce(failing);
      history.push(describeRun(outcome));
      if (outcome.kind === "terminal_included" && "terminal" in outcome) {
        h.emulator.awaitBlock(FINAL_DEPTH);
        return outcome.terminal.correction.referencedProofTokenOutRef;
      }
    }
    throw new Error(`their removal did not land:\n${history.join("\n")}`);
  };

  /**
   * Runs ours until it resolves, each run's outcome in `runs`; `between`
   * runs after each run and may restart the runner. With `sibling`, once the
   * other run's thread is spent, the chain is let past the rollback recovery
   * of our cancel when each of `stagings` is spent, or after `AFTER_CANCEL`
   * runs.
   */
  const runToResolution = async (
    stagings: readonly string[],
    runs: OurRun[],
    between?: () => Promise<void>,
  ) => {
    const { h, failing } = scene();
    let jumped = false;
    let sinceCancel = 0;
    for (let run = 0; run < 60; run += 1) {
      const observedAt = h.emulator.blockHeight;
      const result = await drive.dispute().runner.runOnce(failing);
      runs.push({ result, observedAt });
      if (result.kind === "resolved_by_another_record") return result;
      await between?.();
      if (
        !jumped &&
        siblingThread !== undefined &&
        !onLedger(scene(), parseOwnOutRef(siblingThread)) &&
        ((sinceCancel += 1) > AFTER_CANCEL ||
          stagings.every(
            (outRef) => !onLedger(scene(), parseOwnOutRef(outRef)),
          ))
      ) {
        jumped = true;
        h.emulator.awaitBlock(FINAL_DEPTH);
      }
    }
    throw new Error(
      `our run did not resolve:\n${runs.map(({ result }) => describeRun(result)).join("\n")}`,
    );
  };

  /** The one landed transaction of ours that spent `outRef`, and its
   * block; every signed transaction the ledger took that spends it. */
  const spentBy = async (outRef: string) => {
    const hash = await drive.spenderOf(outRef);
    expect(hash, outRef).toBeDefined();
    return {
      hash: hash!,
      block: await drive.blockOf(hash!),
      signed: [...scene().l1.signedCbors].filter(([, cbor]) =>
        inputsOf(cbor).includes(outRef),
      ),
    };
  };

  /** Expects `outRef` spent back to our wallet alone, by exactly one
   * transaction the ledger took; that spend back and its block. */
  const expectSpentBack = async (outRef: string) => {
    const spent = await spentBy(outRef);
    expect(
      spent.signed.map(([hash]) => hash),
      outRef,
    ).toStrictEqual([spent.hash]);
    const body = CML.Transaction.from_cbor_hex(
      scene().l1.signedCbors.get(spent.hash)!,
    ).body();
    expect(body.inputs().len()).toBe(1);
    expect(body.outputs().len()).toBe(1);
    expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
      scene().h.proverSigner.address,
    );
    return spent;
  };

  /** Our spend backs the journal `entries` shows submitted, by what each
   * spends. */
  const spendBackIntents = (
    entries: readonly FraudProofWorkflowJournalEntry[],
  ) => {
    const intents = new Map<string, Set<string>>();
    for (const { event } of entries)
      if (
        event.kind === "submission_intent" &&
        event.actionInput.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM
      ) {
        const consumes = event.actionInput.consumes as string;
        intents.set(
          consumes,
          (intents.get(consumes) ?? new Set()).add(event.txHash),
        );
      }
    return intents;
  };

  return {
    drive,
    holding,
    setUp,
    playToClose,
    removeByTheirs,
    runToResolution,
    spentBy,
    expectSpentBack,
    spendBackIntents,
    describeRun,
    /** The thread the other run on our key Inited. */
    siblingThread: () => siblingThread!,
    tearDown: () => {
      clock?.mockRestore();
      drive.dispute()?.recorder?.restore();
    },
  };
};
