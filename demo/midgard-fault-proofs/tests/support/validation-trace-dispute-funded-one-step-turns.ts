/** The funded watcher's turns over one defended thread's one-step
 * resolution, on the emulator: each move it builds captured, the thread
 * found wherever it stands, played to a holder, and the watcher process
 * stopped at the submit of the move it signs at a holder, once its intent is
 * durable and before its bytes reach the ledger. */
import type { UTxO } from "@lucid-evolution/lucid";

import { parseValidationTraceAction } from "../../src/validation-trace-dispute/actions.js";
import {
  validationTraceOneStepContracts,
  type ValidationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import {
  type CapturedMove,
  captureMoves,
} from "./validation-trace-dispute-funded-moves.js";
import type {
  AdapterWrap,
  FundedDispute,
} from "./validation-trace-dispute-funded-runner.js";
import {
  confirmedOneStepMoves,
  playThreads,
  type Side,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

export const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Whether the journal `result` holds confirms a move spending `consumes`. */
export const confirmsSpendOf = (
  result: FraudProofWorkflowRunResult,
  consumes: string,
) =>
  "entries" in result &&
  confirmedOneStepMoves(result.entries).some(
    (move) => move.consumes === consumes,
  );

/**
 * The turns of the funded watcher `dispute()` over the thread of `side()`
 * on `scene()`, each move it builds captured into `moves`; the getters read
 * what the calling file's set-up assigned.
 */
export const fundedOneStepTurns = ({
  scene,
  dispute,
  side,
  moves,
}: {
  readonly scene: () => Scene;
  readonly dispute: () => FundedDispute;
  readonly side: () => Side;
  readonly moves: CapturedMove[];
}) => {
  /** The watcher process's turns, each move it builds captured. */
  const turns = (wrap?: AdapterWrap) => ({
    runOnce: async () =>
      await dispute().turn((adapter) => {
        const captured = captureMoves(scene(), moves)(adapter);
        return wrap === undefined ? captured : wrap(captured);
      }),
  });

  /** Where the thread stands now. */
  const place = async () =>
    await threadPlace(scene(), dispute().deployment, side().unit);

  /** Plays the thread until it stands at `holder`, if it does not yet. */
  const playTo = async (holder: ValidationTraceOneStepHolder) => {
    if ((await place())?.holder === holder) return;
    await playThreads({
      scene: scene(),
      runner: turns(),
      sides: [side()],
      until: async () => (await place())?.holder === holder,
      limit: 60,
    });
  };

  /** The address of the one-step holder `holder`. */
  const holderAddress = (holder: ValidationTraceOneStepHolder) =>
    validationTraceOneStepContracts(dispute().deployment).find(
      (contract) => contract.holder === holder,
    )!.address;

  /**
   * Stops the watcher process at the submit of the move it signs for the
   * thread at `holder`, once its intent is durable and before its bytes
   * reach the ledger. The stopped turn never returns.
   */
  const stopBeforeBroadcastAt = async (
    holder: ValidationTraceOneStepHolder,
  ) => {
    const at = await place();
    if (at?.holder !== holder)
      throw new Error(`the thread is not at ${holder}`);
    const consumes = outRefOf(at.utxo);
    let signed: { readonly hash: string; readonly hex: string } | undefined;
    let stopped!: () => void;
    const stop = new Promise<void>((resolve) => {
      stopped = resolve;
    });
    const wrap: AdapterWrap = (adapter) => ({
      ...adapter,
      preflight: async (context) => {
        const preflight = await adapter.preflight(context);
        const transaction = workflowPreflightTransaction(preflight);
        if (
          parseValidationTraceAction(context.action).consumes === consumes &&
          transaction !== undefined
        )
          signed ??= {
            hash: transaction.toHash(),
            hex: transaction.toTransaction().to_cbor_hex(),
          };
        return preflight;
      },
      submit: async (input) => {
        if (
          signed === undefined ||
          parseValidationTraceAction(input.action).consumes !== consumes
        )
          return await adapter.submit(input);
        stopped();
        return await new Promise<never>(() => undefined);
      },
    });
    const play = playThreads({
      scene: scene(),
      runner: turns(wrap),
      sides: [side()],
      until: async () => false,
      limit: 10,
    });
    await Promise.race([
      stop,
      play.then(() => {
        throw new Error(`the process made no move at ${holder}`);
      }),
    ]);
    if (signed === undefined)
      throw new Error(`no move at ${holder} was signed`);
    return { ...signed, consumes };
  };

  return { turns, place, playTo, holderAddress, stopBeforeBroadcastAt };
};
