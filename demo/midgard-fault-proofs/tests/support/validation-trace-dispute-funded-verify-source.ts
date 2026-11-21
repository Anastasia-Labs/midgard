/**
 * The funded validation-trace dispute's VerifySource, as its emulator tests
 * observe it: the stages its processes sent that the ledger confirmed, the
 * VerifySource transactions they sent, the case and bond left live on the
 * ledger, and a watcher process stopped at VerifySource's submit.
 */
import * as SDK from "@al-ft/midgard-sdk/da";
import { expect } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import {
  type AdapterWrap,
  type FundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import {
  BOND_LOVELACE,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";

export const ADA = 1_000_000n;
export const FLOOR = SDK.VALIDATION_CASE_FLOOR_LOVELACE;
/** Every case runs Init, Open and VerifySource on the emulator. */
export const TIMEOUT = 600_000;
/** A wallet that holds the bond, the case floor and every fee besides. */
export const FUNDED = BOND_LOVELACE + 1_000n * ADA;

export const isVerifySource = (action: { readonly input: unknown }) =>
  (action.input as { readonly stage?: unknown }).stage === "verify_source";

/** The stages the dispute's processes sent that the ledger confirmed, in
 * order. */
export const confirmedStages = async (dispute: FundedDispute) =>
  (await dispute.confirmed()).map(({ action }) => action.stage);

/** The case and the bond VerifySource left live, as its signed body pays
 * them. */
export const caseAndBond = async (scene: Scene, dispute: FundedDispute) => {
  const stages = await dispute.confirmed();
  expect(stages.map(({ action }) => action.stage)).toStrictEqual([
    "init",
    "open",
    "verify_source",
  ]);
  const verify = stages[2]!;
  const [caseClaim, bondClaim] = verify.outputs;
  const [caseOutput, bond] = await scene.h.emulator.getUtxosByOutRef([
    parseOwnOutRef(caseClaim!.outRef),
    parseOwnOutRef(bondClaim!.outRef),
  ]);
  return {
    verify,
    caseClaim: caseClaim!,
    bondClaim: bondClaim!,
    caseOutput: caseOutput!,
    bond: bond!,
  };
};

/** The VerifySource transactions the dispute's processes sent, in order. */
export const verifySourceSends = (dispute: FundedDispute) =>
  dispute
    .sent()
    .filter(({ actionId }) =>
      actionId.startsWith("validation-trace-verify-source:"),
    )
    .map(({ txHash }) => txHash);

/** Exactly one live case and one live bond exist at their contracts. */
export const expectOneCaseAndBond = async (scene: Scene) => {
  const { chain, h } = scene;
  for (const address of [
    chain.case.spendingScriptAddress,
    chain.disputeBond.spendingScriptAddress,
  ])
    expect(
      (await h.emulator.getUtxos(address)).filter((utxo) =>
        Object.keys(utxo.assets).some((unit) =>
          unit.startsWith(chain.case.policyId),
        ),
      ),
    ).toHaveLength(1);
};

/**
 * Stops the watcher process at VerifySource's submit: after (`broadcast`)
 * or before its bytes reach the ledger. The stopped turn never returns.
 */
export const stopAtVerifySourceSubmit = ({
  broadcast,
}: {
  readonly broadcast: boolean;
}) => {
  let signedHex: string | undefined;
  let signedHash: string | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const wrap: AdapterWrap = (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      if (isVerifySource(context.action)) {
        const signed = workflowPreflightTransaction(preflight)!;
        signedHex = signed.toTransaction().to_cbor_hex();
        signedHash = signed.toHash();
      }
      return preflight;
    },
    submit: async (input) => {
      if (!isVerifySource(input.action)) return await adapter.submit(input);
      if (broadcast) await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  });
  return {
    /** Runs the process until it stops; a turn that ends first fails. */
    stopped: async (dispute: FundedDispute) => {
      const turns = turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, { wrap });
      await Promise.race([
        stop,
        turns.then(({ history }) => {
          throw new Error(
            `the process ended before submit: ${history.join("\n")}`,
          );
        }),
      ]);
      if (signedHash === undefined)
        throw new Error("VerifySource was not signed");
    },
    signedHex: () => signedHex!,
    signedHash: () => signedHash!,
  };
};
