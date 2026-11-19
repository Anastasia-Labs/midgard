/**
 * A multi-step proof admitted past its latest safe start. The latest safe
 * start is a design margin; the state queue accepts the removal until
 * maturity. So until maturity the run keeps every right an on-time run has:
 * a fresh submission for each later step, and a new attempt for a signed
 * step whose validity lapsed. Only at maturity is a signed attempt merely
 * reconciled.
 */
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
} from "@al-ft/midgard-fault-proofs/workflow/orchestrator";
import { bindWorkflowPreflightTransaction } from "@al-ft/midgard-fault-proofs/workflow/transaction-boundary";
import { CML } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED } from "../../src/fault-proofs/fault-proof-resubmission-hold.js";
import { signReplacement } from "../funding/superseded-attempt-replacement.js";
import { walletAddress } from "../support/fault-proof-funding-fixture.sources-for.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

const LATEST_SAFE_START_OFFSET_MS =
  MIDGARD_RETENTION_WINDOW.maturityMs -
  MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs;

type Signed = ReturnType<typeof signReplacement>;

/** What a family builder hands the workflow for `tx`: a script-free step
 * whose local evaluation passed. */
const preflightOf = (action: FraudProofWorkflowAction, tx: Signed) =>
  bindWorkflowPreflightTransaction(
    {
      actionId: action.actionId,
      txHash: tx.transactionHash,
      scriptExecution: "none" as const,
      localUplcEvaluation: { status: "passed" as const, evaluator: "test" },
      referenceScripts: [],
    },
    {
      toHash: () => tx.transactionHash,
      toTransaction: () =>
        CML.Transaction.from_cbor_hex(tx.signedTransactionCborHex),
    } as Parameters<typeof bindWorkflowPreflightTransaction>[1],
  );

export const registerLateMultiStepTests = (
  setupScenario: typeof setup,
): void => {
  it("carries a multi-step proof admitted past its latest safe start through a lapsed step and a fresh later step to inclusion before maturity", async () => {
    const headerEndTime = Date.now() - LATEST_SAFE_START_OFFSET_MS - 60_000;
    const maturityAtMs = headerEndTime + MIDGARD_RETENTION_WINDOW.maturityMs;
    const test = await setupScenario(BigInt(headerEndTime), true, true);
    const { fixture } = test;
    fixture.useUnspentPendingInputs();
    const funding = fixture.fundingInput;
    // Step 1 is signed twice: its first attempt's validity lapses unlanded.
    // Step 2, the last, spends step 1's change.
    const lapsed = signReplacement(
      funding.outRef,
      BigInt(funding.lovelace),
      100n,
    );
    const init = signReplacement(
      funding.outRef,
      BigInt(funding.lovelace),
      300n,
    );
    const last = signReplacement(
      `${init.transactionHash}#0`,
      BigInt(funding.lovelace) - 1_000_000n,
      400n,
    );
    const steps: Readonly<Record<string, readonly Signed[]>> = {
      init: [lapsed, init],
      last: [last],
    };
    const landed = new Set<string>();
    const expired = new Set<string>();
    // The wallet as the chain shows it: a landed step spends its input and
    // pays its change back.
    const land = (tx: Signed) => {
      landed.add(tx.transactionHash);
      const [change] = tx.producedInputs;
      fixture.walletUtxos.splice(
        0,
        fixture.walletUtxos.length,
        ...fixture.walletUtxos.filter(
          ({ txHash, outputIndex }) =>
            !tx.consumedOutRefs.includes(`${txHash}#${outputIndex}`),
        ),
        {
          txHash: tx.transactionHash,
          outputIndex: 0,
          address: walletAddress,
          assets: { lovelace: BigInt(change!.lovelace) },
        },
      );
    };
    test.getUtxosByOutRef.mockImplementation(async (refs) =>
      fixture.walletUtxos.filter((utxo) =>
        refs.some(
          ({ txHash, outputIndex }) =>
            utxo.txHash === txHash && utxo.outputIndex === outputIndex,
        ),
      ),
    );
    const signed: string[] = [];
    fixture.chain.mockImplementation(async (txHash: string) =>
      landed.has(txHash)
        ? { kind: "confirmed", txHash }
        : expired.has(txHash)
          ? { kind: "not_found" }
          : { kind: "pending", txHash },
    );
    const required = (actionId: string, actionKind: string) => ({
      kind: "action_required" as const,
      action: { actionId, input: { actionKind } },
    });
    vi.mocked(fixture.adapter.observe).mockImplementation(async () =>
      !landed.has(init.transactionHash)
        ? required("init", "proof.init")
        : !landed.has(last.transactionHash)
          ? required("last", "proof.step")
          : { kind: "pending", reason: "awaiting the removal's finality" },
    );
    vi.mocked(fixture.adapter.preflight).mockImplementation(
      async ({ action }) => {
        const tx =
          steps[action.actionId]![
            signed.filter((id) => id === action.actionId).length
          ]!;
        signed.push(action.actionId);
        return preflightOf(action, tx);
      },
    );
    const submit: FraudProofFamilyWorkflowAdapter["submit"] = async ({
      preflight,
    }) => ({ kind: "submitted", txHash: preflight.txHash });
    vi.mocked(fixture.adapter.submit).mockImplementation(submit);
    const supervisor = test.createSupervisor();

    // Admitted late: step 1 is signed and sent, and stays in flight.
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(signed).toEqual(["init"]);

    // Its validity lapses unlanded: the next observation signs it again;
    // that attempt lands, and the last step needs a submission of its own,
    // signed and included before maturity.
    expired.add(lapsed.transactionHash);
    vi.mocked(fixture.adapter.submit).mockImplementation(async (input) => {
      land(
        [init, last].find(
          ({ transactionHash }) => transactionHash === input.preflight.txHash,
        )!,
      );
      return await submit(input);
    });
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expect(signed).toEqual(["init", "init", "last"]);
    const events = (
      await fixture.objectiveJournal().load(fixture.initial.workflowId)
    ).map(({ event }) => event);
    expect(events).toContainEqual(
      expect.objectContaining({
        kind: "confirmed",
        actionId: "last",
        txHash: last.transactionHash,
      }),
    );
    expect(Date.now()).toBeLessThan(maturityAtMs);
    expect(test.readiness().map(({ reason }) => reason)).not.toContain(
      WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
    );
  });
};
