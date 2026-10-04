import "./prover-funding-recovery.funding-recovery-across-authenticated-observation-refresh.js";

import {
  beginWorkflowFundingReservationAction,
  bindWorkflowPreflightTransaction,
  prepareWorkflowFundingReservationTransaction,
} from "@al-ft/midgard-fault-proofs";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherDeploymentAppliedScriptHashes } from "../../src/runtime/deployment-identity.js";
import {
  deploymentIdentity,
  key,
  setupFundingRecoveryFixture as setup,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  assertConfirmedFundingHistoryRetained,
  authorizeConfirmedFundingRefill,
} from "./prover-funding-recovery.authenticated-confirmed-refill.js";

it.each([
  "fraudProofValueNotPreservedUnionMint",
  "fraudProofDoubleWithdraw",
] as const)(
  "admits manifest-bound %s spending custody while refusing policy and unknown addresses",
  async (name) => {
    const context = await setup(false, false, true);
    context.useUnspentPendingInputs();
    const journal = context.bind(
      context.old,
      await context.createPermit(context.old, "1"),
    );
    const hashes = watcherDeploymentAppliedScriptHashes(deploymentIdentity);
    const action = { actionId: "init", input: { actionKind: "proof.init" } };
    await beginWorkflowFundingReservationAction({ journal, action });
    const input = context.plan.inputs.find(({ role }) => role === "funding")!;
    const build = (hash: string) => {
      const inputs = CML.TransactionInputList.new();
      const [txHash, index] = input.outRef.split("#");
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
      const outputs = CML.TransactionOutputList.new();
      const prototype = CML.TransactionOutput.new(
        CML.Address.from_bech32(
          credentialToAddress(
            deploymentIdentity.network,
            scriptHashToCredential(hash),
          ),
        ),
        CML.Value.from_coin(1_000_000n),
      );
      const custody = CML.TransactionOutput.new(
        prototype.address(),
        CML.Value.from_coin(CML.min_ada_required(prototype, 4310n)),
      );
      outputs.add(custody);
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(walletAddress),
          CML.Value.from_coin(
            BigInt(input.lovelace) - custody.amount().coin() - 1_000_000n,
          ),
        ),
      );
      const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
      const witnesses = CML.TransactionWitnessSet.new();
      const signatures = CML.VkeywitnessList.new();
      signatures.add(
        CML.Vkeywitness.new(
          key.to_public(),
          key.sign(CML.hash_transaction(body).to_raw_bytes()),
        ),
      );
      witnesses.set_vkeywitnesses(signatures);
      const tx = CML.Transaction.new(body, witnesses, true);
      const signed = {
        toHash: () => CML.hash_transaction(body).to_hex(),
        toTransaction: () => tx,
      } as Parameters<typeof bindWorkflowPreflightTransaction>[1];
      const preflight = bindWorkflowPreflightTransaction(
        { txHash: signed.toHash() },
        signed,
      );
      return prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: {
          ...context.handoff,
          preflight: { ...context.handoff.preflight, txHash: signed.toHash() },
          submissionIntent: {
            ...context.handoff.submissionIntent,
            txHash: signed.toHash(),
          },
        },
      });
    };
    for (const rejected of [
      hashes.fraudProofMint!,
      hashes.stateQueueCommitWithdraw!,
      "ab".repeat(28),
    ])
      await expect(build(rejected)).rejects.toThrow(
        "funding output escapes the governed contract roster",
      );
    await expect(build(hashes[name]!)).resolves.toBeUndefined();
    expect(
      (await context.records())[0]?.pendingTransition?.transactionHash,
    ).toMatch(/^[0-9a-f]{64}$/u);
  },
  120_000,
);

describe("additive deployed funding roster recovery", () => {
  it("refuses a stored policy outside the exact previously deployed roster", async () => {
    const test = await setup(true, false, false, false, "changed-role");
    const before = await test.records();
    await expect(test.createPermit(test.fresh, "2")).rejects.toThrow(
      "restored prover funding reservation identity mismatch",
    );
    expect(await test.records()).toEqual(before);
  });

  it("reuses an old-policy signed handoff through actual journal recovery without changing reservation identity", async () => {
    const test = await setup(true, false, false, false, true);
    const before = (await test.records())[0]!;
    const handoff = await test.store.readPendingHandoff({
      reservationId: before.reservationId,
    });
    expect(handoff).not.toBeNull();
    const admitted = await test.createPermit(test.fresh, "2");
    expect(await test.records()).toEqual([before]);
    expect(
      await test.store.readPendingHandoff({
        reservationId: before.reservationId,
      }),
    ).toEqual(handoff);
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const entries = await journal.load(test.initial.workflowId);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.txHash === test.transactionHash,
      ),
    ).toBe(true);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === test.transactionHash,
      ),
    ).toBe(true);
    const after = (await test.records())[0]!;
    expect(after.pendingTransition).toBeNull();
    for (const field of [
      "reservationId",
      "policyDigest",
      "reservationBasisDigest",
      "decisionDigest",
      "deploymentFingerprint",
    ] as const)
      expect(after[field]).toBe(before[field]);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    await test.restartStore();
    await test.createPermit(test.fresh, "3");
    expect(await test.records()).toEqual([after]);
  });

  it("refreshes stale wallet inputs under the original reservation identity after roster correction", async () => {
    const test = await setup(false, false, false, false, true);
    const journal = test.bind(
      test.fresh,
      await test.createPermit(test.fresh, "2"),
    );
    await test.run(journal);
    const before = (await test.records())[0]!;
    const spent = before.activeInputs.find(
      ({ role }) => role === "funding",
    )!.outRef;
    test.walletUtxos.splice(
      0,
      test.walletUtxos.length,
      ...test.walletUtxos.filter(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent,
      ),
    );
    await authorizeConfirmedFundingRefill(test, journal);
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "next", input: { actionKind: "proof.init" } },
    });
    const after = (await test.records())[0]!;
    expect(test.readWalletUtxos).toHaveBeenCalledOnce();
    expect(after.activeInputs.map(({ outRef }) => outRef)).not.toContain(spent);
    for (const field of [
      "reservationId",
      "policyDigest",
      "reservationBasisDigest",
    ] as const)
      expect(after[field]).toBe(before[field]);
    await assertConfirmedFundingHistoryRetained(test);
  });
});
