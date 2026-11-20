/**
 * The sweep of an ended workflow's proof materials: it takes only the
 * materials of transactions its own journal confirmed, spends them back in
 * pages of at most `PROOF_MATERIALS_SWEEP_PAGE_INPUTS`, each paying one
 * plain coin to the funding address with no collateral, and holds the
 * ending until each sweep is final at release depth. A sweep that does not
 * build, or is not yet final, holds the ending rather than changing it.
 */
import { Proof } from "@al-ft/midgard-sdk/core";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isPlainFundingOutput } from "../src/funding/plain-funding-output.js";
import { proofMaterialsAddress } from "../src/funding/proof-materials-address.js";
import { splitProofIntoChunkDatums } from "../src/proof-chunk-carriage.js";
import { resolvePublishedProofChunks } from "../src/publish-proof-chunks.js";
import type { FraudProofWorkflowAdapterContext } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../src/workflow/orchestrator.js";
import {
  PROOF_MATERIALS_SWEEP_OUTSTANDING,
  PROOF_MATERIALS_SWEEP_PAGE_INPUTS,
  proofMaterialsSweepInputs,
} from "../src/workflow/proof-materials-sweep.action.js";
import {
  lucidProofMaterialsSweepPort,
  withProofMaterialsSweep,
} from "../src/workflow/proof-materials-sweep.js";
import type { SignedTransactionRecoveryObservation } from "../src/workflow/signed-transaction-reconciliation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const ADA = 1_000_000n;
const FINAL_DEPTH = 10;

/**
 * A prover whose funding address (its enterprise key address) holds plain
 * coins, and a funder (the same key's base wallet) that pays materials to
 * its proof-materials address.
 */
const prover = async () => {
  const account = generateEmulatorAccount({ lovelace: 20_000n * ADA });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const funder = await Lucid(emulator, "Custom");
  funder.selectWallet.fromSeed(account.seedPhrase);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase, {
    addressType: "Enterprise",
  });
  const fundingAddress = await lucid.wallet().address();
  const keyHash = paymentCredentialOf(fundingAddress).hash;
  const materialsAddress = proofMaterialsAddress("Custom", keyHash);
  const submit = async (
    by: LucidEvolution,
    build: (tx: ReturnType<LucidEvolution["newTx"]>) => typeof tx,
  ) => {
    const signed = await (await build(by.newTx()).complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    emulator.awaitBlock();
    return txHash;
  };
  await submit(funder, (tx) =>
    tx.pay.ToAddress(fundingAddress, { lovelace: 5_000n * ADA }),
  );
  /** One transaction paying `count` materials, as a workflow's chunks. */
  const publish = async (count: number, fill: string) =>
    await submit(funder, (tx) => {
      let paying = tx;
      for (let index = 0; index < count; index += 1)
        paying = paying.pay.ToAddressWithData(
          materialsAddress,
          {
            kind: "inline",
            value: Data.to(`${fill}${index.toString(16).padStart(4, "0")}`),
          },
          { lovelace: 2n * ADA },
        );
      return paying;
    });
  return {
    emulator,
    lucid,
    fundingAddress,
    materialsAddress,
    publish,
    at: async (address: string) => await emulator.getUtxos(address),
  };
};

/** Journal entries of a workflow that confirmed each of `txHashes`. */
const confirmedEntries = (txHashes: readonly string[]) =>
  txHashes.flatMap((txHash, index) => [
    {
      recordedAt: new Date(0).toISOString(),
      sequence: index * 2,
      event: {
        kind: "submission_intent",
        actionId: `publish:${index}`,
        actionInput: { stage: "publish" },
        txHash,
      },
    },
    {
      recordedAt: new Date(0).toISOString(),
      sequence: index * 2 + 1,
      event: { kind: "confirmed", actionId: `publish:${index}`, txHash },
    },
  ]);

/** A base family that ends as `ending`. */
const endingAs = (
  ending: FraudProofWorkflowObservation,
): FraudProofFamilyWorkflowAdapter =>
  ({
    category: "doubleSpend",
    observe: async () => ending,
  }) as never;

const CONFLICT: FraudProofWorkflowObservation = {
  kind: "conflict",
  reason: "a rollback replaced the thread",
};
const completedAt = (confirmationDepth: number) =>
  ({
    kind: "completed",
    terminal: { observedAt: { confirmationDepth } },
  }) as never as FraudProofWorkflowObservation;

/**
 * The sweep over `scene`'s prover, `included` saying where each signed
 * sweep stands: included and final unless it says otherwise.
 */
const sweeping = (
  scene: Awaited<ReturnType<typeof prover>>,
  ending: FraudProofWorkflowObservation,
  included: (txHash: string) => "final" | "shallow" | "absent" = () => "final",
) =>
  withProofMaterialsSweep({
    base: endingAs(ending),
    final: (depth) => depth >= FINAL_DEPTH,
    port: () =>
      lucidProofMaterialsSweepPort({
        lucid: scene.lucid,
        fundingAddress: scene.fundingAddress,
        observeSignedTransaction: async (input) => {
          const where = included(input.transactionHash);
          return {
            ...input,
            status: where === "absent" ? "pending" : "included",
            ...(where === "absent"
              ? {}
              : {
                  inclusionPoint: {
                    blockNo: where === "final" ? "1" : "9",
                  },
                }),
            releaseFinalPoint: { blockNo: "5" },
          } as never as SignedTransactionRecoveryObservation;
        },
      }),
  });

const contextOf = (
  entries: readonly object[],
  extra: Partial<FraudProofWorkflowAdapterContext> = {},
): FraudProofWorkflowAdapterContext =>
  ({
    identity: { category: "doubleSpend" },
    workflowId: "workflow-a",
    artifact: {},
    entries,
    ...extra,
  }) as never;

/** Builds and lands the sweep `action`, journaling it into `entries`. */
const landSweep = async (
  scene: Awaited<ReturnType<typeof prover>>,
  adapter: FraudProofFamilyWorkflowAdapter,
  entries: object[],
  action: FraudProofWorkflowAction,
) => {
  const preflight = await adapter.preflight({
    ...contextOf(entries),
    action,
  } as never);
  const signed = workflowPreflightTransaction(preflight)!;
  const txHash = await signed.submit();
  scene.emulator.awaitBlock();
  entries.push(
    {
      recordedAt: new Date(0).toISOString(),
      sequence: entries.length,
      event: {
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        txHash,
        durableRecovery: { signedTransactionCborHex: signed.toCBOR() },
      },
    },
    {
      recordedAt: new Date(0).toISOString(),
      sequence: entries.length + 1,
      event: { kind: "confirmed", actionId: action.actionId, txHash },
    },
  );
  return { signed, txHash };
};

describe("the proof-materials sweep", () => {
  it("spends back only its own workflow's materials, paying one plain coin to funding, and leaves another workflow's live", async () => {
    const scene = await prover();
    const ours = await scene.publish(3, "aa");
    const theirs = await scene.publish(2, "aa");
    const entries: object[] = confirmedEntries([ours]);
    const adapter = sweeping(scene, CONFLICT);

    const observed = await adapter.observe(contextOf(entries));
    expect(observed.kind).toBe("action_required");
    const action = (observed as { action: FraudProofWorkflowAction }).action;
    expect(
      proofMaterialsSweepInputs(action).map((outRef) => outRef.split("#")[0]),
    ).toEqual([ours, ours, ours]);
    const { signed, txHash } = await landSweep(scene, adapter, entries, action);

    const body = signed.toTransaction().body();
    expect(body.collateral_inputs()).toBeUndefined();
    expect(body.outputs().len()).toBe(1);
    const [paid] = (await scene.at(scene.fundingAddress)).filter(
      (utxo) => utxo.txHash === txHash,
    );
    expect(isPlainFundingOutput(paid!)).toBe(true);
    expect(paid!.datum ?? paid!.datumHash).toBeUndefined();
    expect(paid!.assets.lovelace).toBe(6n * ADA - body.fee());
    const left = await scene.at(scene.materialsAddress);
    expect(left.map((utxo) => utxo.txHash)).toEqual([theirs, theirs]);
    // Swept and final: the ending goes through unchanged.
    expect(await adapter.observe(contextOf(entries))).toEqual(CONFLICT);
  }, 300_000);

  it("pages more materials than one sweep holds across sweeps of at most a page each", async () => {
    const scene = await prover();
    const count = PROOF_MATERIALS_SWEEP_PAGE_INPUTS + 30;
    const ours = await scene.publish(count, "bb");
    const entries: object[] = confirmedEntries([ours]);
    const adapter = sweeping(scene, completedAt(FINAL_DEPTH));
    const pages: number[] = [];

    for (let turn = 0; turn < 3; turn += 1) {
      const observed = await adapter.observe(contextOf(entries));
      if (observed.kind !== "action_required") break;
      pages.push(proofMaterialsSweepInputs(observed.action).length);
      const { signed } = await landSweep(
        scene,
        adapter,
        entries,
        observed.action,
      );
      const transaction = signed.toTransaction();
      expect(transaction.body().outputs().len()).toBe(1);
      // A full page, with its one sig(key) lock and signature, stays within
      // a quarter of maxTxSize.
      expect(transaction.witness_set().native_scripts()?.len()).toBe(1);
      expect(transaction.to_cbor_hex().length / 2).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize / 4,
      );
    }

    expect(pages).toEqual([PROOF_MATERIALS_SWEEP_PAGE_INPUTS, 30]);
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
    expect(await adapter.observe(contextOf(entries))).toEqual(
      completedAt(FINAL_DEPTH),
    );
  }, 600_000);

  it("sweeps nothing before the workflow has ended, or before a completed ending is final", async () => {
    const scene = await prover();
    const ours = await scene.publish(1, "cc");
    const entries = confirmedEntries([ours]);
    const pending: FraudProofWorkflowObservation = {
      kind: "pending",
      reason: "the thread is moving",
    };

    expect(await sweeping(scene, pending).observe(contextOf(entries))).toEqual(
      pending,
    );
    expect(
      await sweeping(scene, completedAt(FINAL_DEPTH - 1)).observe(
        contextOf(entries),
      ),
    ).toEqual(completedAt(FINAL_DEPTH - 1));
  }, 300_000);

  it("holds the ending while a sweep does not build or is not yet final, and never changes it", async () => {
    const scene = await prover();
    const ours = await scene.publish(1, "dd");
    const entries: object[] = confirmedEntries([ours]);
    const first = await sweeping(scene, CONFLICT).observe(contextOf(entries));
    const action = (first as { action: FraudProofWorkflowAction }).action;

    const unbuilt = await sweeping(scene, CONFLICT).observe(
      contextOf(entries, { unbuiltActionIds: new Set([action.actionId]) }),
    );
    expect(unbuilt).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(PROOF_MATERIALS_SWEEP_OUTSTANDING),
    });

    const { txHash } = await landSweep(
      scene,
      sweeping(scene, CONFLICT),
      entries,
      action,
    );
    const shallow = await sweeping(scene, CONFLICT, () => "shallow").observe(
      contextOf(entries),
    );
    expect(shallow).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(`the sweep ${txHash} is not yet final`),
    });
    expect(await sweeping(scene, CONFLICT).observe(contextOf(entries))).toEqual(
      CONFLICT,
    );
  }, 300_000);

  it("sweeps a workflow under reconciliation-only authority too, with no funding coin", async () => {
    const scene = await prover();
    const ours = await scene.publish(2, "ee");
    const entries: object[] = confirmedEntries([ours]);
    const adapter = sweeping(scene, CONFLICT);

    const observed = await adapter.observe(
      contextOf(entries, { reconciliationOnly: true }),
    );
    expect(observed.kind).toBe("action_required");
    const action = (observed as { action: FraudProofWorkflowAction }).action;
    const preflight = await adapter.preflight({
      ...contextOf(entries, { reconciliationOnly: true }),
      action,
    } as never);
    const inputs = workflowPreflightTransaction(preflight)!
      .toTransaction()
      .body()
      .inputs();
    const spent: string[] = [];
    for (let index = 0; index < inputs.len(); index += 1) {
      const input = inputs.get(index);
      spent.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
    expect(spent.sort()).toEqual([...proofMaterialsSweepInputs(action)]);
  }, 300_000);

  it("never lets one workflow adopt another's proof chunk, and the other's sweep leaves its own intact", async () => {
    const scene = await prover();
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [datum] = splitProofIntoChunkDatums(proofCbor);
    const pay = async () => {
      const signed = await (
        await scene.lucid
          .newTx()
          .pay.ToAddressWithData(
            scene.materialsAddress,
            { kind: "inline", value: datum! },
            { lovelace: 2n * ADA },
          )
          .complete()
      ).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      scene.emulator.awaitBlock();
      return txHash;
    };
    const workflowA = await pay();
    const resolve = async (own: readonly string[]) =>
      await resolvePublishedProofChunks({
        lucid: scene.lucid,
        address: scene.materialsAddress,
        proofCbor,
        ownTransactions: new Set(own),
      });

    // B confirmed no publication: A's chunk with the same bytes is not B's.
    expect(await resolve([])).toBeUndefined();
    const workflowB = await pay();
    expect(
      (await resolve([workflowB]))!.map(({ utxo }) => utxo.txHash),
    ).toEqual([workflowB]);

    // A's sweep spends A's chunk alone; B's chunk still serves B.
    const entries: object[] = confirmedEntries([workflowA]);
    const adapter = sweeping(scene, CONFLICT);
    const observed = await adapter.observe(contextOf(entries));
    await landSweep(
      scene,
      adapter,
      entries,
      (observed as { action: FraudProofWorkflowAction }).action,
    );
    expect(await resolve([workflowA])).toBeUndefined();
    expect(
      (await resolve([workflowB]))!.map(({ utxo }) => utxo.txHash),
    ).toEqual([workflowB]);
  }, 300_000);
});
