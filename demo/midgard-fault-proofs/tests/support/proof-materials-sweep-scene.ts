/**
 * A prover's funding and proof-materials addresses on an emulator, and the
 * sweep at a workflow's end over the process's one view of them.
 */
import {
  Data,
  datumToHash,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  paymentCredentialOf,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";

import { proofMaterialsAddress } from "../../src/funding/proof-materials-address.js";
import type { FraudProofWorkflowAdapterContext } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../../src/workflow/orchestrator.js";
import { proofMaterialsSweepInputs } from "../../src/workflow/proof-materials-sweep.action.js";
import {
  lucidProofMaterialsSweepPort,
  withProofMaterialsSweep,
} from "../../src/workflow/proof-materials-sweep.js";
import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
  type ProofMaterialsHolder,
  type ProofMaterialsObjective,
  type ProofMaterialsView,
} from "../../src/workflow/proof-materials-view.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";

export const ADA = 1_000_000n;
export const FINAL_DEPTH = 10;
export const HEADER_A = "a1".repeat(32);
export const HEADER_B = "b1".repeat(32);

export const label = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
export const datumOf = (fill: string, index: number) =>
  Data.to(`${fill}${index.toString(16).padStart(4, "0")}`);

/**
 * A prover whose funding address (its enterprise key address) holds plain
 * coins, and a funder (the same key's base wallet) that pays materials to
 * its proof-materials address.
 */
export const prover = async () => {
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
    build: (tx: ReturnType<LucidEvolution["newTx"]>) => typeof tx,
  ) => {
    const signed = await (await build(funder.newTx()).complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    emulator.awaitBlock();
    return txHash;
  };
  await submit((tx) =>
    tx.pay.ToAddress(fundingAddress, { lovelace: 5_000n * ADA }),
  );
  /** One transaction paying `count` plain materials, as a workflow's chunks. */
  const publish = async (count: number, fill: string) => {
    const datums = Array.from({ length: count }, (_, index) =>
      datumOf(fill, index),
    );
    const txHash = await submit((tx) => {
      let paying = tx;
      for (const datum of datums)
        paying = paying.pay.ToAddressWithData(
          materialsAddress,
          { kind: "inline", value: datum },
          { lovelace: 2n * ADA },
        );
      return paying;
    });
    return { txHash, datums };
  };
  /** Copies of `datum` a third party might plant, none of the strict shape. */
  const plantMisshapen = async (datum: string) => {
    const policy = scriptFromNative({ type: "sig", keyHash });
    const unit = `${mintingPolicyToId(policy)}${"74".repeat(4)}`;
    const token = await submit((tx) =>
      tx
        .mintAssets({ [unit]: 1n })
        .attach.MintingPolicy(policy)
        .addSignerKey(keyHash)
        .pay.ToAddressWithData(
          materialsAddress,
          { kind: "inline", value: datum },
          { lovelace: 2n * ADA, [unit]: 1n },
        ),
    );
    const referenceScript = await submit((tx) =>
      tx.pay.ToAddressWithData(
        materialsAddress,
        { kind: "inline", value: datum },
        { lovelace: 5n * ADA },
        policy,
      ),
    );
    const datumHash = await submit((tx) =>
      tx.pay.ToAddressWithData(
        materialsAddress,
        { kind: "hash", value: datumToHash(datum) },
        { lovelace: 2n * ADA },
      ),
    );
    return [token, referenceScript, datumHash];
  };
  return {
    emulator,
    lucid,
    fundingAddress,
    materialsAddress,
    publish,
    plantMisshapen,
    /** Pays the funding address one more plain coin of `lovelace`. */
    fund: async (lovelace: bigint) =>
      await submit((tx) => tx.pay.ToAddress(fundingAddress, { lovelace })),
    at: async (address: string) => await emulator.getUtxos(address),
  };
};

export type Scene = Awaited<ReturnType<typeof prover>>;

/** The process's one view of `scene`'s proof-materials address. */
export const viewOf = (
  scene: Scene,
  liveObjectives?: () => readonly ProofMaterialsObjective[] | undefined,
) =>
  createProofMaterialsView({
    address: scene.materialsAddress,
    reader: lucidProofMaterialsReader(scene.lucid, scene.materialsAddress),
    ...(liveObjectives === undefined ? {} : { liveObjectives }),
  });

export const holderOf = (workflowId: string, headerHash: string) =>
  ({
    workflowId,
    objective: { category: "doubleSpend", headerHash },
  }) satisfies ProofMaterialsHolder;

/** A base family that observes as `ending()` says. */
const endingAs = (
  ending: () => FraudProofWorkflowObservation,
): FraudProofFamilyWorkflowAdapter =>
  ({
    category: "doubleSpend",
    observe: async () => ending(),
  }) as never;

export const CONFLICT: FraudProofWorkflowObservation = {
  kind: "conflict",
  reason: "a rollback replaced the thread",
};
export const PENDING: FraudProofWorkflowObservation = {
  kind: "pending",
  reason: "the thread is moving",
};
export const completedAt = (confirmationDepth: number) =>
  ({
    kind: "completed",
    terminal: { observedAt: { confirmationDepth } },
  }) as never as FraudProofWorkflowObservation;

/** The sweep over `scene`'s prover, selecting through `view`. */
export const sweeping = (
  scene: Scene,
  view: ProofMaterialsView,
  ending: FraudProofWorkflowObservation | (() => FraudProofWorkflowObservation),
) =>
  withProofMaterialsSweep({
    base: endingAs(typeof ending === "function" ? ending : () => ending),
    final: (depth) => depth >= FINAL_DEPTH,
    view: () => view,
    port: () =>
      lucidProofMaterialsSweepPort({
        lucid: scene.lucid,
        fundingAddress: scene.fundingAddress,
        view,
      }),
  });

export const contextOf = (
  extra: Partial<FraudProofWorkflowAdapterContext> = {},
  workflowId = "workflow-a",
  headerHash = HEADER_A,
): FraudProofWorkflowAdapterContext =>
  ({
    identity: {
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash },
    },
    workflowId,
    artifact: {},
    entries: [],
    ...extra,
  }) as never;

export const actionOf = (observed: FraudProofWorkflowObservation) => {
  if (observed.kind !== "action_required")
    throw new Error(`expected a sweep, observed ${observed.kind}`);
  return observed.action;
};

/** Builds the sweep `action` and submits it; `land` adds a block. */
export const submitSweep = async (
  scene: Scene,
  adapter: FraudProofFamilyWorkflowAdapter,
  action: FraudProofWorkflowAction,
  { land = true, context = contextOf() } = {},
) => {
  const preflight = await adapter.preflight({ ...context, action } as never);
  const signed = workflowPreflightTransaction(preflight)!;
  const txHash = await signed.submit();
  if (land) scene.emulator.awaitBlock();
  return { signed, txHash };
};

export const sweptOutRefs = (action: FraudProofWorkflowAction) =>
  proofMaterialsSweepInputs(action);
