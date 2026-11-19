/**
 * The spend back of a workflow's proof materials once the workflow has ended.
 *
 * Every family keeps its in-flight proof materials (proof chunks, field
 * carriage, staged script-decode bytes, transition-trace data carriage) at
 * the prover's proof-materials address. Ours are the outputs there of the
 * transactions our own journal confirmed; nothing at that address is taken
 * for ours by its content, so one workflow never sweeps another's.
 *
 * Once the base workflow has ended (its terminal final at release depth, a
 * chain conflict, or its resolution by another record final at release
 * depth) each page of our live materials is spent back by our key alone, one
 * journaled action per page: no Plutus, no collateral (only the materials'
 * `sig(key)` lock and our signature), its fee taken from
 * the materials, the rest paid to the funding address as one plain coin.
 * Only a page whose remainder would fall under min-ADA adds a reserved
 * funding-address coin. The ending is held until every sweep is final at
 * release depth. A sweep that does not build is the run's recorded alarm and
 * is tried again on every run; it never changes how the workflow ended. A
 * workflow under reconciliation-only authority takes its sweep too.
 */
import {
  getAddressDetails,
  type LucidEvolution,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  proofMaterialsAddressOfFundingAddress,
  spendProofMaterials,
} from "../funding/proof-materials-address.js";
import {
  isFinalAtReleaseDepth,
  ownKeySignedBytes,
  withOwnKeyActions,
} from "../validation-trace-dispute/own-key-actions.js";
import type { FraudProofSignedTransactionRecovery } from "./l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowObservation,
} from "./orchestrator.js";
import { workflowConfirmedTransactions } from "./own-outputs.js";
import {
  assertProofMaterialsSweepOutput,
  isProofMaterialsSweepAction,
  PROOF_MATERIALS_SWEEP_OUTSTANDING,
  PROOF_MATERIALS_SWEEP_PAGE_INPUTS,
  PROOF_MATERIALS_SWEEP_STAGE,
  proofMaterialsSweepAction,
  proofMaterialsSweepInputs,
} from "./proof-materials-sweep.action.js";
import type { SignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";

type Rebroadcast = Parameters<
  FraudProofSignedTransactionRecovery["rebroadcastSignedTransaction"]
>[0];

const label = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** How a sweep reads our materials and builds a page's spend back. */
export type ProofMaterialsSweepPort = Readonly<{
  /** Our live outputs at the proof-materials address: those of `own`. */
  materials: (own: ReadonlySet<string>) => Promise<readonly UTxO[]>;
  /** The signed spend back of `page`; `reconciling` adds no funding coin. */
  build: (page: readonly UTxO[], reconciling: boolean) => Promise<TxSigned>;
  recovery: FraudProofSignedTransactionRecovery;
}>;

/** The sweep port over `lucid`, whose wallet is the restricted funding signer. */
export const lucidProofMaterialsSweepPort = ({
  lucid,
  fundingAddress,
  observeSignedTransaction,
}: {
  readonly lucid: LucidEvolution;
  readonly fundingAddress: string;
  readonly observeSignedTransaction: FraudProofSignedTransactionRecovery["observeSignedTransaction"];
}): ProofMaterialsSweepPort => {
  const materialsAddress =
    proofMaterialsAddressOfFundingAddress(fundingAddress);
  const paymentKeyHash =
    getAddressDetails(fundingAddress).paymentCredential!.hash;
  // Each page spends by its sig(key) lock: one script, our signature.
  const spend = async (page: readonly UTxO[], coinSelection: boolean) =>
    await (
      await spendProofMaterials(
        lucid.newTx().collectFrom([...page]),
        paymentKeyHash,
      ).complete({ coinSelection, changeAddress: fundingAddress })
    ).sign
      .withWallet()
      .complete();
  return Object.freeze({
    materials: async (own: ReadonlySet<string>) =>
      (await lucid.utxosAt(materialsAddress))
        .filter(
          (utxo) => utxo.address === materialsAddress && own.has(utxo.txHash),
        )
        .sort((left, right) =>
          left.txHash === right.txHash
            ? left.outputIndex - right.outputIndex
            : left.txHash < right.txHash
              ? -1
              : 1,
        ),
    build: async (page: readonly UTxO[], reconciling: boolean) => {
      let signed: TxSigned;
      try {
        signed = await spend(page, false);
      } catch (cause) {
        // A page whose remainder falls under min-ADA tops up from the
        // reserved funding coins; none under reconciliation-only authority.
        // The runtime bound refuses a top-up the page did not need.
        if (reconciling) throw cause;
        signed = await spend(page, true);
      }
      assertProofMaterialsSweepOutput(signed, fundingAddress);
      return signed;
    },
    recovery: Object.freeze({
      observeSignedTransaction,
      rebroadcastSignedTransaction: async ({
        transactionHash,
        signedTransactionCborHex,
        authorizeResubmission,
      }: Rebroadcast) => {
        await authorizeResubmission({
          transactionHash,
          signedTransactionCborHex,
        });
        const provider = lucid.config().provider;
        if (provider === undefined)
          throw new Error("the sweep's Lucid has no provider");
        return await provider.submitTx(signedTransactionCborHex);
      },
    }),
  });
};

/**
 * `base` with the sweep of its proof materials in front of its ending.
 * `final(depth)` says whether an ending observed `depth` blocks deep is
 * final at release depth; `port` gives none for a workflow without a
 * production funding signer, which then sweeps nothing.
 */
export const withProofMaterialsSweep = ({
  base,
  port,
  final,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly port: () => ProofMaterialsSweepPort | undefined;
  readonly final: (depth: number) => boolean;
}): FraudProofFamilyWorkflowAdapter => {
  const pages = new Map<string, readonly UTxO[]>();
  const keyOf = (workflowId: string, actionId: string) =>
    `${workflowId}\u0000${actionId}`;
  const required = (): ProofMaterialsSweepPort => {
    const sweeping = port();
    if (sweeping === undefined)
      throw new Error("proof-materials sweep has no funding signer");
    return sweeping;
  };
  const sweep = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    const sweeping = port();
    if (sweeping === undefined) return undefined;
    const outstanding: string[] = [];
    // A sweep of ours on the chain holds the ending until it is final, and
    // its page is spent whatever a lagging read still shows; one the chain
    // dropped leaves its page live, to be swept again.
    const swept = new Set<string>();
    for (const { event } of context.entries) {
      if (
        event.kind !== "submission_intent" ||
        event.actionInput.stage !== PROOF_MATERIALS_SWEEP_STAGE
      )
        continue;
      const signed = ownKeySignedBytes(event.durableRecovery);
      if (
        signed === undefined ||
        !context.entries.some(
          (entry) =>
            entry.event.kind === "confirmed" &&
            entry.event.txHash === event.txHash,
        )
      )
        continue;
      const observed = await sweeping.recovery.observeSignedTransaction({
        transactionHash: event.txHash,
        signedTransactionCborHex: signed,
      });
      if (observed.status !== "included") continue;
      for (const outRef of proofMaterialsSweepInputs({
        actionId: event.actionId,
        input: event.actionInput,
      }))
        swept.add(outRef);
      if (!isFinalAtReleaseDepth(observed))
        outstanding.push(
          `the sweep ${event.txHash} is not yet final at release depth`,
        );
    }
    const live = (
      await sweeping.materials(workflowConfirmedTransactions(context.entries))
    ).filter((utxo) => !swept.has(label(utxo)));
    if (live.length > 0) {
      const page = live.slice(0, PROOF_MATERIALS_SWEEP_PAGE_INPUTS);
      const action = proofMaterialsSweepAction({
        category: context.identity.category,
        outRefs: page.map(label),
      });
      if (context.unbuiltActionIds?.has(action.actionId) !== true) {
        pages.set(keyOf(context.workflowId, action.actionId), page);
        return { kind: "action_required", action };
      }
      outstanding.push(
        `${live.length.toString()} proof materials do not spend back`,
      );
    }
    return outstanding.length === 0
      ? undefined
      : {
          kind: "pending",
          reason: `${PROOF_MATERIALS_SWEEP_OUTSTANDING}: ${outstanding.join("; ")}`,
        };
  };
  const atEnd = async (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ) =>
    observed.kind === "conflict" ||
    (observed.kind === "completed" &&
      final(observed.terminal.observedAt.confirmationDepth))
      ? await sweep(context)
      : undefined;
  return withOwnKeyActions({
    base,
    label: "proof-materials sweep",
    family: "proof-materials",
    own: {
      owns: isProofMaterialsSweepAction,
      next: atEnd,
      reclaim: atEnd,
      resolved: async (context, resolution) =>
        final(resolution.confirmationDepth) ? await sweep(context) : undefined,
      build: async (context, action) => {
        const page = pages.get(keyOf(context.workflowId, action.actionId));
        if (page === undefined)
          throw new Error(`${action.actionId} has no observed page`);
        if (
          page.map(label).join(",") !==
          proofMaterialsSweepInputs(action).join(",")
        )
          throw new Error(`${action.actionId} changed its page`);
        return {
          signed: await required().build(
            page,
            context.reconciliationOnly === true,
          ),
        };
      },
    },
    signedRecovery: Object.freeze({
      observeSignedTransaction: async (input: SignedWorkflowTransaction) =>
        await required().recovery.observeSignedTransaction(input),
      rebroadcastSignedTransaction: async (input: Rebroadcast) =>
        await required().recovery.rebroadcastSignedTransaction(input),
    }),
  });
};
