/**
 * The spend back of proof materials no live workflow needs, once a workflow
 * has ended.
 *
 * Every family keeps its in-flight proof materials (proof chunks, field
 * carriage, staged script-decode bytes, transition-trace data carriage, CEK
 * operand staging) at the prover's proof-materials address, and every
 * workflow of the process finds them by content through one view of that
 * address (`proof-materials-view.ts`). A sweep takes what that view says no
 * live workflow needs and no live thread pins: ours, another workflow's
 * that has ended, or plain ada-only outputs a third party sent. One output
 * that serves two workflows is swept by the last of them to end. A copy of
 * another shape (a token, a reference script, a datum hash) is never taken.
 *
 * Once a workflow has ended (its terminal final at release depth, a chain
 * conflict, or its resolution by another record final at release depth) its
 * needs are released and each page of sweepable materials is spent back by
 * our key alone, one action per page: no Plutus, no collateral
 * (only the materials' `sig(key)` lock and our signature), its fee taken
 * from the materials, the rest paid to the funding address as one plain
 * coin. Only a page whose remainder would fall under min-ADA adds a reserved
 * funding-address coin. The ending waits on no sweep's finality: a sweep
 * that a rollback drops leaves its outputs live, and a later sweep takes
 * them. A sweep that does not build is the run's recorded alarm, and the
 * ending goes through; the next workflow to end takes those materials. A
 * sweep under a reconciliation permit (`proof-materials-sweep-run.ts`) adds
 * no funding-address coin.
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
import { withOwnKeyActions } from "../validation-trace-dispute/own-key-actions.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import type { FraudProofWorkflowIdentity } from "./identity.js";
import type { FraudProofWorkflowAdapterContext } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowObservation,
} from "./orchestrator.js";
import {
  assertProofMaterialsSweepOutput,
  isProofMaterialsSweepAction,
  PROOF_MATERIALS_SWEEP_PAGE_INPUTS,
  proofMaterialsSweepAction,
  proofMaterialsSweepInputs,
} from "./proof-materials-sweep.action.js";
import type {
  ProofMaterialsObjective,
  ProofMaterialsView,
} from "./proof-materials-view.js";
import { isFraudProofResolvedByAnotherRecordError } from "./resolved-by-another-record.js";

const label = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** How a sweep selects its pages and builds a page's spend back. */
export type ProofMaterialsSweepPort = Readonly<{
  /** The process's view of the proof-materials address. */
  view: ProofMaterialsView;
  /** The signed spend back of `page`; `reconciling` adds no funding coin. */
  build: (page: readonly UTxO[], reconciling: boolean) => Promise<TxSigned>;
}>;

/**
 * The sweep port over `lucid`, whose wallet is the restricted funding
 * signer, selecting through `view`, the view of the funding address's
 * proof-materials address.
 */
export const lucidProofMaterialsSweepPort = ({
  lucid,
  fundingAddress,
  view,
}: {
  readonly lucid: LucidEvolution;
  readonly fundingAddress: string;
  readonly view: ProofMaterialsView;
}): ProofMaterialsSweepPort => {
  if (view.address !== proofMaterialsAddressOfFundingAddress(fundingAddress))
    throw new Error(
      `the sweep's view reads ${view.address}, not the proof-materials address of ${fundingAddress}`,
    );
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
    view,
    build: async (page: readonly UTxO[], reconciling: boolean) => {
      let signed: TxSigned;
      try {
        signed = await spend(page, false);
      } catch (cause) {
        // A page whose remainder falls under min-ADA tops up from the
        // prover's funding coins; none under a reconciliation permit.
        // A page that builds alone never tops up.
        if (reconciling) throw cause;
        signed = await spend(page, true);
      }
      assertProofMaterialsSweepOutput(signed, fundingAddress);
      return signed;
    },
  });
};

/** The objective a header workflow serves; none for another target. */
const objectiveOf = (
  identity: FraudProofWorkflowIdentity,
): ProofMaterialsObjective | undefined =>
  identity.target.kind === "state_queue_header"
    ? { category: identity.category, headerHash: identity.target.headerHash }
    : undefined;

/**
 * `base` with the sweep of unneeded proof materials in front of its ending.
 * `final(depth)` says whether an ending observed `depth` blocks deep is
 * final at release depth; `port` gives none for a workflow without a
 * production funding signer, which then sweeps nothing. Each observation of
 * `base` that completes, with an observation or a resolution by another
 * record, is recorded in `view` (none before the workflow's signer is
 * known): a sweep opens only once every live objective has observed.
 */
export const withProofMaterialsSweep = ({
  base,
  port,
  view,
  final,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly port: () => ProofMaterialsSweepPort | undefined;
  readonly view: () => ProofMaterialsView | undefined;
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
  const observedBy = (context: FraudProofWorkflowAdapterContext): void => {
    const objective = objectiveOf(context.identity);
    if (objective !== undefined) view()?.observed(objective);
  };
  const observing: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async (context) => {
      try {
        const observed = await base.observe(context);
        observedBy(context);
        return observed;
      } catch (cause) {
        if (isFraudProofResolvedByAnotherRecordError(cause))
          observedBy(context);
        throw cause;
      }
    },
  };
  const sweep = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    const sweeping = port();
    if (sweeping === undefined) return undefined;
    // The workflow has ended: what it needed is free for the sweep, unless
    // another live workflow needs it too.
    sweeping.view.release(context.workflowId);
    const page = await sweeping.view.sweepable(
      PROOF_MATERIALS_SWEEP_PAGE_INPUTS,
    );
    if (page.length === 0) return undefined;
    const action = proofMaterialsSweepAction({
      category: context.identity.category,
      outRefs: page.map(label),
    });
    // A page that does not build is the run's alarm; the ending goes
    // through and a later sweep takes these materials.
    if (context.unbuiltActionIds?.has(action.actionId) === true)
      return undefined;
    pages.set(keyOf(context.workflowId, action.actionId), page);
    return { kind: "action_required", action };
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
    base: observing,
    label: "proof-materials sweep",
    family: "proof-materials",
    own: {
      owns: isProofMaterialsSweepAction,
      next: atEnd,
      resolved: async (context, resolution) =>
        final(resolution.confirmationDepth) ? await sweep(context) : undefined,
      build: async (context, action) => {
        const sweeping = required();
        const page = pages.get(keyOf(context.workflowId, action.actionId));
        if (page === undefined)
          throw new Error(`${action.actionId} has no observed page`);
        if (
          page.map(label).join(",") !==
          proofMaterialsSweepInputs(action).join(",")
        )
          throw new Error(`${action.actionId} changed its page`);
        // A need registered since the page was read takes it back.
        if (sweeping.view.unneeded(page).length !== page.length)
          throw new WorkflowActionChangedError(
            `${action.actionId} no longer sweeps its whole page`,
          );
        return {
          signed: await sweeping.build(
            page,
            context.reconciliationOnly === true,
          ),
        };
      },
    },
  });
};
