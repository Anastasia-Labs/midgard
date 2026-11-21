/** What our staging (one-step-staging.ts) holds from every sweep. Each
 * thread of ours at the PrepareSelected move's holder will read the staged
 * runs its plan, recomputed from our trace, stages, so they are known from
 * the chain and our trace alone. Each observation registers them, and the
 * live outputs that hold them, as our proof-materials needs and pins before
 * it observes, with the staging outputs our resolve moves read; so the first
 * observation after a restart, which is what opens sweeping, already holds
 * them. The spend back
 * of our staging outputs (one-step-staging-sweep.ts) holds the same. */
import { planMidgardCekStaging } from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validationTraceOneStepContracts } from "./one-step-holders.js";
import { resolvingPlan } from "./one-step-resolving-plan.js";
import type {
  ValidationTraceStagingJournal,
  ValidationTraceStagingOutput,
} from "./one-step-staging-journal.js";
import type { ValidationTraceOwnViewOf } from "./own-view.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** A thread of ours at the PrepareSelected move's holder whose plan is a
 * staged step: the run datums its move reads. */
export type ValidationTraceStagingPlan = Readonly<{
  unit: string;
  outRef: string;
  datums: readonly string[];
}>;

/**
 * The holds of our staging. `plans` reads the staging plans of our threads
 * at the PrepareSelected move's holder, once per observation: a plan is a
 * function of the thread output and our trace, so each is computed once per
 * thread output for the process, and a thread whose step the runner cannot
 * plan yet (its trace not yet rebuilt) is planned again on the next read.
 * `register` makes every hold a need of the workflow's proof materials.
 */
export const validationTraceStagingHolds = ({
  lucid,
  signer,
  deployment,
  materialOf,
  ownView,
  stagings,
  usable,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
  readonly ownView: ValidationTraceOwnViewOf;
  readonly stagings: (
    context: FraudProofWorkflowAdapterContext,
  ) => Promise<ValidationTraceStagingJournal>;
  /** The staging output a PrepareSelected move would read for the run
   * `datumCbor`. */
  readonly usable: (
    known: ValidationTraceStagingJournal,
    datumCbor: string,
  ) => Promise<Readonly<{ staging: ValidationTraceStagingOutput }> | undefined>;
}) => {
  const planned = new Map<string, readonly string[]>();
  const planOf = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<readonly ValidationTraceStagingPlan[]> => {
    // A deployment without the one-step resolution stages nothing.
    const prepare = validationTraceOneStepContracts(deployment).find(
      ({ holder }) => holder === "prepare",
    )?.address;
    if (prepare === undefined) return [];
    const view = ownView(context);
    const material = materialOf(view.artifact().headerHash);
    const plans: ValidationTraceStagingPlan[] = [];
    for (const unit of await view.caseThreads()) {
      const [utxo] = (await lucid.utxosAtWithUnit(prepare, unit)).filter(
        ({ assets }) => assets[unit] === 1n,
      );
      if (utxo === undefined) continue;
      const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
      let datums = planned.get(outRef);
      if (datums === undefined) {
        const plan = await resolvingPlan({
          deployment,
          material,
          utxo,
          holder: "prepare",
          caseThread: undefined,
          carried: undefined,
          staging: async () => undefined,
        }).catch(() => undefined);
        if (plan === undefined) continue;
        datums =
          plan.kind === "core" && plan.core.staged !== undefined
            ? planMidgardCekStaging(plan.core.staged.operands).map(
                ({ datumCbor }) => datumCbor,
              )
            : [];
        planned.set(outRef, datums);
      }
      if (datums.length > 0) plans.push({ unit, outRef, datums });
    }
    return plans;
  };
  const read = new WeakMap<
    FraudProofWorkflowAdapterContext,
    Promise<readonly ValidationTraceStagingPlan[]>
  >();
  const plans = (context: FraudProofWorkflowAdapterContext) => {
    const known = read.get(context);
    if (known !== undefined) return known;
    const reading = planOf(context);
    read.set(context, reading);
    return reading;
  };
  return {
    plans,
    /** Registers, before an observation of the workflow `context`, each
     * staging output and run our threads hold as its proof-materials needs
     * and pins. A staging two threads read is held while either holds it. */
    register: async (context: FraudProofWorkflowAdapterContext) => {
      const known = await stagings(context);
      const datums = new Set<string>();
      const outRefs = new Set<string>();
      for (const { outRef, datumCbor } of await ownView(
        context,
      ).signedStagings()) {
        outRefs.add(outRef);
        if (datumCbor !== undefined) datums.add(datumCbor);
      }
      for (const plan of await plans(context))
        for (const datumCbor of plan.datums) {
          datums.add(datumCbor);
          const found = await usable(known, datumCbor);
          if (found !== undefined) outRefs.add(found.staging.outRef);
        }
      if (datums.size === 0 && outRefs.size === 0) return;
      const { view, holder } = proofMaterialsAccess({
        signer,
        lucid,
        holder: workflowProofMaterialsHolder(context),
      });
      view.need(holder, [...datums], [...outRefs]);
    },
  };
};
