/** The validation-trace dispute runner at a script-sources redeemer
 * normalization step, on the emulator, over a deployment that publishes the
 * redeemer item chain: the phase resolver commits the step's transition and
 * its redeemer item evidence, the redeemer normalization semantic resolver
 * binds the item into its envelope, and the chain's holders verify the item
 * step (the traversal and outer normalizers on the current and the next
 * control, the source authenticator, the executor the step selects and the
 * execution settlement, which awards it)
 * (validation-trace-dispute-one-step-resolution-drive.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { type Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { validationTraceOneStepHolder } from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  PHASE_CHALLENGER_SUCCESSOR_HASH,
  PHASE_RESOLUTIONS,
} from "../../src/validation-trace-dispute/one-step-phase.js";
import {
  planValidationTraceOneStep,
  type ValidationTracePhasePlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import {
  SCRIPT_SOURCES_REDEEMER_SEMANTIC,
  type ValidationTraceScriptSourcesRedeemerHolder,
} from "../../src/validation-trace-dispute/one-step-script-sources-redeemer.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/** The redeemer normalization semantic resolver's index in its group. */
export const REDEEMER_NORMALIZATION_SEMANTIC = 28;

/** `RedeemerItemAuxiliary`'s constructor among the auxiliary witnesses. */
const REDEEMER_ITEM_AUXILIARY = 18;

/**
 * The drive of the successor's thread through the redeemer normalization
 * step after our trace's state `from`, whose item step the executor
 * `executor` verifies, set up with the thread standing at the semantic
 * resolver; over a failing transaction whose redeemer is
 * `failingRedeemerData` when given, and with the CEK context resolution
 * also installed when `contexts`.
 */
export const scriptSourcesRedeemerDrive = ({
  from,
  executor,
  failingRedeemerData,
  contexts = false,
}: {
  readonly from: number;
  readonly executor: ValidationTraceScriptSourcesRedeemerHolder;
  readonly failingRedeemerData?: Uint8Array;
  readonly contexts?: boolean;
}) => {
  const drive = resolutionDrive({
    from,
    at: SCRIPT_SOURCES_REDEEMER_SEMANTIC,
    path: [
      "scriptSourcesPrepare",
      SCRIPT_SOURCES_REDEEMER_SEMANTIC,
      "redeemerItemTraversalNormalizer",
      "redeemerItemOuterNormalizer",
      "redeemerItemTraversalNormalizer",
      "redeemerItemOuterNormalizer",
      "redeemerItemSourceAuthenticator",
      executor,
      "redeemerItemSettlement",
    ],
    labels: [
      "PrepareSelected scriptSources",
      "Redeemer bind envelope",
      "Redeemer normalize current traversal",
      "Redeemer normalize current outer",
      "Redeemer normalize next traversal",
      "Redeemer normalize next outer",
      "Redeemer authenticate source",
      `Redeemer execute ${executor}`,
      "Redeemer settle",
    ],
    installs: {
      phases: true,
      scriptSourcesRedeemers: true,
      ...(contexts ? { contexts } : {}),
      ...(failingRedeemerData === undefined ? {} : { failingRedeemerData }),
    },
    /** Our plan of the step, by the successor hash the semantic resolver
     * keeps. */
    planAt: async (
      dispute,
      holder,
      utxo,
    ): Promise<ValidationTracePhasePlan> => {
      const planning = await planValidationTraceOneStep({
        material: dispute.material,
        challengerHighHash: Data.from<string>(
          plutusConstrFieldCbor(utxo.datum!, [
            ...PHASE_CHALLENGER_SUCCESSOR_HASH[holder],
          ]),
        ),
        phases: ["scriptSources"],
        scriptSourcesRedeemers: true,
      });
      if (planning.kind !== "plan" || planning.plan.kind !== "phase")
        throw new Error("the thread's step is not a script-sources step");
      return planning.plan;
    },
  });

  return {
    setUp: drive.setUp,
    awards: drive.awards,
    pays: drive.pays,
    otherTransition: drive.otherTransition,

    /** Our plan sends the thread's step through the phase resolver to the
     * redeemer normalization semantic resolver, committing the transition
     * beside the redeemer item our trace verifies at the step. */
    routes: async () => {
      const plan = await drive.plan();
      const { scriptSources } = PHASE_RESOLUTIONS;
      expect(plan.stateIndex).toBe(from);
      expect(plan.preState.phase).toBe("scriptSources");
      expect(plan.resolverIndex).toBe(scriptSources.resolverIndex);
      expect(plan.semanticResolverIndex).toBe(REDEEMER_NORMALIZATION_SEMANTIC);
      expect(Number(Data.from<Constr<Data>>(plan.auxiliaryCbor).index)).toBe(
        REDEEMER_ITEM_AUXILIARY,
      );
      expect(plan.phase).toStrictEqual({
        phase: "scriptSources",
        prepare: scriptSources.prepare,
        semantic: SCRIPT_SOURCES_REDEEMER_SEMANTIC,
        evidenceCbors: [],
      });
      // The thread stands at the semantic resolver, the chain's entry.
      const { deployment } = drive.dispute();
      expect(
        validationTraceOneStepHolder(
          deployment,
          SCRIPT_SOURCES_REDEEMER_SEMANTIC,
        ).address,
      ).toBe(
        deployment.chain.scriptSourcesStageOneRedeemerStages.envelope
          .spendingScriptAddress,
      );
    },
  };
};
