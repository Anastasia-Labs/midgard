/** The hop by which a phase semantic resolver verifies its step and awards
 * it (one-step-phase.ts). */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

import { constr, continued, integer, stepDatum } from "./one-step-data.js";
import type {
  ValidationTracePhaseYieldListReads,
  ValidationTracePhaseYieldReads,
} from "./one-step-phase-yields.js";

/** The semantic resolver's hop of the thread of prover `prover`: under its
 * action `action`, it verifies the transition `transitionCbor` beside the
 * auxiliary evidence whose fields are `evidenceCbors`, and the yield
 * `itemYield` invokes beside them (its reference-input index, then its
 * kind, if it has one), or the yields `yieldList`, whose reference-input
 * indices follow them, if any, and awards the step. */
export const phaseSemanticHop = ({
  prover,
  action = 0,
  transitionCbor,
  evidenceCbors,
  itemYield,
  yieldList,
}: {
  readonly prover: string;
  readonly action?: number;
  readonly transitionCbor: string;
  readonly evidenceCbors: readonly string[];
  readonly itemYield?: ValidationTracePhaseYieldReads;
  readonly yieldList?: ValidationTracePhaseYieldListReads;
}) => {
  const reads =
    itemYield === undefined
      ? yieldList
      : { references: [itemYield.reference], scripts: [itemYield.script] };
  const indexOf = (context: RedeemerContext, reference: UTxO) =>
    SDK.requireReferenceInputIndex(context, reference, "phase yield");
  return {
    next: "award" as const,
    datum: stepDatum(prover, constr(0, [integer(1)])),
    redeemer: (
      inputIndex: bigint,
      outputIndex: bigint,
      context: RedeemerContext,
    ) =>
      continued(
        constr(action, [
          integer(inputIndex),
          integer(outputIndex),
          transitionCbor,
          ...evidenceCbors,
          ...(itemYield === undefined
            ? []
            : [
                integer(indexOf(context, itemYield.reference)),
                ...(itemYield.kind === undefined
                  ? []
                  : [integer(itemYield.kind)]),
              ]),
          ...(yieldList === undefined
            ? []
            : [
                Data.to(
                  yieldList.references.map((reference) =>
                    indexOf(context, reference),
                  ),
                ),
              ]),
        ]),
      ),
    proverOnly: false,
    ...(reads === undefined || reads.references.length === 0
      ? {}
      : {
          reads: {
            references: [...reads.references],
            yields: [...reads.scripts],
          },
        }),
  };
};
