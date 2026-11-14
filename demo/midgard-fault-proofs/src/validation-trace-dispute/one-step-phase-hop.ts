/** The hop by which a phase semantic resolver verifies its step and awards
 * it (one-step-phase.ts). */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

import { constr, continued, integer, stepDatum } from "./one-step-data.js";
import {
  fieldItemCertificateIndexCbor,
  fieldItemRouteCbor,
  fieldItemRouteReferences,
  type ValidationTraceFieldItemRouteReads,
} from "./one-step-field-item-route.js";
import type {
  ValidationTracePhaseYieldListReads,
  ValidationTracePhaseYieldReads,
} from "./one-step-phase-yields.js";

/** The semantic resolver's hop of the thread of prover `prover`: under its
 * action `action`, it verifies the transition `transitionCbor` beside the
 * auxiliary evidence whose fields are `evidenceCbors`, and the yield
 * `itemYield` invokes beside them (its reference-input index, then its
 * kind, if it has one), or the yields `yieldList`, whose reference-input
 * indices follow them as one list or each as a field of its own, if any,
 * and awards the step. A stage reading its field's item through a routed
 * door takes `route` beside the evidence: in its redeemer, as the
 * certificate's bare index, or as the item yield's withdraw redeemer. */
export const phaseSemanticHop = ({
  prover,
  action = 0,
  transitionCbor,
  evidenceCbors,
  itemYield,
  yieldList,
  route,
}: {
  readonly prover: string;
  readonly action?: number;
  readonly transitionCbor: string;
  readonly evidenceCbors: readonly string[];
  readonly itemYield?: ValidationTracePhaseYieldReads;
  readonly yieldList?: ValidationTracePhaseYieldListReads;
  readonly route?: ValidationTraceFieldItemRouteReads | undefined;
}) => {
  const reads =
    itemYield === undefined
      ? yieldList
      : { references: [itemYield.reference], scripts: [itemYield.script] };
  const indexOf = (context: RedeemerContext, reference: UTxO) =>
    SDK.requireReferenceInputIndex(context, reference, "phase yield");
  const carrier = route?.read.carrier;
  if (carrier === "yield" && itemYield === undefined)
    throw new Error(
      `validation-trace ${route!.read.holder} takes its route in an item yield it does not invoke`,
    );
  const routed = (context: RedeemerContext): readonly string[] =>
    route === undefined || carrier === "yield"
      ? []
      : carrier === "redeemer"
        ? [fieldItemRouteCbor(route, context)]
        : [fieldItemCertificateIndexCbor(route, context)];
  const certificate = fieldItemRouteReferences(route);
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
          ...routed(context),
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
            : yieldList.fields === true
              ? yieldList.references.map((reference) =>
                  integer(indexOf(context, reference)),
                )
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
      ? certificate.length === 0
        ? {}
        : { reads: { references: [...certificate], yields: [] } }
      : {
          reads: {
            references: [...reads.references, ...certificate],
            yields: [...reads.scripts],
            ...(carrier === "yield" && itemYield !== undefined
              ? {
                  routedYield: {
                    script: itemYield.script,
                    redeemer: (context: RedeemerContext) =>
                      fieldItemRouteCbor(route!, context),
                  },
                }
              : {}),
          },
        }),
  };
};
