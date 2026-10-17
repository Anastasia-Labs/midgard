import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

import { carriedCountedMembership } from "../input-existence/counted-membership-carriage.js";
import type { PublishedProofChunk } from "../proof-chunk-carriage.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { DirectScriptMaterial } from "./material.js";

type CarriageOptions = Readonly<{
  directRewardAddress: string;
  publishedChunks?: readonly PublishedProofChunk[];
}>;

/** Both states are prepared from the same authenticated retained event. The
 * physical validators reauthenticate the counted openings independently. */
export const directBindingStates = (material: DirectScriptMaterial) => {
  const selected: SDK.DirectEventSelected = {
    ...material.sourceBound,
    step_index: material.eventMembership.value.step_index,
  };
  return { source: material.sourceBound, selected, bound: material.bound };
};

const exactThreadDatum = (thread: UTxO, expected: string, label: string) => {
  if (
    thread.datum === undefined ||
    thread.datum === null ||
    canonicalPlutusDataCbor(thread.datum) !== canonicalPlutusDataCbor(expected)
  )
    throw new Error(`${label}: thread datum differs from prepared custody`);
};

const layout = ({
  ctx,
  thread,
  unit,
  address,
  datum,
  label,
}: {
  readonly ctx: RedeemerContext;
  readonly thread: UTxO;
  readonly unit: string;
  readonly address: string;
  readonly datum: string;
  readonly label: string;
}) => {
  SDK.requireOwnSpendPurpose(ctx, thread, label);
  return {
    input_index: SDK.requireInputIndex(ctx, thread, label),
    output_index: SDK.requireUniqueOutputIndex(
      ctx.outputs,
      computationThreadOutputPredicate({ address, datum, unit }),
      label,
    ),
  };
};

/** The callback runs against Lucid's completed layout. It never invents a
 * withdrawal/reference position or accepts a different source datum. */
export const directBindEventRedeemer = ({
  material,
  fraudProver,
  ctx,
  thread,
  unit,
  nextAddress,
  carriage,
}: {
  readonly material: DirectScriptMaterial;
  readonly fraudProver: string;
  readonly ctx: RedeemerContext;
  readonly thread: UTxO;
  readonly unit: string;
  readonly nextAddress: string;
  readonly carriage: CarriageOptions;
}): string => {
  const states = directBindingStates(material);
  exactThreadDatum(
    thread,
    Data.to(
      { fraud_prover: fraudProver, data: states.source },
      SDK.DirectBindEventDatum,
    ),
    "direct event",
  );
  const datum = Data.to(
    { fraud_prover: fraudProver, data: states.selected },
    SDK.DirectBindPreStateDatum,
  );
  return Data.to(
    {
      Continue: [
        {
          ...layout({
            ctx,
            thread,
            unit,
            address: nextAddress,
            datum,
            label: "direct event output",
          }),
          event_opening: carriedCountedMembership({
            witness: material.eventMembership,
            ctx,
            ...carriage,
            label: "direct event membership",
          }),
        },
      ],
    },
    SDK.DirectBindEventRedeemer,
  );
};

export const directBindPreStateRedeemer = ({
  material,
  fraudProver,
  ctx,
  thread,
  unit,
  nextAddress,
  carriage,
}: {
  readonly material: DirectScriptMaterial;
  readonly fraudProver: string;
  readonly ctx: RedeemerContext;
  readonly thread: UTxO;
  readonly unit: string;
  readonly nextAddress: string;
  readonly carriage: CarriageOptions;
}): string => {
  const states = directBindingStates(material);
  exactThreadDatum(
    thread,
    Data.to(
      { fraud_prover: fraudProver, data: states.selected },
      SDK.DirectBindPreStateDatum,
    ),
    "direct transition",
  );
  const datum = Data.to(
    { fraud_prover: fraudProver, data: states.bound },
    SDK.DirectScriptBoundDatum,
  );
  return Data.to(
    {
      Continue: [
        {
          ...layout({
            ctx,
            thread,
            unit,
            address: nextAddress,
            datum,
            label: "direct transition output",
          }),
          transition_opening: carriedCountedMembership({
            witness: material.transitionMembership,
            ctx,
            ...carriage,
            label: "direct transition membership",
          }),
        },
      ],
    },
    SDK.DirectBindPreStateRedeemer,
  );
};
