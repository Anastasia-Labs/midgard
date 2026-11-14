/** The custody routes of the `receivePurposeLanguage` thread after the
 * shared source walk: the family entry's contradiction straight to the
 * terminal, or the shared receive route to one receive ordinal and the
 * family lookup, which decides the fact or opens the shared claimed field-8
 * scan. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DirectPurposeTransition,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  declarationPass,
  receiveOutputsPass,
} from "../direct-purpose-family/domain-passes.js";
import {
  boundIndex,
  pointerScan,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import { RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL } from "./contracts.js";

/** Twin of `entry.context_v1`: the binding, no observation, the leaf's own
 * purpose. */
export const receivePurposeLanguageContext = (
  inputs: RouteInputs,
): SDK.DirectClaimContext => ({
  bound: boundIndex(inputs),
  observed: null,
  target: "LeafPurpose",
});

export type ReceivePurposeLanguageRoute = {
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
};

const finalDatum = (inputs: RouteInputs, final: SDK.DirectClaimFinalState) =>
  stepDatum(inputs.prover, final, SDK.DirectClaimFinalDatum);

/** A recorded coordinate that is not a receive coordinate: the entry pays
 * the terminal its contradiction. */
export const receivePurposeLanguageContradictionRoute = (
  inputs: RouteInputs,
): ReceivePurposeLanguageRoute => {
  const final = SDK.receivePurposeLanguageContradiction(boundIndex(inputs));
  return {
    final,
    transitions: [
      {
        label: "entry contradict",
        role: "familyEntry",
        next: "terminal",
        datum: finalDatum(inputs, final),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Contradict: indices }] },
            SDK.ReceivePurposeLanguageEntryRedeemer,
          ),
      },
    ],
  };
};

/** The entry's observation through the shared receive route to the
 * selection of receive ordinal `ordinal`; the selection carries no purpose
 * when the ordinal is past the last receive purpose. */
const receiveSelection = async (
  inputs: RouteInputs,
  ordinal: bigint,
): Promise<{
  readonly selection: SDK.DirectPurposeSelection;
  readonly transitions: readonly DirectPurposeTransition[];
}> => {
  const { prover } = inputs;
  const outputs = await receiveOutputsPass(inputs, {
    label: "receive",
    grammar: "receiveGrammar",
    outputs: "receiveOutputs",
    datum: (scan) => stepDatum(prover, scan, SDK.DirectReceiveOutputDatum),
    grammarRedeemer: ({ opening, ...rest }) =>
      Data.to(
        { Continue: [{ ...rest, outputs_opening: opening }] },
        SDK.DirectReceiveGrammarRedeemer,
      ),
    scanRedeemer: ({ indices, opening, checkpoint_bytes, witnesses }) =>
      Data.to(
        {
          Continue: [
            {
              Scan: {
                ...indices,
                outputs_opening: opening,
                checkpoint_bytes,
                witnesses: [...witnesses],
              },
            },
          ],
        },
        SDK.DirectReceiveOutputRedeemer,
      ),
  });
  const declaration = declarationPass(inputs, outputs.evidence, ordinal, {
    label: "receive",
    role: "receiveDeclaration",
    datum: (scan) => stepDatum(prover, scan, SDK.DirectReceiveDeclarationDatum),
    scanRedeemer: (indices, entries) =>
      Data.to(
        { Continue: [{ Scan: { ...indices, entries: [...entries] } }] },
        SDK.DirectReceiveDeclarationRedeemer,
      ),
  });
  const selection: SDK.DirectPurposeSelection = {
    ...boundIndex(inputs),
    coordinate: { kind: 3n, index: ordinal },
    purpose: declaration.state.selected?.purpose ?? null,
  };
  return {
    selection,
    transitions: [
      {
        label: "entry observe",
        role: "familyEntry",
        next: "receiveStart",
        datum: stepDatum(
          prover,
          boundIndex(inputs),
          SDK.DirectReceiveStartDatum,
        ),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Observe: indices }] },
            SDK.ReceivePurposeLanguageEntryRedeemer,
          ),
      },
      {
        label: "receive start",
        role: "receiveStart",
        next: "receiveGrammar",
        datum: outputs.initial,
        fields: [2],
        redeemer: ({ indices, open }) =>
          Data.to(
            { Continue: [{ ...indices, outputs_opening: open(2) }] },
            SDK.DirectReceiveStartRedeemer,
          ),
      },
      ...outputs.transitions,
      {
        label: "receive outputs finish",
        role: "receiveOutputs",
        next: "receiveFinish",
        datum: stepDatum(prover, outputs.state, SDK.DirectReceiveFinishDatum),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.DirectReceiveOutputRedeemer,
          ),
      },
      {
        label: "receive exhaustion",
        role: "receiveFinish",
        next: "receiveDeclaration",
        datum: declaration.initial,
        fields: [2],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  ...indices,
                  outputs_opening: open(2),
                  checkpoint_bytes: outputs.exhausted,
                  target_ordinal: ordinal,
                },
              ],
            },
            SDK.DirectReceiveFinishRedeemer,
          ),
      },
      ...declaration.transitions,
      {
        label: "receive declaration finish",
        role: "receiveDeclaration",
        next: "sourceLookup",
        datum: stepDatum(prover, selection, SDK.DirectSourceLookupDatum),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.DirectReceiveDeclarationRedeemer,
          ),
      },
    ],
  };
};

/** Twin of the family lookup over `selection`: an absent purpose resolves,
 * an absent or non-PlutusV3 source is decided by lookup, and a PlutusV3
 * source opens the claimed field-8 scan. */
const lookupRoute = async (
  inputs: RouteInputs,
  selection: SDK.DirectPurposeSelection,
): Promise<ReceivePurposeLanguageRoute> => {
  const context = receivePurposeLanguageContext(inputs);
  const purpose = selection.purpose;
  if (purpose === null) {
    const final: SDK.DirectClaimFinalState = {
      context,
      claim: { ImpossiblePurpose: { coordinate: selection.coordinate } },
    };
    return {
      final,
      transitions: [
        {
          label: "lookup resolve",
          role: "sourceLookup",
          next: "terminal",
          datum: finalDatum(inputs, final),
          fields: [],
          redeemer: ({ indices }) =>
            Data.to(
              { Continue: [{ Resolve: indices }] },
              SDK.DirectClaimAdjudicateRedeemer,
            ),
        },
      ],
    };
  }
  const { selected: source, proof } = await inputs.evidence.lookup(
    purpose.script_hash,
  );
  const authenticated: SDK.DirectSourceSelection = { selection, source };
  if (
    source === null ||
    source.language_tag !== SDK.RECEIVE_PURPOSE_LANGUAGE_PLUTUS_V3_TAG
  ) {
    const final: SDK.DirectClaimFinalState = {
      context,
      claim:
        source === null
          ? { SourceAbsent: { selection: authenticated } }
          : { SourcePresent: { selection: authenticated } },
    };
    return {
      final,
      transitions: [
        {
          label: "lookup",
          role: "sourceLookup",
          next: "terminal",
          datum: finalDatum(inputs, final),
          fields: [],
          redeemer: ({ indices }) =>
            Data.to(
              { Continue: [{ Lookup: { ...indices, source, proof } }] },
              SDK.DirectClaimAdjudicateRedeemer,
            ),
        },
      ],
    };
  }
  let final: SDK.DirectClaimFinalState | undefined;
  const scan = pointerScan(inputs, {
    label: "claimed",
    authenticated,
    grammar: "claimedRedeemerGrammar",
    scan: "claimedRedeemerScan",
    datum: (state) =>
      stepDatum(
        inputs.prover,
        { context, scan: state },
        SDK.DirectClaimedScanDatum,
      ),
    finish: (decision) => {
      final = {
        context,
        claim: decision.redeemer_missing
          ? { RedeemerAbsent: { decision } }
          : { RedeemerPresent: { decision } },
      };
      return { next: "terminal", datum: finalDatum(inputs, final) };
    },
  });
  if (final === undefined)
    throw new Error(
      `${RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL}: the claimed scan did not finish`,
    );
  return {
    final,
    transitions: [
      {
        label: "lookup scan",
        role: "sourceLookup",
        next: "claimedRedeemerGrammar",
        datum: scan.initial,
        fields: [8],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                { LookupScan: { ...indices, source, proof, opening: open(8) } },
              ],
            },
            SDK.DirectClaimAdjudicateRedeemer,
          ),
      },
      ...scan.transitions,
    ],
  };
};

/** The entry's observation of receive ordinal `ordinal` to the terminal's
 * input. */
export const receivePurposeLanguageReceiveRoute = async (
  inputs: RouteInputs,
  ordinal: bigint,
): Promise<ReceivePurposeLanguageRoute> => {
  if (ordinal < 0n)
    throw new Error(
      `${RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL}: a receive ordinal is non-negative`,
    );
  const { selection, transitions } = await receiveSelection(inputs, ordinal);
  const lookup = await lookupRoute(inputs, selection);
  return {
    final: lookup.final,
    transitions: [...transitions, ...lookup.transitions],
  };
};
