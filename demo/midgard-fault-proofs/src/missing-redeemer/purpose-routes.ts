/** Observed purpose selection for each purpose kind: the purpose-select door
 * through the kind's bounded domain pass, ending at the transition that pays
 * the source lookup the authenticated selection. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { directPurposeAt } from "../direct-script-derivation/index.js";
import {
  type MissingRedeemerIndices,
  type MissingRedeemerTransition,
  stepDatum,
} from "./custody.js";
import {
  declarationPass,
  mintPass,
  missingRedeemerSpendEvidence,
  receiveOutputsPass,
  spendRankPass,
} from "./domain-passes.js";
import { boundIndex, type RouteInputs } from "./pointer-scan.js";

type Coordinate = SDK.DirectPurposeCoordinate;

type Indices = MissingRedeemerIndices;

const finishToLookup = (
  inputs: RouteInputs,
  selection: SDK.DirectPurposeSelection,
  transition: Pick<
    MissingRedeemerTransition,
    "label" | "role" | "fields" | "redeemer"
  >,
): MissingRedeemerTransition => ({
  ...transition,
  next: "sourceLookup",
  datum: stepDatum(inputs.prover, selection, SDK.DirectSourceLookupDatum),
});

const finishAction = (
  label: string,
  role: MissingRedeemerTransition["role"],
  encode: (indices: Indices) => string,
): Pick<
  MissingRedeemerTransition,
  "label" | "role" | "fields" | "redeemer"
> => ({
  label,
  role,
  fields: [],
  redeemer: ({ indices }) => encode(indices),
});

const spendSelection = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
  purpose: SDK.DirectPurpose,
): Promise<readonly MissingRedeemerTransition[]> => {
  const { prover } = inputs;
  const spend = await missingRedeemerSpendEvidence(inputs, coordinate.index);
  if (spend === null)
    throw new Error("missing-redeemer: observed spend is absent");
  const work = (progress: {
    readonly cursor: SDK.DirectFieldCursor;
    readonly rank: bigint;
    readonly complete: boolean;
  }): SDK.DirectSpendWorkState => ({
    source_index: inputs.evidence.index,
    cursor: progress.cursor,
    raw_item_index: spend.rawItemIndex,
    target: { script_hash: purpose.script_hash, subject: purpose.subject },
    rank: progress.rank,
    complete: progress.complete,
  });
  const datum = (state: SDK.DirectSpendWorkState) =>
    stepDatum(prover, state, SDK.DirectSpendWorkDatum);
  const rank = spendRankPass(inputs, {
    label: "observed spend scan",
    role: "spendScan",
    subject: spend.item,
    from: 0,
    state: work,
    datum,
    redeemer: (args) =>
      Data.to({ Continue: [{ Scan: { args } }] }, SDK.DirectSpendScanRedeemer),
  });
  if (rank.final.rank !== coordinate.index)
    throw new Error("missing-redeemer: observed spend rank differs");
  return [
    {
      label: "observe spend select",
      role: "purposeSelect",
      next: "spendSelect",
      datum: stepDatum(
        prover,
        boundIndex(inputs),
        SDK.DirectPurposeSelectDatum,
      ),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Spend: indices }] },
          SDK.DirectPurposeSelectRedeemer,
        ),
    },
    {
      label: "observed spend open",
      role: "spendSelect",
      next: "spendScan",
      datum: datum(work(rank.at(0))),
      fields: [0],
      membership: spend.membership,
      redeemer: (args) =>
        Data.to(
          {
            Continue: [
              {
                ...args.indices,
                raw_item_index: spend.rawItemIndex,
                opening: args.open(0),
                descriptor_cbor: spend.descriptorCbor,
                membership: spend.carriage(args),
              },
            ],
          },
          SDK.DirectSpendSelectRedeemer,
        ),
    },
    ...rank.transitions,
  ];
};

const mintSelection = (
  inputs: RouteInputs,
  coordinate: Coordinate,
): readonly MissingRedeemerTransition[] => {
  const { prover } = inputs;
  const pass = mintPass(inputs, coordinate.index, {
    label: "observed mint",
    grammar: "mintGrammar",
    scan: "mintScan",
    datum: (scan) =>
      stepDatum(
        prover,
        { source_index: inputs.evidence.index, scan },
        SDK.DirectMintWorkDatum,
      ),
    grammarRedeemer: (args) =>
      Data.to({ Continue: [args] }, SDK.DirectMintGrammarRedeemer),
    scanRedeemer: (args) =>
      Data.to({ Continue: [{ Scan: { args } }] }, SDK.DirectMintScanRedeemer),
  });
  return [
    {
      label: "observe mint select",
      role: "purposeSelect",
      next: "mintGrammar",
      datum: pass.initial,
      fields: [5],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                Mint: {
                  ...indices,
                  purpose_index: coordinate.index,
                  opening: open(5),
                },
              },
            ],
          },
          SDK.DirectPurposeSelectRedeemer,
        ),
    },
    ...pass.transitions,
  ];
};

const receiveSelection = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
): Promise<readonly MissingRedeemerTransition[]> => {
  const { prover } = inputs;
  const outputs = await receiveOutputsPass(inputs, {
    label: "observed receive",
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
  const declaration = declarationPass(
    inputs,
    outputs.evidence,
    coordinate.index,
    {
      label: "observed receive",
      role: "receiveDeclaration",
      datum: (scan) =>
        stepDatum(prover, scan, SDK.DirectReceiveDeclarationDatum),
      scanRedeemer: (indices, entries) =>
        Data.to(
          { Continue: [{ Scan: { ...indices, entries: [...entries] } }] },
          SDK.DirectReceiveDeclarationRedeemer,
        ),
    },
  );
  return [
    {
      label: "observe receive select",
      role: "purposeSelect",
      next: "receiveStart",
      datum: stepDatum(prover, boundIndex(inputs), SDK.DirectReceiveStartDatum),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Receive: indices }] },
          SDK.DirectPurposeSelectRedeemer,
        ),
    },
    {
      label: "observed receive start",
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
      label: "observed receive outputs finish",
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
      label: "observed receive exhaustion",
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
                target_ordinal: coordinate.index,
              },
            ],
          },
          SDK.DirectReceiveFinishRedeemer,
        ),
    },
    ...declaration.transitions,
  ];
};

/** Purpose-select through the kind's domain to the selection the source
 * lookup authenticates; the purpose must exist. */
export const missingRedeemerObservedSelection = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
): Promise<{
  readonly selection: SDK.DirectPurposeSelection;
  readonly purpose: SDK.DirectPurpose;
  readonly transitions: readonly MissingRedeemerTransition[];
}> => {
  const purpose = directPurposeAt(inputs.material, coordinate);
  if (purpose === null)
    throw new Error("missing-redeemer: observed purpose is absent");
  const selection: SDK.DirectPurposeSelection = {
    ...boundIndex(inputs),
    coordinate,
    purpose,
  };
  if (coordinate.kind === 0n)
    return {
      selection,
      purpose,
      transitions: [
        ...(await spendSelection(inputs, coordinate, purpose)),
        finishToLookup(
          inputs,
          selection,
          finishAction("observed spend finish", "spendScan", (indices) =>
            Data.to(
              { Continue: [{ Finish: indices }] },
              SDK.DirectSpendScanRedeemer,
            ),
          ),
        ),
      ],
    };
  if (coordinate.kind === 1n)
    return {
      selection,
      purpose,
      transitions: [
        ...mintSelection(inputs, coordinate),
        finishToLookup(
          inputs,
          selection,
          finishAction("observed mint finish", "mintScan", (indices) =>
            Data.to(
              { Continue: [{ Finish: indices }] },
              SDK.DirectMintScanRedeemer,
            ),
          ),
        ),
      ],
    };
  if (coordinate.kind === 3n)
    return {
      selection,
      purpose,
      transitions: [
        ...(await receiveSelection(inputs, coordinate)),
        finishToLookup(
          inputs,
          selection,
          finishAction(
            "observed receive finish",
            "receiveDeclaration",
            (indices) =>
              Data.to(
                { Continue: [{ Finish: indices }] },
                SDK.DirectReceiveDeclarationRedeemer,
              ),
          ),
        ),
      ],
    };
  return {
    selection,
    purpose,
    transitions: [
      finishToLookup(inputs, selection, {
        label: "observe select",
        role: "purposeSelect",
        fields: [3],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  Observe: {
                    ...indices,
                    purpose_index: coordinate.index,
                    opening: open(3),
                  },
                },
              ],
            },
            SDK.DirectPurposeSelectRedeemer,
          ),
      }),
    ],
  };
};
