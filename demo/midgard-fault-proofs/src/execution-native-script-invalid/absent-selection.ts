/** The shared purpose route over a mint or receive ordinal past the end of
 * its domain: the kind's bounded pass runs to exhaustion without selecting
 * a purpose, and its finish pays the source lookup a selection with no
 * purpose, which the lookup resolves to no native execution. The present
 * kinds use the kit's own observed selection. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DirectPurposeIndices,
  type DirectPurposeTransition,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  declarationPass,
  mintPass,
  receiveOutputsPass,
} from "../direct-purpose-family/domain-passes.js";
import {
  boundIndex,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";

type Coordinate = SDK.DirectPurposeCoordinate;

const finish = (
  inputs: RouteInputs,
  selection: SDK.DirectPurposeSelection,
  label: string,
  role: DirectPurposeTransition["role"],
  encode: (indices: DirectPurposeIndices) => string,
): DirectPurposeTransition => ({
  label,
  role,
  next: "sourceLookup",
  datum: stepDatum(inputs.prover, selection, SDK.DirectSourceLookupDatum),
  fields: [],
  redeemer: ({ indices }) => encode(indices),
});

const absentMint = (
  inputs: RouteInputs,
  coordinate: Coordinate,
  selection: SDK.DirectPurposeSelection,
): readonly DirectPurposeTransition[] => {
  const pass = mintPass(inputs, coordinate.index, {
    label: "absent mint",
    grammar: "mintGrammar",
    scan: "mintScan",
    datum: (scan) =>
      stepDatum(
        inputs.prover,
        { source_index: inputs.evidence.index, scan },
        SDK.DirectMintWorkDatum,
      ),
    grammarRedeemer: (args) =>
      Data.to({ Continue: [args] }, SDK.DirectMintGrammarRedeemer),
    scanRedeemer: (args) =>
      Data.to({ Continue: [{ Scan: { args } }] }, SDK.DirectMintScanRedeemer),
  });
  if (pass.state.selected !== null)
    throw new Error("execution-native-script-invalid: the mint is present");
  return [
    {
      label: "absent mint select",
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
    finish(inputs, selection, "absent mint finish", "mintScan", (indices) =>
      Data.to({ Continue: [{ Finish: indices }] }, SDK.DirectMintScanRedeemer),
    ),
  ];
};

const absentReceive = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
  selection: SDK.DirectPurposeSelection,
): Promise<readonly DirectPurposeTransition[]> => {
  const { prover } = inputs;
  const outputs = await receiveOutputsPass(inputs, {
    label: "absent receive",
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
      label: "absent receive",
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
  if (declaration.state.selected !== null)
    throw new Error("execution-native-script-invalid: the receive is present");
  return [
    {
      label: "absent receive select",
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
      label: "absent receive start",
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
      label: "absent receive outputs finish",
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
      label: "absent receive exhaustion",
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
    finish(
      inputs,
      selection,
      "absent receive finish",
      "receiveDeclaration",
      (indices) =>
        Data.to(
          { Continue: [{ Finish: indices }] },
          SDK.DirectReceiveDeclarationRedeemer,
        ),
    ),
  ];
};

/** The route from purpose-select to the source lookup over a mint or receive
 * ordinal its domain does not reach. */
export const executionNativeScriptInvalidAbsentSelection = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
): Promise<{
  readonly selection: SDK.DirectPurposeSelection;
  readonly transitions: readonly DirectPurposeTransition[];
}> => {
  const selection: SDK.DirectPurposeSelection = {
    ...boundIndex(inputs),
    coordinate,
    purpose: null,
  };
  if (coordinate.kind === 1n)
    return {
      selection,
      transitions: absentMint(inputs, coordinate, selection),
    };
  if (coordinate.kind === 3n)
    return {
      selection,
      transitions: await absentReceive(inputs, coordinate, selection),
    };
  throw new Error(
    "execution-native-script-invalid: only a mint or receive ordinal is resolved by exhaustion",
  );
};
