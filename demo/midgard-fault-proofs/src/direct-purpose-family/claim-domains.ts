/** Claim custody over the leaf's own purpose coordinate when it is valid and
 * distinct from the observed one: claim start through that kind's bounded
 * domain to the transition that pays adjudication the claimed purpose. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { directPurposeAt } from "../direct-script-derivation/index.js";
import { type DirectPurposeTransition, stepDatum } from "./custody.js";
import {
  declarationPass,
  directPurposeSpendEvidence,
  mintPass,
  receiveOutputsPass,
  spendRankPass,
} from "./domain-passes.js";
import { DIRECT_PURPOSE_SCAN_BATCH, type RouteInputs } from "./pointer-scan.js";

type Coordinate = SDK.DirectPurposeCoordinate;

export type DirectPurposeClaimDomain = {
  readonly transitions: readonly DirectPurposeTransition[];
  /** The claimed purpose the domain proved, or null when it has none. */
  readonly purpose: SDK.DirectPurpose | null;
};

const factOf = (
  coordinate: Coordinate,
  purpose: SDK.DirectPurpose | null,
): SDK.DirectClaimedPurposeFact =>
  purpose === null
    ? { ImpossiblePurpose: { coordinate } }
    : { ExistingPurpose: { purpose } };

const spendDomain = async (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
  coordinate: Coordinate,
  adjudicate: (fact: SDK.DirectClaimedPurposeFact) => string,
): Promise<DirectPurposeClaimDomain> => {
  const { material, prover } = inputs;
  const spend = await directPurposeSpendEvidence(inputs, coordinate.index);
  if (spend === null) {
    const fact = factOf(coordinate, null);
    return {
      purpose: null,
      transitions: [
        {
          label: "claim spend out of range",
          role: "claimStart",
          next: "claimAdjudicate",
          datum: adjudicate(fact),
          fields: [0],
          redeemer: ({ indices, open }) =>
            Data.to(
              {
                Continue: [
                  { SpendOutOfRange: { ...indices, opening: open(0) } },
                ],
              },
              SDK.DirectClaimStartRedeemer,
            ),
        },
      ],
    };
  }
  const purpose = directPurposeAt(material, coordinate);
  const count = inputs.plan(0).items.length;
  const first = Math.min(inputs.batch ?? DIRECT_PURPOSE_SCAN_BATCH, count);
  const state = (progress: {
    readonly cursor: SDK.DirectFieldCursor;
    readonly rank: bigint;
    readonly complete: boolean;
  }): SDK.DirectClaimSpendState => ({
    context,
    source_index: inputs.evidence.index,
    cursor: progress.cursor,
    raw_item_index: spend.rawItemIndex,
    target: {
      script_hash: purpose?.script_hash ?? null,
      subject: spend.item.toString("hex"),
    },
    rank: progress.rank,
    complete: progress.complete,
  });
  const datum = (value: SDK.DirectClaimSpendState) =>
    stepDatum(prover, value, SDK.DirectClaimSpendDatum);
  const rank = spendRankPass(inputs, {
    label: "claim spend scan",
    role: "claimSpendScan",
    subject: spend.item,
    from: first,
    state,
    datum,
    redeemer: (args) =>
      Data.to({ Continue: [{ Scan: { args } }] }, SDK.DirectClaimSpendRedeemer),
  });
  if (rank.final.rank !== coordinate.index)
    throw new Error("direct purpose: claimed spend rank differs");
  return {
    purpose,
    transitions: [
      {
        label: "claim spend",
        role: "claimStart",
        next: "claimSpendStart",
        datum: stepDatum(prover, context, SDK.DirectClaimStartDatum),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Spend: indices }] },
            SDK.DirectClaimStartRedeemer,
          ),
      },
      {
        label: "claim spend open",
        role: "claimSpendStart",
        next: "claimSpendScan",
        datum: datum(state(rank.at(first))),
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
                  budget: BigInt(first),
                },
              ],
            },
            SDK.DirectClaimSpendOpenRedeemer,
          ),
      },
      ...rank.transitions,
      {
        label: "claim spend finish",
        role: "claimSpendScan",
        next: "claimAdjudicate",
        datum: adjudicate(factOf(coordinate, purpose)),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.DirectClaimSpendRedeemer,
          ),
      },
    ],
  };
};

const mintDomain = (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
  coordinate: Coordinate,
  adjudicate: (fact: SDK.DirectClaimedPurposeFact) => string,
): DirectPurposeClaimDomain => {
  const { prover } = inputs;
  const plan = inputs.plan(5);
  const pass = mintPass(inputs, coordinate.index, {
    label: "claim mint",
    grammar: "claimMintGrammar",
    scan: "claimMintScan",
    datum: (scan) =>
      stepDatum(
        prover,
        { context, work: { source_index: inputs.evidence.index, scan } },
        SDK.DirectClaimMintDatum,
      ),
    grammarRedeemer: (args) =>
      Data.to({ Continue: [args] }, SDK.DirectClaimMintGrammarRedeemer),
    scanRedeemer: (args) =>
      Data.to(
        { Continue: [{ Scan: { args } }] },
        SDK.DirectClaimMintScanRedeemer,
      ),
  });
  const purpose = pass.state.selected;
  return {
    purpose,
    transitions: [
      {
        label: "claim mint",
        role: "claimStart",
        next: "claimMintGrammar",
        datum: pass.initial,
        fields: [5],
        redeemer: ({ indices, open }) =>
          Data.to(
            { Continue: [{ Mint: { ...indices, opening: open(5) } }] },
            SDK.DirectClaimStartRedeemer,
          ),
      },
      ...pass.transitions,
      {
        label: "claim mint finish",
        role: "claimMintScan",
        next: "claimAdjudicate",
        datum: adjudicate(factOf(coordinate, purpose)),
        fields: [5],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  Finish: {
                    args: {
                      ...indices,
                      opening: open(5),
                      checkpoint_bytes: plan.position(
                        "Walking",
                        plan.items.length,
                      ).checkpointBytes,
                    },
                  },
                },
              ],
            },
            SDK.DirectClaimMintScanRedeemer,
          ),
      },
    ],
  };
};

const receiveDomain = async (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
  coordinate: Coordinate,
  adjudicate: (fact: SDK.DirectClaimedPurposeFact) => string,
): Promise<DirectPurposeClaimDomain> => {
  const { prover } = inputs;
  const outputDatum = (scan: SDK.DirectReceiveOutputState) =>
    stepDatum(
      prover,
      { context, target_ordinal: coordinate.index, scan },
      SDK.DirectClaimReceiveOutputDatum,
    );
  const outputs = await receiveOutputsPass(inputs, {
    label: "claim receive",
    grammar: "claimReceiveGrammar",
    outputs: "claimReceiveOutputs",
    datum: outputDatum,
    grammarRedeemer: (args) =>
      Data.to({ Continue: [args] }, SDK.DirectClaimReceiveGrammarRedeemer),
    scanRedeemer: ({ indices, witnesses, ...rest }) =>
      Data.to(
        {
          Continue: [
            { Scan: { ...indices, ...rest, witnesses: [...witnesses] } },
          ],
        },
        SDK.DirectClaimReceiveOutputRedeemer,
      ),
  });
  const declaration = declarationPass(
    inputs,
    outputs.evidence,
    coordinate.index,
    {
      label: "claim receive",
      role: "claimReceiveDeclaration",
      datum: (scan) =>
        stepDatum(
          prover,
          { context, scan },
          SDK.DirectClaimReceiveDeclarationDatum,
        ),
      scanRedeemer: (indices, entries) =>
        Data.to(
          { Continue: [{ Scan: { ...indices, entries: [...entries] } }] },
          SDK.DirectClaimReceiveDeclarationRedeemer,
        ),
    },
  );
  const purpose = declaration.state.selected?.purpose ?? null;
  return {
    purpose,
    transitions: [
      {
        label: "claim receive",
        role: "claimStart",
        next: "claimReceiveGrammar",
        datum: outputs.initial,
        fields: [2],
        redeemer: ({ indices, open }) =>
          Data.to(
            { Continue: [{ Receive: { ...indices, opening: open(2) } }] },
            SDK.DirectClaimStartRedeemer,
          ),
      },
      ...outputs.transitions,
      {
        label: "claim receive outputs finish",
        role: "claimReceiveOutputs",
        next: "claimReceiveFinish",
        datum: outputDatum(outputs.state),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.DirectClaimReceiveOutputRedeemer,
          ),
      },
      {
        label: "claim receive exhaustion",
        role: "claimReceiveFinish",
        next: "claimReceiveDeclaration",
        datum: declaration.initial,
        fields: [2],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  ...indices,
                  opening: open(2),
                  checkpoint_bytes: outputs.exhausted,
                },
              ],
            },
            SDK.DirectClaimReceiveFinishRedeemer,
          ),
      },
      ...declaration.transitions,
      {
        label: "claim receive finish",
        role: "claimReceiveDeclaration",
        next: "claimAdjudicate",
        datum: adjudicate(factOf(coordinate, purpose)),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.DirectClaimReceiveDeclarationRedeemer,
          ),
      },
    ],
  };
};

const observeDomain = (
  inputs: RouteInputs,
  coordinate: Coordinate,
  adjudicate: (fact: SDK.DirectClaimedPurposeFact) => string,
): DirectPurposeClaimDomain => {
  const purpose = directPurposeAt(inputs.material, coordinate);
  return {
    purpose,
    transitions: [
      {
        label: "claim observe",
        role: "claimStart",
        next: "claimAdjudicate",
        datum: adjudicate(factOf(coordinate, purpose)),
        fields: [3],
        redeemer: ({ indices, open }) =>
          Data.to(
            { Continue: [{ Observe: { ...indices, opening: open(3) } }] },
            SDK.DirectClaimStartRedeemer,
          ),
      },
    ],
  };
};

/** The claimed purpose domain for a valid coordinate distinct from the
 * observed one, by purpose kind. */
export const directPurposeClaimDomain = (
  inputs: RouteInputs,
  context: SDK.DirectClaimContext,
  coordinate: Coordinate,
): Promise<DirectPurposeClaimDomain> | DirectPurposeClaimDomain => {
  const adjudicate = (purpose: SDK.DirectClaimedPurposeFact) =>
    stepDatum(
      inputs.prover,
      { context, purpose },
      SDK.DirectClaimAdjudicateDatum,
    );
  if (coordinate.kind === 0n)
    return spendDomain(inputs, context, coordinate, adjudicate);
  if (coordinate.kind === 1n)
    return mintDomain(inputs, context, coordinate, adjudicate);
  if (coordinate.kind === 2n)
    return observeDomain(inputs, coordinate, adjudicate);
  return receiveDomain(inputs, context, coordinate, adjudicate);
};
