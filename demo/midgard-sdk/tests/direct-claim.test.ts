import { computeHash32 } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DirectClaimContext as ClaimContext,
  type DirectClaimContext,
  type DirectClaimedPredicateFact,
  DirectClaimedPredicateFact as PredicateFact,
  type DirectClaimedPurposeFact,
  DirectClaimedPurposeFact as PurposeFact,
  DirectClaimedRedeemerScan as ClaimedScan,
  DirectClaimFinalState as FinalState,
  DirectClaimPurposeState as PurposeState,
  DirectRedeemerDecisionState as DecisionState,
  type DirectRedeemerDecisionState,
  type DirectRedeemerScanState,
  DirectRedeemerScanState as ScanState,
} from "../src/fraud-proof/direct-claim.js";
import type { DirectSourceSelection } from "../src/fraud-proof/direct-purpose-workers.js";
import type { DirectScriptBound } from "../src/fraud-proof/direct-script-derivation.js";
import { rejectionReasonPhase } from "../src/fraud-proof/first-fault.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const bytes = (tag: number, length: number): string =>
  tag.toString(16).padStart(length * 2, "0");

/** Every field carries a distinct value, so any field or constructor reorder
 * changes the encoding. The same values and digests are pinned on-chain in
 * onchain/aiken/lib/midgard/fraud-proofs/direct-script-derivation/claim.test.ak. */
const bound = (): DirectScriptBound => ({
  subject: {
    version: 1n,
    direction: 2n,
    source_kind: 3n,
    transaction_id: bytes(4, 32),
    source_key: bytes(5, 3),
    rejection_reason: {
      RedeemerMissing: { purpose_kind: 6n, purpose_index: 7n },
    },
  },
  compact_cbor: bytes(8, 5),
  network_order_context: { body_network_mismatch: true },
  event_pre_root: bytes(9, 32),
});

const sourceIndex = () => ({ root: bytes(10, 32), count: 11n });

const selection = (): DirectSourceSelection => ({
  selection: {
    bound: bound(),
    source_index: sourceIndex(),
    coordinate: { kind: 12n, index: 13n },
    purpose: {
      coordinate: { kind: 14n, index: 15n },
      script_hash: bytes(16, 28),
      subject: bytes(17, 4),
    },
  },
  source: {
    field_index: 18n,
    item_index: 19n,
    source_key: bytes(20, 2),
    language_tag: 21n,
    script_hash: bytes(22, 28),
    total_length: 23n,
    item_commitment: bytes(24, 32),
  },
});

const decision = (): DirectRedeemerDecisionState => ({
  authenticated: selection(),
  redeemer_missing: true,
});

const scan = (): DirectRedeemerScanState => ({
  authenticated: selection(),
  scan: {
    cursor: {
      bound: bound(),
      field_index: 25n,
      phase: "Walking",
      checkpoint_hash: bytes(26, 32),
    },
    coordinate: { kind: 27n, index: 28n },
    found: true,
    complete: false,
  },
});

const observedContext = (): DirectClaimContext => ({
  bound: { bound: bound(), source_index: sourceIndex() },
  observed: decision(),
  target: "LeafPurpose",
});

const claimedOnlyContext = (): DirectClaimContext => ({
  bound: { bound: bound(), source_index: sourceIndex() },
  observed: null,
  target: "LeafPurpose",
});

const redeemerPointerContext = (): DirectClaimContext => ({
  bound: { bound: bound(), source_index: sourceIndex() },
  observed: null,
  target: {
    RedeemerPointer: {
      redeemer_index: 37n,
      coordinate: { kind: 38n, index: 39n },
    },
  },
});

const purposeFacts: ReadonlyArray<readonly [string, DirectClaimedPurposeFact]> =
  [
    ["NotRequired", "NotRequired"],
    [
      "ImpossiblePurpose",
      { ImpossiblePurpose: { coordinate: { kind: 29n, index: -30n } } },
    ],
    [
      "ExistingPurpose",
      {
        ExistingPurpose: {
          purpose: {
            coordinate: { kind: 31n, index: 32n },
            script_hash: bytes(33, 28),
            subject: bytes(34, 4),
          },
        },
      },
    ],
  ];

const predicateFacts: ReadonlyArray<
  readonly [string, DirectClaimedPredicateFact]
> = [
  ["NotRequired", "NotRequired"],
  [
    "ImpossiblePurpose",
    { ImpossiblePurpose: { coordinate: { kind: 35n, index: -36n } } },
  ],
  ["SourceAbsent", { SourceAbsent: { selection: selection() } }],
  ["NativeSource", { NativeSource: { selection: selection() } }],
  ["SourcePresent", { SourcePresent: { selection: selection() } }],
  ["RedeemerPresent", { RedeemerPresent: { decision: decision() } }],
  ["RedeemerAbsent", { RedeemerAbsent: { decision: decision() } }],
];

const digest = (cbor: string): string =>
  computeHash32(Buffer.from(cbor, "hex")).toString("hex");

const rows: ReadonlyArray<readonly [string, string]> = [
  ["ScanStateV1", Data.to(scan(), ScanState)],
  ["DecisionStateV1", Data.to(decision(), DecisionState)],
  ["ClaimContextV1 observed", Data.to(observedContext(), ClaimContext)],
  ["ClaimContextV1 claimed-only", Data.to(claimedOnlyContext(), ClaimContext)],
  [
    "ClaimContextV1 redeemer-pointer",
    Data.to(redeemerPointerContext(), ClaimContext),
  ],
  [
    "ClaimedRedeemerScanV1",
    Data.to({ context: claimedOnlyContext(), scan: scan() }, ClaimedScan),
  ],
  ...purposeFacts.flatMap(([name, purpose]) => [
    [`ClaimedPurposeFactV1 ${name}`, Data.to(purpose, PurposeFact)] as const,
    [
      `ClaimPurposeStateV1 ${name}`,
      Data.to({ context: observedContext(), purpose }, PurposeState),
    ] as const,
  ]),
  ...predicateFacts.flatMap(([name, claim]) => [
    [`ClaimedPredicateFactV1 ${name}`, Data.to(claim, PredicateFact)] as const,
    [
      `FinalStateV1 ${name}`,
      Data.to({ context: observedContext(), claim }, FinalState),
    ] as const,
  ]),
];

const expected: Readonly<Record<string, string>> = {
  ScanStateV1:
    "136c5e40cb0ff102b446a2306ab39b555d29f3edab928797d7f5c33f479a24b5",
  DecisionStateV1:
    "a12b5393258407877759bd6b7e254d2847d487c517f5285bc43895c7a4bd7d9e",
  "ClaimContextV1 observed":
    "b86648a3c14769574df4019bea164c79cea72d1b0215a44010369d43b061f60d",
  "ClaimContextV1 claimed-only":
    "246f10644565e15e32fa1f90d204cf44472d543d1b3ad561091492341f100cb5",
  "ClaimContextV1 redeemer-pointer":
    "d897523342ad998cf4633e9950d2b189d39417ff735875b34a1249ab97390530",
  ClaimedRedeemerScanV1:
    "1582984d057de8f995a16bcb41828cd8a62290930a08471d0aeb6e9086d8d065",
  "ClaimedPurposeFactV1 NotRequired":
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "ClaimPurposeStateV1 NotRequired":
    "e2537de81a4e6497214628fca0910c8f75afed2fcb3541c043d3a695e7f36561",
  "ClaimedPurposeFactV1 ImpossiblePurpose":
    "b3686503776d562be4716fc8f3a74f8c06343a26f6598b522b107a3979f81929",
  "ClaimPurposeStateV1 ImpossiblePurpose":
    "2e1c13fe67532d4f310733f17b248f917047b9ea0e194195074c039d6799cf9f",
  "ClaimedPurposeFactV1 ExistingPurpose":
    "575c349cfd43837b69746baf9c3fb5eaa7dd36e74378d3fb0096e717f61ceb79",
  "ClaimPurposeStateV1 ExistingPurpose":
    "81f8d7ce617dfc38d8f293751054556ad641f9a1fba0df9f6f22c68cbc02dc65",
  "ClaimedPredicateFactV1 NotRequired":
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "FinalStateV1 NotRequired":
    "e2537de81a4e6497214628fca0910c8f75afed2fcb3541c043d3a695e7f36561",
  "ClaimedPredicateFactV1 ImpossiblePurpose":
    "dc85329bda3029e830f0ab2a3281f4ceda4a53454493240fb5b0149140333489",
  "FinalStateV1 ImpossiblePurpose":
    "15046b11953b67050092c05246eee8015246057292600cc9e829bac88b59ad5a",
  "ClaimedPredicateFactV1 SourceAbsent":
    "ade7342b4a72179f809938dd43d1a2d746388634b95ea1f183e5a0f8ef87a912",
  "FinalStateV1 SourceAbsent":
    "5fcbb6089786e941ff69059fc219fab0a8b392facde19bf425eb04ef54a8aa7a",
  "ClaimedPredicateFactV1 NativeSource":
    "e86893bb86d51e40213d632738d87eef378ec392fa83a8259437f4d06d585539",
  "FinalStateV1 NativeSource":
    "1eb5e97bff58eb1a894b7e99cded6b0d647b9e7fc66e3692d1b96122a4ee4aac",
  "ClaimedPredicateFactV1 SourcePresent":
    "87af174871a277af803d1f66925097b3ae6313ae6c131b72137ba6c0393367fa",
  "FinalStateV1 SourcePresent":
    "8ba36008f1a89ff9df1c148b2618554f63920111d998f9b835afd4d323e34de8",
  "ClaimedPredicateFactV1 RedeemerPresent":
    "25e34f8d63ddb1c2e9ac1912741ca6f15013decf15075fa6609124642f40320b",
  "FinalStateV1 RedeemerPresent":
    "6027727a767e669123236c0c88a02f3a0725bef204b57051ff0da7f9535d03d7",
  "ClaimedPredicateFactV1 RedeemerAbsent":
    "fb887f818181a2605be3168263543ed4ac4ef8c656d4229ec51f1cd9be7cdaf2",
  "FinalStateV1 RedeemerAbsent":
    "7e2a82c9c0ec9663855ee669ed38a2b41e1e2bbf239d5474bc138b9281e4235f",
};

describe("direct claim wire twin", () => {
  it.each(rows)("%s encodes exactly as on-chain", (name, cbor) => {
    expect(digest(cbor)).toBe(expected[name]);
  });

  it.each(purposeFacts)("ClaimPurposeStateV1 %s round-trips", (_n, purpose) => {
    const value = { context: claimedOnlyContext(), purpose };
    expect(Data.from(Data.to(value, PurposeState), PurposeState)).toStrictEqual(
      value,
    );
  });

  it.each(predicateFacts)("FinalStateV1 %s round-trips", (_n, claim) => {
    const value = { context: claimedOnlyContext(), claim };
    expect(Data.from(Data.to(value, FinalState), FinalState)).toStrictEqual(
      value,
    );
  });
});

describe("direct claim phase order", () => {
  const context = { body_network_mismatch: false };
  const outputReferenceScriptItem: RejectionReason = {
    PlutusScriptUndecodable: { item_kind: 1n, item_index: 0n, class: 1n },
  };

  it("decodes an output reference script item with the output scripts", () => {
    expect(rejectionReasonPhase(outputReferenceScriptItem, context)).toBe(
      rejectionReasonPhase(
        { OutputReferenceScriptMalformed: { output_index: 0n } },
        context,
      ),
    );
  });
});
