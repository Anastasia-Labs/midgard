import { computeHash32 } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { DirectSourceSelection } from "../src/fraud-proof/direct-purpose-workers.js";
import type { DirectScriptBound } from "../src/fraud-proof/direct-script-derivation.js";
import {
  MissingRedeemerDecisionState as DecisionState,
  type MissingRedeemerDecisionState,
  type MissingRedeemerScanState,
  MissingRedeemerScanState as ScanState,
} from "../src/fraud-proof/missing-redeemer.js";
import {
  MissingRedeemerClaimContext as ClaimContext,
  type MissingRedeemerClaimContext,
  type MissingRedeemerClaimedPredicateFact,
  MissingRedeemerClaimedPredicateFact as PredicateFact,
  type MissingRedeemerClaimedPurposeFact,
  MissingRedeemerClaimedPurposeFact as PurposeFact,
  MissingRedeemerClaimedRedeemerScan as ClaimedScan,
  MissingRedeemerClaimPurposeState as PurposeState,
  MissingRedeemerFinalState as FinalState,
} from "../src/fraud-proof/missing-redeemer-claim.js";

const bytes = (tag: number, length: number): string =>
  tag.toString(16).padStart(length * 2, "0");

/** Every field carries a distinct value, so any field or constructor reorder
 * changes the encoding. The same values and digests are pinned on-chain in
 * onchain/aiken/lib/midgard/fraud-proofs/missing-redeemer/claim.test.ak. */
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

const decision = (): MissingRedeemerDecisionState => ({
  authenticated: selection(),
  redeemer_missing: true,
});

const scan = (): MissingRedeemerScanState => ({
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

const observedContext = (): MissingRedeemerClaimContext => ({
  bound: { bound: bound(), source_index: sourceIndex() },
  observed: decision(),
});

const claimedOnlyContext = (): MissingRedeemerClaimContext => ({
  bound: { bound: bound(), source_index: sourceIndex() },
  observed: null,
});

const purposeFacts: ReadonlyArray<
  readonly [string, MissingRedeemerClaimedPurposeFact]
> = [
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
  readonly [string, MissingRedeemerClaimedPredicateFact]
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
    "db57e1fef74bb13a829bb9ef59a3b9a78d30387ea67bb2f10de90a4f0651d046",
  "ClaimContextV1 claimed-only":
    "1c62f54d2394d22bb9272222219cde5df548b911f9c0874756c8261091fb4fa5",
  ClaimedRedeemerScanV1:
    "ce4d65208a2f267b54adc031fa71f77163ca64b14f343c2257e9e3954f49c334",
  "ClaimedPurposeFactV1 NotRequired":
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "ClaimPurposeStateV1 NotRequired":
    "4203430cd36ce9eb01ed87f16d9862e9a2975b9103635e0be1c3fa98523af4f1",
  "ClaimedPurposeFactV1 ImpossiblePurpose":
    "b3686503776d562be4716fc8f3a74f8c06343a26f6598b522b107a3979f81929",
  "ClaimPurposeStateV1 ImpossiblePurpose":
    "67b4185ace977401416523115a9890e5eabaabd1c3e7d1f6b183575b3ecb638e",
  "ClaimedPurposeFactV1 ExistingPurpose":
    "575c349cfd43837b69746baf9c3fb5eaa7dd36e74378d3fb0096e717f61ceb79",
  "ClaimPurposeStateV1 ExistingPurpose":
    "82f4527d4feee3483e6d27f0167cbc99ca3c9fac5b86a339abb42973c4ffd075",
  "ClaimedPredicateFactV1 NotRequired":
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "FinalStateV1 NotRequired":
    "4203430cd36ce9eb01ed87f16d9862e9a2975b9103635e0be1c3fa98523af4f1",
  "ClaimedPredicateFactV1 ImpossiblePurpose":
    "dc85329bda3029e830f0ab2a3281f4ceda4a53454493240fb5b0149140333489",
  "FinalStateV1 ImpossiblePurpose":
    "28149f174bd31a408ac24b59e8bb67a86162c99001cc022ad5aaf7f738165629",
  "ClaimedPredicateFactV1 SourceAbsent":
    "ade7342b4a72179f809938dd43d1a2d746388634b95ea1f183e5a0f8ef87a912",
  "FinalStateV1 SourceAbsent":
    "961e93653b19a3c3e96b3074512809a28c4f2da6ed1349c460418814c72479b1",
  "ClaimedPredicateFactV1 NativeSource":
    "e86893bb86d51e40213d632738d87eef378ec392fa83a8259437f4d06d585539",
  "FinalStateV1 NativeSource":
    "473b0fbda43e4ff945d415e431fe401f619e038fce0ee29c4a4dc386c1725e3b",
  "ClaimedPredicateFactV1 SourcePresent":
    "87af174871a277af803d1f66925097b3ae6313ae6c131b72137ba6c0393367fa",
  "FinalStateV1 SourcePresent":
    "aef193774f6f7995769397fa245045f1a6c5e88aaad00752b5e13ca963676154",
  "ClaimedPredicateFactV1 RedeemerPresent":
    "25e34f8d63ddb1c2e9ac1912741ca6f15013decf15075fa6609124642f40320b",
  "FinalStateV1 RedeemerPresent":
    "96b9b24dc8909def0c50044debdf9b331a3ecaba4b4c1df75ba0e87906bc2944",
  "ClaimedPredicateFactV1 RedeemerAbsent":
    "fb887f818181a2605be3168263543ed4ac4ef8c656d4229ec51f1cd9be7cdaf2",
  "FinalStateV1 RedeemerAbsent":
    "695088c6eefa6e884b589e688d1c199a97d3e4ea39011d78e254abc7e5ec0aee",
};

describe("missing-redeemer claim wire twin", () => {
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
