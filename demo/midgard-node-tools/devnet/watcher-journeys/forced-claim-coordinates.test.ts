/**
 * A forced leaf's `ForcedTxInvalid{reason}` names coordinates the block
 * producer chose. Classifying a block whose leaf names a coordinate its valid
 * native forced transaction does not have returns a decision; it never
 * throws. Each such wrongful rejection is decided as the fault of the family
 * that owns the reason: the coordinate may be negative, at or past the item
 * count of the field it names, past 2^53, or at either bound of the codec.
 *
 * The forced transaction spends one input, pays one output without a
 * reference script, mints under one native policy and carries one native
 * witness script, one address witness and no redeemers, reference inputs,
 * observers or required signers.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as FP from "@al-ft/midgard-fault-proofs";
import { type HeaderDecision } from "@al-ft/midgard-fault-proofs";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { prepareJourneyForcedTransaction } from "./forced-transaction.js";
import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import { stackNodeFeeSchedule } from "./stack-fee-schedule.test-support.js";

const account = generateEmulatorAccount({ lovelace: 100_000_000n });
const owner = Buffer.from(
  CML.PrivateKey.from_bech32(
    walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_raw_bytes(),
);
const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };

type Predecessor = Parameters<
  typeof prepareJourneyForcedTransaction
>[0]["predecessor"];
let predecessor: Predecessor;
let forced: Readonly<{
  transaction: ReturnType<
    typeof prepareJourneyForcedTransaction
  >["transaction"];
  orderKey: typeof orderKey;
}>;
beforeAll(async () => {
  predecessor = await checkedFixtureBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      ...(await stackNodeFeeSchedule()),
      utxos: [
        {
          key: outRefCbor(0x81, 0n),
          value: encodeMidgardTxOutput({
            address: Buffer.concat([Buffer.from([0x60]), owner]),
            value: { lovelace: 100_000_000n, assets: new Map() },
          }),
        },
      ],
    }),
  );
  forced = {
    transaction: prepareJourneyForcedTransaction({
      predecessor,
      ledgerOwnerSeedPhrase: account.seedPhrase,
    }).transaction,
    orderKey,
  };
});

/** The largest coordinate a `RejectionReasonV1` encodes. */
const MAX = (1n << 64n) - 1n;
/** The smallest coordinate a `RejectionReasonV1` encodes. */
const MIN = -(1n << 64n);
/** The first coordinate past the integers a JavaScript number holds exactly. */
const BIG = (1n << 53n) + 1n;

const reason = (arm: string, fields: Record<string, bigint>) =>
  ({ [arm]: fields }) as unknown as SDK.RejectionReason;

/** A duplicate claim pairing the first spend input with the given input. */
const duplicateOf = (second_field_index: bigint, second_item_index: bigint) =>
  reason("DuplicateInput", {
    first_field_index: 0n,
    first_item_index: 0n,
    second_field_index,
    second_item_index,
  });

const plutusUndecodable = (
  item_kind: bigint,
  item_index: bigint,
  decodeClass: bigint,
) =>
  reason("PlutusScriptUndecodable", {
    item_kind,
    item_index,
    class: decodeClass,
  });

const fault = (category: string) => `fault_detected:${category}`;

const outcome = (decision: HeaderDecision): string =>
  decision.decision === "fault_detected"
    ? fault(decision.category)
    : decision.decision === "unprovable"
      ? `unprovable:${decision.violationId}`
      : decision.decision;

const classifyFull = async (claimed: SDK.RejectionReason) => {
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: forced.transaction.canonicalCbor,
    predecessor,
    ledgerEntries: predecessor.ledger.entries,
    operatorVkey: "82".repeat(28),
    endTime: predecessor.header.endTime + 40_000n,
    blockSlot: predecessor.header.blockSlot + 40n,
    source: {
      kind: "forced",
      orderKey,
      verdict: { ForcedTxInvalid: { reason: claimed } },
    },
  });
  return await classifyFullCatalogueTransactionFixture({
    block,
    predecessor,
    history: [],
    forced,
  });
};

const classify = async (claimed: SDK.RejectionReason): Promise<string> =>
  outcome((await classifyFull(claimed)).decision);

/** One claim per coordinate: past the field, negative, past 2^53, at MAX. */
const absentAt = (
  label: string,
  claim: (coordinate: bigint) => SDK.RejectionReason,
  past: bigint,
  category: string,
): readonly (readonly [string, SDK.RejectionReason, string])[] =>
  (
    [
      ["past the last item", past],
      ["at a negative index", -1n],
      ["past 2^53", BIG],
      ["at the largest index", MAX],
      ["at the smallest index", MIN],
    ] as const
  ).map(([where, coordinate]) => [
    `${label} ${where}`,
    claim(coordinate),
    fault(category),
  ]);

const CASES: readonly (readonly [string, SDK.RejectionReason, string])[] = [
  // Field-shape claims.
  ...absentAt(
    "field length of a field",
    (field_index) => reason("FieldPreimageLengthMismatch", { field_index }),
    9n,
    "fieldPreimageLengthMismatch",
  ),
  [
    "empty reference-input field length",
    reason("FieldPreimageLengthMismatch", { field_index: 1n }),
    fault("fieldPreimageLengthMismatch"),
  ],
  ...absentAt(
    "item width of an item",
    (item_index) =>
      reason("FieldItemWidthIllegal", { field_index: 5n, item_index }),
    1n,
    "fieldItemWidthIllegal",
  ),
  ...absentAt(
    "item width of a field",
    (field_index) =>
      reason("FieldItemWidthIllegal", { field_index, item_index: 0n }),
    9n,
    "fieldItemWidthIllegal",
  ),
  // Input claims.
  ...absentAt(
    "duplicate naming a second spend input",
    (index) => duplicateOf(0n, index),
    1n,
    "inputSetUniqueness",
  ),
  ...absentAt(
    "duplicate naming a second input field",
    (field) => duplicateOf(field, 0n),
    2n,
    "inputSetUniqueness",
  ),
  [
    "duplicate naming an absent reference input",
    duplicateOf(1n, 0n),
    fault("inputSetUniqueness"),
  ],
  [
    "duplicate naming one coordinate twice",
    duplicateOf(0n, 0n),
    fault("inputSetUniqueness"),
  ],
  ...absentAt(
    "missing spend input",
    (input_index) => reason("InputNotFound", { source_kind: 0n, input_index }),
    1n,
    "nonExistentInput",
  ),
  [
    "missing reference input",
    reason("InputNotFound", { source_kind: 1n, input_index: 0n }),
    fault("noReferenceInput"),
  ],
  ...absentAt(
    "missing input of an input kind",
    (source_kind) => reason("InputNotFound", { source_kind, input_index: 0n }),
    2n,
    "nonExistentInput",
  ),
  ...absentAt(
    "non-canonical spent output of a spend input",
    (input_index) =>
      reason("InputSpentOutputNonCanonical", { source_kind: 0n, input_index }),
    1n,
    "resolvedOutputNonCanonical",
  ),
  [
    "non-canonical spent output of an absent reference input",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 1n,
      input_index: 0n,
    }),
    fault("resolvedOutputNonCanonical"),
  ],
  ...absentAt(
    "non-canonical spent output of an input kind",
    (source_kind) =>
      reason("InputSpentOutputNonCanonical", { source_kind, input_index: 0n }),
    2n,
    "resolvedOutputNonCanonical",
  ),
  ...(
    [
      "ResolvedReferenceScriptMalformed",
      "ResolvedReferenceScriptNodeLimit",
      "ResolvedReferenceScriptDepthLimit",
    ] as const
  ).flatMap((arm) => [
    ...absentAt(
      `${arm} of a spend input`,
      (input_index) => reason(arm, { source_kind: 0n, input_index }),
      1n,
      "nativeScriptDecoding",
    ),
    ...absentAt(
      `${arm} of an input kind`,
      (source_kind) => reason(arm, { source_kind, input_index: 0n }),
      2n,
      "nativeScriptDecoding",
    ),
    [
      `${arm} of an absent reference input`,
      reason(arm, { source_kind: 1n, input_index: 0n }),
      fault("nativeScriptDecoding"),
    ] as const,
  ]),
  ...absentAt(
    "spend signer",
    (input_index) => reason("SpendInputSignerMissing", { input_index }),
    1n,
    "spendInputSignerMissing",
  ),
  // Signature claims.
  ...absentAt(
    "address witness",
    (witness_index) =>
      reason("AddressWitnessSignatureInvalid", { witness_index }),
    1n,
    "invalidSignature",
  ),
  ...absentAt(
    "required signer",
    (signer_index) => reason("RequiredSignerUnsigned", { signer_index }),
    0n,
    "missingSignature",
  ),
  // Witness-script claims.
  ...(
    [
      "WitnessScriptHeaderMalformed",
      "WitnessNativeScriptMalformed",
      "WitnessNativeScriptNodeLimit",
      "WitnessNativeScriptDepthLimit",
    ] as const
  ).flatMap((arm) =>
    absentAt(
      `${arm} of a witness script`,
      (script_index) => reason(arm, { script_index }),
      1n,
      "witnessScriptDecoding",
    ),
  ),
  [
    "in-range witness script node limit",
    reason("WitnessNativeScriptNodeLimit", { script_index: 0n }),
    fault("witnessScriptDecoding"),
  ],
  ...absentAt(
    "false witness script",
    (script_index) => reason("WitnessNativeScriptFalse", { script_index }),
    1n,
    "nativeScriptInvalid",
  ),
  ...absentAt(
    "unused witness script",
    (script_index) => reason("UnusedScriptWitness", { script_index }),
    1n,
    "unusedScriptWitness",
  ),
  // Redeemer claims, against a transaction with no redeemers.
  ...absentAt(
    "malformed redeemer",
    (redeemer_index) => reason("RedeemerMalformed", { redeemer_index }),
    0n,
    "redeemerCanonicity",
  ),
  ...absentAt(
    "unused redeemer",
    (redeemer_index) => reason("UnusedRedeemer", { redeemer_index }),
    0n,
    "unusedRedeemer",
  ),
  [
    "failed Plutus execution of a native mint",
    reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index: 0n }),
    fault("redeemerCanonicity"),
  ],
  ...absentAt(
    "failed Plutus execution of a mint",
    (purpose_index) =>
      reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index }),
    1n,
    "redeemerCanonicity",
  ),
  ...absentAt(
    "failed Plutus execution of a purpose kind",
    (purpose_kind) =>
      reason("PlutusExecutionFailed", { purpose_kind, purpose_index: 0n }),
    6n,
    "redeemerCanonicity",
  ),
  // Output claims.
  ...absentAt(
    "non-canonical output",
    (output_index) => reason("OutputNonCanonical", { output_index }),
    1n,
    "transactionOutputNonCanonical",
  ),
  ...absentAt(
    "output below min-ADA",
    (output_index) => reason("OutputBelowMinAda", { output_index }),
    1n,
    "minAda",
  ),
  ...(
    [
      "OutputReferenceScriptMalformed",
      "OutputReferenceScriptNodeLimit",
      "OutputReferenceScriptDepthLimit",
    ] as const
  ).flatMap((arm) => [
    [
      `${arm} of an output without one`,
      reason(arm, { output_index: 0n }),
      fault("outputReferenceScriptDecoding"),
    ] as const,
    ...absentAt(
      `${arm} of an output`,
      (output_index) => reason(arm, { output_index }),
      1n,
      "outputReferenceScriptDecoding",
    ),
  ]),
  ...absentAt(
    "protected output signer",
    (output_index) => reason("ProtectedOutputSignerMissing", { output_index }),
    1n,
    "protectedOutputSignerMissing",
  ),
  ...absentAt(
    "output asset accumulation",
    (output_index) =>
      reason("OutputAssetAccumulationLimit", { output_index, asset_index: 0n }),
    1n,
    "distinctAssetAccumulationLimit",
  ),
  // Mint claims.
  ...absentAt(
    "declared asset limit",
    (policy_index) => reason("MintDeclaredAssetLimit", { policy_index }),
    1n,
    "mintDeclaredAssetLimit",
  ),
  ...absentAt(
    "malformed mint item",
    (policy_index) => reason("MintItemMalformed", { policy_index }),
    1n,
    "mintItemNonCanonical",
  ),
  // Observer claims, against a transaction with no observers.
  ...absentAt(
    "observer order",
    (observer_index) => reason("ObserverOrderInvalid", { observer_index }),
    0n,
    "observerOrderInvalid",
  ),
  // Plutus decode claims.
  ...absentAt(
    "undecodable Plutus witness",
    (index) => plutusUndecodable(0n, index, 1n),
    1n,
    "witnessScriptDecoding",
  ),
  [
    "undecodable Plutus reference script of an output without one",
    plutusUndecodable(1n, 0n, 1n),
    fault("outputReferenceScriptDecoding"),
  ],
  ...absentAt(
    "undecodable Plutus reference script",
    (index) => plutusUndecodable(1n, index, 1n),
    1n,
    "outputReferenceScriptDecoding",
  ),
  ...absentAt(
    "undecodable Plutus item of a kind",
    (kind) => plutusUndecodable(kind, 0n, 1n),
    2n,
    "witnessScriptDecoding",
  ),
  [
    "undecodable Plutus reference script naming no decode class",
    plutusUndecodable(1n, 0n, 0n),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "undecodable Plutus witness naming no decode class",
    plutusUndecodable(0n, 0n, 0n),
    fault("witnessScriptDecoding"),
  ],
];

describe("classifying a wrongful forced rejection whose reason names a coordinate the transaction lacks", () => {
  it.each(CASES)(
    "%s",
    async (_label, claimed, expected) => {
      expect(await classify(claimed)).toBe(expected);
    },
    120_000,
  );
});

/** Durable proof material for a decided claim, through each family's own
 * preparer, as the watcher records it before proving. */
const MATERIAL: readonly (readonly [
  string,
  SDK.RejectionReason,
  string,
  (
    full: Awaited<ReturnType<typeof classifyFull>>,
    detectionId: string,
  ) => Promise<unknown>,
])[] = [
  [
    "malformed mint item past 2^53",
    reason("MintItemMalformed", { policy_index: BIG }),
    "mintItemNonCanonical",
    (full, id) =>
      FP.prepareMintItemNonCanonicalRecoveryMaterial(full.evidence!, id),
  ],
  [
    "non-canonical output past 2^53",
    reason("OutputNonCanonical", { output_index: BIG }),
    "transactionOutputNonCanonical",
    (full, id) =>
      FP.prepareTransactionOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
      ),
  ],
  [
    "non-canonical output at a negative index",
    reason("OutputNonCanonical", { output_index: -1n }),
    "transactionOutputNonCanonical",
    (full, id) =>
      FP.prepareTransactionOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
      ),
  ],
  [
    "non-canonical spent output of a spend input past 2^53",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 0n,
      input_index: BIG,
    }),
    "resolvedOutputNonCanonical",
    (full, id) =>
      FP.prepareResolvedOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
        full.predecessor!,
      ),
  ],
  [
    "non-canonical spent output of an input kind past 2^53",
    reason("InputSpentOutputNonCanonical", {
      source_kind: BIG,
      input_index: 0n,
    }),
    "resolvedOutputNonCanonical",
    (full, id) =>
      FP.prepareResolvedOutputNonCanonicalRecoveryMaterial(
        full.evidence!,
        id,
        full.predecessor!,
      ),
  ],
];

describe("recording proof material for a wrongful forced rejection whose reason names a coordinate the transaction lacks", () => {
  it.each(MATERIAL)(
    "%s",
    async (_label, claimed, category, prepare) => {
      const full = await classifyFull(claimed);
      if (full.decision.decision !== "fault_detected")
        throw new Error(`expected a fault, got ${outcome(full.decision)}`);
      expect(full.decision.category).toBe(category);
      const { detectionId } = full.decision;
      const material = await prepare(full, detectionId);
      expect(material).toMatchObject({ category, detectionId });
      const recorded = JSON.parse(
        JSON.stringify(FP.encodeWorkflowArtifact(material)),
      );
      const fresh = await prepare(full, detectionId);
      expect(FP.requireWorkflowArtifactMatches(recorded, fresh)).toBe(fresh);
    },
    120_000,
  );
});
