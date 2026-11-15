/**
 * A forced leaf's `ForcedTxInvalid{reason}` names coordinates the block
 * producer chose. Classifying a block whose leaf names a coordinate its valid
 * native forced transaction does not have returns a decision; it never
 * throws. Each wrongful rejection is decided as the fault its family proves
 * or, when no family proves a claim at a coordinate the transaction lacks,
 * as an unprovable gap that names the forced leaf.
 *
 * The forced transaction spends one input, pays one output without a
 * reference script, mints under one native policy and carries one native
 * witness script, one address witness and no redeemers, reference inputs,
 * observers or required signers.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID,
  type HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
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

const GAP = `unprovable:${FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID}`;
const fault = (category: string) => `fault_detected:${category}`;

const outcome = (decision: HeaderDecision): string =>
  decision.decision === "fault_detected"
    ? fault(decision.category)
    : decision.decision === "unprovable"
      ? `unprovable:${decision.violationId}`
      : decision.decision;

const classify = async (claimed: SDK.RejectionReason): Promise<string> => {
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
  const full = await classifyFullCatalogueTransactionFixture({
    block,
    predecessor,
    history: [],
    forced,
  });
  return outcome(full.decision);
};

const CASES: readonly (readonly [string, SDK.RejectionReason, string])[] = [
  // Field-shape claims.
  [
    "field index past the last field",
    reason("FieldPreimageLengthMismatch", { field_index: 9n }),
    GAP,
  ],
  [
    "negative field index",
    reason("FieldPreimageLengthMismatch", { field_index: -1n }),
    GAP,
  ],
  [
    "largest field index",
    reason("FieldPreimageLengthMismatch", { field_index: MAX }),
    GAP,
  ],
  [
    "empty reference-input field length",
    reason("FieldPreimageLengthMismatch", { field_index: 1n }),
    fault("fieldPreimageLengthMismatch"),
  ],
  [
    "item past a field's last item",
    reason("FieldItemWidthIllegal", { field_index: 5n, item_index: 1n }),
    fault("fieldItemWidthIllegal"),
  ],
  [
    "item of a field past the last field",
    reason("FieldItemWidthIllegal", { field_index: 9n, item_index: 0n }),
    fault("fieldItemWidthIllegal"),
  ],
  // Input claims.
  [
    "duplicate naming a second spend input that is absent",
    duplicateOf(0n, 1n),
    GAP,
  ],
  ["duplicate naming an absent reference input", duplicateOf(1n, 0n), GAP],
  ["duplicate naming one coordinate twice", duplicateOf(0n, 0n), GAP],
  ["duplicate naming the outputs field", duplicateOf(2n, 0n), GAP],
  ["duplicate at the largest item index", duplicateOf(0n, MAX), GAP],
  [
    "missing input past the last spend input",
    reason("InputNotFound", { source_kind: 0n, input_index: 1n }),
    fault("nonExistentInput"),
  ],
  [
    "missing reference input",
    reason("InputNotFound", { source_kind: 1n, input_index: 0n }),
    fault("noReferenceInput"),
  ],
  [
    "missing input of an input kind that does not exist",
    reason("InputNotFound", { source_kind: 2n, input_index: 0n }),
    GAP,
  ],
  [
    "non-canonical spent output past the last spend input",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 0n,
      input_index: 1n,
    }),
    GAP,
  ],
  [
    "non-canonical spent output of an absent reference input",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 1n,
      input_index: 0n,
    }),
    GAP,
  ],
  [
    "non-canonical spent output at a negative index",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 0n,
      input_index: -1n,
    }),
    GAP,
  ],
  [
    "non-canonical spent output at the largest index",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 0n,
      input_index: MAX,
    }),
    GAP,
  ],
  [
    "non-canonical spent output of an input kind that does not exist",
    reason("InputSpentOutputNonCanonical", {
      source_kind: MAX,
      input_index: 0n,
    }),
    GAP,
  ],
  [
    "resolved reference script of an absent reference input",
    reason("ResolvedReferenceScriptMalformed", {
      source_kind: 1n,
      input_index: 0n,
    }),
    fault("nativeScriptDecoding"),
  ],
  [
    "spend signer past the last spend input",
    reason("SpendInputSignerMissing", { input_index: 1n }),
    fault("spendInputSignerMissing"),
  ],
  [
    "spend signer at the largest index",
    reason("SpendInputSignerMissing", { input_index: MAX }),
    GAP,
  ],
  // Witness claims.
  [
    "address witness past the last one",
    reason("AddressWitnessSignatureInvalid", { witness_index: 1n }),
    fault("invalidSignature"),
  ],
  [
    "witness header past the last witness script",
    reason("WitnessScriptHeaderMalformed", { script_index: 1n }),
    GAP,
  ],
  [
    "witness header at the largest index",
    reason("WitnessScriptHeaderMalformed", { script_index: MAX }),
    GAP,
  ],
  [
    "malformed witness script past the last one",
    reason("WitnessNativeScriptMalformed", { script_index: 1n }),
    GAP,
  ],
  [
    "malformed witness script at a negative index",
    reason("WitnessNativeScriptMalformed", { script_index: -1n }),
    GAP,
  ],
  [
    "witness script node limit past the last one",
    reason("WitnessNativeScriptNodeLimit", { script_index: 1n }),
    GAP,
  ],
  [
    "witness script depth limit past the last one",
    reason("WitnessNativeScriptDepthLimit", { script_index: 1n }),
    GAP,
  ],
  [
    "false witness script past the last one",
    reason("WitnessNativeScriptFalse", { script_index: 1n }),
    GAP,
  ],
  [
    "in-range witness script node limit",
    reason("WitnessNativeScriptNodeLimit", { script_index: 0n }),
    fault("witnessScriptDecoding"),
  ],
  [
    "unused witness script past the last one",
    reason("UnusedScriptWitness", { script_index: 1n }),
    fault("unusedScriptWitness"),
  ],
  // Redeemer claims, against a transaction with no redeemers.
  [
    "malformed redeemer",
    reason("RedeemerMalformed", { redeemer_index: 0n }),
    GAP,
  ],
  [
    "malformed redeemer at the largest index",
    reason("RedeemerMalformed", { redeemer_index: MAX }),
    GAP,
  ],
  [
    "unused redeemer",
    reason("UnusedRedeemer", { redeemer_index: 1n }),
    fault("unusedRedeemer"),
  ],
  [
    "failed Plutus execution of a native mint",
    reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index: 0n }),
    GAP,
  ],
  [
    "failed Plutus execution past the last mint",
    reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index: 1n }),
    GAP,
  ],
  [
    "failed Plutus execution of an absent receive purpose",
    reason("PlutusExecutionFailed", { purpose_kind: 3n, purpose_index: 0n }),
    GAP,
  ],
  // Output claims.
  [
    "non-canonical output past the last output",
    reason("OutputNonCanonical", { output_index: 1n }),
    GAP,
  ],
  [
    "non-canonical output at a negative index",
    reason("OutputNonCanonical", { output_index: -1n }),
    GAP,
  ],
  [
    "output below min-ADA past the last output",
    reason("OutputBelowMinAda", { output_index: 1n }),
    GAP,
  ],
  [
    "output below min-ADA at the largest index",
    reason("OutputBelowMinAda", { output_index: MAX }),
    GAP,
  ],
  [
    "malformed reference script of an output without one",
    reason("OutputReferenceScriptMalformed", { output_index: 0n }),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "reference script node limit of an output without one",
    reason("OutputReferenceScriptNodeLimit", { output_index: 0n }),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "reference script depth limit of an output without one",
    reason("OutputReferenceScriptDepthLimit", { output_index: 0n }),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "malformed reference script past the last output",
    reason("OutputReferenceScriptMalformed", { output_index: 1n }),
    GAP,
  ],
  [
    "malformed reference script at a negative index",
    reason("OutputReferenceScriptMalformed", { output_index: -1n }),
    GAP,
  ],
  [
    "malformed reference script at the largest index",
    reason("OutputReferenceScriptMalformed", { output_index: MAX }),
    GAP,
  ],
  [
    "protected output signer past the last output",
    reason("ProtectedOutputSignerMissing", { output_index: 1n }),
    fault("protectedOutputSignerMissing"),
  ],
  [
    "protected output signer at the largest index",
    reason("ProtectedOutputSignerMissing", { output_index: MAX }),
    GAP,
  ],
  // Mint claims.
  [
    "declared asset limit past the last policy",
    reason("MintDeclaredAssetLimit", { policy_index: 1n }),
    GAP,
  ],
  [
    "declared asset limit at a negative index",
    reason("MintDeclaredAssetLimit", { policy_index: -1n }),
    GAP,
  ],
  [
    "declared asset limit at the largest index",
    reason("MintDeclaredAssetLimit", { policy_index: MAX }),
    GAP,
  ],
  [
    "malformed mint item past the last policy",
    reason("MintItemMalformed", { policy_index: 1n }),
    GAP,
  ],
  // Observer claims, against a transaction with no observers.
  [
    "observer order at the largest index",
    reason("ObserverOrderInvalid", { observer_index: MAX }),
    GAP,
  ],
  [
    "observer order of an absent observer",
    reason("ObserverOrderInvalid", { observer_index: 0n }),
    fault("observerOrderInvalid"),
  ],
  // Plutus decode claims.
  [
    "undecodable Plutus witness past the last witness script",
    plutusUndecodable(0n, 1n, 1n),
    GAP,
  ],
  [
    "undecodable Plutus reference script of an output without one",
    plutusUndecodable(1n, 0n, 1n),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "undecodable Plutus reference script past the last output",
    plutusUndecodable(1n, MAX, 1n),
    GAP,
  ],
  [
    "undecodable Plutus item of a kind that does not exist",
    plutusUndecodable(2n, 0n, 1n),
    GAP,
  ],
  [
    "undecodable Plutus reference script naming no decode class",
    plutusUndecodable(1n, 0n, 0n),
    GAP,
  ],
  [
    "undecodable Plutus witness naming no decode class",
    plutusUndecodable(0n, 0n, 0n),
    GAP,
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
