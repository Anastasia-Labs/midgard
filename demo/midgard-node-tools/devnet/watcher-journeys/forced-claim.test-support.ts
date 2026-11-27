/**
 * One valid native forced transaction, and a classifier for a block whose
 * forced leaf rejects it with a chosen `ForcedTxInvalid{reason}`.
 *
 * The forced transaction spends one input, pays one output without a
 * reference script, mints under one native policy and carries one native
 * witness script, one address witness and no redeemers, reference inputs,
 * observers or required signers.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type { HeaderDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import type * as SDK from "@al-ft/midgard-sdk/core";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";

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

const buildFixture = async () => {
  const predecessor = await checkedFixtureBlock(
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
  const forced = {
    transaction: prepareJourneyForcedTransaction({
      predecessor,
      ledgerOwnerSeedPhrase: account.seedPhrase,
    }).transaction,
    orderKey,
  };
  return { predecessor, forced };
};
let fixture: ReturnType<typeof buildFixture> | undefined;

/** The largest coordinate a `RejectionReasonV1` encodes. */
export const MAX = (1n << 64n) - 1n;
/** The smallest coordinate a `RejectionReasonV1` encodes. */
export const MIN = -(1n << 64n);
/** The first coordinate past the integers a JavaScript number holds exactly. */
export const BIG = (1n << 53n) + 1n;

export const reason = (arm: string, fields: Record<string, bigint>) =>
  ({ [arm]: fields }) as unknown as SDK.RejectionReason;

export const fault = (category: string) => `fault_detected:${category}`;

export const outcome = (decision: HeaderDecision): string =>
  decision.decision === "fault_detected"
    ? fault(decision.category)
    : decision.decision === "unprovable"
      ? `unprovable:${decision.violationId}`
      : decision.decision;

/** Classifies a block whose forced leaf rejects the transaction for `claimed`. */
export const classifyFull = async (claimed: SDK.RejectionReason) => {
  const { predecessor, forced } = await (fixture ??= buildFixture());
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: forced.transaction.canonicalCbor,
    predecessor,
    ledgerEntries: predecessor.ledgerEntries,
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

export const classify = async (claimed: SDK.RejectionReason): Promise<string> =>
  outcome((await classifyFull(claimed)).decision);
