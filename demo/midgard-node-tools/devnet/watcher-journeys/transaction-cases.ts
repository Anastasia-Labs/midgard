import {
  decodeMidgardTxOutput,
  encodeCbor,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk/core";

import {
  journeyOwnerEntries,
  journeyOwnerKey,
  type JourneyOwnerTransactionInput,
} from "./forced-transaction.js";
import { buildAtJourneyMinFee } from "./journey-fee.js";
import {
  checkedJourneyConstruction,
  type JourneyLedgerPredecessor,
} from "./journey-ledger.js";
import {
  type OwnedOutput,
  type OwnerSpend,
  selectOwnerOutput,
} from "./owner-output-selection.js";

type Mutation = (input: {
  spendInput: Buffer;
  output: ReturnType<typeof decodeMidgardTxOutput>;
}) => Partial<FixtureTransactionInput>;

/** Deliberately invalid operator transactions; no watcher decision is supplied. */
const mutations = {
  zeroInput: () => ({ spendInputs: [] }),
  invalidRange: () => ({
    validityIntervalStart: 30n,
    validityIntervalEnd: 20n,
  }),
  invalidSignature: () => ({}),
  inputSetUniqueness: ({ spendInput }) => ({
    referenceInputs: [spendInput],
  }),
  networkId: () => ({ networkId: 1n }),
  mintAuthorization: ({ output }) => {
    const policy = "e3".repeat(28);
    const assets = new Map(output.value.assets);
    const tokens = new Map(assets.get(policy));
    tokens.set("", (tokens.get("") ?? 0n) + 1n);
    assets.set(policy, tokens);
    return {
      mintPolicyItems: [
        encodeCbor([
          Buffer.from(policy, "hex"),
          new Map([[Buffer.alloc(0), 1n]]),
        ]),
      ],
      outputs: [
        encodeMidgardTxOutput({
          ...output,
          value: { ...output.value, assets },
        }),
      ],
    };
  },
  minFee: () => ({ fee: 0n }),
  spendInputSignerMissing: () => ({ addressWitnesses: [] }),
  protectedOutputSignerMissing: ({ output }) => ({
    outputs: [
      encodeMidgardTxOutput({
        ...output,
        address: Buffer.concat([Buffer.from([0x68]), Buffer.alloc(28, 0xe2)]),
      }),
    ],
  }),
  observersForbiddenOnUntaggedNetwork: () => ({
    networkId: 255n,
    requiredObservers: [Buffer.alloc(28, 1)],
    scriptIntegrityHash: Buffer.alloc(32, 1),
  }),
  observerOrderInvalid: () => ({
    scriptIntegrityHash: Buffer.alloc(32, 1),
    requiredObservers: [Buffer.alloc(28, 2), Buffer.alloc(28, 1)],
  }),
} satisfies Partial<Record<SDK.FraudProofCatalogueCategoryName, Mutation>>;

export type JourneyTransactionCategory = keyof typeof mutations;
export const JOURNEY_TRANSACTION_CATEGORIES = Object.keys(
  mutations,
) as JourneyTransactionCategory[];

export type JourneyTransactionInput = {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

/**
 * The family's fee schedule: the minFee family's fault and control are priced
 * at a 1-lovelace constant so its fault breaks only the fee rule.
 */
const familyMinFeeB = (
  input: JourneyOwnerTransactionInput,
  family: JourneyTransactionCategory | undefined,
) => (family === "minFee" ? 1n : input.predecessor.header.minFeeB);

/**
 * The owner's signed transaction spending `spent` into one output carrying its
 * value less the fee, which `category` mutates (none: the valid one). The
 * minFee fault pays no fee. Every other transaction pays the minimum its
 * schedule prices it at, so a fault breaks only its own rule. Its `outputs`
 * are the outputs it pays.
 */
const journeyTransactionOf = (
  input: JourneyOwnerTransactionInput,
  spent: OwnedOutput,
  category: JourneyTransactionCategory | undefined,
  minFeeB: bigint,
) => {
  const key = journeyOwnerKey(input.ledgerOwnerSeedPhrase);
  const output = decodeMidgardTxOutput(spent.output);
  /** The spent output less the fee: the transaction's one output. */
  const paying = (fee: bigint) => ({
    ...output,
    value: { ...output.value, lovelace: output.value.lovelace - fee },
  });
  const transactionInput = (fee: bigint): FixtureTransactionInput => ({
    spendInputs: [spent.outRef],
    outputs: [encodeMidgardTxOutput(paying(fee))],
    fee,
    networkId: input.predecessor.header.expectedNetworkId,
    ...(category === undefined
      ? {}
      : mutations[category]({
          spendInput: spent.outRef,
          output: paying(fee),
        })),
  });
  const sign = (fee: bigint) => {
    const body = transactionInput(fee);
    const unsigned = buildFixtureTransaction(body);
    const transaction = buildFixtureTransaction({
      ...body,
      addressWitnesses: body.addressWitnesses ?? [
        {
          verification_key: Buffer.from(
            key.to_public().to_raw_bytes(),
          ).toString("hex"),
          signature:
            category === "invalidSignature"
              ? "00".repeat(64)
              : key.sign(Buffer.from(unsigned.txId, "hex")).to_hex(),
        },
      ],
    });
    return {
      canonicalCbor: transaction.canonicalCbor,
      transaction,
      outputs: body.outputs ?? [],
    };
  };
  return category === "minFee"
    ? sign(0n)
    : buildAtJourneyMinFee(
        { minFeeA: input.predecessor.header.minFeeA, minFeeB },
        sign,
        output.value.lovelace,
      );
};

/**
 * What a transaction family pays from an output: its valid control and, for a
 * fault family, its fault, so the fault and the control spend the same
 * output.
 */
export const journeyTransactionSpend =
  (
    input: JourneyOwnerTransactionInput,
    family: JourneyTransactionCategory | undefined,
  ): OwnerSpend =>
  (entry) => {
    const minFeeB = familyMinFeeB(input, family);
    return [
      ...journeyTransactionOf(input, entry, undefined, minFeeB).outputs,
      ...(family === undefined
        ? []
        : journeyTransactionOf(input, entry, family, minFeeB).outputs),
    ];
  };

/**
 * The `family`'s transaction (`faulty`: its fault; otherwise its valid
 * control), spending the owner's smallest output that funds both
 * (`selectOwnerOutput`), committed alone in a block on the predecessor.
 */
const buildJourneyTransaction = async (
  input: JourneyTransactionInput & {
    family: JourneyTransactionCategory | undefined;
    faulty: boolean;
  },
) => {
  const { family } = input;
  const minFeeB = familyMinFeeB(input, family);
  const spent = selectOwnerOutput(
    journeyOwnerEntries(input),
    journeyTransactionSpend(input, family),
    `the ${family ?? "valid"} transaction family`,
  );
  const { transaction } = journeyTransactionOf(
    input,
    spent,
    input.faulty ? family : undefined,
    minFeeB,
  );
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    minFeeB,
  });
  if (!input.faulty && block.replay.trace.verdict !== "accepted")
    throw new Error("Journey recovery transaction failed honest validation");
  return checkedJourneyConstruction(block, block.postLedgerEntries);
};

export { buildJourneyScriptTransaction } from "./script-cases.js";

export const buildJourneyTransactionFault = async (
  input: JourneyTransactionInput & { category: JourneyTransactionCategory },
) => {
  const block = await buildJourneyTransaction({
    ...input,
    family: input.category,
    faulty: true,
  });
  if (block.replay.trace.verdict !== "rejected")
    throw new Error(
      `The ${input.category} transaction passed honest validation`,
    );
  return block;
};

/** Accepted signed counterpart against the same ledger, output and fee schedule. */
export const buildJourneyTransactionControl = (
  input: JourneyTransactionInput & { category?: JourneyTransactionCategory },
) =>
  buildJourneyTransaction({ ...input, family: input.category, faulty: false });
