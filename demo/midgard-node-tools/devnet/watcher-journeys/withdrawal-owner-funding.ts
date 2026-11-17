import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_MAX_OUTPUT_INDEX,
  type MidgardTxOutput,
} from "@al-ft/midgard-core";
import { TX_HASH_BYTE_LENGTH } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  outputCborMeetsMinAda,
  outputCborMinAdaLovelace,
} from "@al-ft/midgard-validation";
import type { CML } from "@lucid-evolution/lucid";

import {
  type HistoryPredecessor,
  ownedHistoryLedgerEntries,
  signHistoryTransaction,
} from "./history-cases.js";
import { ownedOutputWithdrawalBody } from "./history-events.js";
import { buildAtJourneyMinFee } from "./journey-fee.js";

/**
 * What a withdrawal family needs of the ledger owner's outputs: one output
 * it withdraws whole, or one it first splits into the output it withdraws
 * and change.
 */
export type WithdrawalFundingShape = "withdrawal" | "split";

const WITHDRAWAL_FUNDING_SHAPES: Readonly<
  Record<string, WithdrawalFundingShape>
> = {
  fabricatedWithdrawal: "withdrawal",
  withdrawalMistag: "withdrawal",
  doubleWithdraw: "withdrawal",
  withdrawnInput: "split",
  withdrawnReferenceInput: "split",
};

/** The family's funding shape; undefined for a family that withdraws nothing. */
export const withdrawalFundingShape = (
  category: string,
): WithdrawalFundingShape | undefined =>
  Object.hasOwn(WITHDRAWAL_FUNDING_SHAPES, category)
    ? WITHDRAWAL_FUNDING_SHAPES[category]
    : undefined;

/**
 * What the SDK requires of a withdrawal of `body`: the lovelace its Order
 * locks, and the least lovelace the withdrawn Value must hold for its payout
 * output. The SDK refuses a withdrawal whose withdrawn lovelace is below
 * either (`assertEventHistoryAdmissionFunding`).
 */
export type WithdrawalQuote = (
  body: SDK.WithdrawalBody,
) => Promise<{ orderLovelace: bigint; targetMinimum: bigint }>;

/** An output of the ledger owner's: its reference and canonical bytes. */
export type OwnedOutput = Readonly<{ outRef: Buffer; output: Buffer }>;

/** The header fields a split prices and signs against. */
export type SplitHeader = Pick<
  SDK.Header,
  "minFeeA" | "minFeeB" | "expectedNetworkId"
>;

const lovelaceOf = (entry: OwnedOutput) =>
  decodeMidgardTxOutput(entry.output).value.lovelace;

const withLovelace = (
  output: MidgardTxOutput,
  lovelace: bigint,
): MidgardTxOutput => ({ ...output, value: { ...output.value, lovelace } });

/**
 * The least lovelace a withdrawal of `output` at `outRef` needs to withdraw.
 * What the SDK requires grows only with the encoding width of the withdrawn
 * lovelace, so the walk starts at the widest amount the SDK funds
 * (`EVENT_HISTORY_MAX_PROTECTION_TIME`, the width its own quote prices
 * against) and steps down to what the previous amount required until that
 * requirement stops falling. Each amount it reaches covers its own
 * requirement, and the walk strictly decreases until it stops.
 */
export const withdrawalRequirement = async (
  quote: WithdrawalQuote,
  input: Readonly<{ owner: string; outRef: Buffer; output: MidgardTxOutput }>,
): Promise<bigint> => {
  const required = async (lovelace: bigint) => {
    const { orderLovelace, targetMinimum } = await quote(
      ownedOutputWithdrawalBody({
        owner: input.owner,
        outRef: input.outRef,
        output: withLovelace(input.output, lovelace),
      }),
    );
    return orderLovelace > targetMinimum ? orderLovelace : targetMinimum;
  };
  let lovelace = SDK.EVENT_HISTORY_MAX_PROTECTION_TIME;
  for (;;) {
    const next = await required(lovelace);
    if (next >= lovelace) return lovelace;
    lovelace = next;
  }
};

/** The split's first output, before the split exists: its index, and a
 * transaction id of the same width. */
const splitFirstOutRef = (txId: Uint8Array) =>
  encodeMidgardSpendInputItem({ txId, outputIndex: 0 });

/**
 * A split of `entry` priced at its minimum fee: the first output keeps the
 * entry's assets with exactly `orderLovelace`, and the change, at the
 * entry's address with no assets, holds `changeLovelace(fee)`.
 */
const pricedSplit = (
  input: Readonly<{
    header: SplitHeader;
    key: CML.PrivateKey;
    entry: OwnedOutput;
    orderLovelace: bigint;
    changeLovelace(fee: bigint): bigint;
    available?: bigint;
  }>,
) => {
  const output = decodeMidgardTxOutput(input.entry.output);
  const first = encodeMidgardTxOutput(
    withLovelace(output, input.orderLovelace),
  );
  return buildAtJourneyMinFee(
    input.header,
    (fee) => {
      const change = encodeMidgardTxOutput({
        ...output,
        value: { lovelace: input.changeLovelace(fee), assets: new Map() },
      });
      const transaction = signHistoryTransaction(
        {
          spendInputs: [input.entry.outRef],
          outputs: [first, change],
          fee,
          networkId: input.header.expectedNetworkId,
        },
        input.key,
      );
      return { ...transaction, transaction, fee, first, change };
    },
    input.available,
  );
};

/**
 * The split of `entry`: its first output keeps the entry's assets with
 * exactly `orderLovelace`, which the family withdraws; the change, at the
 * entry's address, takes the rest less the split's fee. It refuses a split
 * that leaves either output below its minimum ADA.
 */
export const signOwnerSplit = (
  input: Readonly<{
    header: SplitHeader;
    key: CML.PrivateKey;
    entry: OwnedOutput;
    orderLovelace: bigint;
  }>,
) => {
  const available = lovelaceOf(input.entry) - input.orderLovelace;
  const split = pricedSplit({
    ...input,
    changeLovelace: (fee) => available - fee,
    available,
  });
  if (
    !outputCborMeetsMinAda(split.first, input.orderLovelace) ||
    !outputCborMeetsMinAda(split.change, available - split.fee)
  )
    throw new Error(
      "The ledger owner's split leaves an output below its minimum ADA",
    );
  return split.transaction;
};

/**
 * What a split of `entry` needs: the withdrawn first output's requirement,
 * the least change at the entry's address that meets its own minimum ADA,
 * and the fee of the split that pays exactly those. An entry holding at
 * least the total splits with change at least that minimum.
 */
export const splitRequirement = async (
  quote: WithdrawalQuote,
  input: Readonly<{
    owner: string;
    header: SplitHeader;
    key: CML.PrivateKey;
    entry: OwnedOutput;
  }>,
) => {
  const output = decodeMidgardTxOutput(input.entry.output);
  const orderLovelace = await withdrawalRequirement(quote, {
    owner: input.owner,
    outRef: splitFirstOutRef(
      decodeMidgardSpendInputItem(input.entry.outRef).txId,
    ),
    output,
  });
  const changeAt = (lovelace: bigint) =>
    encodeMidgardTxOutput({
      ...output,
      value: { lovelace, assets: new Map() },
    });
  // The minimum grows only with the change's own encoding width.
  let change = 0n;
  for (;;) {
    const minimum = outputCborMinAdaLovelace(changeAt(change));
    if (minimum <= change) break;
    change = minimum;
  }
  const { fee } = pricedSplit({
    header: input.header,
    key: input.key,
    entry: input.entry,
    orderLovelace,
    changeLovelace: () => change,
  });
  return { orderLovelace, total: orderLovelace + change + fee };
};

/**
 * What one withdrawal family needs of the ledger owner, read against the
 * tail it will build on.
 */
export type OwnerFundingRequirement = Readonly<{
  shape: WithdrawalFundingShape;
  /** The lovelace an output at the top-up's address needs to qualify. */
  topUpLovelace: bigint;
  /** What `entry` needs to qualify. */
  requiredOf(entry: OwnedOutput): Promise<bigint>;
}>;

/**
 * What `entry` needs to fund a family of `shape`, with the lovelace its
 * withdrawn output then holds: the whole entry for a withdrawal, the split's
 * first output for a split.
 */
export const ownerOutputRequirement =
  (
    input: Readonly<{
      shape: WithdrawalFundingShape;
      quote: WithdrawalQuote;
      key: CML.PrivateKey;
      header: SplitHeader;
    }>,
  ) =>
  async (
    entry: OwnedOutput,
  ): Promise<{ total: bigint; orderLovelace: bigint }> => {
    const owner = input.key.to_public().hash().to_hex();
    if (input.shape === "split")
      return splitRequirement(input.quote, {
        owner,
        header: input.header,
        key: input.key,
        entry,
      });
    const total = await withdrawalRequirement(input.quote, {
      owner,
      outRef: entry.outRef,
      output: decodeMidgardTxOutput(entry.output),
    });
    return { total, orderLovelace: total };
  };

/**
 * The requirement of a family of `shape`. The top-up is sized for a fresh
 * output of the owner's at `topUpAddress` holding no assets, at the widest
 * reference its spend-input encoding allows: a 32-byte transaction id and
 * the highest output index, whose Data encoding in the withdrawal body is
 * the widest. Whatever reference the deposit's output gets, its requirement
 * is no more than the top-up.
 */
export const withdrawalOwnerRequirement = async (
  input: Readonly<{
    shape: WithdrawalFundingShape;
    quote: WithdrawalQuote;
    key: CML.PrivateKey;
    header: SplitHeader;
    /** The canonical address bytes a top-up deposit's output carries. */
    topUpAddress: Buffer;
  }>,
): Promise<OwnerFundingRequirement> => {
  const required = ownerOutputRequirement(input);
  const requiredOf = async (entry: OwnedOutput) =>
    (await required(entry)).total;
  const template: OwnedOutput = {
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(TX_HASH_BYTE_LENGTH),
      outputIndex: MIDGARD_MAX_OUTPUT_INDEX,
    }),
    output: encodeMidgardTxOutput({
      address: input.topUpAddress,
      value: { lovelace: 0n, assets: new Map() },
    }),
  };
  return {
    shape: input.shape,
    topUpLovelace: await requiredOf(template),
    requiredOf,
  };
};

/**
 * The owner's smallest output that meets `requirement`, so a family leaves
 * the larger ones to later families; undefined when none does.
 */
export const smallestQualifyingOutput = async (
  entries: readonly OwnedOutput[],
  requirement: Pick<OwnerFundingRequirement, "requiredOf">,
): Promise<OwnedOutput | undefined> => {
  const ascending = [...entries].sort((left, right) => {
    const difference = lovelaceOf(left) - lovelaceOf(right);
    return difference < 0n ? -1 : difference > 0n ? 1 : 0;
  });
  for (const entry of ascending)
    if (lovelaceOf(entry) >= (await requirement.requiredOf(entry)))
      return entry;
  return undefined;
};

/** The owner's outputs on `tail` that meet `requirement`, smallest first. */
export const qualifyingOwnedOutput = (
  tail: HistoryPredecessor,
  owner: string,
  requirement: Pick<OwnerFundingRequirement, "requiredOf">,
) =>
  smallestQualifyingOutput(ownedHistoryLedgerEntries(tail, owner), requirement);

/** What a withdrawal family's funding before its timer reads and does. */
export type WithdrawalOwnerFundingPorts = Readonly<{
  owner: string;
  /** The tail the family builds on; undefined before the run's first block. */
  readTail(): Promise<HistoryPredecessor | undefined>;
  requirement(tail: HistoryPredecessor): Promise<OwnerFundingRequirement>;
  /** Deposit exactly `requirement.topUpLovelace` and wait until it is absorbed. */
  topUp(requirement: OwnerFundingRequirement): Promise<void>;
  /** The owner is funded: set aside any top-up still recorded as pending. */
  settle(requirement: OwnerFundingRequirement): Promise<void>;
  onStage(name: string): void;
}>;

/**
 * Before a withdrawal family's timer starts: when no output of the ledger
 * owner's on the tail meets the family's requirement, one deposit of exactly
 * the requirement tops it up, and the new tail must then hold one that
 * does. An earlier family that spent or split the owner's outputs therefore
 * never starves a later one, whatever their order. Whichever way the owner
 * is found funded, no top-up record outlives it. Before the run's first
 * block there is no tail and nothing is topped up: only the deposit trace
 * that commits that block funds the owner, unchecked until the family looks
 * for its output.
 */
export const ensureWithdrawalOwnerFunding = async (
  ports: WithdrawalOwnerFundingPorts,
): Promise<"no-tail" | "funded" | "topped-up"> => {
  const tail = await ports.readTail();
  if (tail === undefined) {
    ports.onStage(
      "no tail yet, so no top-up: only the run's first deposit trace funds the owner",
    );
    return "no-tail";
  }
  const requirement = await ports.requirement(tail);
  if (
    (await qualifyingOwnedOutput(tail, ports.owner, requirement)) !== undefined
  ) {
    await ports.settle(requirement);
    return "funded";
  }
  ports.onStage(
    `topping up the ledger owner with ${requirement.topUpLovelace.toString()} lovelace for a ${requirement.shape}`,
  );
  await ports.topUp(requirement);
  const after = await ports.readTail();
  if (
    after === undefined ||
    (await qualifyingOwnedOutput(after, ports.owner, requirement)) === undefined
  )
    throw new Error(
      `The ledger owner's top-up of ${requirement.topUpLovelace.toString()} lovelace left no output that funds a ${requirement.shape}`,
    );
  await ports.settle(requirement);
  return "topped-up";
};
