import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_MAX_OUTPUT_INDEX,
  paymentCredentialFromMidgardAddress,
} from "@al-ft/midgard-core";
import { TX_HASH_BYTE_LENGTH } from "@al-ft/midgard-core/out-ref";
import {
  outputCborMeetsMinAda,
  outputCborMinAdaLovelace,
} from "@al-ft/midgard-validation";

/** An output of the ledger owner's: its reference and canonical bytes. */
export type OwnedOutput = Readonly<{ outRef: Buffer; output: Buffer }>;

/** The entries whose output the payment key hash `owner` (hex) controls. */
export const ownedLedgerEntries = <T extends OwnedOutput>(
  entries: readonly T[],
  owner: string,
): T[] =>
  entries.filter(({ output }) => {
    const credential = paymentCredentialFromMidgardAddress(
      decodeMidgardTxOutput(output).address,
    );
    return (
      credential.kind === "PubKey" && credential.hash.toString("hex") === owner
    );
  });

/**
 * A fixture's spend of one output of the owner's: every output the
 * transactions it builds from `entry` pay, each at the fee its schedule
 * prices it at, across every variant the family builds from that output (its
 * fault and its valid control), so one output funds all of them.
 */
export type OwnerSpend = (entry: OwnedOutput) => readonly Buffer[];

export const lovelaceOf = (entry: OwnedOutput) =>
  decodeMidgardTxOutput(entry.output).value.lovelace;

const message = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

/**
 * Why `spend` of `entry` is not funded, or undefined when it is: the ledger
 * rules a spend's outputs must meet, each output at least its minimum ADA
 * after the fee, or the build's own refusal (an output or fee the entry
 * cannot pay).
 */
export const ownerSpendShortfall = (
  spend: OwnerSpend,
  entry: OwnedOutput,
): string | undefined => {
  try {
    for (const [index, output] of spend(entry).entries()) {
      const { lovelace } = decodeMidgardTxOutput(output).value;
      if (!outputCborMeetsMinAda(output, lovelace))
        return `its output ${index.toString()} would hold ${lovelace.toString()} lovelace, below its minimum ADA of ${outputCborMinAdaLovelace(output).toString()}`;
    }
    return undefined;
  } catch (error) {
    return message(error);
  }
};

/** `entries` smallest first; equal ones keep their order. */
export const ascendingByLovelace = <T extends OwnedOutput>(
  entries: readonly T[],
): T[] =>
  [...entries].sort((left, right) => {
    const difference = lovelaceOf(left) - lovelaceOf(right);
    return difference < 0n ? -1 : difference > 0n ? 1 : 0;
  });

/**
 * The owner's smallest output that funds `spend`, so a family leaves the
 * larger ones to later families, whatever order the ledger holds them in.
 * Equal outputs keep the ledger's order, so every build from the same
 * predecessor selects the same output. It refuses, naming why each output
 * falls short, when none funds it.
 */
export const selectOwnerOutput = (
  entries: readonly OwnedOutput[],
  spend: OwnerSpend,
  purpose: string,
): OwnedOutput => {
  const shortfalls: string[] = [];
  for (const entry of ascendingByLovelace(entries)) {
    const shortfall = ownerSpendShortfall(spend, entry);
    if (shortfall === undefined) return entry;
    shortfalls.push(
      `${entry.outRef.toString("hex")} (${lovelaceOf(entry).toString()} lovelace): ${shortfall}`,
    );
  }
  throw new Error(
    `No output of the ledger owner's funds ${purpose}: ${
      shortfalls.length === 0 ? "the owner holds none" : shortfalls.join("; ")
    }`,
  );
};

/** RFC 8949 §3: an unsigned integer's argument is at most 8 bytes. */
const CBOR_UNSIGNED_LIMIT = 1n << 64n;

/**
 * The least lovelace `entry`, holding nothing else different, needs for
 * `spend` to be funded: doubling from 1 lovelace to an amount that funds it,
 * then halving the gap. The amount it returns always funds the spend; it is
 * the least one wherever funding only grows with the amount, as it does
 * while the fee and the minimum ADA stay at one encoding width. It refuses a
 * spend no amount an output can hold funds.
 */
export const ownerSpendLeastLovelace = (
  spend: OwnerSpend,
  entry: OwnedOutput,
): bigint => {
  const output = decodeMidgardTxOutput(entry.output);
  const holding = (lovelace: bigint): OwnedOutput => ({
    outRef: entry.outRef,
    output: encodeMidgardTxOutput({
      ...output,
      value: { ...output.value, lovelace },
    }),
  });
  const funds = (lovelace: bigint) =>
    ownerSpendShortfall(spend, holding(lovelace)) === undefined;
  if (funds(0n)) return 0n;
  let low = 0n;
  let high = 1n;
  while (!funds(high)) {
    low = high;
    high *= 2n;
    if (high >= CBOR_UNSIGNED_LIMIT) {
      const widest = CBOR_UNSIGNED_LIMIT - 1n;
      const shortfall = ownerSpendShortfall(spend, holding(widest));
      if (shortfall !== undefined)
        throw new Error(
          `No amount an output can hold funds this spend: ${shortfall}`,
        );
      high = widest;
    }
  }
  while (high - low > 1n) {
    const middle = (low + high) / 2n;
    if (funds(middle)) high = middle;
    else low = middle;
  }
  return high;
};

/**
 * A fresh output of the owner's at `address` holding no assets, at the widest
 * reference its spend-input encoding allows: a 32-byte transaction id and the
 * highest output index. A requirement sized on it covers whatever reference
 * a top-up deposit's output gets, since a wider reference only widens what
 * names it.
 */
export const topUpTemplate = (address: Buffer): OwnedOutput => ({
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(TX_HASH_BYTE_LENGTH),
    outputIndex: MIDGARD_MAX_OUTPUT_INDEX,
  }),
  output: encodeMidgardTxOutput({
    address,
    value: { lovelace: 0n, assets: new Map() },
  }),
});
