import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  paymentCredentialFromMidgardAddress,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildAtJourneyMinFee } from "./journey-fee.js";
import {
  checkedJourneyConstruction,
  type JourneyLedgerPredecessor,
} from "./journey-ledger.js";
import { journeyNetwork } from "./journey-network.js";

export const JOURNEY_HISTORY_TRANSACTION_CATEGORIES = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "noReferenceInput",
  "referenceInputNoIdx",
] as const;
export type JourneyHistoryTransactionCategory =
  (typeof JOURNEY_HISTORY_TRANSACTION_CATEGORIES)[number];

export type HistoryPredecessor = JourneyLedgerPredecessor;

export type HistoryTransactionInput = {
  category: JourneyHistoryTransactionCategory;
  predecessor: HistoryPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  honest?: boolean;
};

const ledgerEntries = (block: HistoryPredecessor) => block.ledger.entries;

/** Select only outputs spendable by the retained ledger owner. */
export const ownedHistoryLedgerEntries = (
  block: HistoryPredecessor,
  owner: string,
) =>
  ledgerEntries(block).filter(({ output }) => {
    const credential = paymentCredentialFromMidgardAddress(
      decodeMidgardTxOutput(output).address,
    );
    return (
      credential.kind === "PubKey" && credential.hash.toString("hex") === owner
    );
  });

/** Sign every body after mutation so only the selected ledger rule is broken. */
export const signHistoryTransaction = (
  transaction: FixtureTransactionInput,
  key: CML.PrivateKey,
): FixtureTransaction => {
  const unsigned = buildFixtureTransaction(transaction);
  return buildFixtureTransaction({
    ...transaction,
    addressWitnesses: [
      {
        verification_key: Buffer.from(key.to_public().to_raw_bytes()).toString(
          "hex",
        ),
        signature: key.sign(Buffer.from(unsigned.txId, "hex")).to_hex(),
      },
    ],
  });
};

/**
 * Assemble consecutive transaction transitions from actual validation-machine
 * traces. The caller supplies the prior ledger, sourced from staged protocol
 * history; no made-up predecessor ledger or validation witness is introduced.
 */
export const retainHistoryTransactions = async (
  input: Omit<
    HistoryTransactionInput,
    "category" | "ledgerOwnerSeedPhrase" | "honest"
  > & {
    transactions: readonly FixtureTransaction[];
  },
) => {
  let ledger = ledgerEntries(input.predecessor);
  let priorRoot = input.predecessor.header.utxosRoot;
  const retained: Awaited<ReturnType<typeof retainedTransactionFixture>>[] = [];
  for (const transaction of input.transactions) {
    const block = await retainedTransactionFixture({
      ...input,
      canonicalTransactionCbor: transaction.canonicalCbor,
      ledgerEntries: ledger,
      predecessor: {
        header: { ...input.predecessor.header, utxosRoot: priorRoot },
        headerHash: input.predecessor.headerHash,
      },
    });
    retained.push(block);
    ledger = (await checkedJourneyConstruction(block, block.postLedgerEntries))
      .ledger.entries;
    priorRoot = block.header.utxosRoot;
  }
  const base = await buildCanonicalBlockFixture({
    transactions: input.transactions,
    utxos: ledger.map(({ outRef, output }) => ({ key: outRef, value: output })),
    startTime: input.predecessor.header.endTime,
    endTime: input.endTime,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    minFeeA: input.predecessor.header.minFeeA,
    minFeeB: input.predecessor.header.minFeeB,
  });
  const descriptors = retained.flatMap(
    (block) => block.payload.block_body.validation_traces,
  );
  const transitions: SDK.DaPayloadEntry[] = retained.map((block, index) => {
    const source = block.payload.block_body.transition_trace[0];
    if (source === undefined)
      throw new Error("Retained transaction has no transition");
    return [
      Data.to(BigInt(index)),
      Data.to(
        {
          ...Data.from(source[1], SDK.TransitionStep),
          step_index: BigInt(index),
        },
        SDK.TransitionStep,
      ),
    ];
  });
  const root = (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const transition = await root(SDK.ROOT_DOMAINS.transitionTrace, transitions);
  // The header keeps the base's pinned validation-trace pair; the body keeps
  // the retained traces.
  const header: SDK.Header = {
    ...base.header,
    expectedNetworkId: input.predecessor.header.expectedNetworkId,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    transitionTraceRoot: transition.root,
    transitionStepCount: BigInt(transitions.length),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const sorted = (entries: SDK.DaPayloadEntry[]) =>
    entries.sort(([left], [right]) => left.localeCompare(right));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts: {
        ...base.payload.block_body.counts,
        transitionStepCount: BigInt(transitions.length),
      },
      validation_traces: sorted(descriptors),
      validation_trace_witnesses: [],
      transition_trace: sorted(transitions),
    },
  };
  return checkedJourneyConstruction(
    {
      header,
      headerHash,
      payload,
      payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
        mode: "identity",
      }),
      transactions: input.transactions,
      replays: retained.map((block) => block.replay),
    },
    ledger,
  );
};

export const buildJourneyHistoryTransaction = async (
  input: HistoryTransactionInput,
) => {
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, {
      network: journeyNetwork().network,
    }).paymentKey,
  );
  const spent = ownedHistoryLedgerEntries(
    input.predecessor,
    key.to_public().hash().to_hex(),
  )[0];
  if (spent === undefined)
    throw new Error(
      "History journey requires an output controlled by its ledger owner",
    );
  const output = decodeMidgardTxOutput(spent.output);
  /** An output to the spent output's address paying `lovelace` and `assets`. */
  const paying = (lovelace: bigint, assets = output.value.assets) =>
    encodeMidgardTxOutput({ ...output, value: { lovelace, assets } });
  /**
   * The signed transaction spending `spendInput`, worth `available`, at the
   * minimum fee the predecessor's schedule prices it at plus `surplus`; its
   * last output, the `change`, pays `available` less the fee and the earlier
   * outputs.
   */
  const transaction = (
    spendInput: Buffer,
    available: bigint,
    changes: Partial<FixtureTransactionInput> = {},
    surplus = 0n,
  ) => {
    const { outputs: fixed = [], ...rest } = changes;
    const kept = fixed.reduce(
      (sum, bytes) => sum + decodeMidgardTxOutput(bytes).value.lovelace,
      0n,
    );
    return buildAtJourneyMinFee(
      input.predecessor.header,
      (minimum) => {
        const fee = minimum + surplus;
        const change = available - kept - fee;
        return {
          ...signHistoryTransaction(
            {
              spendInputs: [spendInput],
              outputs: [
                ...fixed,
                paying(
                  change,
                  fixed.length === 0 ? output.value.assets : new Map(),
                ),
              ],
              fee,
              networkId: input.predecessor.header.expectedNetworkId,
              ...rest,
            },
            key,
          ),
          change,
        };
      },
      available - kept,
    );
  };
  const lovelace = output.value.lovelace;
  let transactions: FixtureTransaction[];
  if (
    input.category === "nonExistentInput" ||
    input.category === "noReferenceInput"
  ) {
    const absent = outRefCbor(0xf1, 0n);
    if (
      ledgerEntries(input.predecessor).some((entry) =>
        entry.outRef.equals(absent),
      )
    )
      throw new Error("Absent-input fixture collides with retained ledger");
    transactions = [
      input.category === "nonExistentInput"
        ? transaction(input.honest ? spent.outRef : absent, lovelace)
        : transaction(spent.outRef, lovelace, {
            referenceInputs: input.honest ? [] : [absent],
          }),
    ];
  } else if (input.category === "referenceInputNoIdx") {
    // The producer creates two outputs: consume one and reference the other.
    // This avoids overlapping spend/reference sets in the accepted control.
    const half = lovelace / 2n;
    const producer = transaction(spent.outRef, lovelace, {
      outputs: [paying(half)],
    });
    const produced = (index: number) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: index,
      });
    const consumer = transaction(produced(0), half, {
      referenceInputs: [produced(input.honest ? 1 : 2)],
    });
    transactions = [producer, consumer];
  } else {
    const producer = transaction(spent.outRef, lovelace);
    const produced = (index: number) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: index,
      });
    transactions = [
      producer,
      input.category === "doubleSpend"
        ? input.honest
          ? transaction(produced(0), producer.change)
          : // One lovelace above the minimum fee makes this a distinct signed
            // body spending the producer's input again.
            transaction(spent.outRef, lovelace, {}, 1n)
        : transaction(
            input.honest ? produced(0) : produced(1),
            producer.change,
          ),
    ];
  }
  return retainHistoryTransactions({ ...input, transactions });
};
