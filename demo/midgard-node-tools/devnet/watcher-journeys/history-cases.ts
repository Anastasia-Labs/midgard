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

/** Catalogue ownership; readiness is established separately by the fixture tests. */
export const JOURNEY_HISTORY_CATEGORIES = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "transitionTrace",
  "noReferenceInput",
  "referenceInputNoIdx",
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "withdrawnReferenceInput",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "withdrawnInput",
] as const satisfies readonly SDK.FraudProofCatalogueCategoryName[];

export const JOURNEY_HISTORY_TRANSACTION_CATEGORIES = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "noReferenceInput",
  "referenceInputNoIdx",
] as const;
export type JourneyHistoryTransactionCategory =
  (typeof JOURNEY_HISTORY_TRANSACTION_CATEGORIES)[number];

export type HistoryPredecessor = {
  header: SDK.Header;
  headerHash: string;
  payload: SDK.DaPayload;
};

export type HistoryTransactionInput = {
  category: JourneyHistoryTransactionCategory;
  predecessor: HistoryPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  honest?: boolean;
};

const ledgerEntries = (block: HistoryPredecessor) =>
  block.payload.block_body.utxos.map(([key, value]) => ({
    outRef: Buffer.from(key, "hex"),
    output: Buffer.from(value, "hex"),
  }));

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
    ledger = ledgerEntries(block);
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
  const witnesses = retained.flatMap(
    (block) => block.payload.block_body.validation_trace_witnesses,
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
  const [validation, transition] = await Promise.all([
    root(SDK.ROOT_DOMAINS.validationTraces, descriptors),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const header: SDK.Header = {
    ...base.header,
    expectedNetworkId: input.predecessor.header.expectedNetworkId,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    validationTracesRoot: validation.root,
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
      validation_trace_witnesses: sorted(witnesses),
      transition_trace: sorted(transitions),
    },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    transactions: input.transactions,
    replays: retained.map((block) => block.replay),
  };
};

export const buildJourneyHistoryTransaction = async (
  input: HistoryTransactionInput,
) => {
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
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
  const transaction = (
    spendInput: Buffer,
    changes: Partial<FixtureTransactionInput> = {},
  ) =>
    signHistoryTransaction(
      {
        spendInputs: [spendInput],
        outputs: [spent.output],
        fee: 0n,
        networkId: input.predecessor.header.expectedNetworkId,
        ...changes,
      },
      key,
    );
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
        ? transaction(input.honest ? spent.outRef : absent)
        : transaction(spent.outRef, {
            referenceInputs: input.honest ? [] : [absent],
          }),
    ];
  } else {
    const producer = transaction(spent.outRef);
    const produced = (index: number) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: index,
      });
    const validInput = produced(0);
    let consumer: FixtureTransaction;
    if (input.category === "doubleSpend") {
      // A different fee and balanced output make this a distinct signed body.
      consumer = transaction(input.honest ? validInput : spent.outRef, {
        fee: 1n,
        outputs: [
          encodeMidgardTxOutput({
            ...output,
            value: { ...output.value, lovelace: output.value.lovelace - 1n },
          }),
        ],
      });
    } else if (input.category === "nonExistentInputNoIndex") {
      consumer = transaction(input.honest ? validInput : produced(1));
    } else {
      // The producer creates two outputs: consume one and reference the other.
      // This avoids overlapping spend/reference sets in the accepted control.
      const half = output.value.lovelace / 2n;
      const producerWithReference = transaction(spent.outRef, {
        outputs: [
          encodeMidgardTxOutput({
            ...output,
            value: { ...output.value, lovelace: half },
          }),
          encodeMidgardTxOutput({
            ...output,
            value: {
              lovelace: output.value.lovelace - half,
              assets: new Map(),
            },
          }),
        ],
      });
      const producerRef = (index: number) =>
        encodeMidgardSpendInputItem({
          txId: Buffer.from(producerWithReference.txId, "hex"),
          outputIndex: index,
        });
      consumer = transaction(producerRef(0), {
        referenceInputs: [producerRef(input.honest ? 1 : 2)],
        outputs: [
          encodeMidgardTxOutput({
            ...output,
            value: { ...output.value, lovelace: half },
          }),
        ],
      });
      transactions = [producerWithReference, consumer];
      return retainHistoryTransactions({ ...input, transactions });
    }
    transactions = [producer, consumer];
  }
  return retainHistoryTransactions({ ...input, transactions });
};
