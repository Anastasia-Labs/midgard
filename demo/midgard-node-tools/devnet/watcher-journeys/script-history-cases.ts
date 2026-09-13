import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

import {
  type HistoryTransactionInput,
  retainHistoryTransactions,
} from "./history-cases.js";
import { buildJourneyScriptTransaction } from "./script-cases.js";

export const JOURNEY_SCRIPT_HISTORY_CATEGORIES = [
  "missingNativeScriptTx",
  "missingNativeScriptUtxo",
] as const;
export type JourneyScriptHistoryCategory =
  (typeof JOURNEY_SCRIPT_HISTORY_CATEGORIES)[number];
type HistoryBuildInput = Omit<HistoryTransactionInput, "category" | "honest">;
export type JourneyScriptHistoryInput = HistoryBuildInput & {
  category: JourneyScriptHistoryCategory;
};

const producerTransaction = (input: HistoryBuildInput) =>
  buildJourneyScriptTransaction({
    ...input,
    variant: "nativeReceive",
    outputReferenceScript: false,
  }).transaction;

const consumerTransaction = (
  input: HistoryBuildInput,
  spent: { outRef: Buffer; output: Buffer },
  honest: boolean,
) => {
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
  );
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const tx: FixtureTransactionInput = {
    spendInputs: [spent.outRef],
    outputs: [
      encodeMidgardTxOutput({
        ...decodeMidgardTxOutput(spent.output),
        address: Buffer.concat([Buffer.from([0x68]), owner]),
        script_ref: undefined,
      }),
    ],
    fee: 0n,
    networkId: input.predecessor.header.expectedNetworkId,
    requiredSigners: [owner],
    scriptWitnesses: honest ? [Buffer.from("820043820180", "hex")] : [],
  };
  const unsigned = buildFixtureTransaction(tx);
  return buildFixtureTransaction({
    ...tx,
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

/** The live adapter commits this valid native receive as actual protocol history. */
export const buildJourneyScriptHistoryProducer = (input: HistoryBuildInput) =>
  retainHistoryTransactions({
    ...input,
    transactions: [producerTransaction(input)],
  });

/** Spend the actual retained producer, preserving its out-ref on resume. */
export const buildJourneyScriptHistoryConsumer = (
  input: HistoryBuildInput & { honest: boolean },
) => {
  const address = Buffer.concat([
    Buffer.from([0x78]),
    Buffer.from(
      hashMidgardVersionedScript(
        decodeMidgardVersionedScript(Buffer.from("820043820180", "hex")),
      ),
      "hex",
    ),
  ]);
  const producer =
    input.predecessor.payload.block_body.transaction_preimages.find(
      ([, bytes]) => {
        const outputs = decodeMidgardFieldPreimage(
          decodeMidgardNativeTxFullFromCanonicalCbor(Buffer.from(bytes, "hex"))
            .body.outputsPreimageCbor,
        );
        return (
          outputs[0] !== undefined &&
          decodeMidgardTxOutput(outputs[0]).address.equals(address)
        );
      },
    );
  if (producer === undefined)
    throw new Error(
      "Native script history has no introducing transaction in its retained predecessor",
    );
  const outputKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(producer[0], "hex"),
    outputIndex: 0,
  }).toString("hex");
  const spent = input.predecessor.payload.block_body.utxos.find(
    ([key]) => key === outputKey,
  );
  if (spent === undefined)
    throw new Error(
      "Native script consumer requires its retained script-locked producer",
    );
  return retainHistoryTransactions({
    ...input,
    transactions: [
      consumerTransaction(
        input,
        {
          outRef: Buffer.from(spent[0], "hex"),
          output: Buffer.from(spent[1], "hex"),
        },
        input.honest,
      ),
    ],
  });
};

/** A valid producer supplies the retained native preimage; the faulty consumer omits it. */
export const buildJourneyScriptHistoryFault = async (
  input: JourneyScriptHistoryInput,
) => {
  if (input.category === "missingNativeScriptTx") {
    const producer = producerTransaction(input);
    const output = decodeMidgardFieldPreimage(
      decodeMidgardNativeTxFullFromCanonicalCbor(producer.canonicalCbor).body
        .outputsPreimageCbor,
    )[0];
    if (output === undefined)
      throw new Error("Native script producer omitted its output");
    const spent = {
      outRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: 0,
      }),
      output,
    };
    const fault = await retainHistoryTransactions({
      ...input,
      transactions: [producer, consumerTransaction(input, spent, false)],
    });
    const control = await retainHistoryTransactions({
      ...input,
      transactions: [producer, consumerTransaction(input, spent, true)],
    });
    return { fault, control, preparedPredecessor: undefined };
  }
  const preparedPredecessor = await buildJourneyScriptHistoryProducer(input);
  const next = {
    ...input,
    predecessor: preparedPredecessor,
    endTime: input.endTime + 40_000n,
    blockSlot: input.blockSlot + 40n,
  };
  const fault = await buildJourneyScriptHistoryConsumer({
    ...next,
    honest: false,
  });
  const control = await buildJourneyScriptHistoryConsumer({
    ...next,
    honest: true,
  });
  return { fault, control, preparedPredecessor };
};
