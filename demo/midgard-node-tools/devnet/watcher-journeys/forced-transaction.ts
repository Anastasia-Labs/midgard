import {
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  encodeCbor,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

import { buildAtJourneyMinFee } from "./journey-fee.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import { journeyNetwork } from "./journey-network.js";
import {
  ownedLedgerEntries,
  type OwnedOutput,
  type OwnerSpend,
  selectOwnerOutput,
} from "./owner-output-selection.js";

/** `[0, all []]`: the versioned native script that every transaction satisfies. */
const alwaysNativeScript = decodeMidgardVersionedScript(
  Buffer.from("820043820180", "hex"),
);

export type JourneyOwnerTransactionInput = {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
};

/** The ledger owner's payment key. */
export const journeyOwnerKey = (seedPhrase: string) =>
  CML.PrivateKey.from_bech32(
    walletFromSeed(seedPhrase, { network: journeyNetwork().network })
      .paymentKey,
  );

/** The ledger owner's outputs on `predecessor`. */
export const journeyOwnerEntries = (input: JourneyOwnerTransactionInput) =>
  ownedLedgerEntries(
    input.predecessor.ledgerEntries,
    journeyOwnerKey(input.ledgerOwnerSeedPhrase).to_public().hash().to_hex(),
  );

/**
 * The ledger owner's signed transaction that spends `spent`, one of its own
 * retained outputs, into one protected output carrying that output's value
 * plus one token minted under `policyScript`. Its `outputs` are the outputs
 * it pays.
 */
export const buildJourneyOwnerMintTransaction = (
  input: JourneyOwnerTransactionInput & {
    spent: OwnedOutput;
    policyScript: MidgardVersionedScript;
    /** Paid from the spent output. */
    fee: bigint;
    outputReferenceScript?: MidgardVersionedScript;
    redeemerWitnesses?: readonly Buffer[];
    scriptIntegrityHash?: Buffer;
    /** Retained outputs the transaction reads without spending. */
    referenceInputs?: readonly Buffer[];
  },
) => {
  const key = journeyOwnerKey(input.ledgerOwnerSeedPhrase);
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const { spent } = input;
  const output = decodeMidgardTxOutput(spent.output);
  const policy = hashMidgardVersionedScript(input.policyScript);
  const assets = new Map(
    [...output.value.assets].map(([hash, tokens]) => [hash, new Map(tokens)]),
  );
  const tokens = assets.get(policy) ?? new Map<string, bigint>();
  tokens.set("", (tokens.get("") ?? 0n) + 1n);
  assets.set(policy, tokens);
  const transactionInput: FixtureTransactionInput = {
    spendInputs: [spent.outRef],
    referenceInputs: input.referenceInputs ?? [],
    outputs: [
      encodeMidgardTxOutput({
        ...output,
        address: protectMidgardAddress(output.address),
        value: {
          ...output.value,
          lovelace: output.value.lovelace - input.fee,
          assets,
        },
        script_ref: input.outputReferenceScript,
      }),
    ],
    fee: input.fee,
    networkId: input.predecessor.header.expectedNetworkId,
    requiredSigners: [owner],
    mintPolicyItems: [
      encodeCbor([
        Buffer.from(policy, "hex"),
        new Map([[Buffer.alloc(0), 1n]]),
      ]),
    ],
    scriptWitnesses: [encodeMidgardVersionedScript(input.policyScript)],
    redeemerWitnesses: input.redeemerWitnesses ?? [],
    scriptIntegrityHash: input.scriptIntegrityHash,
  };
  const unsigned = buildFixtureTransaction(transactionInput);
  return {
    outputs: transactionInput.outputs ?? [],
    transaction: buildFixtureTransaction({
      ...transactionInput,
      addressWitnesses: [
        {
          verification_key: Buffer.from(
            key.to_public().to_raw_bytes(),
          ).toString("hex"),
          signature: key.sign(Buffer.from(unsigned.txId, "hex")).to_hex(),
        },
      ],
    }),
  };
};

/** The forced transaction spending `spent`, at its minimum fee. */
const forcedTransactionOf = (
  input: JourneyOwnerTransactionInput & {
    referenceInputs?: readonly Buffer[];
  },
  spent: OwnedOutput,
) =>
  buildAtJourneyMinFee(input.predecessor.header, (fee) => {
    const built = buildJourneyOwnerMintTransaction({
      predecessor: input.predecessor,
      ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
      spent,
      policyScript: alwaysNativeScript,
      fee,
      referenceInputs: input.referenceInputs,
    });
    return { canonicalCbor: built.transaction.canonicalCbor, built };
  }).built;

/** What a forced-order fixture's valid transaction pays from an output. */
export const journeyForcedTransactionSpend =
  (
    input: JourneyOwnerTransactionInput & {
      referenceInputs?: readonly Buffer[];
    },
  ): OwnerSpend =>
  (entry) =>
    forcedTransactionOf(input, entry).outputs;

/**
 * The exact valid transaction every forced-order fixture places in its L1
 * order. Forced transactions are native-only: it carries no redeemer, Plutus
 * witness, script integrity hash, output reference script, auxiliary data or
 * validity upper bound. Its single spend input, output, required signer, mint
 * policy and native script witness are the coordinates the wrongful claims
 * name. It pays the minimum fee the predecessor's schedule prices it at, the
 * schedule its block's replay applies, from the owner's smallest output that
 * pays that fee and the output's minimum ADA (`selectOwnerOutput`).
 */
export const prepareJourneyForcedTransaction = (
  input: JourneyOwnerTransactionInput & {
    /** Retained outputs the transaction reads without spending; none by default. */
    referenceInputs?: readonly Buffer[];
  },
) => ({
  transaction: forcedTransactionOf(
    input,
    selectOwnerOutput(
      journeyOwnerEntries(input),
      journeyForcedTransactionSpend(input),
      "a forced-order fixture's valid transaction",
    ),
  ).transaction,
});
