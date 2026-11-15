import {
  decodeMidgardAddressBytes,
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

/** `[0, all []]`: the versioned native script that every transaction satisfies. */
const alwaysNativeScript = decodeMidgardVersionedScript(
  Buffer.from("820043820180", "hex"),
);

export type JourneyOwnerTransactionInput = {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
};

/**
 * The ledger owner's signed transaction that spends its own retained output
 * into one protected output carrying that output's value plus one token minted
 * under `policyScript`.
 */
export const buildJourneyOwnerMintTransaction = (
  input: JourneyOwnerTransactionInput & {
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
  const wallet = walletFromSeed(input.ledgerOwnerSeedPhrase, {
    network: journeyNetwork().network,
  });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const spent = input.predecessor.ledger.entries.find(({ output: bytes }) => {
    const credential = decodeMidgardAddressBytes(
      decodeMidgardTxOutput(bytes).address,
    ).paymentCredential;
    return (
      credential.kind === "PubKey" && Buffer.from(credential.hash).equals(owner)
    );
  });
  if (spent === undefined)
    throw new Error("Owner journey transaction needs a retained ledger input");
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

/**
 * The exact valid transaction every forced-order fixture places in its L1
 * order. Forced transactions are native-only: it carries no redeemer, Plutus
 * witness, script integrity hash, output reference script, auxiliary data or
 * validity upper bound. Its single spend input, output, required signer, mint
 * policy and native script witness are the coordinates the wrongful claims
 * name. It pays the minimum fee the predecessor's schedule prices it at, the
 * schedule its block's replay applies.
 */
export const prepareJourneyForcedTransaction = (
  input: JourneyOwnerTransactionInput & {
    /** Retained outputs the transaction reads without spending; none by default. */
    referenceInputs?: readonly Buffer[];
  },
) => ({
  transaction: buildAtJourneyMinFee(
    input.predecessor.header,
    (fee) =>
      buildJourneyOwnerMintTransaction({
        predecessor: input.predecessor,
        ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
        policyScript: alwaysNativeScript,
        fee,
        referenceInputs: input.referenceInputs,
      }).transaction,
  ),
});
