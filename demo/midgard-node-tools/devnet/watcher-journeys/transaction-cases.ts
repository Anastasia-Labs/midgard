import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  encodeCbor,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

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
  predecessor: {
    header: SDK.Header;
    headerHash: string;
    payload: SDK.DaPayload;
  };
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

const buildJourneyTransaction = async (
  input: JourneyTransactionInput & {
    category?: JourneyTransactionCategory;
    minFeeB?: bigint;
  },
) => {
  const ledgerEntries = input.predecessor.payload.block_body.utxos.map(
    ([key, value]) => ({
      outRef: Buffer.from(key, "hex"),
      output: Buffer.from(value, "hex"),
    }),
  );
  const wallet = walletFromSeed(input.ledgerOwnerSeedPhrase, {
    network: "Custom",
  });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const ownerHash = Buffer.from(key.to_public().hash().to_raw_bytes());
  const spent = ledgerEntries.find(({ output }) => {
    const { paymentCredential } = decodeMidgardAddressBytes(
      decodeMidgardTxOutput(output).address,
    );
    return (
      paymentCredential.kind === "PubKey" &&
      paymentCredential.hash.equals(ownerHash)
    );
  });
  if (spent === undefined)
    throw new Error(
      "Transaction journey requires a retained output owned by the ledger signer",
    );
  const output = decodeMidgardTxOutput(spent.output);
  const changes: Partial<FixtureTransactionInput> =
    input.category === undefined
      ? {}
      : mutations[input.category]({ spendInput: spent.outRef, output });
  const fee =
    input.category === undefined
      ? (input.minFeeB ?? input.predecessor.header.minFeeB)
      : 0n;
  const transactionInput: FixtureTransactionInput = {
    spendInputs: [spent.outRef],
    outputs: [
      encodeMidgardTxOutput({
        ...output,
        value: { ...output.value, lovelace: output.value.lovelace - fee },
      }),
    ],
    fee,
    networkId: input.predecessor.header.expectedNetworkId,
    ...changes,
  };
  const unsigned = buildFixtureTransaction(transactionInput);
  const transaction = buildFixtureTransaction({
    ...transactionInput,
    addressWitnesses: changes.addressWitnesses ?? [
      {
        verification_key: Buffer.from(key.to_public().to_raw_bytes()).toString(
          "hex",
        ),
        signature:
          input.category === "invalidSignature"
            ? "00".repeat(64)
            : key.sign(Buffer.from(unsigned.txId, "hex")).to_hex(),
      },
    ],
  });
  if (transaction.txId !== unsigned.txId)
    throw new Error(
      "Adding an address witness changed the transaction body id",
    );
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    minFeeB: input.minFeeB ?? input.predecessor.header.minFeeB,
  });
  return block;
};

export { buildJourneyScriptTransaction } from "./script-cases.js";

export const buildJourneyTransactionFault = async (
  input: JourneyTransactionInput & { category: JourneyTransactionCategory },
) => {
  const block = await buildJourneyTransaction({
    ...input,
    ...(input.category === "minFee" ? { minFeeB: 1n } : {}),
  });
  if (block.replay.trace.verdict !== "rejected")
    throw new Error(
      `The ${input.category} transaction passed honest validation`,
    );
  return block;
};

/** Accepted signed counterpart against the same ledger and fee schedule. */
export const buildJourneyTransactionControl = (
  input: JourneyTransactionInput & { category?: JourneyTransactionCategory },
) =>
  buildJourneyTransaction({
    ...input,
    category: undefined,
    ...(input.category === "minFee" ? { minFeeB: 1n } : {}),
  });
