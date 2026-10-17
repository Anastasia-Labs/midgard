import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import { type MidgardTxInput, Proof } from "@al-ft/midgard-sdk";
import {
  Data,
  type EmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import { resolveProverSigner } from "../src/index.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import { nativeTxFromCoreCompact } from "./support/legacy-submit-emulator.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  trieRootHex,
} from "./support/submit-init-emulator-shared.js";

/** The out-of-range output index the fraudulent spender claims. */
export const CHALLENGED_OUTPUT_INDEX = 7n;

/**
 * 365 spend inputs (constant §5.3 stride of 40 bytes each) make a 14,603-byte
 * field-0 preimage: past §8.4's tier-1 bound, inside the single-publication
 * tier-2 window `(14,336, 15,148]` — the size alone selects `RawUtxo`.
 */
export const TIER2_SPEND_INPUT_COUNT = 365;

/**
 * 340 canonical producing outputs (43-byte §5.1 stride each) make a
 * 14,623-byte field-2 preimage: past §8.4's tier-1 bound, inside the
 * single-publication tier-2 window `(14,336, 15,148]` — the size alone
 * selects `RawUtxo` for step-04's outputs opening.
 */
export const TIER2_PRODUCING_OUTPUT_COUNT = 340;

// Public BIP39 vectors used only by this emulator test. They are not secrets
// and must never fund a real wallet.
const TEST_ONLY_FUNDER_SEED =
  "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";

const TEST_ONLY_PROVER_SEED =
  "test test test test test test test test test test test junk";

const FIXED_EMULATOR_UNIX_MS = 1_735_689_600_000;

const fixedBaseEmulatorAccount = (
  seedPhrase: string,
  lovelace: bigint,
): EmulatorAccount => ({
  seedPhrase,
  privateKey: "",
  address: walletFromSeed(seedPhrase, {
    addressType: "Base",
    accountIndex: 0,
    network: "Custom",
  }).address,
  assets: { lovelace },
});

/**
 * The same fixed-seed determinism for the prover, funded at the address
 * `resolveProverSigner` derives — the enterprise one, since the fraud-prover
 * reward output is matched by payment credential with no stake part. The
 * harness selects the prover wallet through that signer, so a base-address
 * seeding leaves every prover transaction building against an empty wallet.
 */
const fixedProverEmulatorAccount = (
  seedPhrase: string,
  lovelace: bigint,
): EmulatorAccount => ({
  seedPhrase,
  privateKey: "",
  address: resolveProverSigner({ network, walletSeedPhrase: seedPhrase })
    .address,
  assets: { lovelace },
});

export const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

/** One canonical native output: enterprise pubkey address, lovelace only. */
const nativeOutputCbor = (paymentByte: number, lovelace: bigint): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

export type InputNoIdxBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly producingTxId: string;
  readonly producingTxOutputsHash: string;
  readonly producingOutputsCbor: readonly string[];
  readonly badTxId: string;
  readonly badInput: MidgardTxInput;
  readonly badInputs: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
  readonly verifiedTxInputsHash: string;
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly producingTxInclusion: SubmitStep01TxInclusion;
};

/**
 * Commits a producer with `producingOutputs` canonical outputs and a spender
 * of `(producerTxId, challengedOutputIndex)` as the two native-compact leaves
 * of one block's transactions MPF. The block carries the `input-no-idx`
 * violation exactly when `challengedOutputIndex >= producingOutputs.length`.
 */
export const buildInputNoIdxBlockFixture = async ({
  producingOutputCount,
  challengedOutputIndex = CHALLENGED_OUTPUT_INDEX,
  badSpendInputCount = 1,
}: {
  readonly producingOutputCount: number;
  readonly challengedOutputIndex?: bigint;
  readonly badSpendInputCount?: number;
}): Promise<InputNoIdxBlockFixture> => {
  if (!Number.isSafeInteger(badSpendInputCount) || badSpendInputCount <= 0) {
    throw new Error("badSpendInputCount must be a positive safe integer");
  }
  const producingOutputs = Array.from(
    { length: producingOutputCount },
    (_, index) =>
      nativeOutputCbor((0x40 + index) % 0x100, 5_000_000n + BigInt(index)),
  );
  const producingTx = makeNativeTx({
    spendInputCbors: [inputCbor("99".repeat(32), 0n)],
    fee: 7n,
    ...(producingOutputs.length === 0 ? {} : { outputCbors: producingOutputs }),
  });
  const producingTxId = computeMidgardNativeTxId(producingTx).toString("hex");
  const challengedInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: challengedOutputIndex,
  };
  const badInputs = [
    challengedInput,
    ...Array.from({ length: badSpendInputCount - 1 }, (_, index) => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    })),
  ].sort((left, right) =>
    Buffer.compare(
      inputCbor(left.tx_id, left.output_index),
      inputCbor(right.tx_id, right.output_index),
    ),
  );
  const badInputsIndex = badInputs.findIndex(
    (input) =>
      input.tx_id === challengedInput.tx_id &&
      input.output_index === challengedInput.output_index,
  );
  if (badInputsIndex < 0) {
    throw new Error("Expected challenged input in canonical bad input list");
  }
  const badTx = makeNativeTx({
    spendInputCbors: badInputs.map((input) =>
      inputCbor(input.tx_id, input.output_index),
    ),
    fee: 9n,
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");

  const producingCompactCbor = encodeMidgardNativeTxCompact(
    producingTx.compact,
  );
  const badCompactCbor = encodeMidgardNativeTxCompact(badTx.compact);
  const producingSourceCbor = l2TransactionSourceCborV1(producingTx);
  const badSourceCbor = l2TransactionSourceCborV1(badTx);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(producingTxId, "hex"),
    Buffer.from(producingSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badSourceCbor, "hex"),
  );
  const producingProof = await trie.prove(Buffer.from(producingTxId, "hex"));
  const badProof = await trie.prove(Buffer.from(badTxId, "hex"));
  const transactionsRoot = trieRootHex(trie);
  const producingCompact = nativeTxFromCoreCompact(producingTx.compact);
  const badCompact = nativeTxFromCoreCompact(badTx.compact);

  const inclusionFor = (
    nativeTxId: string,
    compactCbor: Buffer,
    l2TransactionSourceCbor: string,
    proofCbor: string,
    compact: typeof producingTx.compact,
  ): SubmitStep01TxInclusion => ({
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: compactCbor.toString("hex"),
    l2TransactionSourceCbor,
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  });

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    producingTxId,
    producingTxOutputsHash: producingCompact.body.outputs_hash,
    producingOutputsCbor: producingOutputs.map((item) => item.toString("hex")),
    badTxId,
    badInput: challengedInput,
    badInputs,
    badInputsIndex,
    verifiedTxInputsHash: badCompact.body.spend_inputs_hash,
    badTxInclusion: inclusionFor(
      badTxId,
      badCompactCbor,
      badSourceCbor,
      badProof.toCBOR().toString("hex"),
      badTx.compact,
    ),
    producingTxInclusion: inclusionFor(
      producingTxId,
      producingCompactCbor,
      producingSourceCbor,
      producingProof.toCBOR().toString("hex"),
      producingTx.compact,
    ),
  };
};

export const makeEmulatorHarness = async () =>
  await makeFaultProofEmulatorHarness({
    contractOptions: { realInputNoIdx: true, alwaysFraudProofCatalogue: true },
    accounts: {
      funder: fixedBaseEmulatorAccount(TEST_ONLY_FUNDER_SEED, 40_000_000_000n),
      prover: fixedProverEmulatorAccount(
        TEST_ONLY_PROVER_SEED,
        20_000_000_000n,
      ),
    },
    emulatorTimeMs: FIXED_EMULATOR_UNIX_MS,
  });

export const STEP02_RELEASE_MEMORY_LIMIT = 13_200_000n;

export const STEP02_RELEASE_CPU_LIMIT = 8_000_000_000n;

export const HALF_CANONICAL_MATURITY_MS = 302_400_000;
