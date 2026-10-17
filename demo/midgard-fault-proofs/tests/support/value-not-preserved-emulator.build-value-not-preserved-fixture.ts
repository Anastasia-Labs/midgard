import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardFieldItems,
  encodeMidgardNativeTxCompact,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardMintPolicyItem,
  type MidgardNativeTxFull,
  type MidgardTxOutput,
} from "@al-ft/midgard-core";
import type { MidgardValue } from "@al-ft/midgard-core/codec";
import {
  encodeMidgardTxInputCanonical,
  type MidgardTxInput,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { type ValueNotPreservedLedgerTrieHandle } from "../../src/value-not-preserved/evidence.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Building blocks: out-refs, values, outputs
// ---------------------------------------------------------------------------

/** A readable fixture out-ref: `tx_id` is one byte repeated 32 times. */
export const vnpOutRef = (
  txIdByte: string,
  outputIndex: number,
): MidgardTxInput => ({
  tx_id: txIdByte.repeat(32),
  output_index: BigInt(outputIndex),
});

/** A `MidgardValue` from lovelace plus optional single-policy token entries. */
export const vnpValue = (
  lovelace: bigint,
  tokens: readonly {
    readonly policyIdHex: string;
    readonly assetNameHex: string;
    readonly quantity: bigint;
  }[] = [],
): MidgardValue => {
  const assets = new Map<string, Map<string, bigint>>();
  for (const token of tokens) {
    const names = assets.get(token.policyIdHex) ?? new Map<string, bigint>();
    names.set(
      token.assetNameHex,
      (names.get(token.assetNameHex) ?? 0n) + token.quantity,
    );
    assets.set(token.policyIdHex, names);
  }
  return { lovelace, assets };
};

/** The fixture payment credential every committed output pays. */
const VNP_OUTPUT_ADDRESS = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x42),
]);

/** The fixture payment credential every SPENT (pre-state) output pays. */
const VNP_LEDGER_OUTPUT_ADDRESS = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x99),
]);

/**
 * A canonical Aiken-serialised PlutusData datum of `chunkCount` 64-byte
 * strings inside an indefinite list — `2 + 66 × chunkCount` bytes. This is
 * the §8.4 sizing instrument: inline datums are the realistic way a
 * committed L2 output gets large, and the on-chain output decoder slices a
 * datum without walking it, so a datum-heavy outputs field crosses the
 * 14,336-byte tier-1 cap without inflating the fold's execution cost.
 */
export const vnpLargeDatumCbor = (chunkCount: number, seed: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x9f]),
    ...Array.from({ length: chunkCount }, (_, index) =>
      Buffer.concat([
        Buffer.from([0x58, 0x40]),
        Buffer.alloc(64, (seed + index) & 0xff),
      ]),
    ),
    Buffer.from([0xff]),
  ]);

/** One committed output, optionally padded with a large inline datum. */
export const vnpOutput = ({
  value,
  datumChunks = 0,
  seed = 0,
}: {
  readonly value: MidgardValue;
  readonly datumChunks?: number;
  readonly seed?: number;
}): MidgardTxOutput => ({
  address: VNP_OUTPUT_ADDRESS,
  value,
  ...(datumChunks > 0
    ? {
        datum: {
          kind: "inline" as const,
          cbor: vnpLargeDatumCbor(datumChunks, seed),
        },
      }
    : {}),
});

// ---------------------------------------------------------------------------
// The committed transaction, its MPF inclusion, and the pre-state ledger
// ---------------------------------------------------------------------------

/** One spend input of the fixture transaction with its pre-state value. */
export type ValueNotPreservedFixtureSpentInput = {
  readonly input: MidgardTxInput;
  readonly spentValue: MidgardValue;
};

export type ValueNotPreservedFixture = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputsPreimageCbor: Buffer;
  /** The committed §5.1 outputs-field preimage — the §8.4 tier decider. */
  readonly outputsPreimageCbor: Buffer;
  readonly outputs: readonly MidgardTxOutput[];
  readonly mintItems: readonly MidgardMintPolicyItem[];
  readonly ledger: {
    readonly rootHex: string;
    readonly trie: ValueNotPreservedLedgerTrieHandle;
    /** Per spend input, in field-0 order: the facts step-02 folds. */
    readonly spentInputs: readonly {
      readonly input: MidgardTxInput;
      readonly descriptorCbor: string;
      readonly outputCbor: string;
      readonly spentValue: MidgardValue;
    }[];
  };
};

/**
 * Materializes a committed native transaction with caller-chosen structured
 * outputs (§5.5) and mint items (§5.6), commits it into a transactions MPF
 * beside one honest decoy leaf, and files each spent input's genuine
 * `LedgerOutputCommitmentV1` descriptor in a fresh pre-state ledger MPF —
 * the header committed for the scenario carries that trie's root as
 * `prev_utxos_root`.
 *
 * `validity: "TxIsInvalid"` builds the §1.4 negative — a transaction the
 * operator honestly recorded as a no-op, which the family must never convict
 * however unbalanced its value equation looks.
 */
export const buildValueNotPreservedFixture = async ({
  spentInputs,
  outputs,
  mintItems = [],
  fee = 1_000_000n,
  validity = "TxIsValid",
}: {
  readonly spentInputs: readonly ValueNotPreservedFixtureSpentInput[];
  readonly outputs: readonly MidgardTxOutput[];
  readonly mintItems?: readonly MidgardMintPolicyItem[];
  readonly fee?: bigint;
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): Promise<ValueNotPreservedFixture> => {
  const spendItems = spentInputs.map(({ input }) =>
    Buffer.from(encodeMidgardTxInputCanonical(input)),
  );
  const outputItems = encodeMidgardFieldItems({
    fieldIndex: 2,
    items: outputs,
  });
  const mintItemBuffers = encodeMidgardFieldItems({
    fieldIndex: 5,
    items: mintItems,
  });
  const spendInputsPreimageCbor = encodeCbor(spendItems);
  const outputsPreimageCbor = encodeCbor([...outputItems]);
  const badTx: MidgardNativeTxFull = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor:
        mintItemBuffers.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor([...mintItemBuffers]),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor([Buffer.from("f1".repeat(32), "hex")]),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  // One honest decoy leaf, so the membership proof has at least one step.
  const decoyTx = makeNativeTx({
    spendInputCbors: [
      Buffer.from(encodeMidgardTxInputCanonical(vnpOutRef("dd", 0))),
    ],
    fee: 5n,
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
  const badTxCompactCbor = Buffer.from(
    encodeMidgardNativeTxCompact(badTx.compact),
  ).toString("hex");
  const badTxSourceCbor = l2TransactionSourceCborV1(badTx);
  const decoyTxSourceCbor = l2TransactionSourceCborV1(decoyTx);
  const decoyTxId = computeMidgardNativeTxId(decoyTx).toString("hex");
  if (decoyTxId === badTxId) {
    throw new Error("fixture decoy collides with the disputed transaction");
  }
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await txTrie.insert(
    Buffer.from(decoyTxId, "hex"),
    Buffer.from(decoyTxSourceCbor, "hex"),
  );
  const proof = await txTrie.prove(Buffer.from(badTxId, "hex"));
  const txMembershipProofCbor = proof.toCBOR().toString("hex");

  // The pre-state ledger: one genuine descriptor per spent input, derived
  // from the spent output's own bytes so lovelace, asset count and the asset
  // frontier commitment all agree with the value the witness walks.
  const ledgerStore = new Store(undefined);
  await ledgerStore.ready();
  const ledgerTrie = new Trie(ledgerStore);
  const ledgerSpentInputs: {
    readonly input: MidgardTxInput;
    readonly descriptorCbor: string;
    readonly outputCbor: string;
    readonly spentValue: MidgardValue;
  }[] = [];
  for (const { input, spentValue } of spentInputs) {
    const material = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: Number(input.output_index),
      outputCbor: encodeMidgardTxOutput({
        address: VNP_LEDGER_OUTPUT_ADDRESS,
        value: spentValue,
      }),
    });
    await ledgerTrie.insert(
      encodeMidgardTxInputCanonical(input),
      material.descriptorCbor,
    );
    ledgerSpentInputs.push({
      input,
      descriptorCbor: material.descriptorCbor.toString("hex"),
      outputCbor: encodeMidgardTxOutput({
        address: VNP_LEDGER_OUTPUT_ADDRESS,
        value: spentValue,
      }).toString("hex"),
      spentValue,
    });
  }
  // Decoy siblings, so no membership proof is over a single-leaf trie.
  for (let index = 0; index < 2; index += 1) {
    await ledgerTrie.insert(
      Buffer.concat([Buffer.alloc(37, 0xee), Buffer.from([index])]),
      Buffer.from([0xd0 + index]),
    );
  }
  const ledgerRootHex = trieRootHex(ledgerTrie);

  return {
    nativeTx: badTx,
    transactionsRoot: trieRootHex(txTrie),
    l2TransactionCount: 2n,
    nativeTxId: badTxId,
    nativeTxCompactCbor: badTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(badTx.compact),
      nativeTxCompactCbor: badTxCompactCbor,
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: trieRootHex(txTrie),
      txMembershipProof: Data.from(txMembershipProofCbor, Proof),
      txMembershipProofCbor,
    },
    spendInputsPreimageCbor,
    outputsPreimageCbor,
    outputs,
    mintItems,
    ledger: {
      rootHex: ledgerRootHex,
      trie: {
        rootHex: ledgerRootHex,
        prove: async (key: Buffer) =>
          Buffer.from((await ledgerTrie.prove(key)).toCBOR()),
      },
      spentInputs: ledgerSpentInputs,
    },
  };
};
