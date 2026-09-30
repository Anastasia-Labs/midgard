import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  encodeMidgardTxInputCanonical,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  type MidgardTxInput,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";

import type { InputSetUniquenessContracts } from "../../src/input-set-uniqueness/contracts.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import { countedTransactionsRoot } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeHeader,
  makeNativeTx,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// The committed transaction and its MPF inclusion
// ---------------------------------------------------------------------------

/** A readable fixture out-ref: `tx_id` is one byte repeated 32 times. */
export const isuOutRef = (
  txIdByte: string,
  outputIndex: number,
): MidgardTxInput => ({
  tx_id: txIdByte.repeat(32),
  output_index: BigInt(outputIndex),
});

/** The canonical §5.3 item bytes for one out-ref, hex. */
export const isuItemCbor = (outRef: MidgardTxInput): string =>
  Buffer.from(encodeMidgardTxInputCanonical(outRef)).toString("hex");

export type InputSetUniquenessFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly forcedSource: {
    readonly compact_cbor: string;
    readonly witness_set_compact_cbor: string;
    readonly field_preimage_lengths_cbor: string;
  };
  readonly forcedFullTransactionCbor: Buffer;
};

/**
 * Materializes a committed native transaction with caller-chosen §2.5 fields
 * 0 and 1 (canonical out-ref items in committed order), commits it into a
 * transactions MPF trie beside one honest decoy leaf, and returns the full
 * step-01 inclusion material.
 *
 * `validity: "TxIsInvalid"` builds the §2.4.3(d) negative — a transaction
 * the operator honestly recorded as a no-op, which the family must never
 * convict however degenerate its input sets are.
 */
export const buildInputSetUniquenessFixture = async ({
  spendInputs,
  referenceInputs,
  validity = "TxIsValid",
}: {
  readonly spendInputs: readonly MidgardTxInput[];
  readonly referenceInputs: readonly MidgardTxInput[];
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): Promise<InputSetUniquenessFixture> => {
  const spendItems = spendInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonical(outRef)),
  );
  const referenceItems = referenceInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonical(outRef)),
  );
  const badTx: MidgardNativeTxFull = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: encodeCbor(spendItems),
      referenceInputsPreimageCbor:
        referenceItems.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor(referenceItems),
      outputsPreimageCbor: encodeCbor([Buffer.from("f0".repeat(32), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 1_000n,
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
      Buffer.from(encodeMidgardTxInputCanonical(isuOutRef("dd", 0))),
    ],
    fee: 5n,
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
  const badTxCompactCbor = Buffer.from(
    encodeMidgardNativeTxCompact(badTx.compact),
  ).toString("hex");
  const badTxSourceCbor = l2TransactionSourceCborV1(badTx);
  const forcedSource = deriveMidgardForcedTxProofSource(
    materializeMidgardForcedTxFromCanonical(badTx),
  );
  const decoyTxSourceCbor = l2TransactionSourceCborV1(decoyTx);
  const decoyTxId = computeMidgardNativeTxId(decoyTx).toString("hex");
  if (decoyTxId === badTxId) {
    throw new Error("fixture decoy collides with the disputed transaction");
  }
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(decoyTxId, "hex"),
    Buffer.from(decoyTxSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const txMembershipProofCbor = proof.toCBOR().toString("hex");
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 2n,
    nativeTxId: badTxId,
    nativeTxCompactCbor: badTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(badTx.compact),
      nativeTxCompactCbor: badTxCompactCbor,
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProof: Data.from(txMembershipProofCbor, Proof),
      txMembershipProofCbor,
    },
    spendInputItemCbors: spendItems.map((item) => item.toString("hex")),
    referenceInputItemCbors: referenceItems.map((item) => item.toString("hex")),
    forcedSource: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    forcedFullTransactionCbor: encodeMidgardForcedTxCanonical(badTx),
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeInputSetUniquenessEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realInputSetUniqueness: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.inputSetUniqueness;
  const category = harness.catalogue.categories.inputSetUniqueness;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the input-set-uniqueness contracts/category",
    );
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.inputSetUniqueness
  ) {
    throw new Error("Unexpected input-set-uniqueness catalogue category id");
  }
  return { ...harness, family, category };
};

export type InputSetUniquenessHarness = Awaited<
  ReturnType<typeof makeInputSetUniquenessEmulatorHarness>
>;

/**
 * Commits a header carrying the fixture's counted `transactions_root` on the
 * emulator, ready for Init.
 */
export const setupInputSetUniquenessScenario = async ({
  harness,
  fixture,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly fixture: InputSetUniquenessFixture;
}) => {
  const {
    emulator,
    funderLucid,
    proverLucid,
    realBlueprint,
    contracts,
    family,
    catalogue,
    nonceUtxo,
  } = harness;
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: family.computationThread.mintingScript,
      fraudProofMintingScript: family.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const header = makeHeader(
    funderKeyHash,
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
  });
  return { header, setup: { ...setup, witnessReferenceScripts } };
};

/**
 * Publishes all four step validators as reference scripts (production deployment
 * shape per the standing reference-script ruling).
 */
export const publishInputSetUniquenessReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: InputSetUniquenessContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `input-set-uniqueness step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};
