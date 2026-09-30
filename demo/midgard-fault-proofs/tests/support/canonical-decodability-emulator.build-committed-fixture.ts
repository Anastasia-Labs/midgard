import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash32,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  midgardNativeTxProofFieldPreimageLengths,
  type MidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  miscountedMidgardFieldPreimage,
  type NativeTxWitnessSetCompact,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalDecodabilityContracts,
  prepareCanonicalDecodability,
} from "../../src/canonical-decodability/index.js";
import { encodeL2TransactionSourceValue } from "../../src/prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import {
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const CANONICAL_DECODABILITY_BODY_FIELD_INDEX = 2;

export const CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX = 6;

export type CanonicalDecodabilityCommittedFieldFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: 1n;
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prepared: ReturnType<typeof prepareCanonicalDecodability> | null;
};

const witnessSetData = (
  compact: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact>,
): NativeTxWitnessSetCompact => ({
  addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
  script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
  redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
    "hex",
  ),
});

const buildCommittedFixture = async ({
  compact,
  fieldIndex,
  committedPreimage,
  witnessSet,
  proofSource,
  allowGrammatical = false,
}: {
  readonly compact: MidgardNativeTxCompact;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly proofSource: MidgardNativeTxProofSource;
  readonly allowGrammatical?: boolean;
}): Promise<CanonicalDecodabilityCommittedFieldFixture> => {
  const badTxId = computeMidgardNativeTxId(compact).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(compact);
  const l2TransactionSourceCbor = encodeL2TransactionSourceValue({
    txId: badTxId,
    proofSource,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  const nativeTxCompactCbor = compactCbor.toString("hex");
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId: badTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor,
    l2TransactionSourceCbor,
    transactionsPhasRoot: trieRootHex(trie),
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  };
  let prepared: ReturnType<typeof prepareCanonicalDecodability> | null = null;
  try {
    prepared = prepareCanonicalDecodability({
      badTxId,
      nativeTxCompactCbor,
      fieldIndex,
      committedPreimage,
      ...(witnessSet === undefined ? {} : { witnessSet }),
    });
  } catch (cause) {
    if (!allowGrammatical) throw cause;
  }
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTxId,
    nativeTxCompactCbor,
    fieldIndex,
    committedPreimage,
    ...(witnessSet === undefined ? {} : { witnessSet }),
    txInclusion,
    prepared,
  };
};

const proofSourceForCommittedField = ({
  honest,
  compact,
  fieldIndex,
  committedPreimage,
  witnessSetCompactCbor,
}: {
  readonly honest: MidgardNativeTxFull;
  readonly compact: MidgardNativeTxCompact;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSetCompactCbor?: Buffer;
}): MidgardNativeTxProofSource => {
  const base = deriveMidgardNativeTxProofSource(honest);
  const lengths = [...midgardNativeTxProofFieldPreimageLengths(honest)];
  lengths[fieldIndex] = committedPreimage.length;
  return {
    compactCbor: encodeMidgardNativeTxCompact(compact),
    witnessSetCompactCbor: witnessSetCompactCbor ?? base.witnessSetCompactCbor,
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(lengths),
  };
};

export const buildCanonicalDecodabilityBodyFixture = async ({
  grammatical = false,
}: {
  readonly grammatical?: boolean;
} = {}): Promise<CanonicalDecodabilityCommittedFieldFixture> => {
  const honest = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    referenceByte: "61",
    outputByte: "62",
    witnessByte: "63",
  });
  const committedPreimage = grammatical
    ? Buffer.from(honest.body.outputsPreimageCbor)
    : miscountedMidgardFieldPreimage(1, [
        Buffer.from("aa", "hex"),
        Buffer.from("bb", "hex"),
      ]);
  const compact: MidgardNativeTxCompact = {
    ...honest.compact,
    transactionBody: {
      ...honest.compact.transactionBody,
      outputsHash: computeHash32(committedPreimage),
    },
  };
  return await buildCommittedFixture({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_BODY_FIELD_INDEX,
    committedPreimage,
    proofSource: proofSourceForCommittedField({
      honest,
      compact,
      fieldIndex: CANONICAL_DECODABILITY_BODY_FIELD_INDEX,
      committedPreimage,
    }),
    allowGrammatical: grammatical,
  });
};

export const buildCanonicalDecodabilityWitnessFixture = async () => {
  const honest = makeNativeTx({ spendInputCbors: [], fee: 9n });
  const committedPreimage = Buffer.from([0x81]);
  const original = deriveMidgardNativeTxWitnessSetCompact(honest.witnessSet);
  const mutated = {
    ...original,
    scriptTxWitsHash: computeHash32(committedPreimage),
  };
  const witnessSet = witnessSetData(mutated);
  const compact: MidgardNativeTxCompact = {
    ...honest.compact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(mutated),
    ),
  };
  return await buildCommittedFixture({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX,
    committedPreimage,
    witnessSet,
    proofSource: proofSourceForCommittedField({
      honest,
      compact,
      fieldIndex: CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX,
      committedPreimage,
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(mutated),
    }),
  });
};

export const makeCanonicalDecodabilityEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realCanonicalDecodability: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const canonicalDecodability = harness.contracts.canonicalDecodability;
  const category = harness.catalogue.categories.canonicalDecodability;
  if (canonicalDecodability === undefined || category === undefined) {
    throw new Error(
      "Harness did not build canonical-decodability contracts/category",
    );
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.canonicalDecodability
  ) {
    throw new Error("Unexpected canonical-decodability category id");
  }
  return { ...harness, canonicalDecodability, category };
};

export const setupCanonicalDecodabilityScenario = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeCanonicalDecodabilityEmulatorHarness>
  >;
  readonly fixture: CanonicalDecodabilityCommittedFieldFixture;
}) =>
  await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });

export const publishCanonicalDecodabilityReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: CanonicalDecodabilityContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `canonical-decodability step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO];
};
