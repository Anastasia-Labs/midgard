import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type MidgardTxInput,
  missingSignatureVkeyHash,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { MinAdaContracts } from "../../src/min-ada/contracts.js";
import type { MissingNativeScriptUtxoContracts } from "../../src/missing-native-script-utxo/contracts.js";
import type { PreparedMissingNativeScriptUtxo } from "../../src/missing-native-script-utxo/prepare.js";
import type { NativeScriptInvalidContracts } from "../../src/native-script-invalid/contracts.js";
import type { PreparedNativeScriptInvalid } from "../../src/native-script-invalid/prepare.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import {
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { nativeWitnessSet } from "./final-catalogue-emulator.build-min-ada-post-utxo-emulator-fixture.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const buildMissingNativeScriptUtxoEmulatorFixture = async ({
  decoyWitnessCount = 0,
}: {
  readonly decoyWitnessCount?: number;
} = {}) => {
  const missingNativeScriptBytes = encodeMidgardNativeScript({
    type: "sig",
    keyHash: Buffer.alloc(28, 0x44),
  });
  const missingVersioned = {
    language: "NativeCardano" as const,
    scriptBytes: missingNativeScriptBytes,
    nativeScript: {
      type: "sig" as const,
      keyHash: Buffer.alloc(28, 0x44),
    },
  };
  const expectedMissingScriptHash =
    hashMidgardVersionedScript(missingVersioned);
  const predecessorOutput = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x70]),
      Buffer.from(expectedMissingScriptHash, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const outRef = { transactionId: "ab".repeat(32), outputIndex: 0n } as const;
  const outRefKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: 0,
  });
  const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: outRefKey,
    outputCbor: predecessorOutput,
  }).descriptorCbor;
  const previous = await keyValuePhasRootWithCount([
    { key: outRefKey, value: descriptorCbor },
  ]);
  const membershipProof = await keyValuePhasProof(
    previous,
    outRefKey,
    descriptorCbor,
  );
  const spendInputs: readonly MidgardTxInput[] = [
    { tx_id: outRef.transactionId, output_index: outRef.outputIndex },
  ];
  const decoys = Array.from({ length: decoyWitnessCount }, (_, index) => {
    const scriptBytes = encodeMidgardNativeScript({
      type: "sig",
      keyHash: Buffer.alloc(28, (index % 250) + 1),
    });
    return encodeMidgardVersionedScript({
      language: "NativeCardano",
      scriptBytes,
      nativeScript: {
        type: "sig",
        keyHash: Buffer.alloc(28, (index % 250) + 1),
      },
    });
  });
  const tx = makeNativeTx({
    spendInputCbors: spendInputs.map(encodeMidgardTxInputCanonical),
    fee: 7n,
    scriptTxWitsPreimageCbor: encodeCbor(decoys),
  });
  const badTxId = computeMidgardNativeTxId(tx).toString("hex");
  const nativeTxCompactCbor = encodeMidgardNativeTxCompact(tx.compact).toString(
    "hex",
  );
  const transactionSourceCbor = l2TransactionSourceCborV1(tx);
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(transactionSourceCbor, "hex"),
  );
  const txProof = await txTrie.prove(Buffer.from(badTxId, "hex"));
  const scriptWitnessItems = decodeMidgardFieldPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );
  const prepared: PreparedMissingNativeScriptUtxo = {
    headerHash: "",
    badTxId,
    nativeTxCanonicalCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
    nativeTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(tx.compact),
      nativeTxCompactCbor,
      l2TransactionSourceCbor: transactionSourceCbor,
      transactionsPhasRoot: trieRootHex(txTrie),
      txMembershipProofCbor: txProof.toCBOR().toString("hex"),
    },
    badInputIndex: 0n,
    spendInputItemCbors: spendInputs.map((input) =>
      encodeMidgardTxInputCanonical(input).toString("hex"),
    ),
    outRef,
    descriptorCbor: descriptorCbor.toString("hex"),
    prevUtxosRoot: previous.root,
    membershipProof,
    membershipProofCbor: Data.to(membershipProof, Proof),
    missingNativeScriptBytes: missingNativeScriptBytes.toString("hex"),
    expectedMissingScriptHash,
    scriptWitnessItemCbors: scriptWitnessItems.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
  };
  return {
    transactionsRoot: trieRootHex(txTrie),
    l2TransactionCount: 1n,
    prevUtxosRoot: previous.root,
    utxosRoot: previous.root,
    prepared,
    spendInputs,
    witnessSet: nativeWitnessSet(tx),
    scriptWitnessItems,
  };
};

const sortedAddressWitnesses = (count: number) =>
  Array.from({ length: count }, (_, index) => {
    const verificationKey = Buffer.alloc(32);
    verificationKey.writeUInt32BE(index, 28);
    return {
      verificationKey,
      signerHash: Buffer.from(
        missingSignatureVkeyHash(verificationKey.toString("hex")),
        "hex",
      ),
    };
  })
    .sort((left, right) => Buffer.compare(left.signerHash, right.signerHash))
    .map(({ verificationKey }) => ({
      verificationKey,
      item: encodeMidgardAddressWitnessItem({
        verificationKey,
        signature: Buffer.alloc(64, 0x55),
      }),
    }));

export const buildNativeScriptInvalidEmulatorFixture = async ({
  signerCount = 33,
}: {
  readonly signerCount?: number;
} = {}) => {
  const witnesses = sortedAddressWitnesses(signerCount);
  const nativeScript = {
    type: "all" as const,
    scripts: Array.from({ length: 31 }, (_, index) => ({
      type: "sig" as const,
      keyHash: Buffer.alloc(28, 0x80 + index),
    })),
  };
  const scriptBytes = encodeMidgardNativeScript(nativeScript);
  const scriptItem = encodeMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript,
  });
  const tx = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    addrTxWitsPreimageCbor: encodeCbor(witnesses.map(({ item }) => item)),
    scriptTxWitsPreimageCbor: encodeCbor([scriptItem]),
    validityIntervalStart: 0n,
    validityIntervalEnd: 100n,
  });
  const badTxId = computeMidgardNativeTxId(tx).toString("hex");
  const nativeTxCompactCbor = encodeMidgardNativeTxCompact(tx.compact).toString(
    "hex",
  );
  const transactionSourceCbor = l2TransactionSourceCborV1(tx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(transactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const addressWitnessItems = witnesses.map(({ item }) => item);
  const prepared: PreparedNativeScriptInvalid = {
    headerHash: "",
    badTxId,
    nativeTxCanonicalCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
    nativeTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(tx.compact),
      nativeTxCompactCbor,
      l2TransactionSourceCbor: transactionSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
    scriptIndex: 0n,
    scriptItemCbor: scriptItem.toString("hex"),
    scriptHash: hashMidgardVersionedScript({
      language: "NativeCardano",
      scriptBytes,
      nativeScript,
    }),
    addrWitnessItemCbors: addressWitnessItems.map((item) =>
      item.toString("hex"),
    ),
    scriptWitnessItemCbors: [scriptItem.toString("hex")],
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    prepared,
    witnessSet: nativeWitnessSet(tx),
    scriptItem,
    scriptWitnessItems: [scriptItem] as const,
    addressWitnessItems,
    addressWitnessVerificationKeys: witnesses.map(
      ({ verificationKey }) => verificationKey,
    ),
  };
};

export type FinalFamilyHarness<Family> = Awaited<
  ReturnType<typeof makeFaultProofEmulatorHarness>
> & {
  readonly family: Family;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
};

export type FinalMinAdaFamily = MinAdaContracts;

export type FinalMissingNativeScriptUtxoFamily =
  MissingNativeScriptUtxoContracts;

export type FinalNativeScriptInvalidFamily = NativeScriptInvalidContracts;
