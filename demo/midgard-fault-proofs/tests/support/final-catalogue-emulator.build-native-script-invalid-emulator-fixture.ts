import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";

import type { PreparedNativeScriptInvalid } from "../../src/native-script-invalid/prepare.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import { nativeWitnessSet } from "./final-catalogue-emulator.build-min-ada-post-utxo-emulator-fixture.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

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
