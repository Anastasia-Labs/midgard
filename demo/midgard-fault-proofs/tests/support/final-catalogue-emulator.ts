import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
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
  buildNativeScriptInvalidFaultProofContracts,
  encodeMidgardTxInputCanonical,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type MidgardTxInput,
  type MinAdaFault,
  missingSignatureVkeyHash,
  type NativeTxWitnessSetCompact,
  parseFaultProofBlueprint,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
} from "@al-ft/midgard-validation";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { MinAdaContracts } from "../../src/min-ada/contracts.js";
import type {
  PreparedMinAdaTx,
  PreparedMinAdaUtxo,
} from "../../src/min-ada/prepare.js";
import type { MissingNativeScriptUtxoContracts } from "../../src/missing-native-script-utxo/contracts.js";
import type { PreparedMissingNativeScriptUtxo } from "../../src/missing-native-script-utxo/prepare.js";
import type { NativeScriptInvalidContracts } from "../../src/native-script-invalid/contracts.js";
import type { PreparedNativeScriptInvalid } from "../../src/native-script-invalid/prepare.js";
import { nativeTxFromCoreCompact } from "../../src/submit-step-01.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { registerPexcludesExclusionRewardAccount } from "./submit-init-emulator-fixtures.js";
import {
  buildCatalogueDeploymentInfo,
  cloneBlueprint,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const makeMinAdaEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMinAda: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: async (lucid, blueprint) => {
      await registerPexcludesExclusionRewardAccount(lucid, blueprint);
    },
  });
  const family = harness.contracts.minAda;
  if (family === undefined) throw new Error("Harness did not build min-ada");
  const category = harness.catalogue.categories.minAda;
  return { ...harness, family, category };
};

export const makeMissingNativeScriptUtxoEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMissingNativeScriptUtxo: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.missingNativeScriptUtxo;
  if (family === undefined) {
    throw new Error("Harness did not build missing-native-script-utxo");
  }
  return {
    ...harness,
    family,
    category: harness.catalogue.categories.missingNativeScriptUtxo,
  };
};

export const makeNativeScriptInvalidEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      alwaysFraudProofCatalogue: true,
    },
  });
  const built = await Effect.runPromise(
    buildNativeScriptInvalidFaultProofContracts({
      blueprint: parseFaultProofBlueprint(
        cloneBlueprint(harness.realBlueprint),
      ),
      network,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId:
        harness.contracts.fraudProofCatalogue.policyId,
    }),
  );
  if (
    built.fraudProof.policyId !== harness.contracts.fraudProof.policyId ||
    built.computationThread.policyId !==
      harness.contracts.computationThread.policyId
  ) {
    throw new Error("native-script-invalid did not share harness policies");
  }
  const family: NativeScriptInvalidContracts = {
    steps: built.nativeScriptInvalid.steps,
    computationThread: built.computationThread,
    fraudProof: built.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
  };
  const fraudProofContracts = {
    ...harness.contracts.fraudProofContracts,
    nativeScriptInvalid: built.nativeScriptInvalid,
  };
  const contracts = {
    ...harness.contracts,
    nativeScriptInvalid: family,
    fraudProofContracts,
    fraudProofs: {
      ...harness.contracts.fraudProofs,
      nativeScriptInvalid: built.nativeScriptInvalid.firstStep,
    },
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  return {
    ...harness,
    contracts,
    catalogue,
    family,
    category: catalogue.categories.nativeScriptInvalid,
  };
};

export const publishFinalFamilyReferenceScripts = async <
  Family extends {
    readonly steps: readonly { readonly spendingScript: Script }[];
  },
>({
  lucid,
  family,
  label,
  enforceL1Envelope = false,
  onPublication,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly family: Family;
  readonly label: string;
  readonly enforceL1Envelope?: boolean;
  readonly onPublication?: (
    stepIndex: number,
    publication: Awaited<ReturnType<typeof publishPlainReferenceScriptUtxo>>,
  ) => void;
}): Promise<readonly UTxO[]> => {
  const refs: UTxO[] = [];
  for (const [index, step] of family.steps.entries()) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript,
      label: `${label} step-${(index + 1).toString().padStart(2, "0")}`,
      oversized: !enforceL1Envelope,
    });
    onPublication?.(index, publication);
    refs.push(publication.utxo);
  }
  return refs;
};

export const buildMinAdaTxEmulatorFixture = async () => {
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
    value: { lovelace: 0n, assets: new Map() },
  });
  const tx = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor,
  });
  const badTxId = computeMidgardNativeTxId(tx).toString("hex");
  const nativeTxCompactCbor = encodeMidgardNativeTxCompact(tx.compact).toString(
    "hex",
  );
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(tx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor,
  });
  const fault = {
    MinAdaTx: { output_index: 0n },
  } as MinAdaFault;
  const prepared: PreparedMinAdaTx = {
    kind: "min-ada-tx",
    headerHash: "",
    badTxId,
    badOutputIndex: 0n,
    nativeTxCanonicalCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
    nativeTxCompactCbor,
    outputItemCbors: decodeMidgardFieldPreimage(
      tx.body.outputsPreimageCbor,
    ).map((item) => Buffer.from(item).toString("hex")),
    descriptorCbor: material.descriptorCbor.toString("hex"),
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(tx.compact),
      nativeTxCompactCbor,
      l2TransactionSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
    fault,
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    prepared,
  };
};

export const buildMinAdaPostUtxoEmulatorFixture = async ({
  emptyPrevious = false,
}: {
  readonly emptyPrevious?: boolean;
} = {}) => {
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace: 0n, assets: new Map() },
  });
  const tx = makeNativeTx({ spendInputCbors: [], fee: 7n, outputCbor });
  const transactionId = computeMidgardNativeTxId(tx).toString("hex");
  const outRef = { transactionId, outputIndex: 0n } as const;
  const outRefKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });
  const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: outRefKey,
    outputCbor,
  }).descriptorCbor;
  const postStore = new Store(undefined);
  await postStore.ready();
  const postTrie = new Trie(postStore);
  await postTrie.insert(outRefKey, descriptorCbor);
  const postMembershipProofCbor = (await postTrie.prove(outRefKey))
    .toCBOR()
    .toString("hex");
  const previous = await keyValuePhasRootWithCount(
    emptyPrevious
      ? []
      : [{ key: Buffer.alloc(outRefKey.length, 0xa5), value: descriptorCbor }],
  );
  const predecessorNonMembershipProof = await keyValuePhasNonMembershipProof(
    previous,
    outRefKey,
  );
  const predecessorNonMembershipProofCbor = Data.to(
    predecessorNonMembershipProof,
    Proof,
  );
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(
    Buffer.from(transactionId, "hex"),
    Buffer.from(l2TransactionSourceCborV1(tx), "hex"),
  );
  const prepared: PreparedMinAdaUtxo = {
    kind: "min-ada-utxo",
    headerHash: "",
    outRef,
    outRefKeyCbor: outRefKey.toString("hex"),
    descriptorCbor: descriptorCbor.toString("hex"),
    postUtxosRoot: trieRootHex(postTrie),
    prevUtxosRoot: previous.root,
    postMembershipProof: Data.from(postMembershipProofCbor, Proof),
    postMembershipProofCbor,
    predecessorNonMembershipProof,
    predecessorNonMembershipProofCbor,
    fault: "MinAdaUtxo" as MinAdaFault,
  };
  return {
    transactionsRoot: trieRootHex(txTrie),
    l2TransactionCount: 1n,
    prevUtxosRoot: prepared.prevUtxosRoot,
    utxosRoot: prepared.postUtxosRoot,
    prepared,
  };
};

const nativeWitnessSet = (
  tx: ReturnType<typeof makeNativeTx>,
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

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
