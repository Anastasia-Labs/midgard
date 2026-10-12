import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildNativeScriptInvalidFaultProofContracts,
  type MinAdaFault,
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

import type {
  PreparedMinAdaTx,
  PreparedMinAdaUtxo,
} from "../../src/min-ada/prepare.js";
import type { NativeScriptInvalidContracts } from "../../src/native-script-invalid/contracts.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import {
  keyValuePhasNonMembershipProof,
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
  onPublication,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly family: Family;
  readonly label: string;
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

export const nativeWitnessSet = (
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
