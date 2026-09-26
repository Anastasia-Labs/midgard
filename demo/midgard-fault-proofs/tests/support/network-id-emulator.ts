import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildNetworkIdFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  NETWORK_ID_FRAUD_CATEGORY_ID,
  NetworkIdFault,
  parseFaultProofBlueprint,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type NetworkIdContracts } from "../../src/network-id/contracts.js";
import type {
  PreparedNetworkIdPostUtxoProof,
  PreparedNetworkIdProof,
} from "../../src/network-id/prepare.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import { type CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";
import { registerPexcludesExclusionRewardAccount } from "./submit-init-emulator-fixtures.js";
import {
  buildCatalogueDeploymentInfo,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const NETWORK_ID_EMULATOR_CATEGORY_ID = NETWORK_ID_FRAUD_CATEGORY_ID;

export const buildNetworkIdFixture = async ({
  outputNetworkId = 1,
  protectedAddress = true,
}: {
  /** Logical id 0..7; protection adds raw network-nibble bit 3. */
  readonly outputNetworkId?: number;
  readonly protectedAddress?: boolean;
} = {}) => {
  if (
    !Number.isInteger(outputNetworkId) ||
    outputNetworkId < 0 ||
    outputNetworkId > 7
  ) {
    throw new Error("network-id emulator logical output network must be 0..7");
  }
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | outputNetworkId | (protectedAddress ? 0x08 : 0)]),
      Buffer.alloc(28, 0x44),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const tx = makeNativeTx({ spendInputCbors: [], fee: 7n, outputCbor });
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
  const proofCbor = proof.toCBOR().toString("hex");
  const fault = {
    OutputNetwork: { output_index: 0n },
  } as NetworkIdFault;
  const prepared: PreparedNetworkIdProof = {
    headerHash: "", // rebound after the state-queue fixture hashes its header
    expectedNetworkId: 0n,
    badTxId,
    nativeTxCanonicalCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
    nativeTxCompactCbor,
    outputsItemCbors: decodeMidgardFieldPreimage(
      tx.body.outputsPreimageCbor,
    ).map((item) => Buffer.from(item).toString("hex")),
    faultClaim: { kind: "output-network", outputIndex: 0n },
    fault,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(tx.compact),
      nativeTxCompactCbor,
      l2TransactionSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProofCbor: proofCbor,
    },
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    prepared,
  };
};

/**
 * A zero-transaction transition that introduces a foreign-network compact
 * ledger descriptor. This is the independent OUTPUT-NETWORK-UTXO route: the
 * proof authenticates the exact descriptor under `header.utxos_root` and its
 * absence under the empty predecessor root, without carrying the full output.
 */
export const buildNetworkIdPostUtxoFixture = async ({
  outputNetworkId = 2,
  protectedAddress = false,
}: {
  readonly outputNetworkId?: number;
  readonly protectedAddress?: boolean;
} = {}) => {
  if (
    !Number.isInteger(outputNetworkId) ||
    outputNetworkId < 0 ||
    outputNetworkId > 7 ||
    outputNetworkId === 0
  ) {
    throw new Error(
      "network-id post-UTxO emulator output network must be foreign logical id 1..7",
    );
  }
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | outputNetworkId | (protectedAddress ? 0x08 : 0)]),
      Buffer.alloc(28, 0x55),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const outRef = {
    transactionId: "cc".repeat(32),
    outputIndex: 0n,
  } as const;
  const outRefKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });
  const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: outRefKey,
    outputCbor,
  }).descriptorCbor;
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outRefKey, descriptorCbor);
  const proofCbor = (await trie.prove(outRefKey)).toCBOR().toString("hex");
  const membershipProof = Data.from(proofCbor, Proof);
  const emptyProof = Data.from(Data.to([], Proof), Proof);
  const fault = {
    OutputNetworkUtxo: { observed_network_id: BigInt(outputNetworkId) },
  } as NetworkIdFault;
  const prepared: PreparedNetworkIdPostUtxoProof = {
    headerHash: "", // rebound after the state-queue fixture hashes its header
    expectedNetworkId: 0n,
    outRef,
    outRefKeyCbor: outRefKey.toString("hex"),
    descriptorCbor: descriptorCbor.toString("hex"),
    postUtxosRoot: trieRootHex(trie),
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    membershipProofCbor: proofCbor,
    membershipProof,
    predecessor: "Introduced",
    predecessorProof: emptyProof,
    predecessorProofCbor: Data.to(emptyProof, Proof),
    faultClaim: {
      kind: "post-utxo-network",
      outRef,
      observedNetworkId: BigInt(outputNetworkId),
    },
    fault,
  };
  return {
    transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    l2TransactionCount: 0n,
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: trieRootHex(trie),
    prepared,
  };
};

export const makeNetworkIdEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
    registerAdditionalRewardAccounts: async (lucid, blueprint) => {
      await registerPexcludesExclusionRewardAccount(lucid, blueprint);
      await registerChunkedVerifyRewardAccount(lucid, blueprint);
    },
  });
  const built = await Effect.runPromise(
    buildNetworkIdFaultProofContracts({
      blueprint: parseFaultProofBlueprint(harness.realBlueprint),
      network,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId:
        harness.contracts.fraudProofCatalogue.policyId,
    }),
  );
  const { steps, forcedStep, forcedScan } = built.networkId;
  const [step01] = steps;
  const networkId: NetworkIdContracts = {
    steps,
    forcedStep,
    forcedScan,
    expectedNetworkId: 0n,
    computationThread: built.computationThread,
    fraudProof: built.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId: built.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      built.fieldPreimageCertificate.mintingScript,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    networkId: step01,
  });
  const category = catalogue.categories.networkId;
  if (category.scriptHash !== step01.spendingScriptHash) {
    throw new Error("network-id emulator catalogue registered a stale step-01");
  }
  return { ...harness, networkId, catalogue, category };
};

/**
 * Publishes the four applied reference scripts (step 01, step 02, forced step,
 * forced scan) and returns their UTxOs with the measured publication
 * transactions so a suite can record the publication margins in its fit
 * ledger.
 */
/** The four scripts the forced direction needs published as references. */
export type NetworkIdPublishedReferenceScriptName =
  | "step01"
  | "step02"
  | "forcedStep"
  | "forcedScan";

export const publishNetworkIdReferenceScriptsMeasured = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NetworkIdContracts;
}): Promise<{
  readonly utxos: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly measurements: readonly {
    readonly name: NetworkIdPublishedReferenceScriptName;
    readonly scriptHash: string;
    readonly measurement: CompleteSignedTransactionMeasurement;
  }[];
}> => {
  if (contracts.forcedStep === undefined)
    throw new Error("network-id emulator harness deploys the forced step");
  if (contracts.forcedScan === undefined)
    throw new Error("network-id emulator harness deploys the forced scan");
  const targets = [
    ["step01", contracts.steps[0]],
    ["step02", contracts.steps[1]],
    ["forcedStep", contracts.forcedStep],
    ["forcedScan", contracts.forcedScan],
  ] as const;
  const utxos: UTxO[] = [];
  const measurements: {
    name: NetworkIdPublishedReferenceScriptName;
    scriptHash: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  for (const [name, step] of targets) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `network-id ${name}`,
    });
    utxos.push(published.utxo);
    measurements.push({
      name,
      scriptHash: step.spendingScriptHash,
      measurement: published.publicationMeasurement,
    });
  }
  return {
    utxos: utxos as unknown as readonly [UTxO, UTxO, UTxO, UTxO],
    measurements,
  };
};

export const publishNetworkIdReferenceScripts = async (params: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NetworkIdContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> =>
  (await publishNetworkIdReferenceScriptsMeasured(params)).utxos;
