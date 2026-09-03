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
  AddressData,
  addressDataFromBech32,
  EMPTY_MERKLE_TREE_ROOT,
  NETWORK_ID_FRAUD_CATEGORY_ID,
  NetworkIdFault,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  NETWORK_ID_BLUEPRINT_TITLES,
  type NetworkIdContracts,
} from "../../src/network-id/contracts.js";
import type {
  PreparedNetworkIdPostUtxoProof,
  PreparedNetworkIdProof,
} from "../../src/network-id/prepare.js";
import { nativeTxFromCoreCompact } from "../../src/submit-step-01.js";
import { registerPexcludesExclusionRewardAccount } from "./submit-init-emulator-fixtures.js";
import {
  applyCompiledScript,
  buildCatalogueDeploymentInfo,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  makeSpendingValidator,
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
  const fraudProofTokenAddressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      harness.realBlueprint,
      NETWORK_ID_BLUEPRINT_TITLES.step02,
      [
        harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        harness.contracts.computationThread.policyId,
        harness.contracts.fieldPreimageCertificate.policyId,
      ],
    ),
  );
  const forcedStep = makeSpendingValidator(
    applyCompiledScript(
      harness.realBlueprint,
      NETWORK_ID_BLUEPRINT_TITLES.forcedStep,
      [
        step02.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        0n,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      harness.realBlueprint,
      NETWORK_ID_BLUEPRINT_TITLES.step01,
      [
        step02.spendingScriptHash,
        forcedStep.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.hubOracle.policyId,
        0n,
      ],
    ),
  );
  const networkId: NetworkIdContracts = {
    steps: [step01, step02],
    forcedStep,
    expectedNetworkId: 0n,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
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

export const publishNetworkIdReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NetworkIdContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    published.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid,
          script: step.spendingScript as Script,
          label: `network-id step-0${(index + 1).toString()}`,
        })
      ).utxo,
    );
  }
  return published as unknown as readonly [UTxO, UTxO];
};
