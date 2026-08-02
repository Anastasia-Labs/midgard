import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
  type MidgardNativeTxFullV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  buildPhasMembershipRewardRegistrationTxProgram,
  buildValidationTraceDisputeFaultProofContracts,
  commitCountedRootProgram,
  createReferenceScriptAuthPolicy,
  DA_PAYLOAD_V1_VERSION,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_MERKLE_TREE_ROOT,
  EMPTY_SPEND_INPUTS_HASH,
  encodeDaPayloadV1,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  type FraudProofCatalogueDeploymentInfo,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  headerHashFromStateQueueUTxO,
  HeaderV1,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  invalidOneStepTransitionFault,
  InvalidRangeStep02Datum,
  invalidRangeViolationReason,
  type MidgardValidators,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  OutputReference,
  parseFaultProofBlueprint,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  TransitionStepSchema,
  utxosToStateQueueUTxOs,
  utxoToStateQueueUTxO,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
  ZeroInputStep02Datum,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundleV1,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildInvalidForcedTransactionNoOpWitness,
  buildTransitionFaultProof,
  encodeData,
  keyValuePhasRootWithCount,
  reconstructDaPayloadV1,
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
  submitTransitionTraceProof,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  validationDisputeValidityRange,
} from "../src/index.js";
import { buildNonMembershipProof, type TrieEntry } from "../src/ne-proofs.js";
import {
  nativeTxFromCoreCompact,
  type NeInputPreimageEntry,
  neSubmitStep01,
  neSubmitStep02,
  neSubmitStep03,
  neSubmitStep04,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitInvalidRangeStep01,
  submitInvalidRangeStep02,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
  submitZeroInputStep01,
  submitZeroInputStep02,
} from "./support/legacy-submit-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  buildCatalogueDeploymentInfo,
  buildForcedValidationDisputeCommitments,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  cloneBlueprint,
  type CompleteSignedTransactionMeasurement,
  deploymentManifest,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  firstWalletUtxo,
  getCompiledScript,
  h32,
  makeHeader,
  makeNativeTx,
  network,
  publishAuthenticatedValidationDisputeControl,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  publishValidationDisputeReferenceScript,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  type RemovalReferenceScriptName,
  runEmulatorLifecycleStage,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
  trieRootHex,
  type ValidationDisputeControlPublicationTarget,
  validationDisputeControlPublicationTargets,
} from "./support/submit-init-emulator-shared.js";

const positiveNonAdaAssets = (utxo: UTxO) =>
  Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );

const expectStateQueueHeaderOrder = async ({
  lucid,
  contracts,
  expectedHeaderHashes,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly expectedHeaderHashes: readonly string[];
}) => {
  const utxos = await lucid.utxosAt(contracts.stateQueue.spendingScriptAddress);
  const parsedStateQueueUtxos = await Effect.runPromise(
    utxosToStateQueueUTxOs(utxos, contracts.stateQueue.policyId),
  );
  expect(parsedStateQueueUtxos).toHaveLength(expectedHeaderHashes.length + 1);
  expect(
    parsedStateQueueUtxos.map(({ assetName }) => assetName).sort(),
  ).toEqual(
    [
      STATE_QUEUE_ROOT_ASSET_NAME,
      ...expectedHeaderHashes.map(
        (headerHash) => STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      ),
    ].sort(),
  );

  const sortedStateQueueUtxos = await Effect.runPromise(
    Effect.succeed(parsedStateQueueUtxos).pipe(
      Effect.andThen(sortStateQueueUTxOs),
    ),
  );
  expect(sortedStateQueueUtxos).toHaveLength(parsedStateQueueUtxos.length);
  const [root, ...blocks] = sortedStateQueueUtxos;
  if (root === undefined) {
    throw new Error("Expected state-queue topology to include the root node");
  }
  expect(root.assetName).toBe(STATE_QUEUE_ROOT_ASSET_NAME);
  expect(root.datum.key).toBe("Empty");
  expect(root.datum.next).toEqual(
    expectedHeaderHashes[0] === undefined
      ? "Empty"
      : { Key: { key: expectedHeaderHashes[0] } },
  );

  const observedHeaderHashes = await Promise.all(
    blocks.map((block) =>
      Effect.runPromise(headerHashFromStateQueueUTxO(block)),
    ),
  );
  expect(observedHeaderHashes).toEqual(expectedHeaderHashes);
  expect(new Set(observedHeaderHashes).size).toBe(observedHeaderHashes.length);

  for (let index = 0; index < blocks.length; index += 1) {
    const block = blocks[index]!;
    const expectedHeaderHash = expectedHeaderHashes[index]!;
    const nextExpectedHeaderHash = expectedHeaderHashes[index + 1];
    expect(block.datum.key).toEqual({ Key: { key: expectedHeaderHash } });
    expect(block.datum.next).toEqual(
      nextExpectedHeaderHash === undefined
        ? "Empty"
        : { Key: { key: nextExpectedHeaderHash } },
    );
  }
};

type TestOutputReference = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

type TransactionInclusionEntry = {
  readonly inclusion: unknown;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxId: string;
  readonly spendInputCbors: readonly string[];
};

const tx1InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("a1"), outputIndex: 0n },
  { transactionId: h32("a2"), outputIndex: 1n },
];

const tx2InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("b1"), outputIndex: 0n },
  tx1InputsPreimage[1]!,
];

const outputReferenceCbor = (outRef: TestOutputReference): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(outRef.transactionId),
      outRef.outputIndex,
    ).to_cbor_bytes(),
  );

const largeFittingOutputCbor = (
  inlineDatumPayloadBytes: number = 13_600,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace: 100_000_000n, assets: new Map() },
    datum: {
      kind: "inline",
      cbor: Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          CML.PlutusData.new_bytes(
            Buffer.alloc(inlineDatumPayloadBytes, 0xa5),
          ).to_cbor_hex(),
        ),
        "hex",
      ),
    },
  });

const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

const compactTxEntry = (
  nativeTx: MidgardNativeTxFullV1,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxIdV1(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFullV1,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));

const buildTransactionInclusionFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly tx1: TransactionInclusionEntry;
  readonly tx2: TransactionInclusionEntry;
  readonly tx1InputsPreimage: readonly TestOutputReference[];
  readonly tx2InputsPreimage: readonly TestOutputReference[];
  readonly tx1SpendInputCbors: readonly string[];
  readonly tx2SpendInputCbors: readonly string[];
}> => {
  const tx1Native = makeNativeTx({
    spendInputCbors: tx1InputsPreimage.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "13",
    outputByte: "14",
    witnessByte: "20",
  });
  const tx2Native = makeNativeTx({
    spendInputCbors: tx2InputsPreimage.map(outputReferenceCbor),
    fee: 1n,
    referenceByte: "23",
    outputByte: "24",
    witnessByte: "30",
  });
  const tx1 = compactTxEntry(tx1Native);
  const tx2 = compactTxEntry(tx2Native);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [tx1, tx2]) {
    await trie.insert(
      Buffer.from(entry.nativeTxId, "hex"),
      Buffer.from(
        encodeMidgardNativeTxCompactV1(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ),
      ),
    );
  }
  const withProof = async (
    entry: typeof tx1,
  ): Promise<TransactionInclusionEntry> => {
    const txKey = Buffer.from(entry.nativeTxId, "hex");
    const proof = await trie.prove(txKey);
    return {
      inclusion: {
        nativeTxId: entry.nativeTxId,
        nativeTx: entry.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: entry.nativeTx,
      nativeTxId: entry.nativeTxId,
      spendInputCbors: entry.spendInputCbors,
    };
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 2n,
    tx1: await withProof(tx1),
    tx2: await withProof(tx2),
    tx1InputsPreimage,
    tx2InputsPreimage,
    tx1SpendInputCbors: tx1.spendInputCbors,
    tx2SpendInputCbors: tx2.spendInputCbors,
  };
};

const buildInvalidRangeTransactionInclusionFixture = async ({
  blockValidFrom,
  blockValidTo,
}: {
  readonly blockValidFrom: bigint;
  readonly blockValidTo: bigint;
}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly normalizedValidityRange: ReturnType<
    typeof normalizeNativeTxValidityRange
  >;
  readonly violationReason: NonNullable<
    ReturnType<typeof invalidRangeViolationReason>
  >;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [outputReferenceCbor(tx1InputsPreimage[0]!)],
    fee: 3n,
    referenceByte: "41",
    outputByte: "42",
    witnessByte: "43",
    validityIntervalStart: blockValidFrom - 1n,
    validityIntervalEnd: blockValidTo,
  });
  const badTx = compactTxEntry(badNativeTx);
  const normalizedValidityRange = normalizeNativeTxValidityRange(
    badTx.nativeTx.body,
  );
  const violationReason = invalidRangeViolationReason({
    blockValidFrom,
    blockValidTo,
    normalizedRange: normalizedValidityRange,
  });
  if (violationReason === null) {
    throw new Error(
      "Invalid-range fixture transaction does not violate block validity.",
    );
  }

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(encodeMidgardNativeTxCompactV1(badNativeTx.compact)),
  );
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          badNativeTx.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    normalizedValidityRange,
    violationReason,
  };
};

// Zero-input fixture: a bad L2 tx that spends nothing at all, violating the
// "at least one input" ledger rule. Its `spend_inputs_hash` is the hash of the
// empty definite-length CBOR array, which is precisely the constant step-02
// compares against.
const buildZeroInputTransactionInclusionFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 5n,
    referenceByte: "51",
    outputByte: "52",
    witnessByte: "53",
  });
  const badTx = compactTxEntry(badNativeTx);

  if (
    !nativeTxBodyHasZeroInputViolation({ txBody: badTx.nativeTx.body }) ||
    badTx.spendInputCbors.length !== 0
  ) {
    throw new Error(
      "Zero-input fixture transaction does not spend an empty input list.",
    );
  }
  expect(badTx.nativeTx.body.spend_inputs_hash).toBe(EMPTY_SPEND_INPUTS_HASH);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(encodeMidgardNativeTxCompactV1(badNativeTx.compact)),
  );
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          badNativeTx.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
  };
};

// Non-existent-input fixture: a bad L2 tx spends an input whose producing
// transaction never existed. The transactions trie is keyed by the raw native
// tx id (matching the node); the ledger non-membership is proven against the
// empty prev-ledger (`EMPTY_MERKLE_TREE_ROOT`, the genesis confirmed-state root
// the setup block builds on); and the phantom input's producing tx id is proven
// absent from the block's transactions.
const buildNonExistentInputFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  readonly badInputIndex: bigint;
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly missingInputTxId: string;
  readonly nativeTxId: string;
}> => {
  const phantomOutRef: TestOutputReference = {
    transactionId: h32("de"),
    outputIndex: 0n,
  };
  const badTxNative = makeNativeTx({
    spendInputCbors: [outputReferenceCbor(phantomOutRef)],
    fee: 0n,
    referenceByte: "e3",
    outputByte: "e4",
    witnessByte: "e5",
  });
  const badTx = compactTxEntry(badTxNative);
  const badTxCompactCbor = encodeMidgardNativeTxCompactV1(badTxNative.compact);

  // A second, well-formed L2 tx so the transactions trie is non-trivial (proofs
  // for a single-element trie are degenerate).
  const otherTxNative = makeNativeTx({
    spendInputCbors: [
      outputReferenceCbor({ transactionId: h32("c1"), outputIndex: 0n }),
    ],
    fee: 1n,
    referenceByte: "c3",
    outputByte: "c4",
    witnessByte: "c5",
  });
  const otherTx = compactTxEntry(otherTxNative);
  const otherTxCompactCbor = encodeMidgardNativeTxCompactV1(
    otherTxNative.compact,
  );

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxCompactCbor),
  );
  await trie.insert(
    Buffer.from(otherTx.nativeTxId, "hex"),
    Buffer.from(otherTxCompactCbor),
  );
  const transactionsRoot = trieRootHex(trie);
  const membershipProof = await trie.prove(
    Buffer.from(badTx.nativeTxId, "hex"),
  );

  const txsEntries: TrieEntry[] = [
    {
      key: Buffer.from(badTx.nativeTxId, "hex"),
      value: Buffer.from(badTxCompactCbor),
    },
    {
      key: Buffer.from(otherTx.nativeTxId, "hex"),
      value: Buffer.from(otherTxCompactCbor),
    },
  ];
  const txsNonMembershipProofCbor = await buildNonMembershipProof(
    txsEntries,
    Buffer.from(phantomOutRef.transactionId, "hex"),
  );
  const ledgerNonMembershipProofCbor = await buildNonMembershipProof(
    [],
    outputReferenceCbor(phantomOutRef),
  );

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    inclusion: parseSubmitStep01TxInclusion({
      nativeTxId: badTx.nativeTxId,
      nativeTx: badTx.nativeTx,
      nativeTxCompactCbor: badTxCompactCbor.toString("hex"),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: membershipProof.toCBOR().toString("hex"),
    }),
    inputsPreimage: [
      { txId: phantomOutRef.transactionId, index: phantomOutRef.outputIndex },
    ],
    badInputIndex: 0n,
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
    missingInputTxId: phantomOutRef.transactionId,
    nativeTxId: badTx.nativeTxId,
  };
};

const registerPexcludesExclusionRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, "pexcludes.exclusion.withdraw"),
  };
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, {
      script: pexcludesScript,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

// Commit a raw transactions MPF root the way the node does: wrap it with the
// counted-root hash under the transactions domain. Fault-proof inclusion then
// authenticates the raw root against this committed value.
const countedTransactionsRoot = (
  rawRoot: string,
  count: bigint,
): Promise<string> =>
  Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: rawRoot,
      count,
    }),
  );

const transitionTraceRawEntry = (
  key: string,
  value: string,
): [string, string] => [key, value];

const sortedDaEntries = (
  entries: readonly [string, string][],
): [string, string][] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const buildInvalidForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    `825820${h32("01")}00`,
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const finalDescriptor = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalUtxosRoot = await keyValuePhasRootWithCount([
    {
      key: Buffer.from(finalUtxo[0], "hex"),
      value: finalDescriptor,
    },
  ]);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputByte: "b2",
    witnessByte: "b8",
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: computeMidgardNativeTxIdV1(forcedNativeTx).toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(forcedSource).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    operator_validity: "FailedScript",
  };
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction",
    pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
    post_utxos_root: finalUtxosRoot.root,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction",
  };
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: forcedTransaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const forcedPreimageEntries = [
    transitionTraceRawEntry(
      forcedEntries[0]![0],
      forcedCanonicalCbor.toString("hex"),
    ),
  ];
  const validationTraceEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: {
        schema_version: 1n,
        machine_version: 1n,
        trace_root: h32("c1"),
        step_count: 1n,
        initial_state_hash: h32("c2"),
        terminal_state_hash: h32("c3"),
        verdict: "Rejected",
        rejection_code_hash: h32("c4"),
      },
      valueSchema: ValidationTraceDescriptorV1Schema,
    }),
  ];
  const traceEntries = [
    transitionTraceDaEntry({
      key: step.step_index,
      keySchema: Data.Integer() as never,
      value: step,
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventToStepEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: eventToStepValue,
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const forcedRoot = await buildCountedRoot(
    ROOT_DOMAINS.forcedTransactionsV1,
    forcedEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const traceRoot = await buildCountedRoot(
    ROOT_DOMAINS.transitionTrace,
    traceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const eventToStepRoot = await buildCountedRoot(
    ROOT_DOMAINS.eventToStep,
    eventToStepEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const validationTracesRoot = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    validationTraceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header: HeaderV1 = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    encodeDaPayloadV1({
      version: DA_PAYLOAD_V1_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: sortedDaEntries([finalUtxo]),
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(traceEntries),
        event_to_step: sortedDaEntries(eventToStepEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries(forcedPreimageEntries),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationTraceEntries),
        counts,
      },
    }),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayloadV1({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
  const fault = invalidOneStepTransitionFault(
    await buildInvalidForcedTransactionNoOpWitness({
      reconstruction,
      stepIndex: 0n,
    }),
  );
  return {
    header,
    headerHash,
    proof: buildTransitionFaultProof({ reconstruction, fault }),
  };
};

const buildInvalidForcedValidationDisputeFixture = async ({
  operatorVkey,
  now,
  inlineDatumPayloadBytes = 13_600,
  minimumCompleteItemBytes = MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly inlineDatumPayloadBytes?: number;
  readonly minimumCompleteItemBytes?: number;
}) => {
  const txOrderId = transitionTraceOutRef("e1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: largeFittingOutputCbor(inlineDatumPayloadBytes),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const decodedForcedNativeTx =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(forcedCanonicalCbor);
  if (
    decodeMidgardNativeByteListPreimage(
      decodedForcedNativeTx.witnessSet.addrTxWitsPreimageCbor,
      "test.forced_native_tx.addr_tx_wits",
    ).length !== 0
  ) {
    throw new Error(
      "forced validation-dispute fixture unexpectedly contains vkey witnesses",
    );
  }
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(forcedSource).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    operator_validity: "TxIsValid" as const,
  };
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.EmptyInputs,
    }),
  );
  const disputedWitnessIndex = challengerTrace.witnesses.findIndex(
    (witness) =>
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.itemCbor.length > minimumCompleteItemBytes,
  );
  if (disputedWitnessIndex < 0) {
    throw new Error(
      "validation-dispute fixture is missing its selected fitting complete item",
    );
  }
  const completeItemWitness = challengerTrace.witnesses[disputedWitnessIndex]!;
  if (completeItemWitness.auxiliary?.kind !== "transactionFieldItem") {
    throw new Error(
      "validation-dispute fixture selected a non-item canonical witness",
    );
  }
  const completeItemBytes = completeItemWitness.auxiliary.itemCbor.length;
  const operatorRejectionCodeHash = Buffer.alloc(32);
  const operatorStates = challengerTrace.states.map((state, index) =>
    index >= disputedWitnessIndex + 1
      ? {
          ...state,
          workRoot: Buffer.alloc(32, 0x7e),
          ...(index === challengerTrace.states.length - 1
            ? {
                verdict: "accepted" as const,
                rejectionCodeHash: operatorRejectionCodeHash,
              }
            : {}),
        }
      : state,
  );
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineStateV1),
      "accepted",
      operatorRejectionCodeHash,
    ),
    verdict: "accepted",
    rejectionCode: null,
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    challengerTrace.tree.descriptor,
  );
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor,
    evidence,
    completeItemBytes,
  };
};

const submitSuccessorBlockTx = async ({
  lucid,
  contracts,
  anchorBlockUnit,
  header,
  hubOracle,
  scheduler,
  activeOperatorNode,
  activeOperatorNodeUnit,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly anchorBlockUnit: string;
  readonly header: HeaderV1;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
}): Promise<{
  readonly continuedAnchorOutRef: string;
  readonly successorOutRef: string;
  readonly successorHeaderHash: string;
  readonly successorBlockUnit: string;
  readonly activeOperatorNode: UTxO;
}> => {
  const [anchorBlockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  if (anchorBlockUtxo === undefined) {
    throw new Error("Expected live state-queue anchor block for successor");
  }
  const anchorBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(anchorBlockUtxo, contracts.stateQueue.policyId),
  );
  const successorHeaderHash = await Effect.runPromise(
    hashBlockHeaderV1(header),
  );
  const successorBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHeaderHash,
  );
  const commitFeeInput = await firstWalletUtxo(
    lucid,
    "successor commit fee input",
  );
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "successor commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "successor commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracle,
            "successor commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "successor commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: anchorBlock,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: scheduler,
        additionalRefInputs: [hubOracle],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    ),
  );
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const [continuedAnchorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  const [successorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    successorBlockUnit,
  );
  const [continuedActiveOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  if (
    continuedAnchorUtxo === undefined ||
    successorUtxo === undefined ||
    continuedActiveOperatorNode === undefined
  ) {
    throw new Error("Successor commit did not preserve expected queue nodes");
  }
  const continuedAnchor = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedAnchorUtxo, contracts.stateQueue.policyId),
  );
  await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(continuedAnchor.datum),
  );
  expect(continuedAnchor.datum.next).toEqual({
    Key: { key: successorHeaderHash },
  });

  return {
    continuedAnchorOutRef: outRefLabel(continuedAnchorUtxo),
    successorOutRef: outRefLabel(successorUtxo),
    successorHeaderHash,
    successorBlockUnit,
    activeOperatorNode: continuedActiveOperatorNode,
  };
};

type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: HeaderV1;
};

type ProvedDoubleSpendFixture = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly contracts: MidgardValidators;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly transactionInclusion: Awaited<
    ReturnType<typeof buildTransactionInclusionFixture>
  >;
  readonly fraudulentHeader: HeaderV1;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly successors: readonly SuccessorBlockFixture[];
  readonly deploymentInfo: ReturnType<typeof buildRemovalDeploymentInfo>;
  readonly fraudulentBlockOutRef: string;
  readonly submitInitResult: Awaited<ReturnType<typeof submitInit>>;
  readonly step04Result: Awaited<ReturnType<typeof submitStep04>>;
  readonly fraudProofUtxo: UTxO;
  readonly proverPaymentKeyHash: string;
};

type RemovalEvent =
  | { readonly kind: "stateQueue.utxosAt"; readonly call: number }
  | { readonly kind: "scheduler.utxosAtWithUnit"; readonly call: number }
  | { readonly kind: "awaitTx"; readonly txHash: string }
  | { readonly kind: "lease.acquire" }
  | { readonly kind: "lease.renew"; readonly call: number }
  | { readonly kind: "lease.release" }
  | { readonly kind: "lease.fail"; readonly error: string };

const eventIndexes = (
  events: readonly RemovalEvent[],
  kind: RemovalEvent["kind"],
): number[] =>
  events.flatMap((event, index) => (event.kind === kind ? [index] : []));

const createRecordingLeaseCoordinator = (
  events: RemovalEvent[],
): StateQueueMutationLeaseCoordinator => {
  let renewCalls = 0;
  return {
    acquire: async () => {
      events.push({ kind: "lease.acquire" });
      return {
        token: "emulator-fault-proof-removal",
        source: "emulator",
        renew: async () => {
          renewCalls += 1;
          events.push({ kind: "lease.renew", call: renewCalls });
        },
        release: async () => {
          events.push({ kind: "lease.release" });
        },
        fail: async (error: string) => {
          events.push({ kind: "lease.fail", error });
        },
      };
    },
  };
};

const instrumentLucidForRemoval = ({
  lucid,
  contracts,
  events,
  failStateQueueUtxosAtCall,
  failSchedulerUtxosAtWithUnitCall,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly events: RemovalEvent[];
  readonly failStateQueueUtxosAtCall?: number;
  readonly failSchedulerUtxosAtWithUnitCall?: number;
}): Awaited<ReturnType<typeof Lucid>> => {
  let stateQueueUtxosAtCalls = 0;
  let schedulerUtxosAtWithUnitCalls = 0;
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SCHEDULER_ASSET_NAME,
  );
  return new Proxy(lucid, {
    get(target, property, receiver) {
      if (property === "utxosAt") {
        return async (address: string, ...rest: unknown[]) => {
          if (address === contracts.stateQueue.spendingScriptAddress) {
            stateQueueUtxosAtCalls += 1;
            events.push({
              kind: "stateQueue.utxosAt",
              call: stateQueueUtxosAtCalls,
            });
            if (stateQueueUtxosAtCalls === failStateQueueUtxosAtCall) {
              throw new Error("instrumented state-queue topology load failure");
            }
          }
          return await target.utxosAt(address, ...(rest as []));
        };
      }
      if (property === "utxosAtWithUnit") {
        return async (address: string, unit: string, ...rest: unknown[]) => {
          if (
            address === contracts.scheduler.spendingScriptAddress &&
            unit === schedulerUnit
          ) {
            schedulerUtxosAtWithUnitCalls += 1;
            events.push({
              kind: "scheduler.utxosAtWithUnit",
              call: schedulerUtxosAtWithUnitCalls,
            });
            if (
              schedulerUtxosAtWithUnitCalls === failSchedulerUtxosAtWithUnitCall
            ) {
              throw new Error("instrumented scheduler lookup failure");
            }
          }
          return await target.utxosAtWithUnit(address, unit, ...(rest as []));
        };
      }
      if (property === "awaitTx") {
        return async (txHash: string, ...rest: unknown[]) => {
          events.push({ kind: "awaitTx", txHash });
          return await target.awaitTx(txHash, ...(rest as []));
        };
      }
      const value = Reflect.get(target, property, receiver);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
};

const buildProvedDoubleSpendFixture = async ({
  successorCount = 0,
}: {
  readonly successorCount?: number;
} = {}): Promise<ProvedDoubleSpendFixture> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }

  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const transactionInclusion = await buildTransactionInclusionFixture();
  // Removal needs the state-queue, operator-directory and scheduler validators.
  // Publishing them as reference-script UTxOs is what the deployed node does and
  // is what keeps the removal transaction inside the literal 16,384-byte L1
  // envelope; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit that envelope. Published from the prover wallet before
  // the header clock is sampled so the funder's nonce UTxO survives and the
  // whole fixture timeline shifts uniformly.
  const removalReferenceScriptPublications =
    await publishRemovalReferenceScripts({
      lucid: proverLucid,
      contracts,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderPaymentCredential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (
    funderPaymentCredential === undefined ||
    funderPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  const fraudulentHeader = makeHeader(
    funderPaymentCredential.hash,
    headerStartTime,
    await countedTransactionsRoot(
      transactionInclusion.transactionsRoot,
      transactionInclusion.l2TransactionCount,
    ),
    transactionInclusion.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: fraudulentHeader,
  });
  const { headerHash } = setup;

  const successors: SuccessorBlockFixture[] = [];
  let anchorBlockUnit = setup.stateQueueBlockUnit;
  let activeOperatorNode = setup.activeOperatorNode;
  let previousHeader = fraudulentHeader;
  let previousHeaderHash = headerHash;
  for (let index = 0; index < successorCount; index += 1) {
    const successorHeader = {
      ...makeHeader(
        funderPaymentCredential.hash,
        Number(previousHeader.endTime),
        EMPTY_MERKLE_TREE_ROOT,
      ),
      prevHeaderHash: previousHeaderHash,
    };
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      contracts,
      anchorBlockUnit,
      header: successorHeader,
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    successors.push({ ...successor, header: successorHeader });
    anchorBlockUnit = successor.successorBlockUnit;
    activeOperatorNode = successor.activeOperatorNode;
    previousHeader = successorHeader;
    previousHeaderHash = successor.successorHeaderHash;
  }

  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [
      headerHash,
      ...successors.map((successor) => successor.successorHeaderHash),
    ],
  });

  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    undefined,
    undefined,
    removalReferenceScriptPublications.published,
  );
  const fraudulentBlockOutRef =
    successors[0]?.continuedAnchorOutRef ?? setup.fraudulentBlockOutRef;

  const submitInitResult = await submitInit({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudulentBlockOutRef,
    awaitConfirmation: true,
  });

  expect(submitInitResult.txHash).toHaveLength(64);
  expect(submitInitResult.fraudulentHeaderHash).toBe(headerHash);
  expect(submitInitResult.computationThreadAssetName).toBe(
    `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
  );

  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const stepDatum = Data.from(
    firstStepUtxo.datum!,
    FraudProofComputationThreadStepDatum,
  );
  const proverPaymentCredential = getAddressDetails(
    await proverLucid.wallet().address(),
  ).paymentCredential;
  expect(proverPaymentCredential?.type).toBe("Key");
  const proverPaymentKeyHash = proverPaymentCredential!.hash;
  expect(stepDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: null,
  });
  expect(firstStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);
  expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
    [submitInitResult.computationThreadUnit, 1n],
  ]);

  const step01Result = await submitStep01({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(
      transactionInclusion.tx1.inclusion,
    ),
    awaitConfirmation: true,
  });

  expect(step01Result.txHash).toHaveLength(64);
  expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
  const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFirstStepUtxos).toHaveLength(0);
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step02Datum = Data.from(secondStepUtxo.datum!, DoubleSpendStep02Datum);
  expect(step02Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_id: transactionInclusion.tx1.nativeTxId,
      verified_tx1_spend_inputs_hash:
        transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    },
  });
  expect(secondStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step02Result = await submitStep02({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(secondStepUtxo),
    stateQueueBlockOutRef: fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(
      transactionInclusion.tx2.inclusion,
    ),
    awaitConfirmation: true,
  });

  expect(step02Result.txHash).toHaveLength(64);
  expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step02Result.verifiedTx1Id).toBe(transactionInclusion.tx1.nativeTxId);
  expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
  expect(step02Result.verifiedTx1SpendInputsHash).toBe(
    transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
  );
  expect(step02Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingSecondStepUtxos).toHaveLength(0);
  const thirdStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step02Result.thirdStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
  expect(step03Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_spend_inputs_hash:
        transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
      verified_tx2_spend_inputs_hash:
        transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    },
  });
  expect(thirdStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);

  const step03Result = await submitStep03({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(thirdStepUtxo),
    tx1SpendInputCbors: parseSpendInputCbors(
      transactionInclusion.tx1SpendInputCbors,
      "--tx1-inputs",
    ),
    doubleSpentInputIndex: 1n,
    awaitConfirmation: true,
  });

  expect(step03Result.txHash).toHaveLength(64);
  expect(step03Result.verifiedTx1SpendInputsHash).toBe(
    transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
  );
  expect(step03Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  expect(step03Result.doubleSpentInputIndex).toBe(1);
  expect(step03Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
  );
  expect(step03Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx1SpendInputCbors[1],
  );
  expect(step03Result.tx1SpendInputsWitnessCreated).toBe(true);
  expect(step03Result.tx1SpendInputsWitnessOutRef).toMatch(
    /^[0-9a-f]{64}#\d+$/,
  );
  expect(step03Result.tx1SpendInputsRefInputIndex).toBe(0);
  const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
    step02Result.thirdStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingThirdStepUtxos).toHaveLength(0);
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step04Datum = Data.from(fourthStepUtxo.datum!, DoubleSpendStep04Datum);
  expect(step04Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx2_spend_inputs_hash:
        transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
      double_spent_input: midgardTxInput(
        transactionInclusion.tx1InputsPreimage[1]!,
      ),
    },
  });
  expect(fourthStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step04Result = await submitStep04({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(fourthStepUtxo),
    tx2SpendInputCbors: parseSpendInputCbors(
      transactionInclusion.tx2SpendInputCbors,
      "--tx2-inputs",
    ),
    doubleSpentInputIndex: 1n,
    awaitConfirmation: true,
  });

  expect(step04Result.txHash).toHaveLength(64);
  expect(step04Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  expect(step04Result.doubleSpentInputIndex).toBe(1);
  expect(step04Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
  );
  expect(step04Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx2SpendInputCbors[1],
  );
  expect(step04Result.tx2SpendInputsWitnessCreated).toBe(true);
  expect(step04Result.tx2SpendInputsWitnessOutRef).toMatch(
    /^[0-9a-f]{64}#\d+$/,
  );
  expect(step04Result.tx2SpendInputsRefInputIndex).toBe(0);
  expect(step04Result.fraudProofAssetName).toBe(
    submitInitResult.computationThreadAssetName,
  );
  expect(step04Result.fraudProofUnit).toBe(
    toUnit(
      contracts.fraudProof.policyId,
      submitInitResult.computationThreadAssetName,
    ),
  );
  expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
    step04Result.computationThreadMintRedeemerIndex,
  );

  const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFourthStepUtxos).toHaveLength(0);
  const fraudProofUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step04Result.fraudProofAddress,
    step04Result.fraudProofUnit,
  );
  const fraudProofDatum = Data.from(
    fraudProofUtxo.datum!,
    FraudProofTokenDatum,
  );
  expect(fraudProofDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
  });
  expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
  expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
    [step04Result.fraudProofUnit, 1n],
  ]);

  return {
    emulator,
    realBlueprint,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
    transactionInclusion,
    fraudulentHeader,
    headerHash,
    setup,
    successors,
    deploymentInfo,
    fraudulentBlockOutRef,
    submitInitResult,
    step04Result,
    fraudProofUtxo,
    proverPaymentKeyHash,
  };
};

const submitRemovalForFixture = async (
  fixture: ProvedDoubleSpendFixture,
  options: {
    readonly lucid?: Awaited<ReturnType<typeof Lucid>>;
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  } = {},
) => {
  const removeNow = BigInt(fixture.emulator.now());
  return await submitRemoveFraudulentBlock({
    lucid: options.lucid ?? fixture.proverLucid,
    blueprint: fixture.realBlueprint,
    deploymentInfo: fixture.deploymentInfo,
    network,
    signer: fixture.proverSigner,
    fraudulentHeaderHash: fixture.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
    validTo: removeNow + 300_000n,
    ...(options.stateQueueMutationLeaseCoordinator === undefined
      ? {}
      : {
          stateQueueMutationLeaseCoordinator:
            options.stateQueueMutationLeaseCoordinator,
        }),
  });
};

const expectRemovedFraudProofState = async (
  fixture: ProvedDoubleSpendFixture,
) => {
  await expectStateQueueHeaderOrder({
    lucid: fixture.funderLucid,
    contracts: fixture.contracts,
    expectedHeaderHashes: [],
  });
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      fixture.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  for (const successor of fixture.successors) {
    await expect(
      fixture.funderLucid.utxosAtWithUnit(
        fixture.contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.activeOperators.spendingScriptAddress,
      fixture.setup.activeOperatorNodeUnit,
    ),
  ).resolves.toHaveLength(0);
  const [finalSchedulerUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.scheduler.spendingScriptAddress,
    toUnit(fixture.contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  );
  if (finalSchedulerUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the scheduler");
  }
  expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
    "NoActiveOperators",
  );
  const [finalRootUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.stateQueue.spendingScriptAddress,
    fixture.setup.stateQueueRootUnit,
  );
  if (finalRootUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the state-queue root");
  }
  const finalRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(finalRootUtxo, fixture.contracts.stateQueue.policyId),
  );
  expect(finalRoot.datum.next).toBe("Empty");
  const retainedFraudProof = await expectSingleUtxoWithUnit(
    fixture.proverLucid,
    fixture.step04Result.fraudProofAddress,
    fixture.step04Result.fraudProofUnit,
  );
  expect(outRefLabel(retainedFraudProof)).toBe(
    outRefLabel(fixture.fraudProofUtxo),
  );
  expect(retainedFraudProof.assets[fixture.step04Result.fraudProofUnit]).toBe(
    1n,
  );
};

describe("fault-proof emulator integration", () => {
  it("publishes every authenticated validation-dispute control under the exact L1 envelope", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const publisher = generateEmulatorAccount({
      lovelace: 40_000_000_000n,
    });
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected publisher wallet to expose a nonce UTxO");
    }
    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        realValidationTraceDispute: true,
        alwaysFraudProofCatalogue: true,
      },
    );
    const targets = validationDisputeControlPublicationTargets(contracts);
    const authPolicy = createReferenceScriptAuthPolicy(lucid, emulator.now());
    const measurements = {} as Record<
      ValidationDisputeControlPublicationTarget["control"],
      CompleteSignedTransactionMeasurement
    >;

    for (const target of targets) {
      const publication = await runEmulatorLifecycleStage(
        `reference-script.publish-authenticated.${target.control}`,
        () =>
          publishAuthenticatedValidationDisputeControl({
            lucid,
            target,
            authPolicy,
          }),
      );
      measurements[target.control] = publication.publicationMeasurement;
    }

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { validationDisputeControlPublications: measurements },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }

    expect(Object.keys(measurements)).toHaveLength(targets.length);
    for (const measurement of Object.values(measurements)) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExMem,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExSteps,
      );
      expect(measurement.inputCount).toBe(1);
      expect(measurement.referenceInputCount).toBe(0);
      expect(measurement.outputCount).toBe(3);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.nativeScriptCount).toBe(1);
      expect(measurement.redeemerCount).toBe(0);
      expect(measurement.datumCount).toBe(0);
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
    }
  }, 300_000);

  it("proves and removes a non-tail double-spend block by pruning successors first", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const transactionInclusion = await buildTransactionInclusionFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const funderAddress = await funderLucid.wallet().address();
    const funderPaymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      contracts,
      anchorBlockUnit: setup.stateQueueBlockUnit,
      header: {
        ...makeHeader(
          funderPaymentCredential.hash,
          Number(fraudulentHeader.endTime),
          EMPTY_MERKLE_TREE_ROOT,
        ),
        prevHeaderHash: headerHash,
      },
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode: setup.activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash, successor.successorHeaderHash],
    });
    const fraudulentBlockOutRef = successor.continuedAnchorOutRef;
    const deploymentEntry = (
      scriptHash: string,
      script: Script,
      referenceName?: RemovalReferenceScriptName,
    ) => {
      const published =
        referenceName === undefined
          ? undefined
          : removalReferenceScriptPublications.published[referenceName];
      return {
        scriptHash,
        refScriptUTxO:
          published === undefined
            ? null
            : {
                txHash: published.txHash,
                outputIndex: published.outputIndex,
              },
        contract: {
          type: script.type,
          cborHex: script.script,
        },
      };
    };
    const deploymentInfo = deploymentManifest({
      hubOracleMint: { scriptHash: contracts.hubOracle.policyId },
      fraudProofCatalogueMint: {
        scriptHash: contracts.fraudProofCatalogue.policyId,
        fraudProofCatalogue: catalogue,
      },
      fraudProofCatalogueSpend: {
        scriptHash: contracts.fraudProofCatalogue.spendingScriptHash,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofSpend: {
        scriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofDoubleSpend: {
        scriptHash: contracts.fraudProofs.doubleSpend.spendingScriptHash,
      },
      stateQueueMint: deploymentEntry(
        contracts.stateQueue.policyId,
        contracts.stateQueue.mintingScript,
        "stateQueueMint",
      ),
      stateQueueSpend: deploymentEntry(
        contracts.stateQueue.spendingScriptHash,
        contracts.stateQueue.spendingScript,
        "stateQueueSpend",
      ),
      retiredOperatorsMint: deploymentEntry(
        contracts.retiredOperators.policyId,
        contracts.retiredOperators.mintingScript,
        "retiredOperatorsMint",
      ),
      retiredOperatorsSpend: deploymentEntry(
        contracts.retiredOperators.spendingScriptHash,
        contracts.retiredOperators.spendingScript,
        "retiredOperatorsSpend",
      ),
      registeredOperatorsMint: {
        scriptHash: contracts.registeredOperators.policyId,
      },
      registeredOperatorsSpend: deploymentEntry(
        contracts.registeredOperators.spendingScriptHash,
        contracts.registeredOperators.spendingScript,
      ),
      activeOperatorsMint: deploymentEntry(
        contracts.activeOperators.policyId,
        contracts.activeOperators.mintingScript,
        "activeOperatorsMint",
      ),
      activeOperatorsSpend: deploymentEntry(
        contracts.activeOperators.spendingScriptHash,
        contracts.activeOperators.spendingScript,
        "activeOperatorsSpend",
      ),
      schedulerMint: { scriptHash: contracts.scheduler.policyId },
      schedulerSpend: deploymentEntry(
        contracts.scheduler.spendingScriptHash,
        contracts.scheduler.spendingScript,
        "schedulerSpend",
      ),
      settlementMint: { scriptHash: contracts.settlement.policyId },
    });

    const result = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(result.txHash).toHaveLength(64);
    expect(result.fraudulentHeaderHash).toBe(headerHash);
    expect(result.computationThreadAssetName).toBe(
      `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    const stepDatum = Data.from(
      firstStepUtxo.datum!,
      FraudProofComputationThreadStepDatum,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    expect(stepDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: null,
    });
    expect(firstStepUtxo.assets[result.computationThreadUnit]).toBe(1n);
    expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
      [result.computationThreadUnit, 1n],
    ]);

    const step01Result = await submitStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        transactionInclusion.tx1.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
    const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFirstStepUtxos).toHaveLength(0);
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      DoubleSpendStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_id: transactionInclusion.tx1.nativeTxId,
        verified_tx1_spend_inputs_hash:
          transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
      },
    });
    expect(secondStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step02Result = await submitStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        transactionInclusion.tx2.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.verifiedTx1Id).toBe(
      transactionInclusion.tx1.nativeTxId,
    );
    expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
    expect(step02Result.verifiedTx1SpendInputsHash).toBe(
      transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    );
    expect(step02Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingSecondStepUtxos).toHaveLength(0);
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
    expect(step03Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_spend_inputs_hash:
          transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
        verified_tx2_spend_inputs_hash:
          transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
      },
    });
    expect(thirdStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step03Result = await submitStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      tx1SpendInputCbors: parseSpendInputCbors(
        transactionInclusion.tx1SpendInputCbors,
        "--tx1-inputs",
      ),
      doubleSpentInputIndex: 1n,
      awaitConfirmation: true,
    });

    expect(step03Result.txHash).toHaveLength(64);
    expect(step03Result.verifiedTx1SpendInputsHash).toBe(
      transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    );
    expect(step03Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    expect(step03Result.doubleSpentInputIndex).toBe(1);
    expect(step03Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
    );
    expect(step03Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx1SpendInputCbors[1],
    );
    expect(step03Result.tx1SpendInputsWitnessCreated).toBe(true);
    expect(step03Result.tx1SpendInputsWitnessOutRef).toMatch(
      /^[0-9a-f]{64}#\d+$/,
    );
    expect(step03Result.tx1SpendInputsRefInputIndex).toBe(0);
    const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingThirdStepUtxos).toHaveLength(0);
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    const step04Datum = Data.from(
      fourthStepUtxo.datum!,
      DoubleSpendStep04Datum,
    );
    expect(step04Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx2_spend_inputs_hash:
          transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
        double_spent_input: midgardTxInput(
          transactionInclusion.tx1InputsPreimage[1]!,
        ),
      },
    });
    expect(fourthStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step04Result = await submitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      tx2SpendInputCbors: parseSpendInputCbors(
        transactionInclusion.tx2SpendInputCbors,
        "--tx2-inputs",
      ),
      doubleSpentInputIndex: 1n,
      awaitConfirmation: true,
    });

    expect(step04Result.txHash).toHaveLength(64);
    expect(step04Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    expect(step04Result.doubleSpentInputIndex).toBe(1);
    expect(step04Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
    );
    expect(step04Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx2SpendInputCbors[1],
    );
    expect(step04Result.tx2SpendInputsWitnessCreated).toBe(true);
    expect(step04Result.tx2SpendInputsWitnessOutRef).toMatch(
      /^[0-9a-f]{64}#\d+$/,
    );
    expect(step04Result.tx2SpendInputsRefInputIndex).toBe(0);
    expect(step04Result.fraudProofAssetName).toBe(
      result.computationThreadAssetName,
    );
    expect(step04Result.fraudProofUnit).toBe(
      toUnit(contracts.fraudProof.policyId, result.computationThreadAssetName),
    );
    expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
      step04Result.computationThreadMintRedeemerIndex,
    );

    const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFourthStepUtxos).toHaveLength(0);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    const fraudProofDatum = Data.from(
      fraudProofUtxo.datum!,
      FraudProofTokenDatum,
    );
    expect(fraudProofDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
    });
    expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
    expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
      [step04Result.fraudProofUnit, 1n],
    ]);

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "emulator-fault-proof-removal",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    expect(removeResult.fraudulentHeaderHash).toBe(headerHash);
    expect(removeResult.fraudProver).toBe(proverPaymentCredential!.hash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [successor.successorHeaderHash, headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Remove transaction did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const [finalRootUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error(
        "Remove transaction did not preserve the state-queue root",
      );
    }
    const finalRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(finalRootUtxo, contracts.stateQueue.policyId),
    );
    expect(finalRoot.datum.next).toBe("Empty");
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("proves and removes a tail invalid-range block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realInvalidRange: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const invalidRangeInclusion =
      await buildInvalidRangeTransactionInclusionFixture({
        blockValidFrom: BigInt(headerStartTime),
        blockValidTo: BigInt(headerStartTime + 1_000),
      });
    expect(invalidRangeInclusion.violationReason).toBe("lower-before-block");

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        invalidRangeInclusion.transactionsRoot,
        invalidRangeInclusion.l2TransactionCount,
      ),
      invalidRangeInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidRange",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("invalidRange");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.invalidRange.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01Result = await submitInvalidRangeStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        invalidRangeInclusion.badTx.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(
      invalidRangeInclusion.badTx.nativeTxId,
    );
    expect(step01Result.blockValidFrom).toBe(fraudulentHeader.startTime);
    expect(step01Result.blockValidTo).toBe(fraudulentHeader.endTime);
    expect(step01Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    expect(step01Result.violationReason).toBe("lower-before-block");
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      InvalidRangeStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        block_valid_from: fraudulentHeader.startTime,
        block_valid_to: fraudulentHeader.endTime,
        bad_tx_normalized_validity_range:
          invalidRangeInclusion.normalizedValidityRange,
      },
    });

    const step02Result = await submitInvalidRangeStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(step02Result.violationReason).toBe("lower-before-block");
    expect(step02Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidRange",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("invalidRange");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("proves and removes a tail zero-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const zeroInputInclusion =
      await buildZeroInputTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        zeroInputInclusion.transactionsRoot,
        zeroInputInclusion.l2TransactionCount,
      ),
      zeroInputInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("zeroInput");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.zeroInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01Result = await submitZeroInputStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        zeroInputInclusion.badTx.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(zeroInputInclusion.badTx.nativeTxId);
    expect(step01Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(secondStepUtxo.datum!, ZeroInputStep02Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: { bad_tx_spend_inputs_hash: EMPTY_SPEND_INPUTS_HASH },
    });

    const step02Result = await submitZeroInputStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("zeroInput");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("rejects a spending transaction before a zero-input thread can advance", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const transactionInclusion = await buildTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    await expect(
      submitZeroInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(
          transactionInclusion.tx1.inclusion,
        ),
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      "--tx-inclusion.nativeTx spends at least one input, so it does not violate the zero-input ledger rule.",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);

  it("proves and removes a tail non-existent-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    await registerPexcludesExclusionRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realNonExistentInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const fixture = await buildNonExistentInputFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
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
      header: fraudulentHeader,
    });
    const { headerHash } = setup;

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    expect(initResult.fraudCategoryName).toBe("nonExistentInput");
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.nonExistentInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Result = await neSubmitStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.inclusion,
      awaitConfirmation: true,
    });
    expect(step01Result.nativeTxId).toBe(fixture.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Result = await neSubmitStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: fixture.inputsPreimage,
      badInputIndex: fixture.badInputIndex,
      awaitConfirmation: true,
    });
    expect(step02Result.missingInput.tx_id).toBe(fixture.missingInputTxId);

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await neSubmitStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
      awaitConfirmation: true,
    });
    expect(step03Result.missingInputTxId).toBe(fixture.missingInputTxId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04Result = await neSubmitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      txsNonMembershipProofCbor: fixture.txsNonMembershipProofCbor,
      awaitConfirmation: true,
    });
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const proverPaymentKeyHash = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential!.hash;
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInput",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removeResult.fraudCategory).toBe("nonExistentInput");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("submits and removes a tail transition-trace fraud proof end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realTransitionTrace: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const traceFixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: funderPaymentCredential.hash,
      now: headerStartTime,
    });
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: traceFixture.header,
    });
    expect(setup.headerHash).toBe(traceFixture.headerHash);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [traceFixture.headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(initResult.fraudCategoryName).toBe("transitionTrace");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.transitionTrace.categoryId}${traceFixture.headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const proofResult = await submitTransitionTraceProof({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      proof: traceFixture.proof,
      awaitConfirmation: true,
    });

    expect(proofResult.txHash).toHaveLength(64);
    expect(proofResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(proofResult.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(proofResult.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(proofResult.fraudProofMintRedeemerIndex).not.toBe(
      proofResult.computationThreadMintRedeemerIndex,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentHeaderHash: traceFixture.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("transitionTrace");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [traceFixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[proofResult.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it.each([
    {
      name: "direct",
      inlineDatumPayloadBytes: 7_976,
      minimumCompleteItemBytes: 0,
      expectedCarriage: "direct" as const,
    },
    {
      name: "reference",
      inlineDatumPayloadBytes: 13_600,
      minimumCompleteItemBytes:
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
      expectedCarriage: "reference" as const,
    },
  ])(
    "opens, bisects, resolves a fitting complete item by $name, and awards a validation dispute",
    async ({
      inlineDatumPayloadBytes,
      minimumCompleteItemBytes,
      expectedCarriage,
    }) => {
      const realBlueprint = readBlueprint(realBlueprintPath);
      const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
      const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
      const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
      const feeUtxoCount = 12;
      const feeUtxoLovelace = 100_000_000n;
      const emulator = new Emulator(
        [
          {
            ...operator,
            assets: {
              lovelace:
                operator.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
            },
          },
          ...Array.from({ length: feeUtxoCount }, () => ({
            ...operator,
            assets: { lovelace: feeUtxoLovelace },
          })),
          {
            ...challenger,
            assets: {
              lovelace:
                challenger.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
            },
          },
          ...Array.from({ length: feeUtxoCount }, () => ({
            ...challenger,
            assets: { lovelace: feeUtxoLovelace },
          })),
        ],
        EMULATOR_PROTOCOL_PARAMETERS,
      );
      const operatorLucid = await Lucid(emulator, "Custom");
      const challengerLucid = await Lucid(emulator, "Custom");
      operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
      challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
      const operatorSigner = resolveProverSigner({
        network,
        walletSeedPhrase: operator.seedPhrase,
      });
      const challengerSigner = resolveProverSigner({
        network,
        walletSeedPhrase: challenger.seedPhrase,
      });
      const validityRange = () =>
        validationDisputeValidityRange(emulator.now());

      await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
      const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
      if (nonceUtxo === undefined) {
        throw new Error("Expected operator wallet to expose a nonce UTxO");
      }
      const contracts = await buildMinimalFaultProofContracts(
        realBlueprint,
        alwaysBlueprint,
        nonceUtxo,
        {
          realValidationTraceDispute: true,
          alwaysFraudProofCatalogue: true,
        },
      );
      // Re-derive the applied canonical-decode item-semantic validator (the
      // same deterministic build the submit path performs) so its reference
      // script can be published and its body pinned as absent from the proof
      // transactions.
      const validationDisputeSdkContracts = await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
        }),
      );
      const itemSemanticContract =
        validationDisputeSdkContracts.validationTraceDispute
          .semanticResolvers[1];
      const catalogue = await buildCatalogueDeploymentInfo(
        contracts.fraudProofs,
      );
      const operatorPaymentCredential = getAddressDetails(
        await operatorLucid.wallet().address(),
      ).paymentCredential;
      if (
        operatorPaymentCredential === undefined ||
        operatorPaymentCredential.type !== "Key"
      ) {
        throw new Error(
          "Expected operator wallet to expose a payment key hash",
        );
      }
      const headerStartTime =
        alignUnixTimeToEmulatorSlotBoundary(
          operatorLucid,
          emulator.now() + 120_000,
        ) - 1;
      const fixture = await buildInvalidForcedValidationDisputeFixture({
        operatorVkey: operatorPaymentCredential.hash,
        now: headerStartTime,
        inlineDatumPayloadBytes,
        minimumCompleteItemBytes,
      });
      const setup = await runEmulatorLifecycleStage("setup", () =>
        submitSetupTx({
          lucid: operatorLucid,
          contracts,
          nonceUtxo,
          catalogue,
          header: fixture.header,
        }),
      );
      const publicationSlotConfig = operatorLucid.config().slotConfig;
      if (publicationSlotConfig === undefined) {
        throw new Error(
          "Expected reference-script publisher Lucid to expose its Custom slot config",
        );
      }
      const setupProtocolParameters = emulator.protocolParameters;
      emulator.protocolParameters = {
        ...setupProtocolParameters,
        maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      };
      const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
        slotConfig: publicationSlotConfig,
      });
      referenceScriptPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
      const validationDisputePublication = await runEmulatorLifecycleStage(
        "reference-script.publish-authenticated",
        async () => {
          try {
            return await publishValidationDisputeReferenceScript({
              lucid: referenceScriptPublisherLucid,
              contracts,
              now: emulator.now(),
            });
          } finally {
            emulator.protocolParameters = setupProtocolParameters;
          }
        },
      );
      const itemSemanticPublication = await runEmulatorLifecycleStage(
        "reference-script.publish-item-semantic",
        async () => {
          emulator.protocolParameters = {
            ...setupProtocolParameters,
            maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
          };
          try {
            return await publishPlainReferenceScriptUtxo({
              lucid: referenceScriptPublisherLucid,
              script: itemSemanticContract.spendingScript,
              label: "validation item-semantic",
            });
          } finally {
            emulator.protocolParameters = setupProtocolParameters;
          }
        },
      );
      const deploymentInfo = buildRemovalDeploymentInfo(
        contracts,
        catalogue,
        validationDisputePublication,
        {
          scriptHash: itemSemanticContract.spendingScriptHash,
          utxo: itemSemanticPublication.utxo,
        },
      );
      const initResult = await runEmulatorLifecycleStage("init", () =>
        submitInit({
          lucid: challengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: challengerSigner,
          fraudCategory: "validationTraceDispute",
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          awaitConfirmation: true,
        }),
      );
      const functionalProtocolParameters = emulator.protocolParameters;
      const functionalSlotConfig = challengerLucid.config().slotConfig;
      if (functionalSlotConfig === undefined) {
        throw new Error(
          "Expected functional emulator Lucid to expose its Custom slot config",
        );
      }
      emulator.protocolParameters = {
        ...functionalProtocolParameters,
        maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      };
      const targetOperatorLucid = await Lucid(emulator, "Custom", {
        slotConfig: functionalSlotConfig,
      });
      const targetChallengerLucid = await Lucid(emulator, "Custom", {
        slotConfig: functionalSlotConfig,
      });
      targetOperatorLucid.selectWallet.fromSeed(operator.seedPhrase);
      targetChallengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
      const firstStepUtxo = await expectSingleUtxoWithUnit(
        targetChallengerLucid,
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      );
      const openSubmission = await runEmulatorLifecycleStage("open", () =>
        captureEmulatorSubmission(emulator, () =>
          submitValidationDisputeOpen({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: outRefLabel(firstStepUtxo),
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            claim: fixture.claim,
            challengerDescriptor: fixture.challengerDescriptor,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
        ),
      );
      const openResult = openSubmission.result;
      const publicationMeasurement = openSubmission.measurement;
      const sourceResult = await runEmulatorLifecycleStage("source", () =>
        submitValidationDisputeVerifySource({
          lucid: targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: challengerSigner,
          threadOutRef: openResult.nextThreadOutRef,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
      );

      let threadOutRef = sourceResult.nextThreadOutRef;
      for (const move of fixture.evidence.moves) {
        const revealResult = await runEmulatorLifecycleStage(
          `reveal.${move.role}`,
          () =>
            submitValidationDisputeReveal({
              lucid:
                move.role === "operator"
                  ? targetOperatorLucid
                  : targetChallengerLucid,
              blueprint: realBlueprint,
              deploymentInfo,
              network,
              signer:
                move.role === "operator" ? operatorSigner : challengerSigner,
              threadOutRef,
              role: move.role,
              proof: move.proof,
              validityRange: validityRange(),
              awaitConfirmation: true,
            }),
        );
        threadOutRef = revealResult.nextThreadOutRef;
      }

      const resolutionResult = await runEmulatorLifecycleStage(
        "enter-resolution",
        () =>
          submitValidationDisputeEnterResolution({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
      );
      const { lowIndex, highIndex } = fixture.evidence.finalDispute;
      const prepareResult = await runEmulatorLifecycleStage(
        "prepare-resolution",
        () =>
          submitValidationDisputePrepareResolution({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: resolutionResult.nextThreadOutRef,
            preState: validationMachineStateDataFromCore(
              fixture.operatorTrace.states[lowIndex]!,
            ),
            operatorPost: validationTraceProofDataFromCore(
              fixture.operatorTrace.tree.proofs[highIndex]!,
            ),
            challengerPost: validationTraceProofDataFromCore(
              fixture.challengerTrace.tree.proofs[highIndex]!,
            ),
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
      );
      const selectedResult = await runEmulatorLifecycleStage(
        "prepare-selected",
        () =>
          submitValidationDisputePrepareSelected({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: prepareResult.nextThreadOutRef,
            oneStepArgument: fixture.evidence.oneStepArgument,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
      );
      const semanticSubmission = await runEmulatorLifecycleStage(
        "semantic-resolution",
        () =>
          captureEmulatorSubmission(emulator, () =>
            submitValidationDisputeSemanticResolution({
              lucid: targetChallengerLucid,
              blueprint: realBlueprint,
              deploymentInfo,
              network,
              signer: challengerSigner,
              threadOutRef: selectedResult.nextThreadOutRef,
              oneStepArgument: fixture.evidence.oneStepArgument,
              validityRange: validityRange(),
              awaitConfirmation: true,
            }),
          ),
      );
      const semanticResult = semanticSubmission.result;
      const awardSubmission = await runEmulatorLifecycleStage("award", () =>
        captureEmulatorSubmission(emulator, () =>
          submitValidationDisputeAward({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: semanticResult.nextThreadOutRef,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
        ),
      );
      const awardResult = awardSubmission.result;
      const proofTransactionMeasurements = {
        referenceScriptPublication:
          validationDisputePublication.publicationMeasurement,
        publication: publicationMeasurement,
        resolution: semanticSubmission.measurement,
        resolutionTransactions: semanticSubmission.measurements,
        award: awardSubmission.measurement,
      };
      const allProofTransactionMeasurements = [
        validationDisputePublication.publicationMeasurement,
        publicationMeasurement,
        ...semanticSubmission.measurements,
        awardSubmission.measurement,
      ];
      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify(
            {
              completeItemBytes: fixture.completeItemBytes,
              transactions: proofTransactionMeasurements,
            },
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ),
        );
      }

      expect(fixture.evidence.finalDispute.turn).toEqual({
        type: "readyForOneStep",
      });
      expect(fixture.evidence.moves.length).toBeGreaterThan(0);
      expect(prepareResult.resolverIndex).toBe(
        fixture.evidence.oneStepArgument.resolverIndex,
      );
      expect(selectedResult.semanticResolverIndex).toBe(
        fixture.evidence.oneStepArgument.semanticResolverIndex,
      );
      expect(semanticResult.proofItemCarriage).toBe(expectedCarriage);
      if (expectedCarriage === "reference") {
        expect(semanticResult.proofItemReferenceOutRef).toBe(
          semanticResult.proofItemPublication?.outRef,
        );
        expect(semanticResult.proofItemPublication).toMatchObject({
          awaitedConfirmation: true,
        });
        expect(
          semanticResult.proofItemPublication?.completeSignedBytes,
        ).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxSize);
        expect(
          semanticResult.proofItemPublication?.lovelace ?? 0n,
        ).toBeGreaterThan(0n);
      } else {
        expect(semanticResult.proofItemReferenceOutRef).toBeUndefined();
        expect(semanticResult.proofItemPublication).toBeUndefined();
      }
      expect(semanticResult.stageTransactions).toHaveLength(5);
      expect(semanticSubmission.measurements).toHaveLength(
        expectedCarriage === "reference" ? 6 : 5,
      );
      // The semantic-resolution (authentication) proof transaction sources
      // the item-semantic validator from the published reference script: one
      // extra reference input beside the direct route, two beside the
      // published proof item on the reference route.
      expect(
        semanticSubmission.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual(
        expectedCarriage === "reference" ? [0, 2, 0, 1, 0, 0] : [1, 0, 0, 0, 0],
      );
      expect(
        semanticSubmission.measurements.every(
          (measurement) =>
            measurement.completeSignedBytes <=
            PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
        ),
      ).toBe(true);
      // C21-DISPUTE-SUBMIT defect 2: the representative complete-item
      // semantic-resolution transaction stays at or below the literal
      // 16,384-byte L1 envelope and does not embed the ~3.4 KiB applied
      // item-semantic validator body — no Plutus script witness at all; the
      // validator arrives via the published reference script.
      const resolutionMeasurements = semanticSubmission.measurements.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const resolutionCbors = semanticSubmission.transactionCbors.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const authenticationMeasurement = resolutionMeasurements[0]!;
      const authenticationCbor = resolutionCbors[0]!;
      expect(authenticationMeasurement.completeSignedBytes).toBeLessThanOrEqual(
        16_384,
      );
      expect(authenticationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV2ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV1ScriptCount).toBe(0);
      expect(authenticationMeasurement.nativeScriptCount).toBe(0);
      expect(itemSemanticContract.spendingScript.script.length).toBeGreaterThan(
        0,
      );
      expect(
        authenticationCbor.includes(itemSemanticContract.spendingScript.script),
      ).toBe(false);
      expect(
        semanticResult.stageTransactions?.map(
          (transaction) => transaction.completeSignedBytes,
        ),
      ).toEqual(
        semanticSubmission.measurements
          .slice(expectedCarriage === "reference" ? 1 : 0)
          .map((measurement) => measurement.completeSignedBytes),
      );
      expect(semanticResult.nextThreadOutRef).toBe(awardResult.threadOutRef);
      expect(awardResult.txHash).toHaveLength(64);
      expect(awardResult.fraudProofUnit).toBe(
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      );
      expect(publicationMeasurement.l1ByteMargin).toBeGreaterThan(0);
      expect(publicationMeasurement.referenceInputCount).toBe(3);
      expect(
        validationDisputePublication.publicationMeasurement.nativeScriptCount,
      ).toBe(1);
      expect(publicationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(semanticSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(awardSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      for (const measurement of allProofTransactionMeasurements) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExMem,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExSteps,
        );
      }
      await expect(
        targetChallengerLucid.utxosAtWithUnit(
          contracts.fraudProof.spendingScriptAddress,
          awardResult.fraudProofUnit,
        ),
      ).resolves.toHaveLength(1);
    },
    300_000,
  );

  it("coordinates non-tail removal with lease acquire, refetch, renew, and release ordering", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.fraudulentHeaderHash).toBe(fixture.headerHash);
    expect(removeResult.fraudProver).toBe(fixture.proverPaymentKeyHash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.successors[0]!.successorHeaderHash, fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndexes = eventIndexes(events, "lease.renew");
    const awaitTxIndexes = eventIndexes(events, "awaitTx");
    const releaseIndex = eventIndexes(events, "lease.release")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(3);
    expect(renewIndexes).toHaveLength(4);
    expect(awaitTxIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(renewIndexes[0]!).toBeLessThan(awaitTxIndexes[0]!);
    expect(awaitTxIndexes[0]!).toBeLessThan(renewIndexes[1]!);
    expect(renewIndexes[1]!).toBeLessThan(stateQueueLoadIndexes[2]!);
    expect(stateQueueLoadIndexes[2]!).toBeLessThan(renewIndexes[2]!);
    expect(renewIndexes[2]!).toBeLessThan(awaitTxIndexes[1]!);
    expect(awaitTxIndexes[1]!).toBeLessThan(renewIndexes[3]!);
    expect(renewIndexes[3]!).toBeLessThan(releaseIndex);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("rejects non-tail removal without a state-queue mutation lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });

    await expect(submitRemovalForFixture(fixture)).rejects.toThrow(
      "requires a live Midgard node state-queue mutation lease",
    );
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when post-acquire topology refetch fails", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failStateQueueUtxosAtCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented state-queue topology load failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(failIndex);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented state-queue topology load failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when removal preparation fails after acquisition", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failSchedulerUtxosAtWithUnitCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented scheduler lookup failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const schedulerIndexes = eventIndexes(events, "scheduler.utxosAtWithUnit");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndex = eventIndexes(events, "lease.renew")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(schedulerIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(1);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(renewIndex);
    expect(renewIndex).toBeLessThan(schedulerIndexes[1]!);
    expect(schedulerIndexes[1]!).toBeLessThan(failIndex);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented scheduler lookup failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("removes a tail double-spend block without acquiring a lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture();
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
    });

    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    expect(eventIndexes(events, "lease.acquire")).toHaveLength(0);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(1);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(1);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("removes a non-tail double-spend block with multiple successors in queue order", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 2 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [
        fixture.successors[0]!.successorHeaderHash,
        fixture.successors[1]!.successorHeaderHash,
        fixture.headerHash,
      ],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
      "OperatorAlreadySlashed",
    ]);

    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(4);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(6);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(3);
    expect(eventIndexes(events, "lease.release")).toHaveLength(1);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);

    await expectRemovedFraudProofState(fixture);
  }, 300_000);
});
