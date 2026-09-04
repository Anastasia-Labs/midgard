/**
 * Emulator journey fixtures shared by the `submit-init-emulator-*.test.ts`
 * family.
 *
 * These builders were local to `submit-init-emulator.test.ts` until that file
 * was broken up: `@lucid-evolution/uplc` through 0.2.22 leaked the wasm linear
 * memory it allocated per script evaluation, vitest isolates per FILE, and a
 * single file holding every heavy journey walked into the ~4 GiB wasm32
 * ceiling on CI. That leak is fixed upstream; lifting the fixtures here still
 * lets each journey theme run in its own worker while sharing one definition
 * of every fixture.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_ENVELOPE_MEASUREMENTS,
  type MidgardNativeTxFull,
  outRefLabel,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  buildPhasMembershipRewardRegistrationTxProgram,
  commitCountedRootProgram,
  CORRECTION_LOCK_ASSET_NAME,
  createReferenceScriptAuthPolicy,
  DA_PAYLOAD_VERSION,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_MERKLE_TREE_ROOT,
  EMPTY_SPEND_INPUTS_HASH,
  encodeDaPayload,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  type FraudProofCatalogueDeploymentInfo,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  getProtocolParameters,
  hashBlockHeader,
  Header,
  headerHashFromStateQueueUTxO,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  invalidOneStepTransitionFault,
  invalidRangeViolationReason,
  type MidgardValidators,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  OutputReference,
  outputReferenceFromUTxO,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
  ROOT_DOMAINS,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerSpendRedeemer,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  TransitionStepSchema,
  utxosToStateQueueUTxOs,
  utxoToStateQueueUTxO,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";
import {
  assetsToValue,
  type BuildTxWithRedeemer,
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildCountedRoot,
  buildInvalidForcedTransactionNoOpWitness,
  buildTransitionFaultProof,
  encodeData,
  type FraudProofPreSubmitBoundary,
  keyValuePhasRootWithCount,
  reconstructDaPayload,
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
  workflowTransactionReferenceInputOutRefs,
} from "../../src/index.js";
import {
  buildNonMembershipProof,
  type TrieEntry,
} from "../../src/ne-proofs.js";
import { findStateQueueYieldReferenceScript } from "./emulator/reference-scripts.js";
import {
  nativeTxFromCoreCompact,
  type NeInputPreimageEntry,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "./legacy-submit-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  buildCatalogueDeploymentInfo,
  buildForcedValidationDisputeCommitments,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  firstWalletUtxo,
  fundedProverEmulatorAccount,
  funderPaymentKeyHash,
  getCompiledScript,
  h32,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeHeader,
  makeNativeTx,
  network,
  publishFaultProofWitnessReferenceScripts,
  publishFraudProofChainReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const positiveNonAdaAssets = (utxo: UTxO) =>
  Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );

export const expectStateQueueHeaderOrder = async ({
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

export type TestOutputReference = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

export type TransactionInclusionEntry = {
  readonly inclusion: unknown;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxId: string;
  readonly spendInputCbors: readonly string[];
};

export const tx1InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("a1"), outputIndex: 0n },
  { transactionId: h32("a2"), outputIndex: 1n },
];

export const tx2InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("b1"), outputIndex: 0n },
  tx1InputsPreimage[1]!,
];

/**
 * Distinct filler spend inputs, used to drive the spend-input preimage
 * cardinality axis (finding Q1X-F6, issue #549). A filler is never the input a
 * family selects — it is what the step's authenticated preimage must
 * nevertheless re-hash, item by item, before it can select anything.
 */
export const spendInputFiller = (
  domain: number,
  index: number,
): TestOutputReference => {
  const transactionId = Buffer.alloc(32, 0x00);
  transactionId.writeUInt32BE(domain >>> 0, 0);
  transactionId.writeUInt32BE(index >>> 0, 4);
  return { transactionId: transactionId.toString("hex"), outputIndex: 0n };
};

/**
 * `selected` at the LAST position of a `cardinality`-long preimage, which is
 * the worst case for both costs this axis has: the whole collection is
 * re-hashed either way, and the selection walk is longest at the last index.
 */
export const spendInputsOfCardinality = ({
  selected,
  cardinality,
  domain,
}: {
  readonly selected: TestOutputReference;
  readonly cardinality: number;
  readonly domain: number;
}): readonly TestOutputReference[] => {
  if (!Number.isInteger(cardinality) || cardinality < 1) {
    throw new Error(
      `Spend-input cardinality must be a positive integer, got ${String(cardinality)}.`,
    );
  }
  return [
    ...Array.from({ length: cardinality - 1 }, (_unused, index) =>
      spendInputFiller(domain, index),
    ),
    selected,
  ];
};

export const outputReferenceCbor = (outRef: TestOutputReference): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });

export const largeFittingOutputCbor = (
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

export const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

export const compactTxEntry = (
  nativeTx: MidgardNativeTxFull,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxId(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

export const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));

// ---------------------------------------------------------------------------
// Adversarial MPF membership-proof depth (GOAL_SPEC.md 9.1 output 5, axis 1)
// ---------------------------------------------------------------------------
//
// Every family's step-01 redeemer carries `tx_membership_proof`, and that proof
// is the only part of a fault proof whose size an adversary controls by
// choosing what the challenged block contains. The proof serializes one CBOR
// step per level at which the trie branches along the challenged
// transaction's hashed path, so the lever is not "more transactions" — a
// larger random block only grows the path logarithmically — but
// "transactions whose hashed paths branch at consecutive nibbles of the
// challenged transaction's own path".
//
// Two siblings per level force the on-path node to hold at least three
// children, which is what makes each step serialize as the largest shape the
// MPF proof encoding has (`branch`, carrying four 32-byte neighbour hashes)
// rather than the smaller `fork` or `leaf`. That is the worst case, not a
// typical one.
//
// Grinding is deterministic: candidate keys are a counter written into a fixed
// buffer, so the same fixture is reproduced byte for byte on any machine, in
// the same spirit as the deterministic emulator wallets.
//
// Forcing a branch at level `i` costs ~16^i digest evaluations, so this axis is
// bounded by adversary WORK, not by protocol structure. See
// `membershipProofBranchLevelsReachableWithWork`.
export const ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS = 5;

// Marginal CBOR cost of one additional `branch` proof step. Measured, and
// re-measured by `tests/max-proof-fit-membership-depth.test.ts` rather than
// assumed: the MPF proof encoding is a definite list of fixed-shape steps, so
// the cost per level is exactly constant.
export const MPF_BRANCH_PROOF_STEP_CBOR_BYTES = 139;

// Marginal cost of one additional forced branch level in the COMPLETE SIGNED
// step-01 transaction, which is what the L1 envelope actually measures. It is
// not the MPF CBOR figure above: the proof reaches the chain as Plutus data in
// the step redeemer, and that representation is roughly twice the size of the
// library's own compact CBOR. Measured end to end by
// `submit-init-emulator-max-proof-fit.test.ts` at two real depths, not derived.
export const PROOF_TRANSACTION_BRANCH_LEVEL_BYTES = 276;

const MPF_PATH_NIBBLES = 64;

const mpfPathDigest = (key: Buffer): Buffer =>
  Buffer.from(computeHash32(Uint8Array.from(key)));

const sharedNibbleCount = (left: Buffer, right: Buffer): number => {
  const a = left.toString("hex");
  const b = right.toString("hex");
  let shared = 0;
  while (shared < a.length && a[shared] === b[shared]) {
    shared += 1;
  }
  return shared;
};

/**
 * Deterministically grind trie keys whose hashed path diverges from
 * `targetKey`'s hashed path at exactly level 0, 1, ... `branchLevels - 1`, two
 * per level. `domain` separates one family's grind from another's so two
 * fixtures in the same block cannot reuse each other's keys.
 */
export const adversarialMembershipSiblingKeys = ({
  targetKey,
  branchLevels,
  domain,
}: {
  readonly targetKey: Buffer;
  readonly branchLevels: number;
  readonly domain: number;
}): readonly Buffer[] => {
  if (branchLevels < 0 || branchLevels > MPF_PATH_NIBBLES) {
    throw new Error(
      `Adversarial branch level count ${branchLevels.toString()} is outside the 0..${MPF_PATH_NIBBLES.toString()} nibbles of an MPF path.`,
    );
  }
  const targetPath = mpfPathDigest(targetKey);
  const candidate = Buffer.alloc(32, 0x00);
  candidate.writeUInt32BE(domain >>> 0, 8);
  const keys: Buffer[] = [];
  let counter = 0;
  for (let level = 0; level < branchLevels; level += 1) {
    // Two siblings that diverge at this level are not enough on their own: if
    // they diverge into the SAME nibble slot the on-path node still holds only
    // two children and serializes as the cheaper `fork`. Requiring distinct
    // divergence nibbles makes every on-path node a three-child node, which is
    // what forces the largest `branch` step shape at every level.
    const takenNibbles = new Set<string>();
    while (takenNibbles.size < 2) {
      candidate.writeUInt32BE(counter >>> 0, 0);
      counter += 1;
      const path = mpfPathDigest(candidate);
      if (sharedNibbleCount(path, targetPath) !== level) {
        continue;
      }
      const nibble = path.toString("hex")[level]!;
      if (takenNibbles.has(nibble)) {
        continue;
      }
      takenNibbles.add(nibble);
      keys.push(Buffer.from(candidate));
    }
  }
  return keys;
};

/** Every grinded sibling carries the same one-byte filler value. */
export const ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE = Buffer.from("ad", "hex");

/**
 * The deepest branch level at which the L1 envelope still admits the proof,
 * derived from a MEASURED transaction at a measured branch depth plus the
 * measured constant marginal cost of one further level.
 */
export const membershipProofBranchLevelByteCeiling = ({
  measuredTransactionBytes,
  measuredBranchLevels,
  l1MaxTxSize,
}: {
  readonly measuredTransactionBytes: number;
  readonly measuredBranchLevels: number;
  readonly l1MaxTxSize: number;
}): number =>
  measuredBranchLevels +
  Math.floor(
    (l1MaxTxSize - measuredTransactionBytes) /
      PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  );

/**
 * The deepest branch level reachable by an adversary willing to spend `2^n`
 * digest evaluations. Forcing a branch at level `i` means finding a key whose
 * hashed path agrees with the challenged transaction's path on `i` chosen
 * nibbles, which is a fixed-target search costing ~16^i = 2^(4i).
 */
export const membershipProofBranchLevelsReachableWithWork = (
  log2Work: number,
): number => Math.floor(log2Work / 4);

export type MembershipProofShape = {
  readonly branchLevels: number;
  readonly siblingCount: number;
  readonly proofSteps: number;
  readonly proofCborBytes: number;
};

/**
 * Insert the grinded siblings of every named target into `trie` and return how
 * many keys were added. A zero `branchLevels` leaves the trie untouched, which
 * is exactly the minimal fixture the four families already measured.
 */
export const insertAdversarialMembershipSiblings = async ({
  trie,
  targets,
  branchLevels,
}: {
  readonly trie: Trie;
  readonly targets: readonly {
    readonly key: Buffer;
    readonly domain: number;
  }[];
  readonly branchLevels: number;
}): Promise<number> => {
  if (branchLevels === 0) {
    return 0;
  }
  const reserved = new Set(targets.map(({ key }) => key.toString("hex")));
  let inserted = 0;
  for (const target of targets) {
    for (const key of adversarialMembershipSiblingKeys({
      targetKey: target.key,
      branchLevels,
      domain: target.domain,
    })) {
      const label = key.toString("hex");
      if (reserved.has(label)) {
        throw new Error(
          `Grinded adversarial sibling ${label} collides with an existing trie key.`,
        );
      }
      reserved.add(label);
      await trie.insert(key, ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE);
      inserted += 1;
    }
  }
  return inserted;
};

export const membershipProofShape = async ({
  trie,
  key,
  branchLevels,
  siblingCount,
}: {
  readonly trie: Trie;
  readonly key: Buffer;
  readonly branchLevels: number;
  readonly siblingCount: number;
}): Promise<MembershipProofShape> => {
  const proof = await trie.prove(key);
  const steps = proof.toJSON() as readonly { readonly type: string }[];
  if (branchLevels > 0 && steps.length < branchLevels) {
    throw new Error(
      `Adversarial fixture asked for ${branchLevels.toString()} branch levels but the membership proof carries only ${steps.length.toString()} steps.`,
    );
  }
  return {
    branchLevels,
    siblingCount,
    proofSteps: steps.length,
    proofCborBytes: Buffer.from(proof.toCBOR()).length,
  };
};

export const buildTransactionInclusionFixture = async ({
  adversarialBranchLevels = 0,
  spendInputCardinality,
  emptyAddressWitnesses = false,
}: {
  readonly adversarialBranchLevels?: number;
  /**
   * Use canonical empty address-witness fields when this block also drives a
   * real field-opening family. The legacy 32-byte marker is useful only as a
   * distinct hash preimage; it is not a canonical address-witness item.
   */
  readonly emptyAddressWitnesses?: boolean;
  /**
   * How many inputs each conflicting transaction spends. The default is the
   * fixture's minimal two; larger values drive the spend-input preimage
   * cardinality axis (finding Q1X-F6) with the double-spent input last.
   */
  readonly spendInputCardinality?: number;
} = {}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly tx1: TransactionInclusionEntry;
  readonly tx2: TransactionInclusionEntry;
  readonly tx1Full: MidgardNativeTxFull;
  readonly tx2Full: MidgardNativeTxFull;
  readonly tx1InputsPreimage: readonly TestOutputReference[];
  readonly tx2InputsPreimage: readonly TestOutputReference[];
  readonly tx1SpendInputCbors: readonly string[];
  readonly tx2SpendInputCbors: readonly string[];
  readonly tx1MembershipProof: MembershipProofShape;
  readonly tx2MembershipProof: MembershipProofShape;
}> => {
  // The double-spent input is the one both transactions carry, and it sits
  // last so the selection walk is at its longest on both sides.
  const doubleSpentInput = tx1InputsPreimage[1]!;
  const tx1Inputs =
    spendInputCardinality === undefined
      ? tx1InputsPreimage
      : spendInputsOfCardinality({
          selected: doubleSpentInput,
          cardinality: spendInputCardinality,
          domain: 0x0a01,
        });
  const tx2Inputs =
    spendInputCardinality === undefined
      ? tx2InputsPreimage
      : spendInputsOfCardinality({
          selected: doubleSpentInput,
          cardinality: spendInputCardinality,
          domain: 0x0a02,
        });
  const tx1Native = makeNativeTx({
    spendInputCbors: tx1Inputs.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "13",
    outputByte: "14",
    ...(emptyAddressWitnesses ? {} : { witnessByte: "20" }),
  });
  const tx2Native = makeNativeTx({
    spendInputCbors: tx2Inputs.map(outputReferenceCbor),
    fee: 1n,
    referenceByte: "23",
    outputByte: "24",
    ...(emptyAddressWitnesses ? {} : { witnessByte: "30" }),
  });
  const tx1 = compactTxEntry(tx1Native);
  const tx2 = compactTxEntry(tx2Native);
  const tx1SourceCbor = l2TransactionSourceCborV1(tx1Native);
  const tx2SourceCbor = l2TransactionSourceCborV1(tx2Native);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [tx1, tx2]) {
    await trie.insert(
      Buffer.from(entry.nativeTxId, "hex"),
      Buffer.from(entry === tx1 ? tx1SourceCbor : tx2SourceCbor, "hex"),
    );
  }
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [
      { key: Buffer.from(tx1.nativeTxId, "hex"), domain: 0x0a01 },
      { key: Buffer.from(tx2.nativeTxId, "hex"), domain: 0x0a02 },
    ],
    branchLevels: adversarialBranchLevels,
  });
  const withProof = async (
    entry: typeof tx1,
  ): Promise<TransactionInclusionEntry> => {
    const txKey = Buffer.from(entry.nativeTxId, "hex");
    const proof = await trie.prove(txKey);
    return {
      inclusion: {
        nativeTxId: entry.nativeTxId,
        nativeTx: entry.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: entry === tx1 ? tx1SourceCbor : tx2SourceCbor,
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
    l2TransactionCount: BigInt(2 + siblingCount),
    tx1: await withProof(tx1),
    tx2: await withProof(tx2),
    tx1Full: tx1Native,
    tx2Full: tx2Native,
    tx1InputsPreimage: tx1Inputs,
    tx2InputsPreimage: tx2Inputs,
    tx1SpendInputCbors: tx1.spendInputCbors,
    tx2SpendInputCbors: tx2.spendInputCbors,
    tx1MembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(tx1.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
    tx2MembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(tx2.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
  };
};

export const buildInvalidRangeTransactionInclusionFixture = async ({
  blockSlot,
  adversarialBranchLevels = 0,
}: {
  readonly blockSlot: bigint;
  readonly adversarialBranchLevels?: number;
}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly badTxMembershipProof: MembershipProofShape;
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
    validityIntervalStart: blockSlot + 1n,
    validityIntervalEnd: blockSlot + 101n,
  });
  const badTx = compactTxEntry(badNativeTx);
  const badTxSourceCbor = l2TransactionSourceCborV1(badNativeTx);
  const normalizedValidityRange = normalizeNativeTxValidityRange(
    badTx.nativeTx.body,
  );
  const violationReason = invalidRangeViolationReason({
    blockSlot,
    normalizedRange: normalizedValidityRange,
  });
  if (violationReason === null) {
    throw new Error(
      "Invalid-range fixture transaction does not exclude the block slot.",
    );
  }

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [{ key: Buffer.from(badTx.nativeTxId, "hex"), domain: 0x0c01 }],
    branchLevels: adversarialBranchLevels,
  });
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: BigInt(1 + siblingCount),
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          badNativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: badTxSourceCbor,
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
    normalizedValidityRange,
    violationReason,
  };
};

// Zero-input fixture: a bad L2 tx that spends nothing at all, violating the
// "at least one input" ledger rule. Its `spend_inputs_hash` is the hash of the
// empty definite-length CBOR array, which is precisely the constant step-02
// compares against.
export const buildZeroInputTransactionInclusionFixture = async ({
  adversarialBranchLevels = 0,
}: {
  readonly adversarialBranchLevels?: number;
} = {}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly badTxMembershipProof: MembershipProofShape;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 5n,
    referenceByte: "51",
    outputByte: "52",
    witnessByte: "53",
  });
  const badTx = compactTxEntry(badNativeTx);
  const badTxSourceCbor = l2TransactionSourceCborV1(badNativeTx);

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
    Buffer.from(badTxSourceCbor, "hex"),
  );
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [{ key: Buffer.from(badTx.nativeTxId, "hex"), domain: 0x0e01 }],
    branchLevels: adversarialBranchLevels,
  });
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: BigInt(1 + siblingCount),
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          badNativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: badTxSourceCbor,
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
  };
};

// Non-existent-input fixture: a bad L2 tx spends an input whose producing
// transaction never existed. The transactions trie is keyed by the raw native
// tx id (matching the node); the ledger non-membership is proven against the
// empty prev-ledger (`EMPTY_MERKLE_TREE_ROOT`, the genesis confirmed-state root
// the setup block builds on); and the phantom input's producing tx id is proven
// absent from the block's transactions.
export const buildNonExistentInputFixture = async ({
  adversarialBranchLevels = 0,
  spendInputCardinality,
}: {
  readonly adversarialBranchLevels?: number;
  /**
   * How many inputs the challenged transaction spends. The default is the
   * fixture's minimal one; larger values drive the spend-input preimage
   * cardinality axis (finding Q1X-F6) with the phantom input last.
   */
  readonly spendInputCardinality?: number;
} = {}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  readonly badInputIndex: bigint;
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly missingInputTxId: string;
  readonly nativeTxId: string;
  readonly badTxMembershipProof: MembershipProofShape;
  readonly txsNonMembershipProofCborBytes: number;
}> => {
  const phantomOutRef: TestOutputReference = {
    transactionId: h32("de"),
    outputIndex: 0n,
  };
  // The phantom input sits last, which is the worst case for the selection
  // walk; the whole preimage is re-hashed either way.
  const badTxInputs =
    spendInputCardinality === undefined
      ? [phantomOutRef]
      : spendInputsOfCardinality({
          selected: phantomOutRef,
          cardinality: spendInputCardinality,
          domain: 0x0b03,
        });
  const badTxNative = makeNativeTx({
    spendInputCbors: badTxInputs.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "e3",
    outputByte: "e4",
    witnessByte: "e5",
  });
  const badTx = compactTxEntry(badTxNative);
  const badTxCompactCbor = encodeMidgardNativeTxCompact(badTxNative.compact);
  const badTxSourceCbor = l2TransactionSourceCborV1(badTxNative);

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
  const otherTxSourceCbor = l2TransactionSourceCborV1(otherTxNative);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(otherTx.nativeTxId, "hex"),
    Buffer.from(otherTxSourceCbor, "hex"),
  );
  // Both proof-carrying legs of this family are pushed together: the step-01
  // membership proof of the challenged transaction, and the step-04
  // non-membership proof of the phantom input's producing transaction id.
  const adversarialSiblings =
    adversarialBranchLevels === 0
      ? []
      : [
          ...adversarialMembershipSiblingKeys({
            targetKey: Buffer.from(badTx.nativeTxId, "hex"),
            branchLevels: adversarialBranchLevels,
            domain: 0x0b01,
          }),
          ...adversarialMembershipSiblingKeys({
            targetKey: Buffer.from(phantomOutRef.transactionId, "hex"),
            branchLevels: adversarialBranchLevels,
            domain: 0x0b02,
          }),
        ];
  for (const key of adversarialSiblings) {
    await trie.insert(key, ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE);
  }
  const transactionsRoot = trieRootHex(trie);
  const membershipProof = await trie.prove(
    Buffer.from(badTx.nativeTxId, "hex"),
  );

  const txsEntries: TrieEntry[] = [
    {
      key: Buffer.from(badTx.nativeTxId, "hex"),
      value: Buffer.from(badTxSourceCbor, "hex"),
    },
    {
      key: Buffer.from(otherTx.nativeTxId, "hex"),
      value: Buffer.from(otherTxSourceCbor, "hex"),
    },
    ...adversarialSiblings.map((key) => ({
      key,
      value: ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE,
    })),
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
    l2TransactionCount: BigInt(2 + adversarialSiblings.length),
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount: adversarialSiblings.length,
    }),
    txsNonMembershipProofCborBytes: txsNonMembershipProofCbor.length / 2,
    inclusion: parseSubmitStep01TxInclusion({
      nativeTxId: badTx.nativeTxId,
      nativeTx: badTx.nativeTx,
      nativeTxCompactCbor: badTxCompactCbor.toString("hex"),
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: membershipProof.toCBOR().toString("hex"),
    }),
    inputsPreimage: badTxInputs.map((input) => ({
      txId: input.transactionId,
      index: input.outputIndex,
    })),
    badInputIndex: BigInt(badTxInputs.length - 1),
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
    missingInputTxId: phantomOutRef.transactionId,
    nativeTxId: badTx.nativeTxId,
  };
};

export const registerPexcludesExclusionRewardAccount = async (
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
export const countedTransactionsRoot = (
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

export const transitionTraceRawEntry = (
  key: string,
  value: string,
): [string, string] => [key, value];

export const sortedDaEntries = (
  entries: readonly [string, string][],
): [string, string][] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const buildInvalidForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
  fieldPreimageLengthMismatchIndex,
  fieldItemWidthIllegalCoordinate,
  redeemerMalformedIndex,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly fieldPreimageLengthMismatchIndex?: number;
  readonly fieldItemWidthIllegalCoordinate?: {
    readonly fieldIndex: number;
    readonly itemIndex: number;
  };
  readonly redeemerMalformedIndex?: number;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const finalDescriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
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
    ...(redeemerMalformedIndex === undefined
      ? {}
      : {
          redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimage([
            encodeMidgardRedeemerWitnessItem({
              purpose: "Spend",
              index: BigInt(redeemerMalformedIndex),
              redeemerCbor: Buffer.from("00", "hex"),
              executionUnits: { memory: 1n, steps: 2n },
            }),
          ]),
        }),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonical(forcedNativeTx);
  // The leaf is rejected, so its committed source is the operator-adjudicated
  // (TxIsInvalid-stamped) triple; the DA preimage row stays the submitted
  // canonical bytes, which is exactly what reconstruction re-adjudicates.
  const forcedSource = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(forcedNativeTx, "TxIsInvalid"),
  );
  const forcedTransaction = {
    tx_id: computeMidgardNativeTxId(forcedNativeTx).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason:
          redeemerMalformedIndex !== undefined
            ? {
                RedeemerMalformed: {
                  redeemer_index: BigInt(redeemerMalformedIndex),
                },
              }
            : fieldItemWidthIllegalCoordinate !== undefined
              ? {
                  FieldItemWidthIllegal: {
                    field_index: BigInt(
                      fieldItemWidthIllegalCoordinate.fieldIndex,
                    ),
                    item_index: BigInt(
                      fieldItemWidthIllegalCoordinate.itemIndex,
                    ),
                  },
                }
              : fieldPreimageLengthMismatchIndex === undefined
                ? { PlutusExecutionFailed: { execution_index: 0n } }
                : {
                    FieldPreimageLengthMismatch: {
                      field_index: BigInt(fieldPreimageLengthMismatchIndex),
                    },
                  },
      },
    },
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
      valueSchema: ValidationTraceDescriptorSchema,
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
  const header: Header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    encodeDaPayload({
      version: DA_PAYLOAD_VERSION,
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
        validation_trace_witnesses: [],
        counts,
      },
    }),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayload({
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
    reconstruction,
    eventKey,
    forcedNativeTx,
    forcedTransaction,
    proof: buildTransitionFaultProof({ reconstruction, fault }),
  };
};

export const buildInvalidForcedValidationDisputeFixture = async ({
  operatorVkey,
  now,
  inlineDatumPayloadBytes = 13_600,
  minimumCompleteItemBytes = MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
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
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonical(forcedNativeTx);
  const decodedForcedNativeTx =
    decodeMidgardNativeTxFullFromCanonicalCbor(forcedCanonicalCbor);
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
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
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
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
      committedForcedVerdict: "accepted",
    }),
  );
  // #600: the complete-item witness carries the carriage **plan input** — which
  // field, and its whole §5.1 preimage — and the tier is resolved later, at
  // evidence commitment. So the size this fixture selects on is the preimage the
  // step reads, which is what the L1 envelope actually has to hold, and it is
  // read straight off the plan input rather than out of a tier-1 carriage.
  const disputedWitnessIndex = challengerTrace.witnesses.findIndex(
    (witness) =>
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.fieldPreimage.length > minimumCompleteItemBytes,
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
  const completeItemBytes = completeItemWitness.auxiliary.fieldPreimage.length;
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
      operatorStates.map(hashMidgardValidationMachineState),
      "accepted",
      operatorRejectionCodeHash,
    ),
    verdict: "accepted",
    rejectionCode: null,
  };
  const evidence = buildValidationDisputeEvidenceBundle({
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

export const submitSuccessorBlockTx = async ({
  lucid,
  emulator,
  contracts,
  anchorBlockUnit,
  header,
  hubOracle,
  scheduler,
  activeOperatorNode,
  activeOperatorNodeUnit,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly emulator: Emulator;
  readonly contracts: MidgardValidators;
  readonly anchorBlockUnit: string;
  readonly header: Header;
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
  const successorHeaderHash = await Effect.runPromise(hashBlockHeader(header));
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
  expect(
    commitValidTo,
    "successor commit validTo must be later than the emulator clock before submission",
  ).toBeGreaterThan(BigInt(emulator.now()));
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs),
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
  const [confirmedStateRefInput] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME),
  );
  if (confirmedStateRefInput === undefined) {
    throw new Error("successor commit found no confirmed-state root witness");
  }
  const confirmedState = await Effect.runPromise(
    utxoToStateQueueUTxO(confirmedStateRefInput, contracts.stateQueue.policyId),
  );
  const [correctionLockUtxo] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME),
  );
  if (correctionLockUtxo === undefined) {
    throw new Error("successor commit found no correction-lock witness");
  }
  if (confirmedState.datum.next === "Empty") {
    throw new Error("successor commit found an empty state-queue root");
  }
  const headHeaderHash = confirmedState.datum.next.Key.key;
  const anchorHeaderHash = anchorBlock.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  const headStateQueueNodeRefInput =
    headHeaderHash === anchorHeaderHash
      ? undefined
      : (
          await lucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            toUnit(
              contracts.stateQueue.policyId,
              STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headHeaderHash,
            ),
          )
        )[0];
  if (
    headHeaderHash !== anchorHeaderHash &&
    headStateQueueNodeRefInput === undefined
  ) {
    throw new Error(
      `successor commit found no authenticated queue-head witness ${headHeaderHash}`,
    );
  }
  const commitYieldRef = await findStateQueueYieldReferenceScript({
    lucid,
    contracts,
    arm: "commit",
  });
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
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        ...(headStateQueueNodeRefInput === undefined
          ? {}
          : { headStateQueueNodeRefInput }),
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
        yieldWitness: {
          referenceInput: commitYieldRef,
          script: contracts.stateQueue.yields.commit.withdrawalScript,
        },
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
  await Effect.runPromise(getHeaderFromStateQueueDatum(continuedAnchor.datum));
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

export type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: Header;
};

/**
 * Publication labels for the four double-spend step validators, following the
 * `fraudProof<Family>StepNN` deployment-entry naming style. Local to the
 * emulator fixtures: the step references reach the submitters as explicit
 * `referenceScriptUtxo` parameters, not through the deployment manifest.
 */
export const DOUBLE_SPEND_STEP_REFERENCE_NAMES = [
  "fraudProofDoubleSpend",
  "fraudProofDoubleSpendStep02",
  "fraudProofDoubleSpendStep03",
  "fraudProofDoubleSpendStep04",
] as const;

export type ProvedDoubleSpendFixture = {
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
  readonly fraudulentHeader: Header;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly successors: readonly SuccessorBlockFixture[];
  readonly deploymentInfo: ReturnType<typeof buildRemovalDeploymentInfo>;
  readonly removalReferenceScriptPublications: Awaited<
    ReturnType<typeof publishRemovalReferenceScripts>
  >;
  readonly fraudulentBlockOutRef: string;
  readonly submitInitResult: Awaited<ReturnType<typeof submitInit>>;
  readonly submitInitMeasurement: CompleteSignedTransactionMeasurement;
  readonly step04Result: Awaited<ReturnType<typeof submitStep04>>;
  readonly step04Measurement: CompleteSignedTransactionMeasurement;
  readonly doubleSpendStepReferenceScripts: Awaited<
    ReturnType<typeof publishFraudProofChainReferenceScripts>
  >;
  readonly witnessReferenceScripts: Awaited<
    ReturnType<typeof publishFaultProofWitnessReferenceScripts>
  >;
  readonly fraudProofUtxo: UTxO;
  readonly proverPaymentKeyHash: string;
};

export type RemovalEvent =
  | { readonly kind: "stateQueue.utxosAt"; readonly call: number }
  | { readonly kind: "scheduler.utxosAtWithUnit"; readonly call: number }
  | { readonly kind: "awaitTx"; readonly txHash: string }
  | { readonly kind: "lease.acquire" }
  | { readonly kind: "lease.renew"; readonly call: number }
  | { readonly kind: "lease.release" }
  | { readonly kind: "lease.fail"; readonly error: string };

export const eventIndexes = (
  events: readonly RemovalEvent[],
  kind: RemovalEvent["kind"],
): number[] =>
  events.flatMap((event, index) => (event.kind === kind ? [index] : []));

export const createRecordingLeaseCoordinator = (
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

export const instrumentLucidForRemoval = ({
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

export const buildProvedDoubleSpendFixture = async ({
  successorCount = 0,
  headerMinimumFee = 0n,
}: {
  readonly successorCount?: number;
  /** Optional second violation used by cross-family Q53 idempotency tests. */
  readonly headerMinimumFee?: bigint;
} = {}): Promise<ProvedDoubleSpendFixture> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = fundedProverEmulatorAccount(20_000_000_000n);
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  // Selected through the signer so the prover Lucid instance and every
  // `signer.selectWallet(lucid)` call site address the same funded wallet.
  proverSigner.selectWallet(proverLucid);

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }

  const referenceScriptAuth = createReferenceScriptAuthPolicy(
    funderLucid,
    emulator.now(),
  );
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        realMinFee: headerMinimumFee > 0n,
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
      },
    )),
    referenceScriptAuth,
  };
  // Operator registration and activation source their four directory
  // validators from published reference scripts. Published from the prover
  // wallet before the header clock is sampled so the funder's nonce UTxO
  // survives and the whole fixture timeline shifts uniformly.
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: proverLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const transactionInclusion = await buildTransactionInclusionFixture({
    emptyAddressWitnesses: headerMinimumFee > 0n,
  });
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
  // Owner ruling 2026-08-26: every script a fault-proof transaction executes
  // is consumed from a published reference script, never inline-attached. The
  // four double-spend step validators and the shared witness scripts are
  // published from the prover wallet alongside the removal roster above.
  const doubleSpendStepReferenceScripts =
    await publishFraudProofChainReferenceScripts({
      lucid: proverLucid,
      steps: contracts.fraudProofContracts.doubleSpend.steps,
      entryNames: DOUBLE_SPEND_STEP_REFERENCE_NAMES,
      familyLabel: "double-spend",
    });
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
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
  const fraudulentHeader = {
    ...makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    ),
    minFeeA: 0n,
    minFeeB: headerMinimumFee,
    endTime:
      BigInt(headerStartTime) + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
  };
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
    const successorStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: previousHeader.endTime,
      emulator,
    });
    const baseSuccessorHeader = makeHeader(
      funderPaymentCredential.hash,
      successorStart,
      EMPTY_MERKLE_TREE_ROOT,
    );
    const successorHeader = {
      ...baseSuccessorHeader,
      endTime:
        baseSuccessorHeader.startTime +
        BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
      prevHeaderHash: previousHeaderHash,
    };
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      emulator,
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

  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    removalReferenceScripts: removalReferenceScriptPublications.published,
  });
  const fraudulentBlockOutRef =
    successors[0]?.continuedAnchorOutRef ?? setup.fraudulentBlockOutRef;

  const submitInitCapture = await captureEmulatorSubmission(emulator, () =>
    submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentBlockOutRef,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const submitInitResult = submitInitCapture.result;
  const submitInitMeasurement = submitInitCapture.measurement;

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
    referenceScriptUtxo:
      doubleSpendStepReferenceScripts["fraudProofDoubleSpend"]!.utxo,
    witnessReferenceScripts,
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
    referenceScriptUtxo:
      doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep02"]!.utxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });

  expect(step02Result.txHash).toHaveLength(64);
  expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step02Result.verifiedTx1Id).toBe(transactionInclusion.tx1.nativeTxId);
  expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
  // #604: the thread carries both §2.5 anchors; the field-0 commitments it used
  // to forward are re-derived at the door from the compact structures instead.
  expect(step02Result.verifiedTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
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
      verified_tx1_id: transactionInclusion.tx1.nativeTxId,
      verified_tx2_id: transactionInclusion.tx2.nativeTxId,
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
    nativeTxCompactCbor: parseSubmitStep01TxInclusion(
      transactionInclusion.tx1.inclusion,
    ).nativeTxCompactCbor,
    doubleSpentInputIndex: 1n,
    referenceScriptUtxo:
      doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep03"]!.utxo,
    awaitConfirmation: true,
  });

  expect(step03Result.txHash).toHaveLength(64);
  expect(step03Result.verifiedTx1SpendInputsHash).toBe(
    transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
  );
  expect(step03Result.doubleSpentInputIndex).toBe(1);
  expect(step03Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
  );
  expect(step03Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx1SpendInputCbors[1],
  );
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
      verified_tx2_id: transactionInclusion.tx2.nativeTxId,
      double_spent_input: midgardTxInput(
        transactionInclusion.tx1InputsPreimage[1]!,
      ),
    },
  });
  expect(fourthStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step04Capture = await captureEmulatorSubmission(emulator, () =>
    submitStep04({
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
      nativeTxCompactCbor: parseSubmitStep01TxInclusion(
        transactionInclusion.tx2.inclusion,
      ).nativeTxCompactCbor,
      doubleSpentInputIndex: 1n,
      referenceScriptUtxo:
        doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep04"]!.utxo,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const step04Result = step04Capture.result;
  const step04Measurement = step04Capture.measurement;

  expect(step04Result.txHash).toHaveLength(64);
  expect(step04Result.doubleSpentInputIndex).toBe(1);
  expect(step04Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
  );
  expect(step04Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx2SpendInputCbors[1],
  );
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
    removalReferenceScriptPublications,
    fraudulentBlockOutRef,
    submitInitResult,
    submitInitMeasurement,
    step04Result,
    step04Measurement,
    doubleSpendStepReferenceScripts,
    witnessReferenceScripts,
    fraudProofUtxo,
    proverPaymentKeyHash,
  };
};

const requireCurrentUnitUtxo = async ({
  lucid,
  address,
  unit,
  label,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  if (utxos.length !== 1 || utxos[0] === undefined) {
    throw new Error(
      `${label} expected exactly one UTxO carrying ${unit}, found ${utxos.length.toString()}.`,
    );
  }
  return utxos[0];
};

const activeOperatorDatumFromUtxo = async (
  utxo: UTxO,
): Promise<ActiveOperatorDatum> => {
  const node = await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo));
  return Data.castFrom(
    node.data as never,
    ActiveOperatorDatum as never,
  ) as ActiveOperatorDatum;
};

const cleanAdaOnlyWalletFeeInput = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const feeInput = (await lucid.wallet().getUtxos()).find(
    (utxo) =>
      utxo.datum === undefined &&
      utxo.datumHash === undefined &&
      utxo.scriptRef === undefined &&
      Object.entries(utxo.assets).every(
        ([unit, amount]) => unit === "lovelace" && amount > 0n,
      ),
  );
  if (feeInput === undefined) {
    throw new Error(`Expected a clean ADA-only wallet UTxO for ${label}.`);
  }
  return feeInput;
};

const expectReferenceScriptOnlyTransaction = ({
  signed,
  measurement,
  expectedReferenceInputs,
}: {
  readonly signed: Parameters<
    typeof workflowTransactionReferenceInputOutRefs
  >[0];
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly expectedReferenceInputs: readonly UTxO[];
}): void => {
  expect(measurement.plutusV1ScriptCount).toBe(0);
  expect(measurement.plutusV2ScriptCount).toBe(0);
  expect(measurement.plutusV3ScriptCount).toBe(0);
  expect(measurement.nativeScriptCount).toBe(0);
  const referenceInputs = workflowTransactionReferenceInputOutRefs(signed);
  expect(referenceInputs).toHaveLength(expectedReferenceInputs.length);
  expect(referenceInputs).toEqual(
    expect.arrayContaining(expectedReferenceInputs.map(outRefLabel)),
  );
};

const minimumLovelaceForInlineValue = ({
  address,
  datum,
  assets,
}: {
  readonly address: string;
  readonly datum: string;
  readonly assets: Readonly<Record<string, bigint>>;
}): bigint => {
  let lovelace = assets.lovelace ?? 0n;
  for (let attempt = 0; attempt < 16; attempt += 1) {
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      assetsToValue({ ...assets, lovelace }),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
      undefined,
    );
    const required = CML.min_ada_required(
      output,
      EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error("Failed to stabilize linked-list inline-datum min-ADA.");
};

/**
 * Drive the fixture's sole active operator through the canonical five-strike
 * inactivity path and transfer its exact remaining bond to the retired set.
 * This is deliberately a real validator lifecycle, not a fabricated retired
 * datum, so the partial Q53 tranche reaches fraud removal with authenticated
 * provenance from the active-operator and scheduler contracts.
 */
export const retireFixtureOperatorAfterInactivity = async (
  fixture: ProvedDoubleSpendFixture,
): Promise<UTxO> => {
  const { activeOperators, retiredOperators, scheduler, stateQueue } =
    fixture.contracts;
  const operator = fixture.fraudulentHeader.operatorVkey;
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const retiredOperatorNodeUnit = toUnit(
    retiredOperators.policyId,
    RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operator,
  );
  const tailStateQueueUnit =
    fixture.successors.at(-1)?.successorBlockUnit ??
    fixture.setup.stateQueueBlockUnit;
  const protocolParameters = getProtocolParameters(network);

  for (
    let expectedInputStrikes = 0n;
    expectedInputStrikes < 5n;
    expectedInputStrikes += 1n
  ) {
    const currentScheduler = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: scheduler.spendingScriptAddress,
      unit: schedulerUnit,
      label: "inactivity strike scheduler",
    });
    const currentActiveOperator = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: activeOperators.spendingScriptAddress,
      unit: fixture.setup.activeOperatorNodeUnit,
      label: "inactivity strike active operator",
    });
    const activeOperatorsRoot = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: activeOperators.spendingScriptAddress,
      unit: fixture.setup.activeOperatorsRootUnit,
      label: "inactivity strike active-operators root",
    });
    const registeredOperatorsRoot = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: fixture.contracts.registeredOperators.spendingScriptAddress,
      unit: toUnit(
        fixture.contracts.registeredOperators.policyId,
        REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      ),
      label: "inactivity strike registered-operators root",
    });
    const tailStateQueueNode = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: stateQueue.spendingScriptAddress,
      unit: tailStateQueueUnit,
      label: "inactivity strike terminal state-queue node",
    });
    const schedulerDatum = Data.from(currentScheduler.datum!, SchedulerDatum);
    if (
      !(
        typeof schedulerDatum === "object" && "ActiveOperator" in schedulerDatum
      )
    ) {
      throw new Error("Inactivity strike expected an appointed scheduler.");
    }
    expect(schedulerDatum.ActiveOperator.operator).toBe(operator);
    const currentActiveDatum = await activeOperatorDatumFromUtxo(
      currentActiveOperator,
    );
    expect(currentActiveDatum.inactivity_strikes).toBe(expectedInputStrikes);

    const inactivityThreshold = Math.max(
      Number(schedulerDatum.ActiveOperator.start_time) + 300_000,
      Number(
        fixture.successors.at(-1)?.header.endTime ??
          fixture.fraudulentHeader.endTime,
      ) + 60_000,
    );
    const firstValidSlot =
      fixture.funderLucid.unixTimeToSlot(inactivityThreshold) + 2;
    const slotsToAdvance = firstValidSlot - fixture.funderLucid.currentSlot();
    if (slotsToAdvance > 0) {
      fixture.emulator.awaitSlot(slotsToAdvance);
    }
    const validFrom = fixture.funderLucid.slotToUnixTime(
      fixture.funderLucid.currentSlot(),
    );
    const validTo = validFrom + 60_000;
    const nextShiftStart = BigInt(validTo - 1);
    const feeInput = await cleanAdaOnlyWalletFeeInput(
      fixture.proverLucid,
      "inactivity strike fee input",
    );
    const schedulerSpendRedeemer = ((ctx) =>
      Data.to(
        {
          scheduler_input_index: requireInputIndex(
            ctx,
            currentScheduler,
            "inactivity strike scheduler input",
          ),
          scheduler_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) => (output.assets[schedulerUnit] ?? 0n) === 1n,
            "inactivity strike scheduler output",
          ),
          advancing_approach: {
            RewindDueToSkippedOperator: {
              active_operators_root_ref_input_index: requireReferenceInputIndex(
                ctx,
                activeOperatorsRoot,
                "inactivity strike active-operators root",
              ),
              skipped_operator_node_input_index: requireInputIndex(
                ctx,
                currentActiveOperator,
                "inactivity strike active-operator input",
              ),
              active_operators_spend_redeemer_index: requireSpendRedeemerIndex(
                ctx,
                currentActiveOperator,
                "inactivity strike active-operator redeemer",
              ),
              state_queue_ref_input_index: requireReferenceInputIndex(
                ctx,
                tailStateQueueNode,
                "inactivity strike state-queue tail",
              ),
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                fixture.setup.hubOracle,
                "inactivity strike hub oracle",
              ),
              m_active_operators_last_node_ref_input_index: null,
              registered_element_ref_input_index: requireReferenceInputIndex(
                ctx,
                registeredOperatorsRoot,
                "inactivity strike registered-operators root",
              ),
              neglected_user_event: "NoNeglectedUserEvent",
            },
          },
        } satisfies SchedulerSpendRedeemer,
        SchedulerSpendRedeemer,
      )) satisfies BuildTxWithRedeemer;
    const activeOperatorSpendRedeemer = ((ctx) =>
      Data.to(
        {
          StrikeForInactivity: {
            active_node_input_index: requireInputIndex(
              ctx,
              currentActiveOperator,
              "inactivity strike active-operator input",
            ),
            active_node_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                (output.assets[fixture.setup.activeOperatorNodeUnit] ?? 0n) ===
                1n,
              "inactivity strike active-operator output",
            ),
            operator,
            active_node_link: null,
            scheduler_input_index: requireInputIndex(
              ctx,
              currentScheduler,
              "inactivity strike scheduler input",
            ),
            scheduler_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              currentScheduler,
              "inactivity strike scheduler redeemer",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              fixture.setup.hubOracle,
              "inactivity strike hub oracle",
            ),
          },
        } satisfies ActiveOperatorSpendRedeemer,
        ActiveOperatorSpendRedeemer,
      )) satisfies BuildTxWithRedeemer;
    const strikeUnsigned = await fixture.proverLucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([currentScheduler], schedulerSpendRedeemer)
      .collectFrom([currentActiveOperator], activeOperatorSpendRedeemer)
      .readFrom([
        fixture.setup.hubOracle,
        activeOperatorsRoot,
        registeredOperatorsRoot,
        tailStateQueueNode,
        fixture.removalReferenceScriptPublications.published
          .activeOperatorsSpend,
        fixture.removalReferenceScriptPublications.published.schedulerSpend,
      ])
      .pay.ToContract(
        scheduler.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(
            {
              ActiveOperator: {
                operator,
                start_time: nextShiftStart,
              },
            },
            SchedulerDatum,
          ),
        },
        currentScheduler.assets,
      )
      .pay.ToContract(
        activeOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeLinkedListNodeView({
            key: { Key: { key: operator } },
            next: "Empty",
            data: Data.castTo(
              {
                bond_unlock_time: currentActiveDatum.bond_unlock_time,
                inactivity_strikes: expectedInputStrikes + 1n,
              },
              ActiveOperatorDatum,
            ),
          }),
        },
        currentActiveOperator.assets,
      )
      .validFrom(validFrom)
      .validTo(validTo)
      .complete({ localUPLCEval: true });
    const strikeSigned = await strikeUnsigned.sign.withWallet().complete();
    const expectedStrikeReferenceInputs = [
      fixture.setup.hubOracle,
      activeOperatorsRoot,
      registeredOperatorsRoot,
      tailStateQueueNode,
      fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.schedulerSpend,
    ];
    const strikeCapture = await captureEmulatorSubmission(
      fixture.emulator,
      async () => {
        const txHash = await strikeSigned.submit();
        await fixture.proverLucid.awaitTx(txHash);
        return txHash;
      },
    );
    expectReferenceScriptOnlyTransaction({
      signed: strikeSigned,
      measurement: strikeCapture.measurement,
      expectedReferenceInputs: expectedStrikeReferenceInputs,
    });
  }

  const activeOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: activeOperators.spendingScriptAddress,
    unit: fixture.setup.activeOperatorsRootUnit,
    label: "inactivity retirement active-operators root",
  });
  const activeOperator = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: activeOperators.spendingScriptAddress,
    unit: fixture.setup.activeOperatorNodeUnit,
    label: "inactivity retirement active operator",
  });
  const retiredOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: retiredOperators.spendingScriptAddress,
    unit: fixture.setup.retiredOperatorsRootUnit,
    label: "inactivity retirement retired-operators root",
  });
  const currentScheduler = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: scheduler.spendingScriptAddress,
    unit: schedulerUnit,
    label: "inactivity retirement scheduler",
  });
  const registeredOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: fixture.contracts.registeredOperators.spendingScriptAddress,
    unit: toUnit(
      fixture.contracts.registeredOperators.policyId,
      REGISTERED_OPERATORS_ROOT_ASSET_NAME,
    ),
    label: "inactivity retirement registered-operators root",
  });
  const activeDatum = await activeOperatorDatumFromUtxo(activeOperator);
  expect(activeDatum.inactivity_strikes).toBe(5n);
  const retirementValidFrom = fixture.funderLucid.slotToUnixTime(
    fixture.funderLucid.currentSlot(),
  );
  const retirementValidTo = retirementValidFrom + 60_001;
  const activeOperatorsMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      activeOperators.policyId,
      "inactivity retirement active-operators mint",
    );
    return Data.to(
      {
        RetireOperator: {
          active_operator_key: operator,
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            fixture.setup.hubOracle,
            "inactivity retirement hub oracle",
          ),
          active_operator_anchor_element_input_outref:
            outputReferenceFromUTxO(activeOperatorsRoot),
          active_operator_anchor_element_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              (output.assets[fixture.setup.activeOperatorsRootUnit] ?? 0n) ===
              1n,
            "inactivity retirement active-operators root output",
          ),
          retired_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            retiredOperators.policyId,
            "inactivity retirement retired-operators mint",
          ),
          penalize_for_inactivity: true,
          operator_removal_scheduler_sync: {
            ShowSchedulerIsAdvancing: {
              scheduler_input_index: requireInputIndex(
                ctx,
                currentScheduler,
                "inactivity retirement scheduler input",
              ),
              scheduler_redeemer_index: requireSpendRedeemerIndex(
                ctx,
                currentScheduler,
                "inactivity retirement scheduler redeemer",
              ),
              removing_operators_anchor_element_key: null,
              removing_operator_is_the_last_member: true,
            },
          },
        },
      } satisfies ActiveOperatorMintRedeemer,
      ActiveOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const retiredOperatorsMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      retiredOperators.policyId,
      "inactivity retirement retired-operators mint",
    );
    return Data.to(
      {
        RetireOperator: {
          new_retired_operator_key: operator,
          bond_unlock_time: activeDatum.bond_unlock_time,
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            fixture.setup.hubOracle,
            "inactivity retirement hub oracle",
          ),
          retired_operator_anchor_element_output_index:
            requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                (output.assets[fixture.setup.retiredOperatorsRootUnit] ??
                  0n) === 1n,
              "inactivity retirement retired-operators root output",
            ),
          retired_operator_inserted_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) => (output.assets[retiredOperatorNodeUnit] ?? 0n) === 1n,
            "inactivity retirement retired-operator node output",
          ),
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            activeOperators.policyId,
            "inactivity retirement active-operators mint",
          ),
        },
      } satisfies RetiredOperatorMintRedeemer,
      RetiredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const schedulerSpendRedeemer = ((ctx) =>
    Data.to(
      {
        scheduler_input_index: requireInputIndex(
          ctx,
          currentScheduler,
          "inactivity retirement scheduler input",
        ),
        scheduler_output_index: requireUniqueOutputIndex(
          ctx.outputs,
          (output) => (output.assets[schedulerUnit] ?? 0n) === 1n,
          "inactivity retirement scheduler output",
        ),
        advancing_approach: {
          RewindDueToOperatorRemoval: {
            active_operators_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              activeOperators.policyId,
              "inactivity retirement active-operators mint",
            ),
            m_active_operators_last_node_ref_input_index: null,
            removal_reason: "OperatorRetirement",
            registered_element_ref_input_index: requireReferenceInputIndex(
              ctx,
              registeredOperatorsRoot,
              "inactivity retirement registered-operators root",
            ),
          },
        },
      } satisfies SchedulerSpendRedeemer,
      SchedulerSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const partialBond =
    protocolParameters.required_bond -
    protocolParameters.inactivity_slashing_penalty;
  const activeOperatorsRootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: "",
  });
  const retiredOperatorsRootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: operator } },
    data: "",
  });
  const retiredOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: operator } },
    next: "Empty",
    data: Data.castTo(
      { bond_unlock_time: activeDatum.bond_unlock_time },
      RetiredOperatorDatum,
    ),
  });
  const retiredRootInputLovelace = retiredOperatorsRoot.assets.lovelace ?? 0n;
  const retiredRootOutputLovelace = minimumLovelaceForInlineValue({
    address: retiredOperators.spendingScriptAddress,
    datum: retiredOperatorsRootDatum,
    assets: retiredOperatorsRoot.assets,
  });
  const retiredRootRentTopUp =
    retiredRootOutputLovelace - retiredRootInputLovelace;
  if (retiredRootRentTopUp < 0n) {
    throw new Error(
      "Retired-operators root min-ADA top-up cannot be negative.",
    );
  }
  const retirementRentInput = await cleanAdaOnlyWalletFeeInput(
    fixture.proverLucid,
    "inactivity retirement linked-list rent",
  );
  const retirementRentInputLovelace = retirementRentInput.assets.lovelace ?? 0n;
  const retirementRentRefund =
    retirementRentInputLovelace - retiredRootRentTopUp;
  if (retirementRentRefund <= 1_000_000n) {
    throw new Error(
      "Inactivity retirement rent input cannot fund a canonical change output.",
    );
  }
  const retirementRefundAddress = await fixture.proverLucid.wallet().address();
  const retirementProtocolInputs =
    (activeOperatorsRoot.assets.lovelace ?? 0n) +
    (activeOperator.assets.lovelace ?? 0n) +
    retiredRootInputLovelace +
    (currentScheduler.assets.lovelace ?? 0n) +
    retirementRentInputLovelace;
  const retirementProtocolOutputsAndFee =
    (activeOperatorsRoot.assets.lovelace ?? 0n) +
    retiredRootOutputLovelace +
    partialBond +
    (currentScheduler.assets.lovelace ?? 0n) +
    retirementRentRefund +
    protocolParameters.inactivity_slashing_penalty;
  expect(retirementProtocolInputs).toBe(retirementProtocolOutputsAndFee);
  const retirementUnsigned = await fixture.proverLucid
    .newTx()
    .collectFrom([retirementRentInput])
    .collectFrom(
      [activeOperatorsRoot, activeOperator],
      Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
    )
    .collectFrom([retiredOperatorsRoot], Data.void())
    .collectFrom([currentScheduler], schedulerSpendRedeemer)
    .readFrom([
      fixture.setup.hubOracle,
      registeredOperatorsRoot,
      fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.activeOperatorsMint,
      fixture.removalReferenceScriptPublications.published
        .retiredOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.retiredOperatorsMint,
      fixture.removalReferenceScriptPublications.published.schedulerSpend,
    ])
    .mintAssets(
      { [fixture.setup.activeOperatorNodeUnit]: -1n },
      activeOperatorsMintRedeemer,
    )
    .mintAssets({ [retiredOperatorNodeUnit]: 1n }, retiredOperatorsMintRedeemer)
    .pay.ToContract(
      activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: activeOperatorsRootDatum,
      },
      activeOperatorsRoot.assets,
    )
    .pay.ToContract(
      retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: retiredOperatorsRootDatum,
      },
      {
        ...retiredOperatorsRoot.assets,
        lovelace: retiredRootOutputLovelace,
      },
    )
    .pay.ToContract(
      retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: retiredOperatorDatum,
      },
      { lovelace: partialBond, [retiredOperatorNodeUnit]: 1n },
    )
    .pay.ToContract(
      scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      currentScheduler.assets,
    )
    .pay.ToAddress(retirementRefundAddress, {
      lovelace: retirementRentRefund,
    })
    .validFrom(retirementValidFrom)
    .validTo(retirementValidTo)
    .setMinFee(protocolParameters.inactivity_slashing_penalty)
    .complete({ coinSelection: false, localUPLCEval: true });
  const retirementSigned = await retirementUnsigned.sign
    .withWallet()
    .complete();
  const expectedRetirementReferenceInputs = [
    fixture.setup.hubOracle,
    registeredOperatorsRoot,
    fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
    fixture.removalReferenceScriptPublications.published.activeOperatorsMint,
    fixture.removalReferenceScriptPublications.published.retiredOperatorsSpend,
    fixture.removalReferenceScriptPublications.published.retiredOperatorsMint,
    fixture.removalReferenceScriptPublications.published.schedulerSpend,
  ];
  const retirementCapture = await captureEmulatorSubmission(
    fixture.emulator,
    async () => {
      const txHash = await retirementSigned.submit();
      await fixture.proverLucid.awaitTx(txHash);
      return txHash;
    },
  );
  expectReferenceScriptOnlyTransaction({
    signed: retirementSigned,
    measurement: retirementCapture.measurement,
    expectedReferenceInputs: expectedRetirementReferenceInputs,
  });
  expect(retirementSigned.toTransaction().body().fee()).toBe(
    protocolParameters.inactivity_slashing_penalty,
  );

  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      activeOperators.spendingScriptAddress,
      fixture.setup.activeOperatorNodeUnit,
    ),
  ).resolves.toHaveLength(0);
  const retiredOperator = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: retiredOperators.spendingScriptAddress,
    unit: retiredOperatorNodeUnit,
    label: "partially inactivity-slashed retired operator",
  });
  expect(retiredOperator.assets.lovelace).toBe(partialBond);
  return retiredOperator;
};

export const submitRemovalForFixture = async (
  fixture: ProvedDoubleSpendFixture,
  options: {
    readonly lucid?: Awaited<ReturnType<typeof Lucid>>;
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
    readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
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
    ...(options.preSubmitBoundary === undefined
      ? {}
      : { preSubmitBoundary: options.preSubmitBoundary }),
  });
};

export const expectRemovedFraudProofState = async (
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

/**
 * Publish the fraudulent block the family suites then prove against: sample
 * the emulator clock one slot before the aligned boundary, commit the
 * fixture's raw transactions root under the counted-root domain, and submit
 * the setup transaction. Every caller passed the same code; only the fixture
 * type differed, so the parameter is structural.
 */
export const setupFraudulentBlock = async ({
  funderLucid,
  emulator,
  contracts,
  catalogue,
  fixture,
}: {
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly emulator: Emulator;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly fixture: {
    readonly transactionsRoot: string;
    readonly l2TransactionCount: bigint;
    readonly prevUtxosRoot?: string;
    readonly utxosRoot?: string;
    /**
     * Optional predecessor duration for journeys whose setup transactions
     * advance the emulator beyond the one-second default header window.
     */
    readonly headerDurationMs?: number;
  };
}) => {
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const baseHeader = makeHeader(
    funderKeyHash,
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const fraudulentHeader = {
    ...baseHeader,
    ...(fixture.headerDurationMs === undefined
      ? {}
      : {
          endTime: baseHeader.startTime + BigInt(fixture.headerDurationMs),
        }),
    ...(fixture.prevUtxosRoot === undefined
      ? {}
      : { prevUtxosRoot: fixture.prevUtxosRoot }),
    ...(fixture.utxosRoot === undefined
      ? {}
      : { utxosRoot: fixture.utxosRoot }),
  };
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo: (await funderLucid.wallet().getUtxos())[0]!,
    catalogue,
    header: fraudulentHeader,
  });
  return { ...setup, header: fraudulentHeader };
};

/**
 * Successor fixtures are often assembled after publishing reference scripts,
 * each of which advances the emulator. Keep the successor monotonic with its
 * predecessor without allowing those preliminary transactions to leave the
 * commit validity interval behind the live emulator clock.
 */
export const emulatorSuccessorHeaderStart = ({
  predecessorEndTime,
  emulator,
}: {
  readonly predecessorEndTime: bigint;
  readonly emulator: Emulator;
}): number => {
  const predecessorEnd = Number(predecessorEndTime);
  const targetStart = Math.max(predecessorEnd, emulator.now());
  expect(
    targetStart,
    "successor fixture predecessor window must remain live to preserve exact header contiguity",
  ).toBe(predecessorEnd);
  return targetStart;
};

/** Setup and successor submissions advance the emulator by roughly 20s. */
export const EMULATOR_HEADER_CLOCK_HEADROOM_MS = 60_000;
