/**
 * Emulator journey fixtures shared by the `submit-init-emulator-*.test.ts`
 * family.
 *
 * These builders were local to `submit-init-emulator.test.ts` until that file
 * had to be broken up: `@lucid-evolution/uplc` never reclaims the wasm linear
 * memory it allocates per script evaluation, vitest isolates per FILE, and a
 * single file holding every heavy journey walked into the ~4 GiB wasm32
 * ceiling on CI. Lifting the fixtures here lets each journey theme run in its
 * own worker with a fresh wasm heap while still sharing one definition of
 * every fixture. See tests/support/uplc-heap-guard.ts.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxIdV1,
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
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  buildPhasMembershipRewardRegistrationTxProgram,
  commitCountedRootProgram,
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
  invalidRangeViolationReason,
  type MidgardValidators,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  OutputReference,
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
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
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
  keyValuePhasRootWithCount,
  reconstructDaPayloadV1,
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../../src/index.js";
import {
  buildNonMembershipProof,
  type TrieEntry,
} from "../../src/ne-proofs.js";
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
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  firstWalletUtxo,
  getCompiledScript,
  h32,
  makeHeader,
  makeNativeTx,
  network,
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
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(outRef.transactionId),
      outRef.outputIndex,
    ).to_cbor_bytes(),
  );

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
  nativeTx: MidgardNativeTxFullV1,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxIdV1(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

export const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFullV1,
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
}: {
  readonly adversarialBranchLevels?: number;
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
    witnessByte: "20",
  });
  const tx2Native = makeNativeTx({
    spendInputCbors: tx2Inputs.map(outputReferenceCbor),
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
    l2TransactionCount: BigInt(2 + siblingCount),
    tx1: await withProof(tx1),
    tx2: await withProof(tx2),
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
  blockValidFrom,
  blockValidTo,
  adversarialBranchLevels = 0,
}: {
  readonly blockValidFrom: bigint;
  readonly blockValidTo: bigint;
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
      value: Buffer.from(badTxCompactCbor),
    },
    {
      key: Buffer.from(otherTx.nativeTxId, "hex"),
      value: Buffer.from(otherTxCompactCbor),
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

export const buildInvalidForcedValidationDisputeFixture = async ({
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

export const submitSuccessorBlockTx = async ({
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

export type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: HeaderV1;
};

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

export const submitRemovalForFixture = async (
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
