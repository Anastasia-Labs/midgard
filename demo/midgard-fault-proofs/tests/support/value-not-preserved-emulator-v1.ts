/**
 * Shared emulator fixtures for the `value-not-preserved` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction that does
 * not conserve one claimed asset. What every scenario needs and no existing
 * helper produces is a committed transaction with STRUCTURED outputs and
 * mint fields (the §5.5/§5.6 canonical encodings the step-03 fold decodes)
 * plus a pre-state ledger MPF whose descriptors commit the spent values the
 * step-02 fold authenticates — so the fixture materializes the native
 * transaction directly from its canonical preimages and files genuine
 * `LedgerOutputCommitmentV1` descriptors under the header's
 * `prev_utxos_root`.
 *
 * Tier sizing is deliberately two-sided (§8.4): the tier-1 scenarios carry
 * realistically small fields, and the tier-2 scenario's outputs preimage is
 * pushed past the 14,336-byte tier-1 cap by large inline datums — data size
 * alone selects `RawUtxo`, no override exists anywhere.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardFieldItemsV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardMintPolicyItemV1,
  type MidgardNativeTxFullV1,
  type MidgardTxOutput,
} from "@al-ft/midgard-core";
import type { MidgardValue } from "@al-ft/midgard-core/codec";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  CORRECTION_LOCK_ASSET_NAME,
  encodeLinkedListNodeView,
  encodeMidgardTxInputCanonicalV1,
  fieldPreimagePublicationDatumCborV1,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  hashBlockHeaderV1,
  type HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  type MidgardTxInput,
  Proof,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  SCHEDULER_ASSET_NAME,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
} from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { ValueNotPreservedContractsV1 } from "../../src/value-not-preserved/contracts-v1.js";
import {
  buildSpentInputValueWitnessV1,
  spendInputsOpeningV1,
  type ValueNotPreservedLedgerTrieHandleV1,
} from "../../src/value-not-preserved/evidence-v1.js";
import {
  type ClaimedAssetV1,
  type ClaimedImbalanceDirectionV1,
  ValueNotPreservedStep01SpendRedeemer,
  ValueNotPreservedStep02Datum,
  type ValueNotPreservedStep02State,
  ValueNotPreservedStep04SpendRedeemer,
} from "../../src/value-not-preserved/schemas-v1.js";
import { requireValueNotPreservedThreadUtxoV1 } from "../../src/value-not-preserved/submit-common-v1.js";
import { submitValueNotPreservedInit } from "../../src/value-not-preserved/submit-value-not-preserved-init-v1.js";
import { submitValueNotPreservedStep01 } from "../../src/value-not-preserved/submit-value-not-preserved-step-01-v1.js";
import {
  submitValueNotPreservedStep02Finish,
  submitValueNotPreservedStep02Fold,
} from "../../src/value-not-preserved/submit-value-not-preserved-step-02-v1.js";
import { submitValueNotPreservedStep03 } from "../../src/value-not-preserved/submit-value-not-preserved-step-03-v1.js";
import { submitValueNotPreservedStep04 } from "../../src/value-not-preserved/submit-value-not-preserved-step-04-v1.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessSpendingValidatorCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../../src/witness-reference-scripts-v1.js";
import {
  findStateQueueYieldReferenceScriptV1,
  publishFaultProofWitnessReferenceScriptsV1,
} from "./emulator/reference-scripts.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS_V1,
  emulatorSuccessorHeaderStartV1,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  funderPaymentKeyHash,
  l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  makeNativeTx,
  network as emulatorNetworkV1,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export { expectOnchainRefusalV1 } from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Building blocks: out-refs, values, outputs
// ---------------------------------------------------------------------------

/** A readable fixture out-ref: `tx_id` is one byte repeated 32 times. */
export const vnpOutRefV1 = (
  txIdByte: string,
  outputIndex: number,
): MidgardTxInput => ({
  tx_id: txIdByte.repeat(32),
  output_index: BigInt(outputIndex),
});

/** A `MidgardValue` from lovelace plus optional single-policy token entries. */
export const vnpValueV1 = (
  lovelace: bigint,
  tokens: readonly {
    readonly policyIdHex: string;
    readonly assetNameHex: string;
    readonly quantity: bigint;
  }[] = [],
): MidgardValue => {
  const assets = new Map<string, Map<string, bigint>>();
  for (const token of tokens) {
    const names = assets.get(token.policyIdHex) ?? new Map<string, bigint>();
    names.set(
      token.assetNameHex,
      (names.get(token.assetNameHex) ?? 0n) + token.quantity,
    );
    assets.set(token.policyIdHex, names);
  }
  return { lovelace, assets };
};

/** The fixture payment credential every committed output pays. */
const VNP_OUTPUT_ADDRESS_V1 = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x42),
]);

/** The fixture payment credential every SPENT (pre-state) output pays. */
const VNP_LEDGER_OUTPUT_ADDRESS_V1 = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x99),
]);

/**
 * A canonical Aiken-serialised PlutusData datum of `chunkCount` 64-byte
 * strings inside an indefinite list — `2 + 66 × chunkCount` bytes. This is
 * the §8.4 sizing instrument: inline datums are the realistic way a
 * committed L2 output gets large, and the on-chain output decoder slices a
 * datum without walking it, so a datum-heavy outputs field crosses the
 * 14,336-byte tier-1 cap without inflating the fold's execution cost.
 */
export const vnpLargeDatumCborV1 = (chunkCount: number, seed: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x9f]),
    ...Array.from({ length: chunkCount }, (_, index) =>
      Buffer.concat([
        Buffer.from([0x58, 0x40]),
        Buffer.alloc(64, (seed + index) & 0xff),
      ]),
    ),
    Buffer.from([0xff]),
  ]);

/** One committed output, optionally padded with a large inline datum. */
export const vnpOutputV1 = ({
  value,
  datumChunks = 0,
  seed = 0,
}: {
  readonly value: MidgardValue;
  readonly datumChunks?: number;
  readonly seed?: number;
}): MidgardTxOutput => ({
  address: VNP_OUTPUT_ADDRESS_V1,
  value,
  ...(datumChunks > 0
    ? {
        datum: {
          kind: "inline" as const,
          cbor: vnpLargeDatumCborV1(datumChunks, seed),
        },
      }
    : {}),
});

// ---------------------------------------------------------------------------
// The committed transaction, its MPF inclusion, and the pre-state ledger
// ---------------------------------------------------------------------------

/** One spend input of the fixture transaction with its pre-state value. */
export type ValueNotPreservedFixtureSpentInputV1 = {
  readonly input: MidgardTxInput;
  readonly spentValue: MidgardValue;
};

export type ValueNotPreservedFixtureV1 = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputsPreimageCbor: Buffer;
  /** The committed §5.1 outputs-field preimage — the §8.4 tier decider. */
  readonly outputsPreimageCbor: Buffer;
  readonly outputs: readonly MidgardTxOutput[];
  readonly mintItems: readonly MidgardMintPolicyItemV1[];
  readonly ledger: {
    readonly rootHex: string;
    readonly trie: ValueNotPreservedLedgerTrieHandleV1;
    /** Per spend input, in field-0 order: the facts step-02 folds. */
    readonly spentInputs: readonly {
      readonly input: MidgardTxInput;
      readonly descriptorCbor: string;
      readonly spentValue: MidgardValue;
    }[];
  };
};

/**
 * Materializes a committed native transaction with caller-chosen structured
 * outputs (§5.5) and mint items (§5.6), commits it into a transactions MPF
 * beside one honest decoy leaf, and files each spent input's genuine
 * `LedgerOutputCommitmentV1` descriptor in a fresh pre-state ledger MPF —
 * the header committed for the scenario carries that trie's root as
 * `prev_utxos_root`.
 *
 * `validity: "TxIsInvalid"` builds the §1.4 negative — a transaction the
 * operator honestly recorded as a no-op, which the family must never convict
 * however unbalanced its value equation looks.
 */
export const buildValueNotPreservedFixtureV1 = async ({
  spentInputs,
  outputs,
  mintItems = [],
  fee = 1_000_000n,
  validity = "TxIsValid",
}: {
  readonly spentInputs: readonly ValueNotPreservedFixtureSpentInputV1[];
  readonly outputs: readonly MidgardTxOutput[];
  readonly mintItems?: readonly MidgardMintPolicyItemV1[];
  readonly fee?: bigint;
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): Promise<ValueNotPreservedFixtureV1> => {
  const spendItems = spentInputs.map(({ input }) =>
    Buffer.from(encodeMidgardTxInputCanonicalV1(input)),
  );
  const outputItems = encodeMidgardFieldItemsV1({
    fieldIndex: 2,
    items: outputs,
  });
  const mintItemBuffers = encodeMidgardFieldItemsV1({
    fieldIndex: 5,
    items: mintItems,
  });
  const spendInputsPreimageCbor = encodeCbor(spendItems);
  const outputsPreimageCbor = encodeCbor([...outputItems]);
  const badTx: MidgardNativeTxFullV1 =
    materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
      validity,
      body: {
        spendInputsPreimageCbor,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor:
          mintItemBuffers.length === 0
            ? EMPTY_CBOR_LIST
            : encodeCbor([...mintItemBuffers]),
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        fee,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: 0n,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: encodeCbor([
          Buffer.from("f1".repeat(32), "hex"),
        ]),
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
  // One honest decoy leaf, so the membership proof has at least one step.
  const decoyTx = makeNativeTx({
    spendInputCbors: [
      Buffer.from(encodeMidgardTxInputCanonicalV1(vnpOutRefV1("dd", 0))),
    ],
    fee: 5n,
  });
  const badTxId = computeMidgardNativeTxIdV1(badTx).toString("hex");
  const badTxCompactCbor = Buffer.from(
    encodeMidgardNativeTxCompactV1(badTx.compact),
  ).toString("hex");
  const badTxSourceCbor = l2TransactionSourceCborV1(badTx);
  const decoyTxSourceCbor = l2TransactionSourceCborV1(decoyTx);
  const decoyTxId = computeMidgardNativeTxIdV1(decoyTx).toString("hex");
  if (decoyTxId === badTxId) {
    throw new Error("fixture decoy collides with the disputed transaction");
  }
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await txTrie.insert(
    Buffer.from(decoyTxId, "hex"),
    Buffer.from(decoyTxSourceCbor, "hex"),
  );
  const proof = await txTrie.prove(Buffer.from(badTxId, "hex"));
  const txMembershipProofCbor = proof.toCBOR().toString("hex");

  // The pre-state ledger: one genuine descriptor per spent input, derived
  // from the spent output's own bytes so lovelace, asset count and the asset
  // frontier commitment all agree with the value the witness walks.
  const ledgerStore = new Store(undefined);
  await ledgerStore.ready();
  const ledgerTrie = new Trie(ledgerStore);
  const ledgerSpentInputs: {
    readonly input: MidgardTxInput;
    readonly descriptorCbor: string;
    readonly spentValue: MidgardValue;
  }[] = [];
  for (const { input, spentValue } of spentInputs) {
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: Number(input.output_index),
      outputCbor: encodeMidgardTxOutput({
        address: VNP_LEDGER_OUTPUT_ADDRESS_V1,
        value: spentValue,
      }),
    });
    await ledgerTrie.insert(
      encodeMidgardTxInputCanonicalV1(input),
      material.descriptorCbor,
    );
    ledgerSpentInputs.push({
      input,
      descriptorCbor: material.descriptorCbor.toString("hex"),
      spentValue,
    });
  }
  // Decoy siblings, so no membership proof is over a single-leaf trie.
  for (let index = 0; index < 2; index += 1) {
    await ledgerTrie.insert(
      Buffer.concat([Buffer.alloc(37, 0xee), Buffer.from([index])]),
      Buffer.from([0xd0 + index]),
    );
  }
  const ledgerRootHex = trieRootHex(ledgerTrie);

  return {
    transactionsRoot: trieRootHex(txTrie),
    l2TransactionCount: 2n,
    nativeTxId: badTxId,
    nativeTxCompactCbor: badTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(badTx.compact),
      nativeTxCompactCbor: badTxCompactCbor,
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: trieRootHex(txTrie),
      txMembershipProof: Data.from(txMembershipProofCbor, Proof),
      txMembershipProofCbor,
    },
    spendInputsPreimageCbor,
    outputsPreimageCbor,
    outputs,
    mintItems,
    ledger: {
      rootHex: ledgerRootHex,
      trie: {
        rootHex: ledgerRootHex,
        prove: async (key: Buffer) =>
          Buffer.from((await ledgerTrie.prove(key)).toCBOR()),
      },
      spentInputs: ledgerSpentInputs,
    },
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeValueNotPreservedEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realValueNotPreserved: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.valueNotPreserved;
  const category = harness.catalogue.categories.valueNotPreserved;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the value-not-preserved contracts/category",
    );
  }
  if (
    category.categoryId !== FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.valueNotPreserved
  ) {
    throw new Error("Unexpected value-not-preserved catalogue category id");
  }
  return { ...harness, family, category };
};

export type ValueNotPreservedHarnessV1 = Awaited<
  ReturnType<typeof makeValueNotPreservedEmulatorHarnessV1>
>;

/**
 * Commits the fraudulent header as the SECOND block after an anchor block
 * carrying the fixture ledger's root as `utxos_root`.
 *
 * The state-queue commit validator chains `prev_utxos_root` exactly: the
 * first block after genesis must carry the genesis sentinel's (empty)
 * `utxo_root`, so a committed header whose `prev_utxos_root` is the
 * fixture's pre-state ledger root — the commitment every step-02 value
 * witness authenticates against — can only exist as a successor of a block
 * whose `utxos_root` IS that root. The setup therefore commits:
 *
 * - anchor block A: empty L2 material, `utxos_root = fixture.ledger.rootHex`;
 * - fraudulent block B: the fixture's counted `transactions_root`,
 *   `prev_utxos_root = fixture.ledger.rootHex`, chained to A by hash and
 *   time exactly as `commit_block_header_carries_previous_block_v1`
 *   demands.
 */
export const setupValueNotPreservedScenarioV1 = async ({
  harness,
  fixture,
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly fixture: ValueNotPreservedFixtureV1;
}) => {
  const {
    emulator,
    funderLucid,
    proverLucid,
    realBlueprint,
    contracts,
    family,
    catalogue,
    nonceUtxo,
  } = harness;
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScriptsV1({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: family.computationThread.mintingScript,
      fraudProofMintingScript: family.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const baseAnchorHeader = makeHeader(funderKeyHash, headerStartTime);
  const anchorHeader = {
    ...baseAnchorHeader,
    endTime:
      baseAnchorHeader.startTime + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS_V1),
    utxosRoot: fixture.ledger.rootHex,
  };
  const anchorSetup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: anchorHeader,
  });
  const successorStart = emulatorSuccessorHeaderStartV1({
    predecessorEndTime: anchorHeader.endTime,
    emulator,
  });
  const header = {
    ...makeHeader(
      funderKeyHash,
      successorStart,
      await countedTransactionsRoot(
        fixture.transactionsRoot,
        fixture.l2TransactionCount,
      ),
      fixture.l2TransactionCount,
    ),
    prevHeaderHash: anchorSetup.headerHash,
    prevUtxosRoot: fixture.ledger.rootHex,
  };
  const commit = await commitHeaderAfterAnchorBlockV1({
    harness,
    anchorBlockOutRef: anchorSetup.fraudulentBlockOutRef,
    header,
  });
  const setup = {
    ...anchorSetup,
    fraudulentBlockOutRef: commit.blockOutRef,
    headerHash: commit.headerHash,
    stateQueueBlockUnit: commit.stateQueueBlockUnit,
    anchorHeaderHash: anchorSetup.headerHash,
    anchorBlockOutRef: anchorSetup.fraudulentBlockOutRef,
    anchorBlockUnit: anchorSetup.stateQueueBlockUnit,
    witnessReferenceScripts,
  };
  return { header, anchorHeader, setup };
};

/**
 * Commits `header` appended after the queued anchor block — the same
 * production-shaped commit transaction the harness setup submits for its
 * first block, re-anchored at a node instead of the confirmed-state root.
 */
const commitHeaderAfterAnchorBlockV1 = async ({
  harness,
  anchorBlockOutRef,
  header,
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly anchorBlockOutRef: string;
  readonly header: HeaderV1;
}): Promise<{
  readonly headerHash: string;
  readonly blockOutRef: string;
  readonly stateQueueBlockUnit: string;
}> => {
  const lucid = harness.funderLucid;
  const { contracts } = harness;
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const anchorUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(anchorBlockOutRef, "--anchor-block-out-ref"),
    label: "value-not-preserved anchor block UTxO",
  });
  const anchorStateQueueUtxo = await Effect.runPromise(
    utxoToStateQueueUTxO(anchorUtxo, contracts.stateQueue.policyId),
  );
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetworkV1,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    unit: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
    label: "value-not-preserved commit hub oracle",
  });
  const schedulerUtxo = await requireSingletonUtxo({
    lucid,
    address: contracts.scheduler.spendingScriptAddress,
    unit: toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    label: "value-not-preserved commit scheduler",
  });
  const correctionLockUtxo = await requireSingletonUtxo({
    lucid,
    address: contracts.correctionLock.spendingScriptAddress,
    unit: toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME),
    label: "value-not-preserved commit correction lock",
  });
  const activeOperatorNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  );
  const activeOperatorNode = await requireSingletonUtxo({
    lucid,
    address: contracts.activeOperators.spendingScriptAddress,
    unit: activeOperatorNodeUnit,
    label: "value-not-preserved commit active-operator node",
  });
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  if (commitValidTo <= BigInt(harness.emulator.now())) {
    throw new Error(
      "value-not-preserved successor commit validTo expired before submission",
    );
  }
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
            "value-not-preserved commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "value-not-preserved commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "value-not-preserved commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "value-not-preserved commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  // Mirrors the harness setup's own commit: the funder wallet's first UTxO
  // funds the fee (the funder is the emulator's operator party; its change
  // may legitimately carry operator-side units).
  const [feeInput] = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.datum == null && utxo.datumHash == null && utxo.scriptRef == null,
  );
  if (feeInput === undefined) {
    throw new Error(
      "value-not-preserved second commit found no funder fee UTxO",
    );
  }
  const [confirmedStateRefInput] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME),
  );
  if (confirmedStateRefInput === undefined) {
    throw new Error(
      "value-not-preserved second commit found no confirmed-state root witness",
    );
  }
  const commitYieldRef = await findStateQueueYieldReferenceScriptV1({
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
        anchorUTxO: anchorStateQueueUtxo,
        newHeader: header,
        additionalInputs: [feeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: schedulerUtxo,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        additionalRefInputs: [hubOracleUtxo],
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
  const unsigned = await commitTx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const stateQueueBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const [blockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueBlockUnit,
  );
  if (blockUtxo === undefined) {
    throw new Error(
      "value-not-preserved fraudulent block missing after the second commit",
    );
  }
  return {
    headerHash,
    blockOutRef: `${blockUtxo.txHash}#${blockUtxo.outputIndex.toString()}`,
    stateQueueBlockUnit,
  };
};

/**
 * Publishes all four step validators as reference scripts (production
 * deployment shape per the standing reference-script ruling). Every emulator
 * transaction that spends a step sources its witness from these.
 */
export const publishValueNotPreservedReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: ValueNotPreservedContractsV1;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `value-not-preserved step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The honest thread, one call: init → bind → fold* → finish [→ 03 [→ 04]]
// ---------------------------------------------------------------------------

export type ValueNotPreservedThreadRunV1 = Awaited<
  ReturnType<typeof runValueNotPreservedThreadV1>
>;

/**
 * Runs the honest submitters over a committed scenario, capturing per-step
 * emulator measurements. `through` picks the stopping point so adversarial
 * suites can park the thread mid-chain and attack from there.
 */
export const runValueNotPreservedThreadV1 = async ({
  harness,
  fixture,
  setup,
  refs,
  claimedAsset,
  claimedDirection,
  through = "step04",
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly fixture: ValueNotPreservedFixtureV1;
  readonly setup: {
    readonly fraudulentBlockOutRef: string;
    readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  };
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly claimedAsset: ClaimedAssetV1;
  readonly claimedDirection: ClaimedImbalanceDirectionV1;
  readonly through?: "finish" | "step03" | "step04";
}) => {
  const { emulator, proverLucid, proverSigner, family, category } = harness;
  const measurements: Record<string, CompleteSignedTransactionMeasurement> = {};
  const catalogue = {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  };

  const initCapture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedInit({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      network: emulatorNetworkV1,
      contracts: family,
      category,
      catalogue,
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["init"] = initCapture.measurement;
  const init = initCapture.result;

  const step01Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep01({
      lucid: proverLucid,
      blueprint: harness.realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network: emulatorNetworkV1,
      signer: proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      claimedAsset,
      claimedDirection,
      prevUtxosRoot: fixture.ledger.rootHex,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["step-01"] = step01Capture.measurement;
  const step01 = step01Capture.result;

  const spendInputsOpening = spendInputsOpeningV1({
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    spendInputsPreimageCbor: fixture.spendInputsPreimageCbor,
  });
  let threadOutRef = step01.nextThreadOutRef;
  for (const [index, spent] of fixture.ledger.spentInputs.entries()) {
    const valueWitness = await buildSpentInputValueWitnessV1({
      claim: claimedAsset,
      descriptorCbor: spent.descriptorCbor,
      spentValue: spent.spentValue,
      trie: fixture.ledger.trie,
      input: spent.input,
      prevUtxosRootHex: fixture.ledger.rootHex,
    });
    const foldCapture = await captureEmulatorSubmission(emulator, async () =>
      submitValueNotPreservedStep02Fold({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef,
        spendInputsOpening,
        valueWitness,
        referenceScriptUtxo: refs[1],
      }),
    );
    measurements[`step-02-fold-${index.toString()}`] = foldCapture.measurement;
    threadOutRef = foldCapture.result.nextThreadOutRef;
  }

  const finishCapture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep02Finish({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef,
      spendInputsOpening,
      spendInputCount: BigInt(fixture.ledger.spentInputs.length),
      referenceScriptUtxo: refs[1],
    }),
  );
  measurements["step-02-finish"] = finishCapture.measurement;
  const finish = finishCapture.result;
  if (through === "finish") {
    return { init, step01, finish, measurements };
  }

  const step03Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep03({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: finish.nextThreadOutRef,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      outputs: fixture.outputs,
      mintItems: claimedAsset === "AdaAsset" ? null : fixture.mintItems,
      referenceScriptUtxo: refs[2],
    }),
  );
  measurements["step-03"] = step03Capture.measurement;
  const step03 = step03Capture.result;
  if (through === "step03") {
    return { init, step01, finish, step03, measurements };
  }

  const step04Capture = await captureEmulatorSubmission(emulator, async () =>
    submitValueNotPreservedStep04({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    }),
  );
  measurements["step-04"] = step04Capture.measurement;
  return {
    init,
    step01,
    finish,
    step03,
    step04: step04Capture.result,
    measurements,
  };
};

// ---------------------------------------------------------------------------
// Tampered tier-2 publication
// ---------------------------------------------------------------------------

/**
 * Publishes `bytes` under the exact §8.5 nothing-but-bytes publication datum
 * at the prover's own address — the shape a genuine tier-2 carriage UTxO
 * has, with bytes that do NOT hash to the committed field commitment. The
 * honest content-addressed resolution can never pick it up
 * (`resolveChunkReferenceIndicesV1` matches by exact datum bytes), so the
 * adversarial suite injects it positionally via the step-03 submitter's
 * test-only escape hatch and watches the §8.8 door's
 * `field_commitment(preimage) == expected_hash` re-hash refuse it.
 */
export const publishTamperedFieldPreimagePublicationV1 = async ({
  harness,
  bytes,
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly bytes: Uint8Array;
}): Promise<UTxO> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  signer.selectWallet(lucid);
  const datum = fieldPreimagePublicationDatumCborV1(bytes);
  const tx = await lucid
    .newTx()
    .pay.ToAddressWithData(
      signer.address,
      { kind: "inline", value: datum },
      { lovelace: 80_000_000n },
    )
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const utxos = await lucid.utxosAt(signer.address);
  const published = utxos.find(
    (utxo) =>
      utxo.txHash === txHash &&
      utxo.datum != null &&
      utxo.datum.toLowerCase() === datum.toLowerCase(),
  );
  if (published === undefined) {
    throw new Error("Tampered publication UTxO not found after confirmation");
  }
  return published;
};

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite can watch the VALIDATOR refuse
// (see `expectOnchainRefusalV1`). Production code never takes these paths.
// ---------------------------------------------------------------------------

/**
 * A raw step-01 bind: the honest submitter's inclusion transaction minus its
 * §1.4 acceptance-gate re-check, so an honestly-rejected committed leaf
 * reaches the validator's own `validity_code == 0` refusal.
 */
export const submitRawValueNotPreservedBindV1 = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claimedAsset,
  claimedDirection,
  prevUtxosRoot,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claimedAsset: ClaimedAssetV1;
  readonly claimedDirection: ClaimedImbalanceDirectionV1;
  readonly prevUtxosRoot: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "raw value-not-preserved state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetworkV1,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "raw value-not-preserved hub oracle",
  });
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    emulatorNetworkV1,
    phasMembershipScript,
  );
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw value-not-preserved PHAS membership",
  });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw value-not-preserved step-01",
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const foldState: ValueNotPreservedStep02State = {
    bad_tx_id: txInclusion.nativeTxId,
    claimed_asset: claimedAsset,
    claimed_direction: claimedDirection,
    committed_fee: txInclusion.nativeTx.body.fee,
    prev_utxos_root: prevUtxosRoot,
    input_cursor: 0n,
    claimed_delta: 0n,
  };
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: foldState },
    ValueNotPreservedStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw value-not-preserved bind");
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "raw value-not-preserved bind",
              ),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw value-not-preserved bind output",
              ),
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo,
                "raw value-not-preserved hub oracle",
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                stateQueueBlockUtxo,
                "raw value-not-preserved state-queue node",
              ),
              native_tx_id: txInclusion.nativeTxId,
              l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
              transactions_phas_root: txInclusion.transactionsPhasRoot,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  "raw value-not-preserved PHAS membership",
                ),
            },
            claimed_asset: claimedAsset,
            claimed_direction: claimedDirection,
          },
        ],
      },
      ValueNotPreservedStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueBlockUtxo,
      ...stepCarriage.referenceInputs,
      ...phasMembershipCarriage.referenceInputs,
    ])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(stepCarriage.attach(base));
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * A raw step-04 finalize: the honest submitter's thread-burn/token-mint
 * transaction minus its local conviction twin, so a thread whose completed
 * fold the validator must refuse — balanced, or unbalanced against the
 * claimed direction — reaches the exact
 * `value_not_preserved_fault_is_established_v1` check on-chain.
 */
export const submitRawValueNotPreservedFinalizeV1 = async ({
  harness,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: ValueNotPreservedHarnessV1;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 3,
      threadOutRef,
    });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw value-not-preserved finalize");
    const outputIndex = ctx.outputs.findIndex(
      (output) => output.address === contracts.fraudProof.spendingScriptAddress,
    );
    if (outputIndex < 0) {
      throw new Error("raw finalize built no fraud-proof output");
    }
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw value-not-preserved finalize",
            ),
            output_index: BigInt(outputIndex),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw value-not-preserved fraud-proof",
            ),
          },
        ],
      },
      ValueNotPreservedStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw value-not-preserved thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw value-not-preserved fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw value-not-preserved thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw value-not-preserved computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw value-not-preserved fraud-proof mint",
  });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw value-not-preserved step-04",
  });
  const referenceInputs = [
    ...stepCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const withReferences = base.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepCarriage.attach(withReferences)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
