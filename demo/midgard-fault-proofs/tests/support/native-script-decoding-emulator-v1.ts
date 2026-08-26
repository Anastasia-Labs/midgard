/**
 * Shared emulator fixtures for the `native-script-decoding` family (#635,
 * #633), offchain plan §8.2 suites 4–7.
 *
 * Three things every decoding emulator scenario needs and none of the
 * existing helpers produce:
 *
 * 1. **A committed block whose transition trace carries the accused step.**
 *    `tests/helpers/canonical-block-evidence-fixture-v1.ts` emits an empty
 *    transition trace, but this family's step-02 opens the event→step leaf
 *    AND the transition step whose `pre_utxos_root` becomes the thread's
 *    `prior_ledger_root`. {@link buildDecodingBlockFixtureV1} assembles the
 *    whole `DaPayloadV1` — counted roots, dense trace, forced leaf and its
 *    DA preimage — the way `tests/transition-trace-challenger.test.ts` does,
 *    but under an emulator-committable header.
 * 2. **A pre-state ledger trie holding the accused outpoint's descriptor.**
 *    {@link buildDecodingLedgerFixtureV1} files a
 *    `MidgardLedgerOutputCommitmentV1` under the §5.3 38-byte out-ref key.
 *    The reference-script facts are supplied rather than derived, because the
 *    whole premise of a direction-A decoding fault is a descriptor the
 *    operator admitted over bytes the canonical builder would have refused.
 * 3. **The four step validators published as reference scripts.** Design §10
 *    Q3: step-03's applied body alone is 25,767 bytes, so no step of this
 *    family can inline-attach.
 *
 * The payload fixtures are deliberately tiny (§8.2's "a handful of nodes"):
 * the multi-chunk direction-A item crosses exactly one 4,095-byte chunk
 * boundary and refuses after three primitive steps, so one Scan transaction
 * and one Verdict cover the whole machine route.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputMaterialV1,
  decodeMidgardLedgerOutputCommitmentV1,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
  MIDGARD_PROTOCOL_V1_VERSION,
  type MidgardLedgerOutputCommitmentFactsV1,
  type MidgardLedgerOutputReferenceScriptLanguageV1,
} from "@al-ft/midgard-core";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonicalV1,
  faultProofStepRedeemerSchema,
  FraudProofComputationThreadRedeemer,
  type MidgardTxInput,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../../src/field-opening-v1.js";
import type { NativeScriptDecodingContractsV1 } from "../../src/native-script-decoding/contracts-v1.js";
import type { NativeScriptDecodingLedgerTrieHandleV1 } from "../../src/native-script-decoding/evidence-v1.js";
import {
  buildNativeScriptDecodingLedgerMembershipV1,
  nativeScriptDecodingOutpointKeyV1,
  nativeScriptDecodingSubjectFieldIndexV1,
} from "../../src/native-script-decoding/evidence-v1.js";
import {
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1,
  type NativeScriptDecodingProverDepsV1,
  type NativeScriptDecodingProverEventV1,
  type NativeScriptDecodingProverPolicyV1,
} from "../../src/native-script-decoding/prover-v1.js";
import { requireNativeScriptDecodingThreadUtxoV1 } from "../../src/native-script-decoding/submit-common-v1.js";
import type { RemoveFraudulentBlockExplicitCategory } from "../../src/remove-fraudulent-block.js";
import {
  type ResolvedProverSigner,
  resolveProverSigner,
} from "../../src/runtime.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  selectFeeInput,
} from "../../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  encodeData,
  reconstructDaPayloadV1,
  type TransitionTraceReconstruction,
} from "../../src/transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  NATIVE_SCRIPT_DECODING_REMOVAL_DEPLOYMENT_ENTRY_V1,
  NATIVE_SCRIPT_DECODING_TEST_CATEGORY_ID_V1,
  network as emulatorNetworkV1,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Reference-script item payloads (§8.2's "handful of nodes")
// ---------------------------------------------------------------------------

/** The single key hash every fixture native script signs under. */
export const DECODING_SIGNER_KEY_V1 = Buffer.alloc(28, 0x55);

const SIGNATURE_NODE_HEX = `8200581c${DECODING_SIGNER_KEY_V1.toString("hex")}`;

/** Wrap a payload as the §5.3 versioned tag-0 item (`[0, payload-bytes]`). */
export const decodingItemFromPayloadV1 = (payload: Buffer): Buffer => {
  const head =
    payload.length <= 23
      ? Buffer.from([0x40 + payload.length])
      : payload.length < 256
        ? Buffer.from([0x58, payload.length])
        : Buffer.from([
            0x59,
            (payload.length >> 8) & 0xff,
            payload.length & 0xff,
          ]);
  return Buffer.concat([Buffer.from("8200", "hex"), head, payload]);
};

/**
 * `all(sig, <undecodable>)` padded past one chunk boundary: the machine
 * advances three primitive steps (container token, leaf token, frame pop) and
 * refuses the fourth token. Two chunks, so every window the plan carries is
 * the mandatory chunk-plus-next shape.
 */
export const decodingMalformedMultiChunkItemV1 = (): Buffer => {
  const core = Buffer.from(`820182${SIGNATURE_NODE_HEX}820700`, "hex");
  return decodingItemFromPayloadV1(
    Buffer.concat([core, Buffer.alloc(4_100 - core.length, 0)]),
  );
};

/** `all(sig)`: canonical, four primitive steps, one chunk. */
export const decodingCanonicalItemV1 = (): Buffer =>
  decodingItemFromPayloadV1(
    encodeMidgardNativeScript({
      type: "all",
      scripts: [{ type: "sig", keyHash: DECODING_SIGNER_KEY_V1 }],
    }),
  );

/** A wrapper whose language tag is outside {0, 3, 128}: malformed at bind. */
export const decodingMalformedWrapperItemV1 = (): Buffer =>
  Buffer.from("8201410a", "hex");

/** A tag-3 (Plutus) item: the direction-B descriptor contradiction. */
export const decodingPlutusItemV1 = (): Buffer =>
  Buffer.from("82034401020304", "hex");

// ---------------------------------------------------------------------------
// Pre-state ledger trie
// ---------------------------------------------------------------------------

const LEDGER_OUTPUT_ADDRESS_V1 = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x99),
]);

/** A plain key-hash output, the descriptor's carrier for every fixture. */
const fixtureOutputCbor = (): Buffer =>
  encodeMidgardTxOutput({
    address: LEDGER_OUTPUT_ADDRESS_V1,
    value: { lovelace: 5_000_000n, assets: new Map() },
  });

export type DecodingLedgerFixtureV1 = {
  readonly descriptorCbor: string;
  readonly rootHex: string;
  readonly trie: NativeScriptDecodingLedgerTrieHandleV1;
  readonly outpointKey: Buffer;
};

/**
 * Files the accused outpoint's descriptor in a fresh MPF, with the
 * reference-script facts named rather than derived: a direction-A fault is
 * precisely a descriptor whose committed reference-script item the canonical
 * builder would refuse to decode.
 */
export const buildDecodingLedgerFixtureV1 = async ({
  txIdHex,
  outputIndex,
  referenceScriptItemBytes,
  referenceScriptLanguage,
  siblings = 1,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptLanguage: Exclude<
    MidgardLedgerOutputReferenceScriptLanguageV1,
    -1
  >;
  readonly siblings?: number;
}): Promise<DecodingLedgerFixtureV1> => {
  const outputCbor = fixtureOutputCbor();
  const base = buildCanonicalMidgardLedgerOutputMaterialV1({
    outputIndex,
    outputCbor,
  });
  const item = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
    itemIndex: outputIndex,
    bytes: referenceScriptItemBytes,
  });
  const {
    version: _version,
    outputIndex: _outputIndex,
    totalLength: _totalLength,
    itemCommitment: _itemCommitment,
    ...baseFacts
  } = base.descriptor;
  const facts: MidgardLedgerOutputCommitmentFactsV1 = {
    ...baseFacts,
    referenceScriptLanguage,
    referenceScriptHash: Buffer.alloc(28, 0x5a),
    referenceScriptTotalLength: referenceScriptItemBytes.length,
    referenceScriptItemCommitment: item.commitment,
  };
  const material = buildMidgardLedgerOutputMaterialV1({
    outputIndex,
    outputCbor,
    facts,
  });
  const outpointKey = nativeScriptDecodingOutpointKeyV1({
    txIdHex,
    outputIndex,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outpointKey, material.descriptorCbor);
  for (let index = 0; index < siblings; index += 1) {
    await trie.insert(
      Buffer.concat([Buffer.alloc(37, 0xee), Buffer.from([index])]),
      Buffer.from([0xd0 + index]),
    );
  }
  const rootHex = (trie.hash as Buffer).toString("hex");
  return {
    descriptorCbor: material.descriptorCbor.toString("hex"),
    rootHex,
    outpointKey,
    trie: {
      rootHex,
      prove: async (target: Buffer) =>
        Buffer.from((await trie.prove(target)).toCBOR()),
    },
  };
};

// ---------------------------------------------------------------------------
// The committed block
// ---------------------------------------------------------------------------

/** A minimal native transaction with the named spend/reference input items. */
export const decodingSubjectTransactionV1 = ({
  spendInputCbors = [],
  referenceInputCbors = [],
  fee = 0n,
}: {
  readonly spendInputCbors?: readonly Buffer[];
  readonly referenceInputCbors?: readonly Buffer[];
  readonly fee?: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputCbors]),
      referenceInputsPreimageCbor: encodeCbor([...referenceInputCbors]),
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const bufferEntries = (entries: readonly SDK.DaPayloadEntry[]) =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export type DecodingSubjectSourceV1 =
  | { readonly kind: "normal"; readonly nativeTx: MidgardNativeTxFullV1 }
  | {
      readonly kind: "forced";
      readonly nativeTx: MidgardNativeTxFullV1;
      readonly orderKey: SDK.OutputReference;
      readonly verdict: SDK.OperatorVerdictV1;
    };

export type DecodingBlockFixtureV1 = {
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly payloadEnvelopeCbor: Buffer;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  /** Direction-A normal-source threads only: the step-01 inclusion evidence. */
  readonly txInclusion: SubmitStep01TxInclusion | null;
  readonly forcedOrderKey: SDK.OutputReference | null;
  readonly transactionsPhasRoot: string;
};

/**
 * The committed block the thread disputes: one event (the accused
 * transaction, normal or forced), one dense transition step carrying
 * `priorLedgerRoot` as its `pre_utxos_root`, and the matching event→step and
 * validation-trace leaves so `header_v1_is_valid` admits the header.
 */
export const buildDecodingBlockFixtureV1 = async ({
  operatorVkey,
  startTime,
  priorLedgerRoot,
  subject,
  decoyTransactionCount = 0,
}: {
  readonly operatorVkey: string;
  readonly startTime: bigint;
  readonly priorLedgerRoot: string;
  readonly subject: DecodingSubjectSourceV1;
  /**
   * Extra committed L2 transactions, present only to give the header's
   * `transactions_root` more than one leaf: a single-leaf MPF proof has zero
   * steps, and the #545 published-chunk carriage has nothing to publish.
   */
  readonly decoyTransactionCount?: number;
}): Promise<DecodingBlockFixtureV1> => {
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(subject.nativeTx);
  const nativeTxId = computeMidgardNativeTxIdV1(subject.nativeTx).toString(
    "hex",
  );
  const compactCbor = encodeMidgardNativeTxCompactV1(subject.nativeTx.compact);

  let transactions: SDK.DaPayloadEntry[] = [];
  let transactionPreimages: SDK.DaPayloadEntry[] = [];
  let forcedTransactions: SDK.DaPayloadEntry[] = [];
  let forcedTransactionPreimages: SDK.DaPayloadEntry[] = [];
  let eventKey: SDK.EventKey;
  const phase: SDK.TransitionPhase =
    subject.kind === "normal" ? "L2Transaction" : "ForcedTransaction";

  if (subject.kind === "normal") {
    // The DA payload files the §2.4 proof-source triple, while the header's
    // `transactions_root` commits the compact bytes step-01's inclusion proof
    // opens (`buildCanonicalBlockFixtureV1`'s `nativeCompact` mode).
    const source =
      deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
    const sourceValue: SDK.L2TransactionSourceV1 = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    transactions = [
      [
        nativeTxId,
        encodeData(
          sourceValue,
          SDK.L2TransactionSourceV1Schema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [[nativeTxId, canonicalCbor.toString("hex")]];
    eventKey = { L2TransactionEventKey: { tx_id: nativeTxId } };
  } else {
    // §2.4.3(e): a rejected forced leaf commits the operator-ADJUDICATED
    // source, while the DA preimage stays the submitted canonical bytes. The
    // adjudicated triple is what the thread's `verified_tx_id` and every
    // downstream replay must key off — never the submitted one.
    const source =
      subject.verdict === "ForcedTxValid"
        ? deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor)
        : deriveMidgardNativeTxProofSourceV1(
            adjudicateMidgardNativeTxFullV1Validity(
              decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor),
              "TxIsInvalid",
            ),
          );
    const leaf: SDK.ForcedInclusionTxV1 = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: subject.verdict,
    };
    const key = encodeData(subject.orderKey, SDK.OutputReference as never);
    forcedTransactions = [
      entry(key, encodeData(leaf, SDK.ForcedInclusionTxV1 as never)),
    ];
    forcedTransactionPreimages = [entry(key, canonicalCbor)];
    eventKey = { ForcedTransactionEventKey: { tx_order_id: subject.orderKey } };
  }

  // The header commits the compact bytes for L2 transactions, because that is
  // what step-01's `transactions_root` inclusion proof opens; the payload's
  // own entries stay the §2.4 proof-source triples the reconstruction reads.
  const nativeCompactTransactions: SDK.DaPayloadEntry[] =
    subject.kind === "normal"
      ? [[nativeTxId, compactCbor.toString("hex")]]
      : [];

  // Decoys are ordinary committed L2 transactions. They exist only so the
  // transactions trie holds more than one leaf.
  // The placeholder validation-trace descriptor is stamped by the COMMITTED
  // LEAF's verdict, not by what a replay would conclude — the #640
  // convention. A `ForcedTxInvalid` leaf therefore carries `Rejected`, so a
  // direction-B fixture never presents a block that accepts and rejects the
  // same event at once.
  const events: {
    readonly eventKey: SDK.EventKey;
    readonly phase: SDK.TransitionPhase;
    readonly verdict: "Accepted" | "Rejected";
  }[] = [
    {
      eventKey,
      phase,
      verdict:
        subject.kind === "forced" && subject.verdict !== "ForcedTxValid"
          ? "Rejected"
          : "Accepted",
    },
  ];
  for (let index = 0; index < decoyTransactionCount; index += 1) {
    const decoy = decodingSubjectTransactionV1({ fee: BigInt(5_000 + index) });
    const decoyCanonical = encodeMidgardNativeTxCanonicalV1(decoy);
    const decoyId = computeMidgardNativeTxIdV1(decoy).toString("hex");
    const decoySource =
      deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(decoyCanonical);
    transactions = [
      ...transactions,
      [
        decoyId,
        encodeData(
          {
            tx_id: decoyId,
            source: {
              compact_cbor: decoySource.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                decoySource.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                decoySource.fieldPreimageLengthsCbor.toString("hex"),
            },
          } satisfies SDK.L2TransactionSourceV1,
          SDK.L2TransactionSourceV1Schema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [
      ...transactionPreimages,
      [decoyId, decoyCanonical.toString("hex")],
    ];
    nativeCompactTransactions.push([
      decoyId,
      encodeMidgardNativeTxCompactV1(decoy.compact).toString("hex"),
    ]);
    events.push({
      eventKey: { L2TransactionEventKey: { tx_id: decoyId } },
      phase: "L2Transaction",
      verdict: "Accepted",
    });
  }

  const transitionTrace: SDK.DaPayloadEntry[] = [];
  const eventToStep: SDK.DaPayloadEntry[] = [];
  const validationTraces: SDK.DaPayloadEntry[] = [];
  for (const [stepIndex, event] of events.entries()) {
    const step: SDK.TransitionStep = {
      schema_version: SDK.TRANSITION_STEP_V1_SCHEMA_VERSION,
      step_index: BigInt(stepIndex),
      event_key: event.eventKey,
      phase: event.phase,
      pre_utxos_root: priorLedgerRoot,
      post_utxos_root: priorLedgerRoot,
    };
    transitionTrace.push(
      entry(
        encodeData(step.step_index, Data.Integer() as never),
        encodeData(step, SDK.TransitionStepSchema as never),
      ),
    );
    eventToStep.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            step_index: BigInt(stepIndex),
            phase: event.phase,
          } satisfies SDK.EventToStepValue,
          SDK.EventToStepValueSchema as never,
        ),
      ),
    );
    validationTraces.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: "1a".repeat(32),
            step_count: 1n,
            initial_state_hash: "1b".repeat(32),
            terminal_state_hash: "1c".repeat(32),
            verdict: event.verdict,
            rejection_code_hash: "1d".repeat(32),
          } satisfies SDK.ValidationTraceDescriptorV1,
          SDK.ValidationTraceDescriptorV1Schema as never,
        ),
      ),
    );
  }

  const nativeCompactRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    bufferEntries(nativeCompactTransactions),
  );
  const utxoRoot = await keyValuePhasRootWithCount([]);
  const roots = {
    withdrawals: await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, []),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      bufferEntries(forcedTransactions),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      bufferEntries(transactions),
    ),
    deposits: await buildCountedRoot(SDK.ROOT_DOMAINS.deposits, []),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      bufferEntries(transitionTrace),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      bufferEntries(eventToStep),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      bufferEntries(validationTraces),
    ),
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: 0n,
    totalEventCount: BigInt(events.length),
    transitionStepCount: BigInt(events.length),
    validationTraceCount: BigInt(events.length),
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: nativeCompactRoot.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime,
    endTime: startTime + 1_000n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  // The two `transactions_root` conventions this repo already carries pull in
  // opposite directions, and a decoding thread needs both: step-01's on-chain
  // inclusion proof opens `(tx_id -> compact_cbor)` (the
  // `nativeCompact` convention `buildCanonicalBlockFixtureV1` documents),
  // while `reconstructDaPayloadV1` re-derives the root over the payload's
  // §2.4 proof-source leaves and refuses a header that disagrees. Step-02
  // opens only the transition-trace and event-to-step roots, which are
  // identical under both, so the fixture reconstructs against the
  // payload-source twin and then re-points the reconstruction at the header
  // the block actually commits. Every root the family opens is unchanged.
  const sourceHeader: SDK.HeaderV1 = {
    ...header,
    transactionsRoot: roots.transactions.root,
  };
  const sourceHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeaderV1(sourceHeader),
  );
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: sourceHeaderHash,
      header: sourceHeader,
      utxos: [],
      withdrawals: [],
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      deposits: [],
      transition_trace: sorted(transitionTrace),
      event_to_step: sorted(eventToStep),
      transaction_preimages: sorted(transactionPreimages),
      forced_transaction_preimages: sorted(forcedTransactionPreimages),
      cek_program_material: [],
      validation_traces: sorted(validationTraces),
      counts,
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    SDK.encodeDaPayloadV1(payload),
    { mode: "identity" },
  );
  const sourceReconstruction = await reconstructDaPayloadV1({
    payloadEnvelopeCbor,
    expectedHeaderHash: sourceHeaderHash,
    committedHeader: sourceHeader,
  });
  const reconstruction: TransitionTraceReconstruction = {
    ...sourceReconstruction,
    header,
    headerHash,
    roots: {
      ...sourceReconstruction.roots,
      transactionsRoot: header.transactionsRoot,
    },
    rootData: {
      ...sourceReconstruction.rootData,
      transactions: nativeCompactRoot,
    },
  };

  let txInclusion: SubmitStep01TxInclusion | null = null;
  if (subject.kind === "normal") {
    const membership = await keyValuePhasProof(
      { ...nativeCompactRoot, root: nativeCompactRoot.phasRoot },
      Buffer.from(nativeTxId, "hex"),
      compactCbor,
    );
    const proofCbor = Data.to(membership, SDK.Proof);
    txInclusion = {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(subject.nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      transactionsPhasRoot: nativeCompactRoot.phasRoot,
      txMembershipProof: membership,
      txMembershipProofCbor: proofCbor,
    };
  }

  return {
    header,
    headerHash,
    payloadEnvelopeCbor,
    reconstruction,
    nativeTxId,
    nativeTxCompactCbor: compactCbor.toString("hex"),
    txInclusion,
    forcedOrderKey: subject.kind === "forced" ? subject.orderKey : null,
    transactionsPhasRoot: nativeCompactRoot.phasRoot,
  };
};

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/**
 * The decoding-family harness: the real four-step chain built from the
 * regenerated blueprint and registered as the extra catalogue category the
 * design's §10 Q2 slot expects.
 */
export const makeDecodingEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realNativeScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
    // The #545 published-chunk carriage withdraws from the merkelized
    // verifier's reward account, which must be registered before any step
    // takes that route.
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const decoding = harness.contracts.nativeScriptDecoding;
  const category = harness.catalogue.extraCategories.nativeScriptDecoding;
  if (decoding === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the native-script-decoding contracts/category",
    );
  }
  if (category.categoryId !== NATIVE_SCRIPT_DECODING_TEST_CATEGORY_ID_V1) {
    throw new Error("Unexpected native-script-decoding catalogue category id");
  }
  // The adversarial suite needs a THIRD party — a wallet that is neither the
  // funder nor the prover, and that must never be able to drive or cancel
  // somebody else's thread. It starts empty; `fundDecodingOutsiderV1` fills
  // it once the setup transaction has consumed the harness nonce UTxO.
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network: emulatorNetworkV1,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return { ...harness, decoding, category, outsiderLucid, outsiderSigner };
};

/**
 * The explicit removal-category record for the family (#635): the removal
 * submitter cannot resolve a pre-registration category through the SDK's
 * canonical builders, so the harness hands it the already-resolved facts —
 * the test-registered category id, the step-01 hash the manifest entry pins,
 * and the shared fraud-proof pair the chain was parameterized with. The
 * spend-script hash rides the harness's shared `fraudProof` contracts because
 * the family record deliberately carries only the pair's policy id and
 * address; the two are the same deployment (see `contracts.ts`).
 */
export const decodingRemovalCategoryV1 = (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>,
): RemoveFraudulentBlockExplicitCategory => ({
  name: "nativeScriptDecoding",
  categoryId: harness.category.categoryId,
  firstStepDeploymentEntry: NATIVE_SCRIPT_DECODING_REMOVAL_DEPLOYMENT_ENTRY_V1,
  firstStepScriptHash: harness.decoding.steps[0].spendingScriptHash,
  fraudProof: {
    policyId: harness.decoding.fraudProof.policyId,
    spendingScriptHash: harness.contracts.fraudProof.spendingScriptHash,
    spendingScriptAddress: harness.decoding.fraudProof.spendingScriptAddress,
  },
});

/**
 * Publishes all four step validators as reference scripts (design §10 Q3).
 * Step-03's 25,767-byte applied body needs the oversized publication shape;
 * publishing every step the same way keeps the four consuming transactions
 * uniform.
 */
export const publishDecodingReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NativeScriptDecodingContractsV1;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `native-script-decoding step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The scenario: a committed block plus the pre-state ledger that resolves the
// accused outpoint, standing on the emulator and ready for Init.
// ---------------------------------------------------------------------------

/** The accused outpoint every scenario files, fixed so ids stay readable. */
export const DECODING_ACCUSED_TX_ID_V1 = "ab".repeat(32);

export type DecodingScenarioSourceV1 =
  | { readonly kind: "normal" }
  | {
      readonly kind: "forced";
      readonly verdict: SDK.OperatorVerdictV1;
      readonly orderKey?: SDK.OutputReference;
    };

export type DecodingScenarioV1 = {
  readonly ledger: DecodingLedgerFixtureV1;
  readonly block: DecodingBlockFixtureV1;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly subjectFieldInputs: readonly MidgardTxInput[];
  readonly accusedSourceKind: bigint;
  readonly referenceScriptItemBytes: Buffer;
};

/**
 * Commits the disputed block on the emulator over a pre-state ledger holding
 * the accused outpoint's descriptor. `accusedSourceKind` picks the §2.5 field
 * the accused ordinal indexes (0 = spend inputs, 1 = reference inputs); the
 * accused outpoint always sits at ordinal 0 of that field, which is the only
 * ordinal any in-domain scenario names.
 */
export const setupDecodingScenarioV1 = async ({
  harness,
  referenceScriptItemBytes,
  referenceScriptLanguage = 0,
  source,
  accusedSourceKind = 1n,
  accusedOutputIndex = 0,
  decoyTransactionCount = 0,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>;
  readonly referenceScriptItemBytes: Buffer;
  readonly referenceScriptLanguage?: Exclude<
    MidgardLedgerOutputReferenceScriptLanguageV1,
    -1
  >;
  readonly source: DecodingScenarioSourceV1;
  readonly accusedSourceKind?: bigint;
  readonly accusedOutputIndex?: number;
  /** Extra committed L2 transactions, so the transactions trie proves in steps. */
  readonly decoyTransactionCount?: number;
}): Promise<DecodingScenarioV1> => {
  const { emulator, funderLucid, contracts, catalogue, nonceUtxo } = harness;
  const ledger = await buildDecodingLedgerFixtureV1({
    txIdHex: DECODING_ACCUSED_TX_ID_V1,
    outputIndex: accusedOutputIndex,
    referenceScriptItemBytes,
    referenceScriptLanguage,
  });
  const accused: MidgardTxInput = {
    tx_id: DECODING_ACCUSED_TX_ID_V1,
    output_index: BigInt(accusedOutputIndex),
  };
  const accusedCbor = encodeMidgardTxInputCanonicalV1(accused);
  const nativeTx = decodingSubjectTransactionV1(
    accusedSourceKind === 0n
      ? { spendInputCbors: [accusedCbor], fee: 1_000n }
      : { referenceInputCbors: [accusedCbor], fee: 1_000n },
  );
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
      1,
  );
  const block = await buildDecodingBlockFixtureV1({
    operatorVkey: funderKeyHash,
    startTime,
    priorLedgerRoot: ledger.rootHex,
    decoyTransactionCount,
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: source.orderKey ?? {
              transactionId: "cd".repeat(32),
              outputIndex: 0n,
            },
            verdict: source.verdict,
          },
  });
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: block.header,
  });
  return {
    ledger,
    block,
    setup,
    subjectFieldInputs: [accused],
    accusedSourceKind,
    referenceScriptItemBytes,
  };
};

// ---------------------------------------------------------------------------
// The §4.3 proving core, wired to a scenario
// ---------------------------------------------------------------------------

/** The emulator has no L1 depth or maturity to observe; both gates are off. */
export const DECODING_EMULATOR_PROVER_POLICY_V1: NativeScriptDecodingProverPolicyV1 =
  {
    ...NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1,
    minSettlementDepth: 0n,
    maturityGuardFactor: 0,
    maxThreadBudgetLovelace: null,
  };

/**
 * The proving core's dependencies for a scenario: every §4.3 evidence
 * callback is answered from the fixture, so what the core drives on chain is
 * exactly the committed material.
 */
export const decodingProverDepsV1 = ({
  harness,
  scenario,
  referenceScriptItemBytes,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>;
  readonly scenario: DecodingScenarioV1;
  /** `null` for the routes that never scan an item (§7.2, contradiction). */
  readonly referenceScriptItemBytes: Uint8Array | null;
  readonly referenceScriptUtxos?: NativeScriptDecodingProverDepsV1["referenceScriptUtxos"];
  readonly journal?: (event: NativeScriptDecodingProverEventV1) => void;
}): NativeScriptDecodingProverDepsV1 => ({
  lucid: harness.proverLucid,
  blueprint: harness.realBlueprint,
  network: emulatorNetworkV1,
  contracts: harness.decoding,
  category: harness.category,
  catalogue: {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  },
  signer: harness.proverSigner,
  evidence: {
    txInclusion: async () => {
      const inclusion = scenario.block.txInclusion;
      if (inclusion === null) {
        throw new Error("this scenario's source is forced; it binds no leaf");
      }
      return inclusion;
    },
    reconstruction: async () => scenario.block.reconstruction,
    subjectTx: async () => ({
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
    }),
    descriptor: async () => ({
      descriptorCbor: scenario.ledger.descriptorCbor,
      referenceScriptItemBytes,
    }),
    ledgerTrie: async () => scenario.ledger.trie,
  },
  observations: {},
  journal: journal ?? (() => undefined),
  policy: DECODING_EMULATOR_PROVER_POLICY_V1,
  referenceScriptUtxos,
});

/**
 * Every step's spend redeemer shares the `Cancel` head; the raw builders
 * below never encode a `Continue` through this schema, so the argument
 * schema is irrelevant.
 */
const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type RawCancelSpendRedeemer = Data.Static<typeof RawCancelSpendRedeemerSchema>;
const RawCancelSpendRedeemer =
  RawCancelSpendRedeemerSchema as unknown as RawCancelSpendRedeemer;

/** The thread layout a raw redeemer builder is handed. */
export type RawDecodingStepLayoutV1 = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

/**
 * A test-only raw thread advancement — the same transaction shape the
 * step-02/step-03 submitters build (fee input, thread spend, advanced state
 * paid to `destinationAddress`, carriage read as reference inputs, Q3
 * step-script sourcing), with NONE of their fail-closed pre-checks.
 *
 * The adversarial suite needs this because every attack it exercises is one
 * the honest submitters refuse locally, before anything is paid for. To
 * observe the ON-CHAIN refusal — the check that actually protects an honest
 * operator against a prover who patched their own tooling — the transaction
 * has to be built past those guards. Production code never takes this path.
 */
export const submitRawDecodingStepV1 = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  destinationAddress,
  nextDatumCbor,
  buildRedeemer,
  carriageUtxos = [],
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly destinationAddress: string;
  readonly nextDatumCbor: string;
  readonly buildRedeemer: (layout: RawDecodingStepLayoutV1) => string;
  readonly carriageUtxos?: readonly UTxO[];
  readonly referenceScriptUtxo?: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const outputMatches = computationThreadOutputPredicate({
    address: destinationAddress,
    datum: nextDatumCbor,
    unit: threadUnit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding step");
    return buildRedeemer({
      inputIndex: requireInputIndex(ctx, threadUtxo, "raw decoding step"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw decoding step output",
      ),
    });
  }) satisfies BuildTxWithRedeemer;

  const withReferences = (() => {
    const base = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([threadUtxo], redeemer);
    const referenceInputs = [
      ...carriageUtxos,
      ...(referenceScriptUtxo === undefined ? [] : [referenceScriptUtxo]),
    ];
    return referenceInputs.length === 0 ? base : base.readFrom(referenceInputs);
  })();
  const paid = withReferences.pay
    .ToContract(
      destinationAddress,
      { kind: "inline", value: nextDatumCbor },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? paid.attach.SpendingValidator(contracts.steps[stepIndex].spendingScript)
      : paid;

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * A test-only raw `ct.Cancel` — the cancel submitter's transaction without
 * its "only the named prover can cancel" pre-check, so a third party's
 * attempt reaches the validator's own signature demand.
 */
export const submitRawDecodingCancelV1 = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  threadAssetName,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly threadAssetName: string;
  readonly referenceScriptUtxo?: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw decoding cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw decoding cancel burn",
          ),
        },
      },
      RawCancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw decoding cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: threadAssetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadUnit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[stepIndex].spendingScript)
      : base.readFrom([referenceScriptUtxo]);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Funds the harness's third-party wallet with plain, token-free outputs so
 * its own transactions can always pay their fee.
 *
 * Deliberately NOT folded into the harness: the funder's opening UTxO is the
 * one-shot nonce the contracts are parameterised by, and the setup
 * transaction has to be the one that spends it.
 */
export const fundDecodingOutsiderV1 = async (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>,
): Promise<void> => {
  const outsiderAddress = await harness.outsiderLucid.wallet().address();
  const funding = await harness.funderLucid
    .newTx()
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await funding.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

/** What every raw step-03 bind builder below needs from the harness. */
type RawDecodingBindContextV1 = {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly subjectFieldInputs: readonly MidgardTxInput[];
  readonly referenceScriptUtxo: UTxO;
};

/** Reads a step-03 thread's scan state without the submitters' pre-checks. */
const rawStep03StateV1 = async (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>,
  threadOutRef: string,
): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly state: SDK.NativeScriptDecodingScanThreadStateV1;
}> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const datum = Data.from(
    threadUtxo.datum!,
    SDK.NativeScriptDecodingStep03Datum,
  );
  if (datum.data === null) {
    throw new Error("step-03 thread carries no scan state");
  }
  return { threadUtxo, threadUnit: threadToken.unit, state: datum.data };
};

/** Opens the accused field through the §8.8 door for a raw step-03 bind. */
const rawSubjectFieldOpeningV1 = async ({
  harness,
  state,
  nativeTxCompactCbor,
  subjectFieldInputs,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarnessV1>>;
  readonly state: SDK.NativeScriptDecodingScanThreadStateV1;
  readonly nativeTxCompactCbor: string;
  readonly subjectFieldInputs: readonly MidgardTxInput[];
}): Promise<{
  readonly opening: SDK.FieldOpeningV1;
  readonly carriageUtxos: readonly UTxO[];
}> => {
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: nativeScriptDecodingSubjectFieldIndexV1(
      state.outpoint_source_kind,
    ),
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor,
    itemCbors: subjectFieldInputs.map(encodeMidgardTxInputCanonicalV1),
    owner: harness.proverSigner.paymentKeyHash,
    publish: false,
    label: "raw decoding subject field",
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "raw decoding subject field",
  });
  return {
    opening: faultProofFieldOpeningV1({
      planned,
      referenceInputs: carriageUtxos,
      certificatePolicyId: harness.decoding.fieldPreimageCertificatePolicyId,
      label: "raw decoding subject field",
    }),
    carriageUtxos,
  };
};

/**
 * A test-only raw `BindOutOfDomain`, built with NO face classification.
 *
 * The submitter refuses an in-domain pair before it pays for anything
 * (`the accused pair is in-domain — bind it through BindOutpoint instead`);
 * this builder exists so the same close reaches the validator's own
 * neutralisation check — the arm the Aiken selector
 * `decoding_step_03_rejects_an_in_domain_ordinal_close` twins.
 */
export const submitRawDecodingBindOutOfDomainV1 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  referenceScriptUtxo,
}: RawDecodingBindContextV1): Promise<string> => {
  const { threadUtxo, threadUnit, state } = await rawStep03StateV1(
    harness,
    threadOutRef,
  );
  const { opening, carriageUtxos } = await rawSubjectFieldOpeningV1({
    harness,
    state,
    nativeTxCompactCbor,
    subjectFieldInputs,
  });
  return submitRawDecodingStepV1({
    lucid: harness.proverLucid,
    contracts: harness.decoding,
    signer: harness.proverSigner,
    stepIndex: 2,
    threadUtxo,
    threadUnit,
    destinationAddress: harness.decoding.steps[3]!.spendingScriptAddress,
    nextDatumCbor: Data.to(
      {
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: {
          ...state,
          refusal_class: SDK.NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
        },
      },
      SDK.NativeScriptDecodingStep03Datum,
    ),
    buildRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              BindOutOfDomain: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                subject_field_opening: opening,
              },
            },
          ],
        },
        SDK.NativeScriptDecodingStep03SpendRedeemer,
      ),
    carriageUtxos,
    referenceScriptUtxo,
  });
};

/**
 * A test-only raw `BindOutpoint` shaped as the direction-B
 * DESCRIPTOR-CONTRADICTION close — straight to step-04, class-malformed, no
 * first chunk proof and no machine start — while the descriptor it carries
 * is the authentic TAG-0 one the ledger really holds.
 *
 * The contradiction close exists for descriptors that name a non-native
 * language; firing it on a tag-0 descriptor would convict an operator for a
 * payload the canonical decoder is perfectly willing to read. The submitter
 * refuses this plan locally ("the plan claims a descriptor contradiction,
 * but the bound descriptor is tag-0"); this builder carries it to the
 * validator.
 */
export const submitRawDecodingTag0ContradictionCloseV1 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  descriptorCbor,
  ledgerTrie,
  referenceScriptUtxo,
}: RawDecodingBindContextV1 & {
  readonly descriptorCbor: string;
  readonly ledgerTrie: NativeScriptDecodingLedgerTrieHandleV1;
}): Promise<string> => {
  const { threadUtxo, threadUnit, state } = await rawStep03StateV1(
    harness,
    threadOutRef,
  );
  const subjectOutpoint = subjectFieldInputs[Number(state.outpoint_cursor)]!;
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(
    Buffer.from(descriptorCbor, "hex"),
  );
  const outpointKey = nativeScriptDecodingOutpointKeyV1({
    txIdHex: subjectOutpoint.tx_id,
    outputIndex: Number(subjectOutpoint.output_index),
  });
  const ledgerMembershipProof =
    await buildNativeScriptDecodingLedgerMembershipV1({
      trie: ledgerTrie,
      outpointKey,
      priorLedgerRootHex: state.prior_ledger_root,
    });
  const bound = await Effect.runPromise(
    SDK.nativeScriptDecodingBoundScanStateV1({
      state,
      outpointKeyBytes: outpointKey.toString("hex"),
      referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
      outputIndex: BigInt(descriptor.outputIndex),
      referenceScriptTotalLength: BigInt(descriptor.referenceScriptTotalLength),
      referenceScriptItemCommitment:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    }),
  );
  const { opening, carriageUtxos } = await rawSubjectFieldOpeningV1({
    harness,
    state,
    nativeTxCompactCbor,
    subjectFieldInputs,
  });
  return submitRawDecodingStepV1({
    lucid: harness.proverLucid,
    contracts: harness.decoding,
    signer: harness.proverSigner,
    stepIndex: 2,
    threadUtxo,
    threadUnit,
    destinationAddress: harness.decoding.steps[3]!.spendingScriptAddress,
    nextDatumCbor: Data.to(
      {
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: {
          ...bound,
          refusal_class: SDK.NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
        },
      },
      SDK.NativeScriptDecodingStep03Datum,
    ),
    buildRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              BindOutpoint: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                subject_field_opening: opening,
                descriptor_cbor: descriptorCbor,
                ledger_membership_proof: ledgerMembershipProof,
                // The contradiction close reads no chunk window.
                first_chunk_proof: null,
              },
            },
          ],
        },
        SDK.NativeScriptDecodingStep03SpendRedeemer,
      ),
    carriageUtxos,
    referenceScriptUtxo,
  });
};

/**
 * Asserts a transaction the validator must refuse does not land, and that it
 * died IN THE VALIDATOR rather than in the transaction builder.
 *
 * `localUPLCEval: true` runs the script during `.complete()`, so a validator
 * abort surfaces as an evaluator failure here. The `failed script execution`
 * requirement is what keeps these negatives honest: an offchain builder
 * error — a missing fee input, an unresolvable layout — would otherwise read
 * as a passing security assertion.
 */
export const expectOnchainRefusalV1 = async (
  build: () => Promise<unknown>,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined) {
    throw new Error(
      "expected the validator to refuse this transaction, but it succeeded",
    );
  }
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution/u.test(text)) {
    throw new Error(
      `expected an on-chain validator refusal, got a non-validator failure: ${text}`,
    );
  }
  return text;
};
