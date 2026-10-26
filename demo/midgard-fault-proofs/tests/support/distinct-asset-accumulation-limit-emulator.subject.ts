/** Committed `distinctAssetAccumulationLimit` subjects for genuine-Init
 * emulator runs: a key-signed transaction whose spent inputs, outputs and
 * mint carry chosen asset entry counts, committed in a block whose
 * predecessor ledger root the event pre-root names. */
import {
  compareMidgardAssetKeyBytes,
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
  type MidgardValue,
} from "@al-ft/midgard-core";
import {
  computeHash32,
  deriveMidgardNativeTxBodyCompact,
  encodeMidgardForcedTxCanonical,
  type MidgardNativeTxBodyCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/index.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../../src/ledger-at.js";
import { nodeForcedVerdict } from "./emulator/node-forced-verdict.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";

/** The producer refuses an asset entry once this many precede it. */
export const ENTRY_BOUND = Number(
  SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_MAX_ENTRIES,
);

/** Most assets a policy holds here: one-byte names, `00` to `ff`. */
export const POLICY_ASSETS = 256;

const SIGNER = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x07));

/** The enterprise key address every spent output and output pays. */
export const SIGNER_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(SIGNER.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

const policyId = (seed: number, index: number): string => {
  const policy = Buffer.alloc(28, 0x33);
  policy.writeUInt8(seed & 0xff, 0);
  policy.writeUInt16BE(index, 1);
  return policy.toString("hex");
};

/** `count` distinct one-byte asset names, in key order. */
const assetNames = (count: number): Buffer[] => {
  if (count < 0 || count > POLICY_ASSETS)
    throw new Error(`a policy holds 0..${POLICY_ASSETS.toString()} assets`);
  return Array.from({ length: count }, (_, index) => Buffer.from([index])).sort(
    compareMidgardAssetKeyBytes,
  );
};

/** One policy per count, each holding that many one-unit assets. */
export const policiesValue = (
  lovelace: bigint,
  policies: readonly number[],
  seed: number,
): MidgardValue => ({
  lovelace,
  assets: new Map(
    policies.map((count, index) => [
      policyId(seed, index),
      new Map(
        assetNames(count).map((name) => [name.toString("hex"), 1n] as const),
      ),
    ]),
  ),
});

/** `entries` asset entries split into full policies and a remainder. */
export const policiesOf = (entries: number): number[] => [
  ...Array.from(
    { length: Math.floor(entries / POLICY_ASSETS) },
    () => POLICY_ASSETS,
  ),
  ...(entries % POLICY_ASSETS === 0 ? [] : [entries % POLICY_ASSETS]),
];

export type DistinctAssetSubjectShape = {
  /** Each spent input's asset entry count. */
  readonly inputs: readonly number[];
  /** Key-locked reference inputs whose out-refs sort below every spend, so
   * each spend's schedule position is its ordinal plus this count. */
  readonly references?: number;
  /** Each output's per-policy asset counts. */
  readonly outputs: readonly (readonly number[])[];
  /** Each mint policy's asset count. */
  readonly mint: readonly number[];
};

const outRef = (fill: number, outputIndex: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, fill), outputIndex });

const SPENT_LOVELACE = 100_000_000n;
const OUTPUT_LOVELACE = 50_000_000n;

export const spentEntries = (shape: DistinctAssetSubjectShape): LedgerEntry[] =>
  shape.inputs.map((entries, index) => ({
    outRef: outRef(0x41, index),
    output: encodeMidgardTxOutput({
      address: SIGNER_ADDRESS,
      value: policiesValue(SPENT_LOVELACE, policiesOf(entries), 0x10 + index),
    }),
  }));

export const referenceEntries = (
  shape: DistinctAssetSubjectShape,
): LedgerEntry[] =>
  Array.from({ length: shape.references ?? 0 }, (_, index) => ({
    outRef: outRef(0x11, index),
    output: encodeMidgardTxOutput({
      address: SIGNER_ADDRESS,
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  }));

export const buildDistinctAssetSubject = (
  shape: DistinctAssetSubjectShape,
): MidgardNativeTxFull => {
  const redeemerTxWitsPreimageCbor = encodeCbor([]);
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: encodeCbor(
      spentEntries(shape).map((entry) => entry.outRef),
    ),
    referenceInputsPreimageCbor: encodeCbor(
      referenceEntries(shape).map((entry) => entry.outRef),
    ),
    outputsPreimageCbor: encodeCbor(
      shape.outputs.map((policies, index) =>
        encodeMidgardTxOutput({
          address: SIGNER_ADDRESS,
          value: policiesValue(OUTPUT_LOVELACE, policies, 0x80 + index),
        }),
      ),
    ),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeCbor([]),
    requiredSignersPreimageCbor: encodeCbor([]),
    mintPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: shape.mint.map((count, index) => ({
        policyId: Buffer.from(policyId(0xf0, index), "hex"),
        assets: assetNames(count).map((assetName) => ({
          assetName,
          quantity: 1n,
        })),
      })),
    }),
    // No redeemer and no script: the absent hash is the expected one.
    scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
      computeHash32(redeemerTxWitsPreimageCbor),
      [],
    ),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  };
  // An address witness signs the body alone.
  const bodyHash = computeMidgardNativeTxId({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompact(body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  return materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor([
        Buffer.from(
          CML.make_vkey_witness(
            CML.TransactionHash.from_raw_bytes(bodyHash),
            SIGNER,
          ).to_cbor_bytes(),
        ),
      ]),
      scriptTxWitsPreimageCbor: encodeCbor([]),
      redeemerTxWitsPreimageCbor,
    },
  });
};

const predecessorEntries = (shape: DistinctAssetSubjectShape) => [
  ...spentEntries(shape),
  ...referenceEntries(shape),
];

/** The verdict the node's forced classifier writes for the subject, against
 * the subject's own committed predecessor outputs. */
export const machineForcedVerdict = async (
  shape: DistinctAssetSubjectShape,
): Promise<SDK.OperatorVerdict> => {
  const nativeTx = buildDistinctAssetSubject(shape);
  return await nodeForcedVerdict({
    transactionId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    forcedCanonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(nativeTx)),
    ledger: predecessorEntries(shape).map(
      (entry) => [entry.outRef, entry.output] as const,
    ),
  });
};

export type DistinctAssetSource =
  | { readonly kind: "normal" }
  | { readonly kind: "forced"; readonly verdict: SDK.OperatorVerdict };

export const rejectedFor = (
  reason: SDK.RejectionReason,
): DistinctAssetSource => ({
  kind: "forced",
  verdict: { ForcedTxInvalid: { reason } },
});

export const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "ce".repeat(32),
  outputIndex: 0n,
};

/** The subject's committed block and derived material, without an L1
 * commit: the header hash is the one a commit would name. */
export const buildDistinctAssetBlock = async ({
  shape,
  source,
  operatorVkey = "aa".repeat(28),
  startTime = 1_000_000n,
  prevHeaderHash,
}: {
  readonly shape: DistinctAssetSubjectShape;
  readonly source: DistinctAssetSource;
  readonly operatorVkey?: string;
  readonly startTime?: bigint;
  readonly prevHeaderHash?: string;
}) => {
  const nativeTx = buildDistinctAssetSubject(shape);
  const entries = predecessorEntries(shape);
  const root = await ledgerEntriesRoot(entries);
  const predecessor = await checkLedgerAgainstRoot({
    entries,
    utxosRoot: root,
    label: "distinct-asset-accumulation-limit predecessor",
  });
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: root,
    prevUtxosRoot: root,
    ...(prevHeaderHash === undefined ? {} : { prevHeaderHash }),
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: FORCED_ORDER_KEY,
            verdict: source.verdict,
          },
  });
  const eventKey: SDK.EventKey =
    source.kind === "normal"
      ? { L2TransactionEventKey: { tx_id: block.nativeTxId } }
      : { ForcedTransactionEventKey: { tx_order_id: FORCED_ORDER_KEY } };
  const material = await prepareDirectScriptMaterial({
    current: block.reconstruction,
    eventKey,
    predecessor,
  });
  return {
    nativeTx,
    block,
    material,
    root,
    headerHash: await Effect.runPromise(SDK.hashBlockHeader(block.header)),
  };
};

type SetupFraudulentBlockArgs = Parameters<typeof setupFraudulentBlock>[0];

/** Commits a queued anchor block whose UTxO root is the subject's
 * predecessor root, then the subject's block after it, and derives its
 * material. */
export const setupDistinctAssetScenario = async ({
  harness,
  shape,
  source,
}: {
  readonly harness: {
    readonly funderLucid: SetupFraudulentBlockArgs["funderLucid"];
    readonly emulator: SetupFraudulentBlockArgs["emulator"];
    readonly contracts: SetupFraudulentBlockArgs["contracts"];
    readonly nonceUtxo: UTxO;
    readonly catalogue: SetupFraudulentBlockArgs["catalogue"];
  };
  readonly shape: DistinctAssetSubjectShape;
  readonly source: DistinctAssetSource;
}) => {
  const root = await ledgerEntriesRoot(predecessorEntries(shape));
  const anchor = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture: {
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      l2TransactionCount: 0n,
      utxosRoot: root,
      headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
    },
  });
  const built = await buildDistinctAssetBlock({
    shape,
    source,
    operatorVkey: anchor.header.operatorVkey,
    startTime: BigInt(
      emulatorSuccessorHeaderStart({
        predecessorEndTime: anchor.header.endTime,
        emulator: harness.emulator,
      }),
    ),
    prevHeaderHash: anchor.headerHash,
  });
  const target = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: anchor.stateQueueBlockUnit,
    header: built.block.header,
    hubOracle: anchor.hubOracle,
    scheduler: anchor.scheduler,
    activeOperatorNode: anchor.activeOperatorNode,
    activeOperatorNodeUnit: anchor.activeOperatorNodeUnit,
  });
  return {
    ...built,
    setup: {
      ...anchor,
      header: target.header,
      headerHash: target.successorHeaderHash,
      fraudulentBlockOutRef: target.successorOutRef,
      stateQueueBlockUnit: target.successorBlockUnit,
      activeOperatorNode: target.activeOperatorNode,
    },
  };
};

export type DistinctAssetScenario = Awaited<
  ReturnType<typeof setupDistinctAssetScenario>
>;
