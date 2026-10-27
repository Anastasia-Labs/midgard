/** Committed `executionNativeScriptInvalid` subjects for genuine-Init emulator
 * runs: the script-integrity subjects' purposes, signed by any number of
 * address witnesses (field 7, in its ascending key-hash order) and carrying a
 * chosen validity interval, committed in a block whose predecessor ledger
 * root the event pre-root names. */
import {
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
  type MidgardVersionedScript,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import {
  compareMidgardAddressWitnessItems,
  computeHash32,
  deriveMidgardNativeTxBodyCompact,
  encodeMidgardForcedTxCanonical,
  type MidgardNativeScript,
  type MidgardNativeTxBodyCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/index.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../../src/ledger-at.js";
import { nodeForcedVerdict } from "./emulator/node-forced-verdict.js";
import {
  missingRedeemerPurposeFields,
  type MissingRedeemerPurposeScript,
  type MissingRedeemerPurposeShape,
  redeemerItem,
} from "./missing-redeemer-emulator.purposes.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import type { IntegritySource } from "./script-integrity-hash-mismatch-emulator.subject.js";
import {
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export type NativeSource = IntegritySource;

/** A distinct signing key per seed. */
export const signingKey = (seed: number): CML.PrivateKey => {
  const bytes = Buffer.alloc(32, 0x3c);
  bytes.writeUInt32BE(seed, 0);
  return CML.PrivateKey.from_normal_bytes(bytes);
};

/** The key hash a `sig` leaf names for `key`. */
export const keyHashOf = (key: CML.PrivateKey): Buffer =>
  Buffer.from(key.to_public().hash().to_raw_bytes());

export const native = (
  nativeScript: MidgardNativeScript,
): MidgardVersionedScript => ({
  language: "NativeCardano",
  scriptBytes: Buffer.alloc(0),
  nativeScript,
});

export const sig = (key: CML.PrivateKey): MidgardNativeScript => ({
  type: "sig",
  keyHash: keyHashOf(key),
});

export type NativeSubjectShape = MissingRedeemerPurposeShape & {
  readonly observers?: readonly MissingRedeemerPurposeScript[];
  /** Committed predecessor outputs; spent and referenced by the subject. */
  readonly spend?: readonly LedgerEntry[];
  readonly reference?: readonly LedgerEntry[];
  /** Committed predecessor outputs the subject neither spends nor references. */
  readonly padding?: readonly LedgerEntry[];
  /** Address witnesses over the body, in any order. */
  readonly signers?: readonly CML.PrivateKey[];
  readonly validityStart?: bigint;
  readonly validityEnd?: bigint;
  /** Raw field-7 items carried after the signers' witnesses. */
  readonly rawAddressItems?: readonly Buffer[];
};

const byHash = (entries: readonly MissingRedeemerPurposeScript[]) =>
  entries
    .map((entry) => ({
      ...entry,
      hash: Buffer.from(hashMidgardVersionedScript(entry.script), "hex"),
    }))
    .sort((left, right) => Buffer.compare(left.hash, right.hash));

/** The subject; its script integrity hash is the one the producer expects
 * for the Plutus V3 executions it carries (none when it carries no
 * redeemer). */
export const buildNativeSubject = (
  shape: NativeSubjectShape,
): MidgardNativeTxFull => {
  const observers = byHash(shape.observers ?? []);
  const data = Buffer.from(Data.to(0n), "hex");
  const purposes = missingRedeemerPurposeFields(shape, data);
  const redeemers = [
    ...purposes.spendRedeemers,
    ...purposes.mintRedeemers,
    ...observers.flatMap((observer, index) =>
      observer.redeemer ? [redeemerItem("Reward", BigInt(index), data)] : [],
    ),
    ...purposes.receiveRedeemers,
  ];
  const redeemerTxWitsPreimageCbor = encodeCbor(redeemers);
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: encodeCbor(
      (shape.spend ?? []).map((entry) => entry.outRef),
    ),
    referenceInputsPreimageCbor: encodeCbor(
      (shape.reference ?? []).map((entry) => entry.outRef),
    ),
    outputsPreimageCbor: purposes.outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: shape.validityStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: shape.validityEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeCbor(
      observers.map((observer) => observer.hash),
    ),
    requiredSignersPreimageCbor: encodeCbor([]),
    mintPreimageCbor: purposes.mintPreimageCbor,
    scriptIntegrityHash:
      redeemers.length === 0
        ? computeScriptIntegrityHashForLanguages(
            computeHash32(redeemerTxWitsPreimageCbor),
            [],
          )
        : computeScriptIntegrityHashForLanguages(
            computeHash32(redeemerTxWitsPreimageCbor),
            ["PlutusV3"],
          ),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    // Observers are admitted only on a tagged network.
    networkId: observers.length === 0 ? MIDGARD_NATIVE_NETWORK_ID_NONE : 0n,
  };
  // An address witness signs the body alone.
  const bodyHash = computeMidgardNativeTxId({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompact(body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  const witnesses = (shape.signers ?? [])
    .map((key) =>
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(bodyHash),
          key,
        ).to_cbor_bytes(),
      ),
    )
    .sort(compareMidgardAddressWitnessItems)
    .concat(shape.rawAddressItems ?? []);
  return materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor(witnesses),
      scriptTxWitsPreimageCbor: encodeCbor(
        [
          ...observers
            .filter((observer) => observer.witness ?? true)
            .map((observer) => observer.script),
          ...purposes.witnesses,
        ].map(encodeMidgardVersionedScript),
      ),
      redeemerTxWitsPreimageCbor,
    },
  });
};

/** Predecessor outputs at `key`'s enterprise address; `scripts[i]` (when
 * present) is carried as output `i`'s reference script. */
export const keyLockedEntries = (
  firstByte: number,
  key: CML.PrivateKey,
  scripts: readonly (MidgardVersionedScript | null)[] = [null],
): LedgerEntry[] => {
  const address = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  return scripts.map((script, index) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, firstByte),
      outputIndex: index,
    }),
    output: encodeMidgardTxOutput({
      address,
      value: { lovelace: 5_000_000n, assets: new Map() },
      ...(script === null ? {} : { script_ref: script }),
    }),
  }));
};

/** The key that spends the admissible subjects' one input. */
export const PAYER = signingKey(0);

/**
 * A forced transaction L1 admission accepts (a spend input, no redeemers,
 * native-only witnesses, no validity upper bound): it spends one output
 * `PAYER` signs for, requires one observer per native script, each sourced
 * only from a reference input's script (so field 6 is empty and phase A
 * evaluates none of them), and is signed by `PAYER` and `signers`.
 */
export const admissibleNativeObservers = (
  scripts: readonly MidgardVersionedScript[],
  {
    signers = [],
    validityStart,
  }: {
    readonly signers?: readonly CML.PrivateKey[];
    readonly validityStart?: bigint;
  } = {},
): NativeSubjectShape => {
  const shape: NativeSubjectShape = {
    observers: scripts.map((script) => ({
      script,
      witness: false,
      redeemer: false,
    })),
    spend: keyLockedEntries(0x21, PAYER),
    reference: keyLockedEntries(0x31, PAYER, scripts),
    signers: [PAYER, ...signers],
    ...(validityStart === undefined ? {} : { validityStart }),
  };
  expect(screenMidgardForcedTxAdmission(buildNativeSubject(shape))).toBe(null);
  return shape;
};

/** The observer coordinate index of `script` in a subject requiring
 * `scripts`: observers are listed in script-hash order. */
export const observerIndexOf = (
  scripts: readonly MidgardVersionedScript[],
  script: MidgardVersionedScript,
): bigint =>
  BigInt(
    scripts
      .map(hashMidgardVersionedScript)
      .sort()
      .indexOf(hashMidgardVersionedScript(script)),
  );

/** The verdict the node's forced classifier writes for the subject, against
 * the subject's own committed predecessor outputs. */
export const machineNativeVerdict = async (
  shape: NativeSubjectShape,
): Promise<SDK.OperatorVerdict> => {
  const nativeTx = buildNativeSubject(shape);
  return await nodeForcedVerdict({
    transactionId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    forcedCanonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(nativeTx)),
    ledger: [...(shape.spend ?? []), ...(shape.reference ?? [])].map(
      (entry) => [entry.outRef, entry.output] as const,
    ),
  });
};

export const nativeFalseAt = (
  purposeKind: bigint,
  purposeIndex: bigint,
): SDK.RejectionReason => ({
  ExecutionNativeScriptFalse: {
    purpose_kind: purposeKind,
    purpose_index: purposeIndex,
  },
});

export const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "cf".repeat(32),
  outputIndex: 0n,
};

const predecessorEntries = (shape: NativeSubjectShape) => [
  ...(shape.spend ?? []),
  ...(shape.reference ?? []),
  ...(shape.padding ?? []),
];

const predecessorRoot = async (
  shape: NativeSubjectShape,
  entries: readonly LedgerEntry[] = predecessorEntries(shape),
) =>
  entries.length === 0
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : await ledgerEntriesRoot(entries);

/** The subject's committed block and derived material, without an L1
 * commit: the header hash is the one a commit would name. */
export const buildNativeBlock = async ({
  shape,
  source,
  operatorVkey = "aa".repeat(28),
  startTime = 1_000_000n,
  prevHeaderHash,
  ledger,
}: {
  readonly shape: NativeSubjectShape;
  readonly source: NativeSource;
  readonly operatorVkey?: string;
  readonly startTime?: bigint;
  readonly prevHeaderHash?: string;
  /** The predecessor ledger; the shape's committed outputs by default. */
  readonly ledger?: readonly LedgerEntry[];
}) => {
  const nativeTx = buildNativeSubject(shape);
  const entries = ledger ?? predecessorEntries(shape);
  const root = await predecessorRoot(shape, entries);
  const predecessor =
    entries.length === 0
      ? undefined
      : await checkLedgerAgainstRoot({
          entries,
          utxosRoot: root,
          label: "execution-native-script-invalid predecessor",
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
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  return {
    nativeTx,
    block,
    material,
    headerHash: await Effect.runPromise(SDK.hashBlockHeader(block.header)),
  };
};

type SetupFraudulentBlockArgs = Parameters<typeof setupFraudulentBlock>[0];

/**
 * Commits the subject's block on the emulator and derives its material. A
 * subject that resolves no committed output commits directly after genesis;
 * one that does commits after a queued anchor block whose UTxO root is its
 * predecessor root.
 */
export const setupNativeScenario = async ({
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
  readonly shape: NativeSubjectShape;
  readonly source: NativeSource;
}) => {
  const root = await predecessorRoot(shape);
  if (root === SDK.EMPTY_MERKLE_TREE_ROOT) {
    const built = await buildNativeBlock({
      shape,
      source,
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: built.block.header,
    });
    return { ...built, setup };
  }
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
  const built = await buildNativeBlock({
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

export type NativeScenario = Awaited<ReturnType<typeof setupNativeScenario>>;
