/** Committed `missingRedeemer` subjects for genuine-Init emulator runs: a
 * transaction whose required observers, mints, protected outputs and spends
 * resolve to field-6 script witnesses (and optionally ledger reference
 * scripts), committed in a block whose predecessor ledger root the event
 * pre-root names. */
import {
  computeMidgardNativeTxId,
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
} from "@al-ft/midgard-core";
import {
  deriveMidgardNativeTxBodyCompact,
  encodeMidgardForcedTxCanonical,
  type MidgardNativeTxBodyCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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

export const plutusScript = (
  seed: number,
  bytes = 2,
): MidgardVersionedScript => {
  const scriptBytes = Buffer.alloc(bytes, 0x5a);
  scriptBytes.writeUInt16BE(seed & 0xffff, 0);
  return { language: "PlutusV3", scriptBytes };
};

export const nativeScript = (slot: bigint): MidgardVersionedScript => ({
  language: "NativeCardano",
  scriptBytes: Buffer.alloc(0),
  nativeScript: { type: "after", slot },
});

export type MissingRedeemerObserver = MissingRedeemerPurposeScript;

export type MissingRedeemerSubjectShape = MissingRedeemerPurposeShape & {
  readonly observers: readonly MissingRedeemerObserver[];
  /** Committed predecessor outputs; spent and referenced by the subject. */
  readonly spend?: readonly LedgerEntry[];
  readonly reference?: readonly LedgerEntry[];
  /** Committed predecessor outputs the subject neither spends nor
   * references (they deepen the ledger trie). */
  readonly padding?: readonly LedgerEntry[];
  /** Extra Reward redeemers past the observer count (pointer padding). */
  readonly paddingRedeemers?: readonly Buffer[];
  /** Redeemer Data bytes for each observer redeemer. */
  readonly redeemerData?: (index: number) => Buffer;
  /** Signs the body with one address witness. */
  readonly signer?: CML.PrivateKey;
};

/** The subject and its observer coordinates in canonical (hash) order. */
export const buildMissingRedeemerSubject = (
  shape: MissingRedeemerSubjectShape,
): {
  readonly nativeTx: MidgardNativeTxFull;
  /** Observer coordinate index of each entry of `shape.observers`. */
  readonly observerIndex: readonly bigint[];
} => {
  const hashed = shape.observers.map((observer, position) => ({
    ...observer,
    position,
    hash: Buffer.from(hashMidgardVersionedScript(observer.script), "hex"),
  }));
  const ordered = [...hashed].sort((left, right) =>
    Buffer.compare(left.hash, right.hash),
  );
  const observerIndex = hashed.map((observer) =>
    BigInt(ordered.indexOf(observer)),
  );
  const data = shape.redeemerData ?? (() => Buffer.from(Data.to(0n), "hex"));
  const purposes = missingRedeemerPurposeFields(shape, data(0));
  // In purpose-tag order: Spend, Mint, Reward, Receive.
  const redeemers = [
    ...purposes.spendRedeemers,
    ...purposes.mintRedeemers,
    ...ordered.flatMap((observer, index) =>
      observer.redeemer
        ? [redeemerItem("Reward", BigInt(index), data(index))]
        : [],
    ),
    ...(shape.paddingRedeemers ?? []).map((item, index) =>
      redeemerItem("Reward", BigInt(ordered.length + index), item),
    ),
    ...purposes.receiveRedeemers,
  ];
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: encodeCbor(
      (shape.spend ?? []).map((entry) => entry.outRef),
    ),
    referenceInputsPreimageCbor: encodeCbor(
      (shape.reference ?? []).map((entry) => entry.outRef),
    ),
    outputsPreimageCbor: purposes.outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeCbor(
      ordered.map((observer) => observer.hash),
    ),
    requiredSignersPreimageCbor: encodeCbor([]),
    mintPreimageCbor: purposes.mintPreimageCbor,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    // Observers are admitted only on a tagged network.
    networkId: ordered.length === 0 ? MIDGARD_NATIVE_NETWORK_ID_NONE : 0n,
  };
  // An address witness signs the body alone.
  const bodyHash = computeMidgardNativeTxId({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompact(body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor(
        shape.signer === undefined
          ? []
          : [
              Buffer.from(
                CML.make_vkey_witness(
                  CML.TransactionHash.from_raw_bytes(bodyHash),
                  shape.signer,
                ).to_cbor_bytes(),
              ),
            ],
      ),
      scriptTxWitsPreimageCbor: encodeCbor(
        [
          ...hashed
            .filter((observer) => observer.witness ?? true)
            .map((observer) => observer.script),
          ...purposes.witnesses,
        ].map(encodeMidgardVersionedScript),
      ),
      redeemerTxWitsPreimageCbor: encodeCbor(redeemers),
    },
  });
  return { nativeTx, observerIndex };
};

/** Predecessor outputs at enterprise key addresses; `scripts[i]` (when
 * present) is carried as output `i`'s reference script. */
export const missingRedeemerLedgerEntries = (
  firstByte: number,
  scripts: readonly (MidgardVersionedScript | null)[],
  address = Buffer.from(`60${"44".repeat(28)}`, "hex"),
): LedgerEntry[] =>
  scripts.map((script, index) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, firstByte + index),
      outputIndex: index,
    }),
    output: encodeMidgardTxOutput({
      address,
      value: { lovelace: 5_000_000n, assets: new Map() },
      ...(script === null ? {} : { script_ref: script }),
    }),
  }));

const HONEST_SIGNER = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x07));

/** A forced transaction the admission screen accepts: it spends one output
 * its witness signs for, and its Plutus observer is sourced only from a
 * reference input's script, so it carries no script witness and no redeemer.
 * Its first fault is RedeemerMissing at that observer. */
export const honestForcedObserverShape = (
  seed: number,
): MissingRedeemerSubjectShape => {
  const script = plutusScript(seed);
  return {
    observers: [{ script, witness: false, redeemer: false }],
    spend: missingRedeemerLedgerEntries(
      0x21,
      [null],
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(HONEST_SIGNER.to_public().hash()),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    reference: missingRedeemerLedgerEntries(0x31, [script]),
    signer: HONEST_SIGNER,
  };
};

/** The verdict the node's forced classifier writes for the subject, against
 * the subject's own committed predecessor outputs. */
export const machineForcedVerdict = async (
  shape: MissingRedeemerSubjectShape,
): Promise<SDK.OperatorVerdict> => {
  const { nativeTx } = buildMissingRedeemerSubject(shape);
  return await nodeForcedVerdict({
    transactionId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    forcedCanonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(nativeTx)),
    ledger: [...(shape.spend ?? []), ...(shape.reference ?? [])].map(
      (entry) => [entry.outRef, entry.output] as const,
    ),
  });
};

export type MissingRedeemerSource =
  | { readonly kind: "normal" }
  | { readonly kind: "forced"; readonly verdict: SDK.OperatorVerdict };

export const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "cd".repeat(32),
  outputIndex: 0n,
};

/** The ledger root the subject's spend and reference entries resolve in. */
const predecessorEntries = (shape: MissingRedeemerSubjectShape) => [
  ...(shape.spend ?? []),
  ...(shape.reference ?? []),
  ...(shape.padding ?? []),
];

const predecessorRoot = async (
  shape: MissingRedeemerSubjectShape,
  entries: readonly LedgerEntry[] = predecessorEntries(shape),
) => {
  return entries.length === 0
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : await ledgerEntriesRoot(entries);
};

/**
 * Commits the subject's block on the emulator and derives its material. A
 * subject that resolves no committed output commits directly after genesis.
 * One that does commits after a queued anchor block whose UTxO root is its
 * predecessor root, since the genesis state's root is empty and a commit
 * carries its anchor's root over.
 */
export const setupMissingRedeemerScenario = async ({
  harness,
  shape,
  source,
  ledger,
}: {
  readonly harness: {
    readonly funderLucid: SetupFraudulentBlockArgs["funderLucid"];
    readonly emulator: SetupFraudulentBlockArgs["emulator"];
    readonly contracts: SetupFraudulentBlockArgs["contracts"];
    readonly nonceUtxo: UTxO;
    readonly catalogue: SetupFraudulentBlockArgs["catalogue"];
  };
  readonly shape: MissingRedeemerSubjectShape;
  readonly source: MissingRedeemerSource;
  /** The predecessor ledger; every spent, referenced and padding entry when
   * absent. */
  readonly ledger?: readonly LedgerEntry[];
}) => {
  const root = await predecessorRoot(shape, ledger);
  if (root === SDK.EMPTY_MERKLE_TREE_ROOT) {
    const built = await buildMissingRedeemerBlock({
      shape,
      source,
      ...(ledger === undefined ? {} : { ledger }),
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
    return {
      subject: built.subject,
      block: built.block,
      setup,
      material: built.material,
    };
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
  const built = await buildMissingRedeemerBlock({
    shape,
    source,
    ...(ledger === undefined ? {} : { ledger }),
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
    subject: built.subject,
    block: built.block,
    setup: {
      ...anchor,
      header: target.header,
      headerHash: target.successorHeaderHash,
      fraudulentBlockOutRef: target.successorOutRef,
      stateQueueBlockUnit: target.successorBlockUnit,
      activeOperatorNode: target.activeOperatorNode,
    },
    material: built.material,
  };
};

type SetupFraudulentBlockArgs = Parameters<typeof setupFraudulentBlock>[0];

/** The subject's committed block and derived material, without an L1
 * commit: the header hash is the one a commit would name. */
export const buildMissingRedeemerBlock = async ({
  shape,
  source,
  operatorVkey = "aa".repeat(28),
  startTime = 1_000_000n,
  prevHeaderHash,
  ledger,
}: {
  readonly shape: MissingRedeemerSubjectShape;
  readonly source: MissingRedeemerSource;
  readonly operatorVkey?: string;
  readonly startTime?: bigint;
  /** The queued block the header follows; genesis when absent. */
  readonly prevHeaderHash?: string;
  /** The predecessor ledger; every spent, referenced and padding entry when
   * absent. */
  readonly ledger?: readonly LedgerEntry[];
}) => {
  const subject = buildMissingRedeemerSubject(shape);
  const entries = ledger ?? predecessorEntries(shape);
  const root = await predecessorRoot(shape, entries);
  const predecessor =
    entries.length === 0
      ? undefined
      : await checkLedgerAgainstRoot({
          entries,
          utxosRoot: root,
          label: "missing-redeemer predecessor",
        });
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: root,
    prevUtxosRoot: root,
    ...(prevHeaderHash === undefined ? {} : { prevHeaderHash }),
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx: subject.nativeTx }
        : {
            kind: "forced",
            nativeTx: subject.nativeTx,
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
    subject,
    block,
    material,
    predecessor,
    headerHash: await Effect.runPromise(SDK.hashBlockHeader(block.header)),
  };
};

export type MissingRedeemerScenario = Awaited<
  ReturnType<typeof setupMissingRedeemerScenario>
>;

export const redeemerMissingVerdict = (
  purposeKind: bigint,
  purposeIndex: bigint,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: {
    reason: {
      RedeemerMissing: {
        purpose_kind: purposeKind,
        purpose_index: purposeIndex,
      },
    },
  },
});
