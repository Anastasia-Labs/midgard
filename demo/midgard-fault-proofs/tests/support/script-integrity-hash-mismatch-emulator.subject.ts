/** Committed `scriptIntegrityHashMismatch` subjects for genuine-Init emulator
 * runs: a transaction whose spends, mints, required observers and protected
 * outputs resolve to script sources, carrying a chosen script integrity hash,
 * committed in a block whose predecessor ledger root the event pre-root
 * names. */
import {
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
  type MidgardVersionedScript,
  type ScriptLanguageName,
} from "@al-ft/midgard-core";
import {
  computeHash32,
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
  scriptAddress,
} from "./missing-redeemer-emulator.purposes.js";
import { missingRedeemerLedgerEntries } from "./missing-redeemer-emulator.subject.js";
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

/** An undecoded Plutus V3 source with a distinct hash per seed. */
export const plutusScript = (
  seed: number,
  language: "PlutusV3" | "MidgardV1" = "PlutusV3",
): MidgardVersionedScript => {
  const scriptBytes = Buffer.alloc(2, 0x5a);
  scriptBytes.writeUInt16BE(seed & 0xffff, 0);
  return { language, scriptBytes };
};

export const nativeScript = (slot: bigint): MidgardVersionedScript => ({
  language: "NativeCardano",
  scriptBytes: Buffer.alloc(0),
  nativeScript: { type: "after", slot },
});

/** How the subject's body field 9 is formed from its redeemer witness hash. */
export type IntegrityHash =
  | {
      readonly kind: "expected";
      readonly languages: readonly ScriptLanguageName[];
    }
  | { readonly kind: "fixed"; readonly hash: Buffer };

/** The hash the producer expects over `languages`. */
export const expectedHash = (
  ...languages: readonly ScriptLanguageName[]
): IntegrityHash => ({ kind: "expected", languages });

/** A non-empty hash no language set yields. */
export const WRONG_HASH: IntegrityHash = {
  kind: "fixed",
  hash: Buffer.alloc(32, 0xab),
};

export type IntegritySubjectShape = MissingRedeemerPurposeShape & {
  readonly observers?: readonly MissingRedeemerPurposeScript[];
  /** Committed predecessor outputs; spent and referenced by the subject. */
  readonly spend?: readonly LedgerEntry[];
  readonly reference?: readonly LedgerEntry[];
  /** Committed predecessor outputs the subject neither spends nor references. */
  readonly padding?: readonly LedgerEntry[];
  readonly integrity: IntegrityHash;
  /** Signs the body with one address witness. */
  readonly signer?: CML.PrivateKey;
};

const byHash = (entries: readonly MissingRedeemerPurposeScript[]) =>
  entries
    .map((entry) => ({
      ...entry,
      hash: Buffer.from(hashMidgardVersionedScript(entry.script), "hex"),
    }))
    .sort((left, right) => Buffer.compare(left.hash, right.hash));

export const buildIntegritySubject = (
  shape: IntegritySubjectShape,
): MidgardNativeTxFull => {
  const observers = byHash(shape.observers ?? []);
  const data = Buffer.from(Data.to(0n), "hex");
  const purposes = missingRedeemerPurposeFields(shape, data);
  // In purpose-tag order: Spend, Mint, Reward, Receive.
  const redeemerTxWitsPreimageCbor = encodeCbor([
    ...purposes.spendRedeemers,
    ...purposes.mintRedeemers,
    ...observers.flatMap((observer, index) =>
      observer.redeemer ? [redeemerItem("Reward", BigInt(index), data)] : [],
    ),
    ...purposes.receiveRedeemers,
  ]);
  const scriptIntegrityHash =
    shape.integrity.kind === "fixed"
      ? shape.integrity.hash
      : computeScriptIntegrityHashForLanguages(
          computeHash32(redeemerTxWitsPreimageCbor),
          shape.integrity.languages,
        );
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
      observers.map((observer) => observer.hash),
    ),
    requiredSignersPreimageCbor: encodeCbor([]),
    mintPreimageCbor: purposes.mintPreimageCbor,
    scriptIntegrityHash,
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
  return materializeMidgardNativeTxFromCanonical({
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

/** One predecessor output at the enterprise script address of each script. */
export const scriptLockedEntries = (
  firstByte: number,
  scripts: readonly MidgardVersionedScript[],
): LedgerEntry[] =>
  scripts.map((script, index) => ({
    ...missingRedeemerLedgerEntries(firstByte + index, [null])[0]!,
    output: encodeMidgardTxOutput({
      address: scriptAddress(script),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  }));

const HONEST_SIGNER = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x07));

/** A forced transaction L1 admission accepts (`forced_tx_is_admissible_v1`:
 * a spend input, no redeemers, native-only script witnesses) and phases before
 * 10 pass: it spends one key-locked output its one address witness signs for,
 * and carries no script. It walks no language, so its expected hash is the
 * absent one (`expectedHash()`); any other hash is a mismatch. */
export const honestForcedShape = (
  integrity: IntegrityHash,
): IntegritySubjectShape => ({
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
  integrity,
  signer: HONEST_SIGNER,
});

/** The verdict the node's forced classifier writes for the subject, against
 * the subject's own committed predecessor outputs. */
export const machineForcedVerdict = async (
  shape: IntegritySubjectShape,
): Promise<SDK.OperatorVerdict> => {
  const nativeTx = buildIntegritySubject(shape);
  return await nodeForcedVerdict({
    transactionId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    forcedCanonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(nativeTx)),
    ledger: [...(shape.spend ?? []), ...(shape.reference ?? [])].map(
      (entry) => [entry.outRef, entry.output] as const,
    ),
  });
};

export type IntegritySource =
  | { readonly kind: "normal" }
  | { readonly kind: "forced"; readonly verdict: SDK.OperatorVerdict };

export const rejectedFor = (reason: SDK.RejectionReason): IntegritySource => ({
  kind: "forced",
  verdict: { ForcedTxInvalid: { reason } },
});

export const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "ce".repeat(32),
  outputIndex: 0n,
};

const predecessorEntries = (shape: IntegritySubjectShape) => [
  ...(shape.spend ?? []),
  ...(shape.reference ?? []),
  ...(shape.padding ?? []),
];

const predecessorRoot = async (shape: IntegritySubjectShape) => {
  const entries = predecessorEntries(shape);
  return entries.length === 0
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : await ledgerEntriesRoot(entries);
};

/** The subject's committed block and derived material, without an L1
 * commit: the header hash is the one a commit would name. */
export const buildIntegrityBlock = async ({
  shape,
  source,
  operatorVkey = "aa".repeat(28),
  startTime = 1_000_000n,
  prevHeaderHash,
}: {
  readonly shape: IntegritySubjectShape;
  readonly source: IntegritySource;
  readonly operatorVkey?: string;
  readonly startTime?: bigint;
  readonly prevHeaderHash?: string;
}) => {
  const nativeTx = buildIntegritySubject(shape);
  const entries = predecessorEntries(shape);
  const root = await predecessorRoot(shape);
  const predecessor =
    entries.length === 0
      ? undefined
      : await checkLedgerAgainstRoot({
          entries,
          utxosRoot: root,
          label: "script-integrity-hash-mismatch predecessor",
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
export const setupIntegrityScenario = async ({
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
  readonly shape: IntegritySubjectShape;
  readonly source: IntegritySource;
}) => {
  const root = await predecessorRoot(shape);
  if (root === SDK.EMPTY_MERKLE_TREE_ROOT) {
    const built = await buildIntegrityBlock({
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
  const built = await buildIntegrityBlock({
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

export type IntegrityScenario = Awaited<
  ReturnType<typeof setupIntegrityScenario>
>;
