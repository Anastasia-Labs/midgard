/** Committed `missingRedeemer` subjects for genuine-Init emulator runs: a
 * transaction whose required observers resolve to field-6 script witnesses
 * (and optionally ledger reference scripts), committed in a block whose
 * predecessor ledger root the event pre-root names. */
import {
  EMPTY_NULL_ROOT,
  encodeMidgardRedeemerWitnessItem,
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
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/index.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../../src/ledger-at.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
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

export type MissingRedeemerObserver = {
  readonly script: MidgardVersionedScript;
  /** Carry the script as a field-6 witness; otherwise it must resolve elsewhere. */
  readonly witness?: boolean;
  readonly redeemer: boolean;
};

export type MissingRedeemerSubjectShape = {
  readonly observers: readonly MissingRedeemerObserver[];
  /** Committed predecessor outputs; spent and referenced by the subject. */
  readonly spend?: readonly LedgerEntry[];
  readonly reference?: readonly LedgerEntry[];
  /** Extra Reward redeemers past the observer count (pointer padding). */
  readonly paddingRedeemers?: readonly Buffer[];
  /** Redeemer Data bytes for each observer redeemer. */
  readonly redeemerData?: (index: number) => Buffer;
};

const redeemerItem = (index: bigint, data: Buffer) =>
  encodeMidgardRedeemerWitnessItem({
    purpose: "Reward",
    index,
    redeemerCbor: data,
    executionUnits: { memory: 1_000n, steps: 1_000n },
  });

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
  const redeemers = [
    ...ordered.flatMap((observer, index) =>
      observer.redeemer ? [redeemerItem(BigInt(index), data(index))] : [],
    ),
    ...(shape.paddingRedeemers ?? []).map((item, index) =>
      redeemerItem(BigInt(ordered.length + index), item),
    ),
  ];
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(
        (shape.spend ?? []).map((entry) => entry.outRef),
      ),
      referenceInputsPreimageCbor: encodeCbor(
        (shape.reference ?? []).map((entry) => entry.outRef),
      ),
      outputsPreimageCbor: encodeCbor([]),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: encodeCbor(
        ordered.map((observer) => observer.hash),
      ),
      requiredSignersPreimageCbor: encodeCbor([]),
      mintPreimageCbor: encodeCbor([]),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      // Observers are admitted only on a tagged network.
      networkId: ordered.length === 0 ? MIDGARD_NATIVE_NETWORK_ID_NONE : 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor([]),
      scriptTxWitsPreimageCbor: encodeCbor(
        hashed
          .filter((observer) => observer.witness ?? true)
          .map((observer) => encodeMidgardVersionedScript(observer.script)),
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
): LedgerEntry[] =>
  scripts.map((script, index) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, firstByte + index),
      outputIndex: index,
    }),
    output: encodeMidgardTxOutput({
      address: Buffer.from(`60${"44".repeat(28)}`, "hex"),
      value: { lovelace: 5_000_000n, assets: new Map() },
      ...(script === null ? {} : { script_ref: script }),
    }),
  }));

export type MissingRedeemerSource =
  | { readonly kind: "normal" }
  | { readonly kind: "forced"; readonly verdict: SDK.OperatorVerdict };

export const FORCED_ORDER_KEY: SDK.OutputReference = {
  transactionId: "cd".repeat(32),
  outputIndex: 0n,
};

/** Commits the subject's block on the emulator and derives its material. */
export const setupMissingRedeemerScenario = async ({
  harness,
  shape,
  source,
}: {
  readonly harness: {
    readonly funderLucid: Parameters<typeof funderPaymentKeyHash>[0];
    readonly emulator: { now(): number };
    readonly contracts: Parameters<typeof submitSetupTx>[0]["contracts"];
    readonly nonceUtxo: Parameters<typeof submitSetupTx>[0]["nonceUtxo"];
    readonly catalogue: Parameters<typeof submitSetupTx>[0]["catalogue"];
  };
  readonly shape: MissingRedeemerSubjectShape;
  readonly source: MissingRedeemerSource;
}) => {
  const subject = buildMissingRedeemerSubject(shape);
  const entries = [...(shape.spend ?? []), ...(shape.reference ?? [])];
  const root =
    entries.length === 0
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : await ledgerEntriesRoot(entries);
  const predecessor =
    entries.length === 0
      ? undefined
      : await checkLedgerAgainstRoot({
          entries,
          utxosRoot: root,
          label: "missing-redeemer predecessor",
        });
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime,
    priorLedgerRoot: root,
    prevUtxosRoot: root,
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
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
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
  return { subject, block, setup, material };
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
