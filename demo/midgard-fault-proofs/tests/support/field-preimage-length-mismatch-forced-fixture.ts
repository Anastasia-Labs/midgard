/**
 * Forced-transaction leaves for the field-preimage-length lifecycle whose
 * committed length vector disagrees with the canonical preimage.
 *
 * Whole-block reconstruction refuses such a payload by design (it re-derives
 * every forced source from its canonical preimage), so these leaves cannot
 * come out of `reconstructDaPayload`. The family authenticates them the way
 * its accepted preparer does: straight from the raw retained root entries,
 * with the membership proof opened against the L1 header's counted root.
 */
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKeySchema,
  EventToStepValueSchema,
  type ForcedInclusionTxV1,
  ForcedInclusionTxV1Schema,
  hashBlockHeader,
  type Header,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  type RootMembershipProof,
  TransitionStepSchema,
} from "@al-ft/midgard-sdk/core";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  commitCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  h32,
  makeHeader,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import {
  outputReferenceCbor,
  transitionTraceRawEntry,
} from "./submit-init/transaction-material.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

export const FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX = 0;

export type FieldPreimageLengthForcedFixture = Readonly<{
  header: Header;
  headerHash: string;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedTransaction: ForcedInclusionTxV1;
  /** The canonical field-0 preimage the leaf's body commits to. */
  preimage: Buffer;
  declaredLength: number;
}>;

/**
 * One forced transaction whose leaf carries `verdict` and whose committed
 * length vector is `lengthsMutation` applied to the honest vector. Every other
 * root and count is the same one-event block the shared forced fixture
 * commits, so the header is admitted by the same setup transaction. A
 * rejection names `rejectedFieldIndex`; `proofDepth` commits the leaf under a
 * synthetic membership proof of that many branch levels.
 */
export const buildFieldPreimageLengthForcedFixture = async ({
  operatorVkey,
  now,
  verdict,
  lengthsMutation = (lengths) => lengths,
  rejectedFieldIndex = BigInt(FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX),
  proofDepth,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly verdict: "rejected" | "valid";
  readonly lengthsMutation?: (lengths: number[]) => number[];
  readonly rejectedFieldIndex?: bigint;
  readonly proofDepth?: number;
}): Promise<FieldPreimageLengthForcedFixture> => {
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
    { key: Buffer.from(finalUtxo[0], "hex"), value: finalDescriptor },
  ]);
  const nativeTx = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputCbors: [],
      fee: 0n,
      referenceByte: "b1",
      outputByte: "b2",
      witnessByte: "b8",
    }),
  );
  const source = deriveMidgardForcedTxProofSource(nativeTx);
  const lengths = lengthsMutation([
    ...decodeMidgardNativeTxProofFieldLengths(source.fieldPreimageLengthsCbor),
  ]);
  const forcedTransaction: ForcedInclusionTxV1 = {
    tx_id: computeMidgardNativeTxId(nativeTx).toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
    verdict:
      verdict === "valid"
        ? "ForcedTxValid"
        : {
            ForcedTxInvalid: {
              reason: {
                FieldPreimageLengthMismatch: {
                  field_index: rejectedFieldIndex,
                },
              },
            },
          },
  };
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: forcedTransaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const transitionEntries = [
    transitionTraceDaEntry({
      key: 0n,
      keySchema: Data.Integer() as never,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "ForcedTransaction",
        pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: finalUtxosRoot.root,
      },
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: { step_index: 0n, phase: "ForcedTransaction" },
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const counted = async (
    domain: Parameters<typeof buildCountedRoot>[0],
    entries: readonly (readonly [string, string])[],
  ) =>
    await buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [countedForcedRoot, transitionRoot, eventRoot] = await Promise.all([
    counted(ROOT_DOMAINS.forcedTransactionsV1, forcedEntries),
    counted(ROOT_DOMAINS.transitionTrace, transitionEntries),
    counted(ROOT_DOMAINS.eventToStep, eventEntries),
  ]);
  const [keyHex, valueHex] = forcedEntries[0]!;
  const deep =
    proofDepth === undefined
      ? undefined
      : syntheticDeepMembershipProof({
          key: Buffer.from(keyHex, "hex"),
          value: Buffer.from(valueHex, "hex"),
          branchLevels: proofDepth,
        });
  const forcedRoot =
    deep === undefined
      ? countedForcedRoot
      : {
          ...countedForcedRoot,
          phasRoot: deep.transactionsPhasRoot,
          root: await commitCountedRoot({
            domain: countedForcedRoot.domain,
            phasRoot: deep.transactionsPhasRoot,
            count: countedForcedRoot.count,
          }),
        };
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
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventRoot.root,
    ...counts,
    // The header pins the validation-trace pair; the body keeps its traces.
    validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1> =
    {
      domain: forcedRoot.domain,
      root: forcedRoot.root,
      phas_root: forcedRoot.phasRoot,
      count: forcedRoot.count,
      key: txOrderId,
      value: forcedTransaction,
      proof:
        deep === undefined
          ? await keyValuePhasProof(
              {
                root: forcedRoot.phasRoot,
                count: forcedRoot.count,
                entries: forcedRoot.entries,
              },
              Buffer.from(keyHex, "hex"),
              Buffer.from(valueHex, "hex"),
            )
          : Data.from(deep.proofCbor, Proof),
    };
  return Object.freeze({
    header,
    headerHash,
    membership,
    forcedTransaction,
    preimage: Buffer.from(nativeTx.body.spendInputsPreimageCbor),
    declaredLength: lengths[FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX]!,
  });
};
