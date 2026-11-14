import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  detectTransitionTraceFaults,
  type TransitionTraceDetection,
  type TransitionTraceFaultKind,
} from "../src/transition-trace/index.js";
import {
  buildPayloadFixture,
  depositEventKey,
  forcedEventKey,
  reconstruct,
} from "./ledger-delta-dense-trace-totality.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  entry,
  eventToStepEntry,
  forcedTx,
  forcedTxInvalidPlutus,
  h32,
  nativeMaterial,
  outRef,
  rawLedgerEntry,
  utxoRootWithDescriptors,
} from "./ledger-delta-dense-trace-totality.native-material.js";

// ---------------------------------------------------------------------------
// One probe per fault kind. Each is an independent minimal fixture (no
// shared mutable state), matching the isolation style of the reference
// challenger suite so a probe's assertion is unambiguously attributable to
// its own fixture.
// ---------------------------------------------------------------------------

export type FaultProbe = {
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly run: () => Promise<readonly TransitionTraceDetection[]>;
};

export const countFaultProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  return detectTransitionTraceFaults({
    ...reconstruction,
    header: { ...reconstruction.header, totalEventCount: 1n },
  });
};

export const traceBoundaryProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const depositId = outRef(600);
  const key = depositEventKey(depositId);
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: h32(601),
    deposits: [
      encodedEntry({
        key: depositId,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(602),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "Deposit",
        pre_utxos_root: h32(603),
        post_utxos_root: h32(604),
      },
    ],
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

export const traceLinkProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const idA = outRef(610);
  const idB = outRef(611);
  const keyA = depositEventKey(idA);
  const keyB = depositEventKey(idB);
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: h32(612),
    deposits: [
      encodedEntry({
        key: idA,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(613),
        valueSchema: SDK.DepositInfoSchema,
      }),
      encodedEntry({
        key: idB,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(614),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: keyA,
        phase: "Deposit",
        pre_utxos_root: h32(612),
        post_utxos_root: h32(615),
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: keyB,
        phase: "Deposit",
        pre_utxos_root: h32(616),
        post_utxos_root: h32(617),
      },
    ],
    eventToStep: [
      eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
      eventToStepEntry(keyB, { step_index: 1n, phase: "Deposit" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

export const eventToStepMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const id = outRef(620);
  const key = depositEventKey(id);
  const fixture = await buildPayloadFixture({
    deposits: [
      encodedEntry({
        key: id,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(621),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "Deposit",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    // Present but wrong: isolates event_to_step_matches_trace from the
    // source-membership checks, which only compare fingerprints/phases
    // against the trace step, not against this (deliberately wrong) mapping.
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "Withdrawal" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

export const sourceMembershipMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const material = nativeMaterial(630);
  const source: SDK.L2TransactionSource = {
    tx_id: material.txId,
    source: material.source,
  };
  const key: SDK.EventKey = { L2TransactionEventKey: { tx_id: material.txId } };
  const fixture = await buildPayloadFixture({
    transactions: [
      entry(
        Buffer.from(material.txId, "hex"),
        Buffer.from(Data.to(source, SDK.L2TransactionSource), "hex"),
      ),
    ],
    transactionPreimages: [
      entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        // Wrong on purpose: the real source is L2Transaction.
        phase: "Deposit",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

export const invalidOneStepTransitionProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const txOrderId = outRef(640);
  const finalUtxo = rawLedgerEntry(640);
  const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
  const key = forcedEventKey(txOrderId);
  const fixture = await buildPayloadFixture({
    utxos: [finalUtxo],
    forcedTransactions: [
      encodedEntry({
        key: txOrderId,
        keySchema: SDK.OutputReference as never,
        value: forcedTx(641, forcedTxInvalidPlutus),
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "ForcedTransaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: finalRoot.root,
      },
    ],
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "ForcedTransaction" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};
