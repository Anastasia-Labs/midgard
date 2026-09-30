import * as SDK from "@al-ft/midgard-sdk";

import {
  detectTransitionTraceFaults,
  type TransitionTraceDetection,
} from "../src/transition-trace/index.js";
import {
  buildAcceptedTransactionTransitionMismatchEvidence,
  countFaultProbe,
  eventToStepMismatchProbe,
  type FaultProbe,
  invalidOneStepTransitionProbe,
  sourceMembershipMismatchProbe,
  traceBoundaryProbe,
  traceLinkProbe,
} from "./ledger-delta-dense-trace-totality.build-accepted-transaction-transition-mismatch-evidence.js";
import {
  buildPayloadFixture,
  depositEventKey,
  forcedEventKey,
  reconstruct,
} from "./ledger-delta-dense-trace-totality.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  eventToStepEntry,
  forcedTx,
  forcedTxInvalidPlutus,
  outRef,
} from "./ledger-delta-dense-trace-totality.native-material.js";

const omittedDueL1EventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  return detectTransitionTraceFaults(reconstruction, {
    omittedDueL1Events: [
      {
        kind: "deposit",
        depositId: outRef(650),
      },
    ],
  });
};

const duplicateTraceEventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const txOrderId = outRef(660);
  const key = forcedEventKey(txOrderId);
  const fixture = await buildPayloadFixture({
    forcedTransactions: [
      encodedEntry({
        key: txOrderId,
        keySchema: SDK.OutputReference as never,
        value: forcedTx(661, forcedTxInvalidPlutus),
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
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: key,
        phase: "ForcedTransaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "ForcedTransaction" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const outOfWindowSourceEventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const depositId = outRef(670);
  const key = depositEventKey(depositId);
  const fixture = await buildPayloadFixture({
    deposits: [
      encodedEntry({
        key: depositId,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(671),
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
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture), {
    outOfWindowSourceEvents: [
      {
        kind: "deposit",
        depositId,
      },
    ],
  });
};

const acceptedTransactionTransitionMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  const { evidence } = buildAcceptedTransactionTransitionMismatchEvidence();
  return detectTransitionTraceFaults(reconstruction, {
    acceptedTransactionTransitionMismatches: [evidence],
  });
};

export const probes: readonly FaultProbe[] = [
  {
    kind: "countFault",
    invariant: "header_total_event_count",
    run: countFaultProbe,
  },
  {
    kind: "traceBoundary",
    invariant: "trace_start_prev_utxos_root",
    run: traceBoundaryProbe,
  },
  {
    kind: "traceLink",
    invariant: "adjacent_trace_roots",
    run: traceLinkProbe,
  },
  {
    kind: "eventToStepMismatch",
    invariant: "event_to_step_matches_trace",
    run: eventToStepMismatchProbe,
  },
  {
    kind: "sourceMembershipMismatch",
    invariant: "source_phase_matches_trace_phase",
    run: sourceMembershipMismatchProbe,
  },
  {
    kind: "invalidOneStepTransition",
    invariant: "invalid_forced_transaction_is_no_op",
    run: invalidOneStepTransitionProbe,
  },
  {
    kind: "omittedDueL1Event",
    invariant: "due_l1_event_is_in_source_root",
    run: omittedDueL1EventProbe,
  },
  {
    kind: "duplicateTraceEvent",
    invariant: "trace_event_key_unique",
    run: duplicateTraceEventProbe,
  },
  {
    kind: "outOfWindowSourceEvent",
    invariant: "source_event_is_within_block_window",
    run: outOfWindowSourceEventProbe,
  },
  {
    kind: "acceptedTransactionTransitionMismatch",
    invariant: "accepted_transaction_uses_validated_ledger_root",
    run: acceptedTransactionTransitionMismatchProbe,
  },
];
