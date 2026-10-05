import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-test-support/hex";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/transition-trace/index.js";
import "./ledger-delta-dense-trace-totality.native-material.js";
import "./ledger-delta-dense-trace-totality.build-payload-fixture.js";
import "./ledger-delta-dense-trace-totality.build-accepted-transaction-transition-mismatch-evidence.js";
import "./ledger-delta-dense-trace-totality.probes.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  detectTransitionTraceFaults,
  TRANSITION_TRACE_FAULT_KINDS,
  type TransitionTraceDetection,
  type TransitionTraceFaultKind,
} from "../src/transition-trace/index.js";
import {
  buildPayloadFixture,
  depositEventKey,
  forcedEventKey,
  reconstruct,
  withdrawalEventKey,
} from "./ledger-delta-dense-trace-totality.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  entry,
  eventToStepEntry,
  forcedTx,
  h32,
  nativeMaterial,
  outRef,
  rawLedgerEntry,
  traceEntry,
  utxoRootWithDescriptors,
  withdrawalInfo,
} from "./ledger-delta-dense-trace-totality.native-material.js";
import { probes } from "./ledger-delta-dense-trace-totality.probes.js";

describe("ledger-delta dense trace totality v1", () => {
  it("reconstructs a dense normal-transaction trace with every intermediate root reproduced and no operation missing or extra", async () => {
    const depositId = outRef(700);
    const withdrawalId = outRef(701);
    const material = nativeMaterial(702);
    const finalDepositId = outRef(703);
    const finalUtxo = rawLedgerEntry(704);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const depositKey = depositEventKey(depositId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const l2Key: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const finalDepositKey = depositEventKey(finalDepositId);
    const l2Source: SDK.L2TransactionSource = {
      tx_id: material.txId,
      source: material.source,
    };

    const r0 = h32(710);
    const r1 = h32(711);
    const r2 = h32(712);
    const r2b = h32(716);
    const r3 = finalRoot.root;

    const steps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: withdrawalKey,
        phase: "Withdrawal",
        pre_utxos_root: r0,
        post_utxos_root: r1,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: l2Key,
        phase: "L2Transaction",
        pre_utxos_root: r1,
        post_utxos_root: r2,
      },
      {
        schema_version: 1n,
        step_index: 2n,
        event_key: depositKey,
        phase: "Deposit",
        pre_utxos_root: r2,
        post_utxos_root: r2b,
      },
      {
        schema_version: 1n,
        step_index: 3n,
        event_key: finalDepositKey,
        phase: "Deposit",
        pre_utxos_root: r2b,
        post_utxos_root: r3,
      },
    ];

    const fixture = await buildPayloadFixture({
      prevUtxosRoot: r0,
      utxos: [finalUtxo],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(713),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: finalDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(714),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(715, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(l2Source, SDK.L2TransactionSource), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps,
      eventToStep: [
        eventToStepEntry(withdrawalKey, {
          step_index: 0n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(l2Key, { step_index: 1n, phase: "L2Transaction" }),
        eventToStepEntry(depositKey, { step_index: 2n, phase: "Deposit" }),
        eventToStepEntry(finalDepositKey, {
          step_index: 3n,
          phase: "Deposit",
        }),
      ],
    });

    const reconstruction = await reconstruct(fixture);

    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    expect(reconstruction.transitionTrace.map(({ key }) => key)).toEqual([
      0n,
      1n,
      2n,
      3n,
    ]);
    expect(reconstruction.sourceEvents).toHaveLength(4);
    expect(reconstruction.eventToStep).toHaveLength(4);

    const ordered = [...reconstruction.transitionTrace].sort((left, right) =>
      Number(left.key - right.key),
    );
    expect(ordered[0]!.value.pre_utxos_root).toBe(
      reconstruction.header.prevUtxosRoot,
    );
    for (let index = 0; index < ordered.length - 1; index += 1) {
      expect(ordered[index]!.value.post_utxos_root).toBe(
        ordered[index + 1]!.value.pre_utxos_root,
      );
    }
    expect(ordered.at(-1)!.value.post_utxos_root).toBe(
      reconstruction.header.utxosRoot,
    );
    expect(reconstruction.header.utxosRoot).toBe(r3);

    for (const source of reconstruction.sourceEvents) {
      const mapped = reconstruction.eventToStepByFingerprint.get(
        source.fingerprint,
      );
      expect(mapped).toBeDefined();
      const step = reconstruction.traceByStepIndex.get(
        mapped!.value.step_index,
      );
      expect(step).toBeDefined();
      expect(step!.value.event_key).toEqual(source.eventKey);
    }
  });

  it("reconstructs a dense forced-transaction trace with every intermediate root reproduced and no operation missing or extra", async () => {
    const depositId = outRef(720);
    const forcedId = outRef(721);
    const withdrawalId = outRef(722);
    const finalDepositId = outRef(723);
    const finalUtxo = rawLedgerEntry(724);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const depositKey = depositEventKey(depositId);
    const forcedKey = forcedEventKey(forcedId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const finalDepositKey = depositEventKey(finalDepositId);

    const r0 = h32(730);
    const r1 = h32(731);
    const r2 = h32(732);
    const r2b = h32(733);
    const r3 = finalRoot.root;

    const steps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: withdrawalKey,
        phase: "Withdrawal",
        pre_utxos_root: r0,
        post_utxos_root: r1,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: forcedKey,
        phase: "ForcedTransaction",
        pre_utxos_root: r1,
        post_utxos_root: r2,
      },
      {
        schema_version: 1n,
        step_index: 2n,
        event_key: depositKey,
        phase: "Deposit",
        pre_utxos_root: r2,
        post_utxos_root: r2b,
      },
      {
        schema_version: 1n,
        step_index: 3n,
        event_key: finalDepositKey,
        phase: "Deposit",
        pre_utxos_root: r2b,
        post_utxos_root: r3,
      },
    ];

    const fixture = await buildPayloadFixture({
      prevUtxosRoot: r0,
      utxos: [finalUtxo],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(734),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: finalDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(735),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(736, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          // TxIsValid: a valid forced inclusion legitimately moves the root,
          // so the default no-op check (which only fires for invalid forced
          // transactions) stays quiet here.
          value: forcedTx(737, "ForcedTxValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps,
      eventToStep: [
        eventToStepEntry(withdrawalKey, {
          step_index: 0n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(forcedKey, {
          step_index: 1n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(depositKey, { step_index: 2n, phase: "Deposit" }),
        eventToStepEntry(finalDepositKey, {
          step_index: 3n,
          phase: "Deposit",
        }),
      ],
    });

    const reconstruction = await reconstruct(fixture);

    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    expect(reconstruction.transitionTrace.map(({ key }) => key)).toEqual([
      0n,
      1n,
      2n,
      3n,
    ]);
    expect(reconstruction.sourceEvents).toHaveLength(4);
    expect(reconstruction.eventToStep).toHaveLength(4);

    const ordered = [...reconstruction.transitionTrace].sort((left, right) =>
      Number(left.key - right.key),
    );
    expect(ordered[0]!.value.pre_utxos_root).toBe(
      reconstruction.header.prevUtxosRoot,
    );
    for (let index = 0; index < ordered.length - 1; index += 1) {
      expect(ordered[index]!.value.post_utxos_root).toBe(
        ordered[index + 1]!.value.pre_utxos_root,
      );
    }
    expect(ordered.at(-1)!.value.post_utxos_root).toBe(
      reconstruction.header.utxosRoot,
    );
    expect(reconstruction.header.utxosRoot).toBe(r3);

    for (const source of reconstruction.sourceEvents) {
      const mapped = reconstruction.eventToStepByFingerprint.get(
        source.fingerprint,
      );
      expect(mapped).toBeDefined();
      const step = reconstruction.traceByStepIndex.get(
        mapped!.value.step_index,
      );
      expect(step).toBeDefined();
      expect(step!.value.event_key).toEqual(source.eventKey);
    }
  });

  it("maps every enabled fault kind to a classified detection with a total event-to-step mapping", async () => {
    // Positive half: one dense trace touching every L1/L2 event kind
    // reconstructs with a bijective event<->step mapping and zero faults.
    const depositId = outRef(800);
    const withdrawalId = outRef(801);
    const forcedId = outRef(802);
    const material = nativeMaterial(803);
    const depositKey = depositEventKey(depositId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const forcedKey = forcedEventKey(forcedId);
    const l2Key: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const l2Source: SDK.L2TransactionSource = {
      tx_id: material.txId,
      source: material.source,
    };

    const totalFixture = await buildPayloadFixture({
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(804),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(805, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(806, "ForcedTxValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(l2Source, SDK.L2TransactionSource), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: withdrawalKey,
          phase: "Withdrawal",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: forcedKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 2n,
          event_key: l2Key,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 3n,
          event_key: depositKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(withdrawalKey, {
          step_index: 0n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(forcedKey, {
          step_index: 1n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(l2Key, { step_index: 2n, phase: "L2Transaction" }),
        eventToStepEntry(depositKey, { step_index: 3n, phase: "Deposit" }),
      ],
    });
    const totalBase = await reconstruct(totalFixture);
    expect(await detectTransitionTraceFaults(totalBase)).toEqual([]);
    expect(totalBase.sourceEvents).toHaveLength(4);
    expect(totalBase.eventToStep).toHaveLength(4);
    for (const source of totalBase.sourceEvents) {
      const mapped = totalBase.eventToStepByFingerprint.get(source.fingerprint);
      expect(mapped).toBeDefined();
      expect(totalBase.traceByStepIndex.has(mapped!.value.step_index)).toBe(
        true,
      );
    }

    // Negative half: every fault kind the production module declares must be
    // both reachable (its probe below produces it) and correctly classified.
    // TRANSITION_TRACE_FAULT_KINDS is read directly off detect.ts — nothing
    // here re-lists the kind strings — so a new kind added there without a
    // probe fails this assertion instead of silently going unmapped.
    const detectionsByKind = new Map<
      TransitionTraceFaultKind,
      readonly TransitionTraceDetection[]
    >();
    const seenKinds = new Set<TransitionTraceFaultKind>();
    for (const probe of probes) {
      const detections = await probe.run();
      expect(
        detections.some(
          (detection) =>
            detection.kind === probe.kind &&
            detection.invariant === probe.invariant,
        ),
      ).toBe(true);
      detectionsByKind.set(probe.kind, detections);
      for (const detection of detections) {
        seenKinds.add(detection.kind);
      }
    }
    expect([...seenKinds].sort()).toEqual(
      [...TRANSITION_TRACE_FAULT_KINDS].sort(),
    );

    // The one kind the 39-test challenger suite never exercises: confirm its
    // fault proof round-trips through CBOR like every other buildable
    // detection's does.
    const acceptedDetection = detectionsByKind
      .get("acceptedTransactionTransitionMismatch")
      ?.find(
        (detection) =>
          detection.kind === "acceptedTransactionTransitionMismatch",
      );
    if (acceptedDetection === undefined || !acceptedDetection.buildable) {
      throw new Error(
        "expected a buildable acceptedTransactionTransitionMismatch detection",
      );
    }
    expect(() =>
      Data.from(
        Data.to(
          acceptedDetection.proof as never,
          SDK.TransitionFaultProof as never,
        ),
        SDK.TransitionFaultProof as never,
      ),
    ).not.toThrow();
  });

  it("rejects extra, missing, reordered, and substituted operations against an otherwise-valid dense trace", async () => {
    const idA = outRef(900);
    const idB = outRef(901);
    const idC = outRef(902);
    const keyA = depositEventKey(idA);
    const keyB = withdrawalEventKey(idB);
    const keyC = depositEventKey(idC);
    const finalUtxo = rawLedgerEntry(903);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const r0 = h32(910);
    const r1 = h32(911);
    const r2 = h32(912);
    const r3 = finalRoot.root;

    const stepB: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: keyB,
      phase: "Withdrawal",
      pre_utxos_root: r0,
      post_utxos_root: r1,
    };
    const stepA: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 1n,
      event_key: keyA,
      phase: "Deposit",
      pre_utxos_root: r1,
      post_utxos_root: r2,
    };
    const stepC: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 2n,
      event_key: keyC,
      phase: "Deposit",
      pre_utxos_root: r2,
      post_utxos_root: r3,
    };

    const depositEntries = [
      encodedEntry({
        key: idA,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(920),
        valueSchema: SDK.DepositInfoSchema,
      }),
      encodedEntry({
        key: idC,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(921),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ];
    const withdrawalEntries = [
      encodedEntry({
        key: idB,
        keySchema: SDK.OutputReference as never,
        value: withdrawalInfo(922, "WithdrawalIsValid"),
        valueSchema: SDK.WithdrawalInfoSchema,
      }),
    ];

    // Control: the unmutated trace is total and reproduces the exact
    // post-state root.
    const baseline = await reconstruct(
      await buildPayloadFixture({
        prevUtxosRoot: r0,
        utxos: [finalUtxo],
        deposits: depositEntries,
        withdrawals: withdrawalEntries,
        steps: [stepB, stepA, stepC],
        eventToStep: [
          eventToStepEntry(keyB, { step_index: 0n, phase: "Withdrawal" }),
          eventToStepEntry(keyA, { step_index: 1n, phase: "Deposit" }),
          eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
        ],
      }),
    );
    expect(await detectTransitionTraceFaults(baseline)).toEqual([]);
    expect(baseline.transitionTrace.at(-1)!.value.post_utxos_root).toBe(r3);
    expect(baseline.header.utxosRoot).toBe(r3);

    // Extra: a fourth, unbacked step is appended past the true final root.
    const stepD: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 3n,
      event_key: depositEventKey(outRef(930)),
      phase: "Deposit",
      pre_utxos_root: r3,
      post_utxos_root: h32(931),
    };
    const extraReconstruction = await reconstruct(
      await buildPayloadFixture({
        prevUtxosRoot: r0,
        utxos: [finalUtxo],
        deposits: depositEntries,
        withdrawals: withdrawalEntries,
        steps: [stepB, stepA, stepC, stepD],
        // stepD is deliberately left out of event_to_step: it has no backing
        // deposit/withdrawal/tx event, so mapping it would inflate
        // event_to_step's member count past total_event_count and fail
        // reconstruction before the detect layer ever runs.
        eventToStep: [
          eventToStepEntry(keyB, { step_index: 0n, phase: "Withdrawal" }),
          eventToStepEntry(keyA, { step_index: 1n, phase: "Deposit" }),
          eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
        ],
      }),
    );
    expect(
      extraReconstruction.transitionTrace.at(-1)!.value.post_utxos_root,
    ).not.toBe(r3);
    const extraDetections =
      await detectTransitionTraceFaults(extraReconstruction);
    expect(
      extraDetections.some(
        (detection) =>
          detection.kind === "countFault" &&
          detection.invariant === "header_transition_step_count",
      ),
    ).toBe(true);
    expect(
      extraDetections.some(
        (detection) =>
          detection.kind === "traceBoundary" &&
          detection.invariant === "trace_end_utxos_root",
      ),
    ).toBe(true);

    // Missing: the middle operation is dropped, but the survivor keeps its
    // original positional index — leaving a hole a dense trace cannot have.
    await expect(
      reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries.slice(1),
          withdrawals: withdrawalEntries,
          transitionTraceEntries: [traceEntry(stepB), traceEntry(stepC)],
          eventToStep: [
            eventToStepEntry(keyB, { step_index: 0n, phase: "Withdrawal" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    ).rejects.toMatchObject({
      code: "invalidPayloadEntries",
      message: expect.stringContaining("outside"),
    });

    // Reordered: B and A swap positions; each keeps its own true pre/post
    // roots, so the chain no longer starts from the committed prev-root.
    const stepAReordered: SDK.TransitionStep = { ...stepA, step_index: 0n };
    const stepBReordered: SDK.TransitionStep = { ...stepB, step_index: 1n };
    const reorderedDetections = await detectTransitionTraceFaults(
      await reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries,
          withdrawals: withdrawalEntries,
          transitionTraceEntries: [
            traceEntry(stepAReordered),
            traceEntry(stepBReordered),
            traceEntry(stepC),
          ],
          eventToStep: [
            eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
            eventToStepEntry(keyB, { step_index: 1n, phase: "Withdrawal" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    );
    expect(
      reorderedDetections.some(
        (detection) =>
          detection.kind === "traceBoundary" &&
          detection.invariant === "trace_start_prev_utxos_root",
      ),
    ).toBe(true);
    expect(
      reorderedDetections.some((detection) => detection.kind === "traceLink"),
    ).toBe(true);

    // Substituted: B keeps its identity and position, but its committed
    // post-root is swapped for a different value, breaking the link to A.
    const stepBSubstituted: SDK.TransitionStep = {
      ...stepB,
      post_utxos_root: h32(940),
    };
    const substitutedDetections = await detectTransitionTraceFaults(
      await reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries,
          withdrawals: withdrawalEntries,
          steps: [stepBSubstituted, stepA, stepC],
          eventToStep: [
            eventToStepEntry(keyB, { step_index: 0n, phase: "Withdrawal" }),
            eventToStepEntry(keyA, { step_index: 1n, phase: "Deposit" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    );
    expect(
      substitutedDetections.some(
        (detection) =>
          detection.kind === "traceLink" &&
          detection.invariant === "adjacent_trace_roots",
      ),
    ).toBe(true);
  });
});
