import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  parseTimeoutCorrectionJournal,
  planNextTimeoutCorrection,
  reconcileCompletedTimeoutCorrectionJournal,
  reconcileLastTimeoutCorrectionStep,
  recoverTimeoutCorrectionAttempt,
  reopenRolledBackTimeoutCorrectionSteps,
  selectTimeoutCorrectionTarget,
  type TimeoutCorrectionAttemptObservation,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionRecovery,
} from "../src/remove-unattested-block.js";
const h = (byte: string) => byte.repeat(28);
const tx = (byte: string) => byte.repeat(32);
const address = credentialToAddress("Preprod", { type: "Key", hash: h("dd") });
const header: SDK.Header = {
  prevUtxosRoot: "55".repeat(32),
  utxosRoot: "55".repeat(32),
  withdrawalsRoot: "55".repeat(32),
  forcedTransactionsRoot: "55".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "55".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "55".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155381n,
  prevHeaderHash: "66".repeat(28),
  operatorVkey: "77".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
};
const block = (
  byte: string,
  endTime = 1n,
  attested = false,
): SDK.StateQueueUTxO => ({
  assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + h(byte),
  datum: {
    key: { Key: { key: h(byte) } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header: { ...header, endTime },
      da_attestation: attested
        ? { Attested: { commitment_hash: tx("ab") } }
        : SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  },
  utxo: {
    txHash: tx(byte),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n },
  },
});
const queue = (...nodes: SDK.StateQueueUTxO[]) => {
  const root: SDK.StateQueueUTxO = {
    ...block("aa"),
    assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    datum: { key: "Empty", next: "Empty", data: "d87980" },
  };
  const all = [root, ...nodes].map((node) => ({
    ...node,
    datum: { ...node.datum },
  }));
  for (let i = 0; i < all.length - 1; i++)
    all[i]!.datum.next = all[i + 1]!.datum.key;
  return all;
};
const journal = (
  status:
    | "prepared"
    | "submitted"
    | "confirmed"
    | "superseded"
    | "retired" = "submitted",
): TimeoutCorrectionJournal => {
  const inputs = CML.TransactionInputList.new();
  for (const byte of ["11", "22", "cc", "dd"])
    inputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(tx(byte)), 0n),
    );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    1n,
  );
  body.set_validity_interval_start(10n);
  body.set_ttl(20n);
  const signed = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    version: 1,
    targetHeaderHash: h("11"),
    targetDeadlineMs: (SDK.DA_ATTESTATION_TIMEOUT_MS + 1n).toString(),
    completed: false,
    steps: [
      {
        kind: "prune-descendant",
        removedHeaderHash: h("22"),
        inputOutRefs: ["11", "22", "cc", "dd"].map((b) => `${tx(b)}#0`),
        txHash: CML.hash_transaction(body).to_hex(),
        signedCbor: signed.to_cbor_hex(),
        validFromSlot: "10",
        validToSlot: "20",
        status,
      },
    ],
  };
};
const recovery = (
  status: TimeoutCorrectionAttemptObservation["status"],
  final = false,
) => ({
  observeAttempt: vi.fn(
    async (
      _signed: Parameters<TimeoutCorrectionRecovery["observeAttempt"]>[0],
    ): Promise<TimeoutCorrectionAttemptObservation> => ({
      status,
      final,
      reason: status,
      canonicalPoint: { slot: 10_000, hash: tx("88") },
      releaseFinalPoint: { slot: 40, hash: tx("77") },
    }),
  ),
});
const recover = (
  source: ReturnType<typeof recovery>,
  nodes = queue(block("11"), block("22")),
  pending = journal(),
) =>
  recoverTimeoutCorrectionAttempt({
    journal: pending,
    queue: nodes,
    observe: source.observeAttempt,
  });

describe("generalized attestation-timeout recovery", () => {
  it("selects and prunes an expired interior target while preserving its attested prefix", async () => {
    const nodes = queue(block("01", 1n, true), block("11"), block("22"));
    expect(
      (await selectTimeoutCorrectionTarget(nodes, 4_000_000n, "Idle"))?.target,
    ).toBe(nodes[2]);
    const plan = planNextTimeoutCorrection(nodes, h("11"));
    expect(plan).toMatchObject({
      kind: "prune-descendant",
      predecessor: nodes[1],
      target: nodes[2],
      removed: nodes[3],
    });
    expect(plan?.inputOutRefs).toEqual([`${tx("11")}#0`, `${tx("22")}#0`]);
    expect(nodes[0]!.utxo.txHash).toBe(tx("aa"));
    expect(nodes[1]!.utxo.txHash).toBe(tx("01"));
  });
  it("removes a tail through its immediate predecessor without consuming the root", () => {
    const nodes = queue(block("01", 1n, true), block("11"));
    const plan = planNextTimeoutCorrection(nodes, h("11"));
    expect(plan).toMatchObject({
      kind: "remove-block",
      predecessor: nodes[1],
      target: nodes[2],
    });
    expect(plan?.inputOutRefs).toEqual([`${tx("01")}#0`, `${tx("11")}#0`]);
  });
  it("resumes a locked later target ahead of another eligible timeout and rejects foreign locks", async () => {
    const nodes = queue(block("01"), block("11"));
    expect(
      (
        await selectTimeoutCorrectionTarget(nodes, 4_000_000n, {
          Locked: {
            target_header_hash: h("11"),
            correction_identity: "AttestationTimeout",
          },
        })
      )?.target,
    ).toBe(nodes[2]);
    await expect(
      selectTimeoutCorrectionTarget(nodes, 4_000_000n, {
        Locked: {
          target_header_hash: h("11"),
          correction_identity: {
            FraudProof: { fraud_proof_asset_name: tx("bb") },
          },
        },
      }),
    ).rejects.toThrow("another correction kind");
  });
  it("keeps attested blocks out of selection and uses the exact deadline boundary", async () => {
    const nodes = queue(block("01", 1n, true), block("11"));
    expect(
      (
        await selectTimeoutCorrectionTarget(
          nodes,
          SDK.DA_ATTESTATION_TIMEOUT_MS,
          "Idle",
        )
      )?.deadline,
    ).toBe(SDK.DA_ATTESTATION_TIMEOUT_MS + 1n);
    expect(
      (
        await selectTimeoutCorrectionTarget(
          nodes,
          SDK.DA_ATTESTATION_TIMEOUT_MS + 1n,
          "Idle",
        )
      )?.target,
    ).toBe(nodes[2]);
    expect(
      await selectTimeoutCorrectionTarget(
        queue(block("01", 1n, true)),
        4_000_000n,
        "Idle",
      ),
    ).toBeUndefined();
  });
  it("rejects disconnected predecessor and descendant plans", () => {
    const nodes = queue(block("01"), block("11"), block("22"));
    nodes[1]!.datum.next = "Empty";
    expect(() => planNextTimeoutCorrection(nodes, h("11"))).toThrow(
      "predecessor",
    );
    nodes[1]!.datum.next = nodes[2]!.datum.key;
    nodes[2]!.datum.next = "Empty";
    expect(() => planNextTimeoutCorrection(nodes, h("11"))).toThrow(
      "descendant",
    );
  });
  it("checks exact signed transaction identity, full ordinary inputs and bounded validity", () => {
    const valid = journal();
    expect(parseTimeoutCorrectionJournal(valid)).toEqual(valid);
    for (const patch of [
      { txHash: tx("ff") },
      { validToSlot: "21" },
      { validFromSlot: "9" },
      { inputOutRefs: valid.steps[0]!.inputOutRefs.slice(1) },
      { signedCbor: "80" },
    ])
      expect(() =>
        parseTimeoutCorrectionJournal({
          ...valid,
          steps: [{ ...valid.steps[0]!, ...patch }],
        }),
      ).toThrow();
  });
  it.each(["failed", "not_found", "unknown", "pending"] as const)(
    "preserves %s despite changed queue hints",
    (status) => {
      const pending = journal();
      const result = reconcileLastTimeoutCorrectionStep(
        pending,
        queue(block("11")),
        status,
      );
      expect(result.disposition).toBe("pending");
      expect(result.journal).toBe(pending);
    },
  );
  it.each(["expired", "invalidated", "conflict"] as const)(
    "retires a %s attempt only once it is final, and abandons it before",
    async (status) => {
      const held = await recover(recovery(status));
      expect(held.disposition).toBe("superseded");
      expect(held.journal.steps[0]!.status).toBe("abandoned");
      const final = await recover(recovery(status, true));
      expect(final.disposition).toBe("superseded");
      expect(final.journal.steps[0]!.status).toBe("retired");
    },
  );
  it("keeps a live attempt pending with its exact retained bytes", async () => {
    const pending = journal();
    const source = recovery("pending");
    const result = await recover(
      source,
      queue(block("11"), block("22")),
      pending,
    );
    expect(result.disposition).toBe("pending");
    expect(result.journal).toBe(pending);
    expect(source.observeAttempt).toHaveBeenCalledWith({
      transactionHash: pending.steps[0]!.txHash,
      signedTransactionCborHex: pending.steps[0]!.signedCbor,
    });
  });
  it("requires exact canonical inclusion and matching queue effects before confirmation", async () => {
    const source = recovery("included");
    expect((await recover(source)).disposition).toBe("pending");
    const continued = block("11");
    continued.utxo.txHash = tx("99");
    expect((await recover(source, queue(continued))).disposition).toBe(
      "confirmed",
    );
  });
  it("preserves attempts on unavailable evidence and rejects substituted bytes", async () => {
    const source = recovery("expired", true);
    source.observeAttempt.mockRejectedValueOnce(new Error("store unavailable"));
    expect((await recover(source)).disposition).toBe("pending");
    expect((await recover(recovery("unknown"))).disposition).toBe("pending");
    const pending = journal();
    const substituted = {
      ...pending,
      steps: [{ ...pending.steps[0]!, txHash: tx("ff") }],
    };
    await expect(
      recover(source, queue(block("11"), block("22")), substituted),
    ).rejects.toThrow("differ from their durable transaction identity");
  });
  it("reopens a rollback-restored objective anywhere without discarding signed attempts", () => {
    const completed = { ...journal("confirmed"), completed: true };
    const restored = reconcileCompletedTimeoutCorrectionJournal(
      completed,
      queue(block("01", 1n, true), block("11"), block("22")),
    );
    expect(restored).toMatchObject({
      completed: false,
      steps: [
        { status: "prepared", signedCbor: completed.steps[0]!.signedCbor },
      ],
    });
    expect(
      reconcileCompletedTimeoutCorrectionJournal(
        completed,
        queue(block("01", 1n, true)),
      ),
    ).toBeUndefined();
  });
  it("reopens legacy superseded steps with restored canonical inputs and retains the exact signed attempt", () => {
    const retained = journal("superseded");
    const restoredQueue = queue(
      block("01", 1n, true),
      block("11"),
      block("22"),
    );
    const reopened = reopenRolledBackTimeoutCorrectionSteps(
      retained,
      restoredQueue,
    );
    expect(reopened.steps).toEqual([
      { ...retained.steps[0], status: "prepared" },
    ]);
    const retired = journal("retired");
    expect(reopenRolledBackTimeoutCorrectionSteps(retired, restoredQueue)).toBe(
      retired,
    );
  });
  it("reopens reverted confirmed steps while the objective is still incomplete", async () => {
    const retained = journal("confirmed");
    const restoredQueue = queue(
      block("01", 1n, true),
      block("11"),
      block("22"),
    );
    const reopened = reopenRolledBackTimeoutCorrectionSteps(
      retained,
      restoredQueue,
    );
    expect(reopened.completed).toBe(false);
    expect(reopened.steps).toEqual([
      { ...retained.steps[0], status: "prepared" },
    ]);
    // Live again: pending on the same signed bytes (the node's intent
    // reconciler, not this workflow, resends them).
    const source = recovery("pending");
    const result = await recover(source, restoredQueue, reopened);
    expect(result.disposition).toBe("pending");
    expect(
      source.observeAttempt.mock.calls[0]![0].signedTransactionCborHex,
    ).toBe(retained.steps[0]!.signedCbor);
    const continued = block("11");
    continued.utxo.txHash = tx("99");
    expect(
      reopenRolledBackTimeoutCorrectionSteps(retained, queue(continued)),
    ).toBe(retained);
  });
});
