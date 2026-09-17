import { LocalKupmiosCheckpointChangedError } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  reconcileSignedCommit,
  type SignedCommitAttempt,
  type SignedCommitReconciliationPorts,
} from "./signed-commit-reconciliation.js";

const inputs = CML.TransactionInputList.new();
inputs.add(
  CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
);
const outputs = CML.TransactionOutputList.new();
outputs.add(
  CML.TransactionOutput.new(
    CML.Address.from_hex("60" + "22".repeat(28)),
    CML.Value.new(2_000_000n, CML.MultiAsset.new()),
  ),
);
const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
body.set_ttl(30n);
const transaction = CML.Transaction.new(
  body,
  CML.TransactionWitnessSet.new(),
  true,
);
const attempt: SignedCommitAttempt = {
  txHash: CML.hash_transaction(body).to_hex(),
  signedCbor: transaction.to_cbor_hex(),
};
type Recovery = Awaited<
  ReturnType<SignedCommitReconciliationPorts["readRecovery"]>
>;
const point = {
  blockHash: "33".repeat(32),
  blockNo: "100",
  slot: "200",
  pointId: "44".repeat(32),
};
const observation = (status: Recovery["status"]): Recovery => ({
  transactionHash: attempt.txHash,
  signedTransactionCborHex: attempt.signedCbor,
  status,
  canonicalPoint: point,
  releaseFinalPoint: point,
  inputs: [],
  reason: `authenticated ${status}`,
});
const ports = (
  statuses: Recovery["status"][],
): SignedCommitReconciliationPorts => ({
  attempt,
  readRecovery: vi.fn(async () => {
    const status = statuses.shift();
    if (status === undefined) throw new Error("unexpected extra recovery read");
    return observation(status);
  }),
  pollDelay: vi.fn(async () => {}),
  resubmit: vi.fn(async () => attempt.txHash),
  onStage: vi.fn(),
});

it.each(["expired", "invalidated"] as const)(
  "authenticated signed recovery retires only the exact %s attempt",
  async (status) => {
    const chain = ports([status]);
    expect(await reconcileSignedCommit(chain)).toEqual({
      kind: "retired",
      reason: `authenticated ${status}`,
    });
    expect(chain.readRecovery).toHaveBeenCalledWith(attempt);
    expect(chain.resubmit).not.toHaveBeenCalled();
  },
);

it("authenticated signed recovery preserves unknown and pending past the local TTL", async () => {
  // Both authenticated points are past TTL, but only the reader can prove expiry.
  const chain = ports(["unknown", "pending", "included"]);
  expect(await reconcileSignedCommit(chain)).toEqual({
    kind: "included",
    txHash: attempt.txHash,
  });
  expect(chain.pollDelay).toHaveBeenCalledTimes(2);
  expect(chain.resubmit).not.toHaveBeenCalled();
});

it.each(["included", "expired"] as const)(
  "retries a moving canonical checkpoint before authenticated %s without replacing signed bytes",
  async (status) => {
    const chain = ports([status]);
    vi.mocked(chain.readRecovery).mockRejectedValueOnce(
      new LocalKupmiosCheckpointChangedError("Kupo advanced during capture"),
    );
    expect(await reconcileSignedCommit(chain)).toEqual(
      status === "included"
        ? { kind: "included", txHash: attempt.txHash }
        : { kind: "retired", reason: "authenticated expired" },
    );
    expect(chain.readRecovery).toHaveBeenCalledTimes(2);
    expect(chain.readRecovery).toHaveBeenNthCalledWith(1, attempt);
    expect(chain.readRecovery).toHaveBeenNthCalledWith(2, attempt);
    expect(chain.pollDelay).toHaveBeenCalledOnce();
    expect(chain.resubmit).not.toHaveBeenCalled();
  },
);

it("authenticated signed recovery rebroadcasts only eligible exact bytes once", async () => {
  const chain = ports(["pending", "rebroadcast", "rebroadcast", "included"]);
  expect(await reconcileSignedCommit(chain)).toEqual({
    kind: "included",
    txHash: attempt.txHash,
  });
  expect(chain.resubmit).toHaveBeenCalledExactlyOnceWith(attempt.signedCbor);
  expect(chain.pollDelay).toHaveBeenCalledTimes(3);
});

it("authenticated signed recovery does not turn an ambiguous RPC failure into retirement", async () => {
  const chain = ports(["rebroadcast", "unknown", "included"]);
  chain.resubmit = vi.fn(async () => {
    throw new Error("RPC response lost");
  });
  expect(await reconcileSignedCommit(chain)).toEqual({
    kind: "included",
    txHash: attempt.txHash,
  });
  expect(chain.resubmit).toHaveBeenCalledTimes(1);
  expect(chain.onStage).toHaveBeenCalledTimes(1);
});

it.each(["source unavailable", "canonical transaction is phase-2 invalid"])(
  "authenticated signed recovery preserves hard reader failure: %s",
  async (message) => {
    const chain = ports([]);
    const cause = new Error(message);
    chain.readRecovery = vi.fn(async () => {
      throw cause;
    });
    await expect(reconcileSignedCommit(chain)).rejects.toBe(cause);
    expect(chain.resubmit).not.toHaveBeenCalled();
  },
);

it("authenticated signed recovery keeps conflict hard and rejects mismatched recovery identity", async () => {
  await expect(reconcileSignedCommit(ports(["conflict"]))).rejects.toThrow(
    "recovery conflict",
  );
  const chain = ports([]);
  chain.readRecovery = vi.fn(async () => ({
    ...observation("invalidated"),
    transactionHash: "55".repeat(32),
  }));
  await expect(reconcileSignedCommit(chain)).rejects.toThrow(
    "recovery changed the recorded transaction identity",
  );
});

it("authenticated signed recovery rejects changed recorded bytes and changed rebroadcast hash", async () => {
  const chain = ports(["rebroadcast"]);
  chain.resubmit = vi.fn(async () => "55".repeat(32));
  await expect(reconcileSignedCommit(chain)).rejects.toThrow(
    "Resubmitted header differs",
  );
  const changed = ports(["expired"]);
  changed.attempt = { ...attempt, txHash: "55".repeat(32) };
  await expect(reconcileSignedCommit(changed)).rejects.toThrow(
    "Recorded header transaction bytes changed",
  );
  expect(changed.readRecovery).not.toHaveBeenCalled();
});
