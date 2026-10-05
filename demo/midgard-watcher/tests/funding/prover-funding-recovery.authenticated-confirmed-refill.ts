import { createHash } from "node:crypto";

import {
  beginWorkflowFundingReservationAction,
  createLocalKupmiosHttpOgmiosRawSource,
  type FraudProofRawL1WebSocketLike,
  type FraudProofWorkflowJournalStore,
  LocalKupmiosCheckpointChangedError,
  localKupmiosHttpOgmiosRawSourceDetails,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  releaseIdleWorkflowFundingReservation,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
  type VerifiedFraudProofReleaseFinalityPolicy,
  WorkflowFundingReservationUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  deploymentIdentity,
  finality,
  setupFundingRecoveryFixture as setup,
} from "../support/fault-proof-funding-fixture.js";

type FundingFixture = Awaited<ReturnType<typeof setup>>;
type Block = {
  slot: number;
  id: string;
  height: number;
  ancestor: string;
  transactions: { id: string; cbor: string }[];
};

// Controlled HTTP/chain-sync responses exercise concrete source admission.
// This is not a native Cardano validation or live finality fixture.
const includedSource = (
  signed: readonly SignedWorkflowTransaction[],
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy,
  depth: number,
  changingHead = false,
) => {
  const hash = (value: unknown) =>
    createHash("sha256").update(JSON.stringify(value)).digest("hex");
  const seed = [releaseFinality, signed];
  const blocks: Block[] = [
    {
      slot: 0,
      id: hash([seed, "anchor"]),
      height: 0,
      ancestor: "genesis",
      transactions: [],
    },
  ];
  // Heights and parent hashes follow the enumerated fixture chain. The exact
  // transaction is in its first block, before the fixture's recorded TTL.
  for (let index = 0; index <= depth; index += 1) {
    const parent = blocks[blocks.length - 1]!;
    const transactions =
      index === 0
        ? signed.map(({ transactionHash, signedTransactionCborHex }) => ({
            id: transactionHash,
            cbor: signedTransactionCborHex,
          }))
        : [];
    blocks.push({
      slot: parent.slot + 1,
      height: parent.height + 1,
      ancestor: parent.id,
      id: hash([parent.id, transactions, index]),
      transactions,
    });
  }
  const tip = blocks[blocks.length - 1]!;
  const inclusion = blocks[1]!;
  const tipPoint = { slot: tip.slot, id: tip.id, height: tip.height };
  const outputsFor = (
    input: SignedWorkflowTransaction,
    transactionIndex: number,
  ) => {
    const transaction = CML.Transaction.from_cbor_hex(
      input.signedTransactionCborHex,
    );
    expect(CML.hash_transaction(transaction.body()).to_hex()).toBe(
      input.transactionHash,
    );
    const ttl = transaction.body().ttl();
    expect(ttl === undefined || BigInt(inclusion.slot) < ttl).toBe(true);
    const validFrom = transaction.body().validity_interval_start();
    expect(validFrom === undefined || BigInt(inclusion.slot) >= validFrom).toBe(
      true,
    );
    const outputs = transaction.body().outputs();
    return Array.from({ length: outputs.len() }, (_, index) => {
      const output = outputs.get(index);
      expect(output.amount().has_multiassets()).toBe(false);
      expect(output.datum()).toBeUndefined();
      expect(output.script_ref()).toBeUndefined();
      return {
        transaction_index: transactionIndex,
        transaction_id: input.transactionHash,
        output_index: index,
        address: output.address().to_bech32(),
        value: { coins: output.amount().coin().toString(), assets: {} },
        datum_hash: null,
        datum: null,
        script_hash: null,
        script: null,
        created_at: { slot_no: inclusion.slot, header_hash: inclusion.id },
        spent_at: null,
      };
    });
  };
  const matches = new Map(
    signed.map(
      (input, index) =>
        [input.transactionHash, outputsFor(input, index)] as const,
    ),
  );
  let requests = 0;
  class Socket implements FraudProofRawL1WebSocketLike {
    private readonly listeners = new Map<string, Set<(event: never) => void>>();
    private cursor = 0;
    private acknowledged = false;
    constructor() {
      queueMicrotask(() => this.emit("open", {}));
    }
    addEventListener(type: string, listener: (event: never) => void): void {
      const listeners = this.listeners.get(type) ?? new Set();
      listeners.add(listener);
      this.listeners.set(type, listeners);
    }
    removeEventListener(type: string, listener: (event: never) => void): void {
      this.listeners.get(type)?.delete(listener);
    }
    private emit(type: string, event: unknown): void {
      for (const listener of this.listeners.get(type) ?? [])
        listener(event as never);
    }
    send(data: string): void {
      requests += 1;
      const request = JSON.parse(data) as {
        id: number;
        method: string;
        params: { points?: ("origin" | { slot: number; id: string })[] };
      };
      let result: unknown;
      if (request.method === "findIntersection") {
        const point = request.params.points![0]!;
        if (point !== "origin") {
          this.cursor = blocks.findIndex(
            ({ slot, id }) => slot === point.slot && id === point.id,
          );
          if (this.cursor === -1)
            throw new Error("unknown fixture intersection");
        }
        this.acknowledged = false;
        result = { intersection: point, tip: tipPoint };
      } else if (request.method === "nextBlock") {
        if (!this.acknowledged) {
          this.acknowledged = true;
          const block = blocks[this.cursor]!;
          result = {
            direction: "backward",
            point: { slot: block.slot, id: block.id },
            tip: tipPoint,
          };
        } else {
          const block = blocks[++this.cursor];
          if (block === undefined) throw new Error("fixture chain exhausted");
          result = { direction: "forward", block, tip: tipPoint };
        }
      } else throw new Error(`unexpected fixture RPC ${request.method}`);
      queueMicrotask(() =>
        this.emit("message", {
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
    }
    close(): void {
      this.emit("close", { code: 1000, reason: "", wasClean: true });
    }
  }
  let httpReads = 0;
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "watcher-confirmed-refill",
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "ws://127.0.0.1:1337",
    releaseFinality,
    observationDepth: "inclusion",
    webSocketFactory: () => new Socket(),
    fetchImpl: async (input) => {
      requests += 1;
      httpReads += 1;
      const url = new URL(input);
      let value: unknown;
      if (url.pathname.startsWith("/checkpoints/")) {
        const slot = Number(url.pathname.slice("/checkpoints/".length));
        const block = blocks[Math.min(slot, tip.slot)]!;
        value = { slot_no: block.slot, header_hash: block.id };
      } else if (url.pathname.startsWith("/matches/")) {
        const pattern = decodeURIComponent(
          url.pathname.slice("/matches/".length),
        );
        const prefix = "*@";
        if (
          !pattern.startsWith(prefix) ||
          !matches.has(pattern.slice(prefix.length))
        )
          throw new Error(`unexpected fixture Kupo pattern ${pattern}`);
        value = matches.get(pattern.slice(prefix.length));
      } else throw new Error(`unexpected fixture HTTP ${url.pathname}`);
      return new Response(JSON.stringify(value), {
        status: 200,
        headers: {
          "content-type": "application/json",
          "x-most-recent-checkpoint": tip.slot.toString(),
          etag:
            changingHead && httpReads % 2 === 0
              ? hash([tip.id, "changed-head"])
              : tip.id,
        },
      });
    },
  });
  expect(localKupmiosHttpOgmiosRawSourceDetails(source)).toMatchObject({
    deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
    blueprintHash: releaseFinality.blueprintHash,
    finalityPolicyDigest: releaseFinality.policyDigest,
    observationDepth: "inclusion",
    confirmationDepth: releaseFinality.policy.confirmationDepth,
    automaticRecoveryMaxDepth: releaseFinality.policy.automaticRecoveryMaxDepth,
  });
  return { source, requests: () => requests };
};

const retirementEvent = (observed: SignedTransactionRecoveryObservation) => {
  expect(observed.status).toBe("included");
  if (observed.inclusionPoint === undefined)
    throw new Error("exact included observation is missing its admitted point");
  return {
    kind: "signed_attempt_retired" as const,
    txHash: observed.transactionHash,
    retirement: {
      transactionHash: observed.transactionHash,
      canonicalPoint: observed.canonicalPoint,
      releaseFinalPoint: observed.inclusionPoint,
      reason: "included" as const,
    },
  };
};

export const assertConfirmedFundingHistoryRetained = async (
  test: FundingFixture,
) => {
  const transition = test.pending.pendingTransition!;
  const protectedRefs = [
    ...transition.consumedOutRefs,
    ...transition.producedInputs.map(({ outRef }) => outRef),
  ];
  const collateral = CML.Transaction.from_cbor_hex(
    transition.signedTransactionCborHex,
  )
    .body()
    .collateral_inputs();
  if (collateral !== undefined)
    for (let index = 0; index < collateral.len(); index += 1) {
      const input = collateral.get(index);
      protectedRefs.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
  const leases = await test.store.readReservedOutRefs({});
  for (const outRef of protectedRefs) expect(leases).toContain(outRef);
  expect(test.store.hasSignedHistory).toBeTypeOf("function");
  expect(
    await test.store.hasSignedHistory!({
      reservationId: test.plan.reservationId,
    }),
  ).toBe(true);
};

export const authorizeConfirmedFundingRefill = async (
  test: FundingFixture,
  journal: FraudProofWorkflowJournalStore,
  checkSourceNegatives = false,
) => {
  const action = { actionId: "next", input: { actionKind: "proof.init" } };
  const records = await test.records();
  const leases = await test.store.readReservedOutRefs({});
  const entries = await journal.load(test.initial.workflowId);
  const signed = new Map<string, SignedWorkflowTransaction>();
  for (const { event } of entries) {
    if (event.kind !== "submission_intent") continue;
    const transition = test.pending.pendingTransition!;
    const signedTransactionCborHex =
      event.durableRecovery?.signedTransactionCborHex ??
      (event.txHash === transition.transactionHash
        ? transition.signedTransactionCborHex
        : undefined);
    if (typeof signedTransactionCborHex !== "string")
      throw new Error(
        "fixture has an intent without exact persisted signed bytes",
      );
    const input = { transactionHash: event.txHash, signedTransactionCborHex };
    const prior = signed.get(event.txHash);
    if (prior !== undefined) expect(input).toEqual(prior);
    signed.set(event.txHash, input);
  }
  expect(signed.size).toBeGreaterThan(0);
  const assertHeld = async () => {
    expect(await test.records()).toEqual(records);
    expect([...(await test.store.readReservedOutRefs({}))].sort()).toEqual(
      [...leases].sort(),
    );
    expect(await journal.load(test.initial.workflowId)).toEqual(entries);
    expect(test.readWalletUtxos).not.toHaveBeenCalled();
    await assertConfirmedFundingHistoryRetained(test);
  };
  await releaseIdleWorkflowFundingReservation({
    journal,
    workflowId: test.initial.workflowId,
  });
  await expect(
    beginWorkflowFundingReservationAction({ journal, action }),
  ).rejects.toBeInstanceOf(WorkflowFundingReservationUnavailableError);
  await assertHeld();
  const policy = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const inputs = [...signed.values()];
  const deep = includedSource(
    inputs,
    policy,
    policy.policy.automaticRecoveryMaxDepth + 1,
  );
  if (checkSourceNegatives) {
    const beforeCopy = deep.requests();
    const copy = { ...deep.source };
    expect(localKupmiosHttpOgmiosRawSourceDetails(copy)).toBeNull();
    await expect(
      readAdmittedLocalKupmiosSignedTransactionRecovery({
        source: copy,
        ...inputs[0]!,
      }),
    ).rejects.toThrow("admitted local Kupo/Ogmios authority");
    expect(deep.requests()).toBe(beforeCopy);
    await assertHeld();
    const changed = includedSource(
      inputs,
      policy,
      policy.policy.automaticRecoveryMaxDepth + 1,
      true,
    );
    await expect(
      readAdmittedLocalKupmiosSignedTransactionRecovery({
        source: changed.source,
        ...inputs[0]!,
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosCheckpointChangedError);
    await assertHeld();
    const shallow = includedSource(
      inputs,
      policy,
      policy.policy.automaticRecoveryMaxDepth,
    );
    const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery({
      source: shallow.source,
      ...inputs[0]!,
    });
    await expect(test.append(retirementEvent(observed))).rejects.toThrow(
      "canonical recovery horizon",
    );
    await assertHeld();
  }
  // Observe every exact intent before admitting any retirement to the journal.
  const observations: SignedTransactionRecoveryObservation[] = [];
  for (const input of inputs) {
    const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery({
      source: deep.source,
      ...input,
    });
    expect(observed.transactionHash).toBe(input.transactionHash);
    expect(observed.signedTransactionCborHex).toBe(
      input.signedTransactionCborHex,
    );
    observations.push(observed);
  }
  for (const observed of observations)
    await test.append(retirementEvent(observed));
  await releaseIdleWorkflowFundingReservation({
    journal,
    workflowId: test.initial.workflowId,
  });
  expect(await test.records()).toEqual(records);
  expect([...(await test.store.readReservedOutRefs({}))].sort()).toEqual(
    [...leases].sort(),
  );
  expect(test.readWalletUtxos).not.toHaveBeenCalled();
  await assertConfirmedFundingHistoryRetained(test);
};

export const registerConfirmedFundingRefillTest = () => {
  it("refreshes externally spent unsigned change after confirmation without rotating healthy inputs", async () => {
    const test = await setup();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const before = await test.records();
    const action = { actionId: "next", input: { actionKind: "proof.init" } };
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(await test.records()).toEqual(before);
    expect(test.readWalletUtxos).not.toHaveBeenCalled();
    const spent = `${test.transactionHash}#0`;
    expect(before[0]!.activeInputs).toContainEqual(
      expect.objectContaining({ role: "funding", outRef: spent }),
    );
    test.walletUtxos.splice(
      0,
      test.walletUtxos.length,
      ...test.walletUtxos.filter(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent,
      ),
    );
    await authorizeConfirmedFundingRefill(test, journal, true);
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(test.readWalletUtxos).toHaveBeenCalledOnce();
    expect(
      (await test.records())[0]!.activeInputs.map(({ outRef }) => outRef),
    ).not.toContain(spent);
    expect((await test.records())[0]!.reservationId).toBe(
      before[0]!.reservationId,
    );
    await assertConfirmedFundingHistoryRetained(test);
  });
};
