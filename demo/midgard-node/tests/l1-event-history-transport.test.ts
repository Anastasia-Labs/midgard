import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  credentialToAddress,
  Data,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { verifyEventHistoryReferenceBody } from "../src/l1-event-history-reference.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import {
  type HistoryTransportOptions,
  locateEventHistoryActivation,
  readEventHistoryCreatingBody,
} from "../src/l1-event-history-transport.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const hash = (n: number) => n.toString(16).padStart(64, "0");
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "ab".repeat(28),
});
const unit = "cd".repeat(28) + "ef";
const datum = "a202a2186400011b00200000000000010100";
const ancestor = { id: hash(1), slot: 1 };
const middle = { id: hash(2), slot: 20, height: 2 };
const target = { id: hash(3), slot: 30, height: 3 };
const fixtureBody = (valid: boolean) => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      assetsToValue({ lovelace: 3_000_000n, [unit]: 9_007_199_254_740_993n }),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const encoded = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    100n,
  ).to_cbor_hex();
  const bodyCbor = "bf" + encoded.slice(2) + "ff";
  const body = CML.TransactionBody.from_cbor_hex(bodyCbor);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    valid,
  );
  return {
    bodyCbor,
    cbor: transaction.to_cbor_hex(),
    ref: {
      txHash: computeHash32(Buffer.from(bodyCbor, "hex")).toString("hex"),
      outputIndex: 0,
    },
  };
};
type Request = { id: number; method: string; params: unknown };
class Socket implements WebSocketLike {
  listeners = new Map<string, ((event: never) => void)[]>();
  requests: Request[] = [];
  responses: unknown[] = [];
  intersection: unknown = ancestor;
  closed = false;
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  send(data: string) {
    const request = JSON.parse(data) as Request;
    this.requests.push(request);
    const result =
      request.method === "findIntersection"
        ? { intersection: this.intersection }
        : this.responses.shift();
    if (result !== undefined)
      queueMicrotask(() =>
        this.emit("message", {
          data: JSON.stringify({ id: request.id, result }),
        }),
      );
  }
  close() {
    if (!this.closed) {
      this.closed = true;
      this.emit("close");
    }
  }
}
const fixture = (valid = true) => {
  const body = fixtureBody(valid);
  const controller = new AbortController();
  const socket = new Socket();
  const blocks = [
    {
      direction: "forward",
      block: {
        type: "praos",
        ...middle,
        ancestor: ancestor.id,
        transactions: [],
      },
    },
    {
      direction: "forward",
      block: {
        type: "praos",
        ...target,
        ancestor: middle.id,
        transactions: [{ id: body.ref.txHash, cbor: body.cbor }],
      },
    },
  ];
  socket.responses = [{ direction: "backward", point: ancestor }, ...blocks];
  const fetchImpl = vi.fn(
    async (url: string) =>
      new Response(
        JSON.stringify(
          url.includes("/checkpoints/")
            ? { slot_no: ancestor.slot, header_hash: ancestor.id }
            : [
                {
                  transaction_id: body.ref.txHash,
                  output_index: 0,
                  datum: null,
                  created_at: { slot_no: target.slot, header_hash: target.id },
                },
              ],
        ),
      ),
  );
  const options: HistoryTransportOptions = {
    kupoUrl: "http://kupo:1442",
    ogmiosUrl: "http://cardano-node-ogmios:1337",
    signal: controller.signal,
    timeoutMs: 2000,
    blockScanLimit: 10,
    maximumResponseBytes: 65536,
    maximumTransactionBytes: 16384,
    fetchImpl,
    webSocketFactory: () => {
      queueMicrotask(() => socket.emit("open"));
      return socket;
    },
  };
  return { ...body, controller, socket, blocks, options, fetchImpl };
};

// Model RPC navigation and exact CBOR; these transactions are not ledger
// submissions. Bound observing-reference authority is an explicit precondition.
describe("node history body transport and activation navigation", () => {
  it.each([true, false])(
    "preserves exact BODY bytes regardless of untrusted validity flag %s",
    async (valid) => {
      const f = fixture(valid);
      const creatingBodyCbor = await readEventHistoryCreatingBody(
        f.options,
        f.ref,
      );
      expect(creatingBodyCbor).toBe(f.bodyCbor);
      const output = verifyEventHistoryReferenceBody({
        transaction: {
          txHash: hash(999),
          spends: "inputs",
          inputs: [],
          references: [f.ref],
          collaterals: [],
          outputs: [],
          mint: {},
          withdrawals: [],
          redeemers: [],
        },
        ref: f.ref,
        creatingBodyCbor,
        maximumBodyBytes: 16384,
      });
      expect(output.datum).toBe(datum);
      expect(output.assets[unit]).toBe(9_007_199_254_740_993n);
      expect(f.socket.closed).toBe(true);
    },
  );

  it("locates from the unspent hub and returns the actual linked predecessor rather than the sparse checkpoint", async () => {
    const f = fixture();
    const contracts = await loadRealMidgardContractsForTest({
      txHash: hash(500),
      outputIndex: 0,
    });
    const histories = SDK.requireEventHistoryContracts(contracts);
    const binding: EventHistorySourceBinding = {
      digest: hash(100),
      manifestId: hash(101),
      network: "Preprod",
      endpointIdentitySha256: hash(102),
      genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
      genesisSha256: hash(103),
      hubAddress: contracts.hubOracle.spendingScriptAddress,
      hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
      hubDatumCbor: Data.to(
        await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
        SDK.HubOracleDatum,
      ),
      deployments: {
        deposit: SDK.eventHistoryDeploymentFromContracts(histories.deposit),
        withdrawal: SDK.eventHistoryDeploymentFromContracts(
          histories.withdrawal,
        ),
      },
    };
    const roots = Object.values(binding.deployments).map(
      (deployment, outputIndex) => ({
        txHash: hash(501),
        outputIndex,
        address: deployment.address,
        assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
        datum: Data.to(
          {
            position: "Root",
            next: null,
            protected_until: 0n,
            payload: "RootContent",
          },
          SDK.EventHistoryNode,
        ),
        hasReferenceScript: false,
      }),
    );
    const capture = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          point: { id: hash(50), slot: 500 },
          addresses: [
            binding.hubAddress,
            ...Object.values(binding.deployments).flatMap((entry) => [
              entry.address,
              entry.retentionAddress,
            ]),
          ],
          outputs: [
            ...roots,
            {
              ...f.ref,
              address: binding.hubAddress,
              assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
              datum: binding.hubDatumCbor,
              hasReferenceScript: false,
            },
          ],
        },
        binding,
      ),
    );
    expect(
      await locateEventHistoryActivation(f.options, {
        binding,
        capture,
        expectedTransactionHash: f.ref.txHash,
      }),
    ).toEqual({
      point: target,
      predecessor: { id: middle.id, slot: middle.slot },
      transactionHash: f.ref.txHash,
    });
    expect(f.fetchImpl.mock.calls[0]![0]).toContain(f.ref.txHash);
    expect(f.fetchImpl.mock.calls[0]![0]).not.toContain("unspent");
    await expect(
      locateEventHistoryActivation(f.options, {
        binding,
        capture,
        expectedTransactionHash: hash(777),
      }),
    ).rejects.toThrow(/manifest initialization/);
  });

  it.each([
    "wrong parent",
    "wrong height",
    "wrong slot",
    "rollback",
    "different intersection",
    "missing CBOR",
    "wrong body",
    "duplicate transaction",
    "scan bound",
    "response bound",
    "transaction bound",
  ])("refuses %s and closes its socket", async (fault) => {
    const f = fixture();
    let options = f.options;
    if (fault === "wrong parent") f.blocks[1]!.block.ancestor = hash(44);
    if (fault === "wrong height") f.blocks[1]!.block.height = 4;
    if (fault === "wrong slot") f.blocks[1]!.block.slot = middle.slot;
    if (fault === "rollback")
      f.socket.responses[2] = { direction: "backward", point: ancestor };
    if (fault === "different intersection") f.socket.intersection = target;
    if (fault === "missing CBOR") f.blocks[1]!.block.transactions[0]!.cbor = "";
    if (fault === "wrong body")
      f.blocks[1]!.block.transactions[0]!.cbor = CML.Transaction.new(
        CML.TransactionBody.new(
          CML.TransactionInputList.new(),
          CML.TransactionOutputList.new(),
          101n,
        ),
        CML.TransactionWitnessSet.new(),
        true,
      ).to_cbor_hex();
    if (fault === "duplicate transaction")
      f.blocks[1]!.block.transactions.push(f.blocks[1]!.block.transactions[0]!);
    if (fault === "scan bound") options = { ...options, blockScanLimit: 1 };
    if (fault === "response bound")
      options = { ...options, maximumResponseBytes: 400 };
    if (fault === "transaction bound")
      options = { ...options, maximumTransactionBytes: 1 };
    await expect(
      readEventHistoryCreatingBody(options, f.ref),
    ).rejects.toThrow();
    expect(f.socket.closed).toBe(true);
  });

  it("cancels a pending block read without waiting for the request deadline", async () => {
    const f = fixture();
    f.socket.responses = [];
    const pending = readEventHistoryCreatingBody(f.options, f.ref);
    await vi.waitFor(() =>
      expect(
        f.socket.requests.some(({ method }) => method === "nextBlock"),
      ).toBe(true),
    );
    f.controller.abort();
    await expect(pending).rejects.toThrow(/aborted/);
    expect(f.socket.closed).toBe(true);
  });

  it("bounds and cancels a Kupo response stream before opening an Ogmios socket", async () => {
    const f = fixture();
    const cancel = vi.fn();
    const stream = new ReadableStream<Uint8Array>({
      start(controller) {
        controller.enqueue(new Uint8Array(1000));
      },
      cancel,
    });
    await expect(
      readEventHistoryCreatingBody(
        {
          ...f.options,
          maximumResponseBytes: 100,
          fetchImpl: async () => new Response(stream),
        },
        f.ref,
      ),
    ).rejects.toThrow(/byte bound/);
    expect(cancel).toHaveBeenCalledOnce();
    expect(f.socket.requests).toEqual([]);
  });

  it.each(["abort", "deadline"])(
    "releases an idle Kupo reader on %s even when underlying cancellation never settles",
    async (mode) => {
      const f = fixture();
      const cancel = vi.fn(() => new Promise<void>(() => undefined));
      const stream = new ReadableStream<Uint8Array>({ cancel });
      const fetchImpl = vi.fn(async () => new Response(stream));
      const pending = readEventHistoryCreatingBody(
        {
          ...f.options,
          timeoutMs: mode === "deadline" ? 30 : 2000,
          fetchImpl,
        },
        f.ref,
      );
      const refusal = expect(pending).rejects.toThrow(/aborted|timeout/i);
      if (mode === "abort") {
        await vi.waitFor(() => expect(stream.locked).toBe(true));
        f.controller.abort();
      }
      await refusal;
      expect(cancel).toHaveBeenCalledOnce();
      expect(stream.locked).toBe(false);
      expect(f.socket.requests).toEqual([]);
    },
  );
});
