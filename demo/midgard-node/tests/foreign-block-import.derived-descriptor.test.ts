import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  createReplayedLedger,
  type LedgerCheckpoint,
  ledgerEntriesRoot,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerSource,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { placeholderValidationTraceDescriptorV1 } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { countsFromLengths } from "./da-payload.record.js";
import { fixture, rebind, verdict } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

// Validation descriptors are derived data the header does not commit. The DA
// committee certifies any canonical bounded descriptor whose verdict matches
// the committed one (its own fixtures put the transaction id where the
// transaction commitment goes); import and the replayed ledger accept what it
// certifies and refuse what it refuses.

type Descriptor = SDK.ValidationTraceDescriptor;

/** The V1 step-count cap, written out rather than read from the profile. */
const MAX_STEP_COUNT = 4_294_967_295n;

const encoded = (descriptor: Descriptor) =>
  Data.to(descriptor, SDK.ValidationTraceDescriptor);

/** The committee fixture's descriptor: the transaction id as commitment. */
const committeeDescriptor = (
  eventKeyHex: string,
  commitmentHex: string,
  verdictName: "accepted" | "rejected",
  rejection: SDK.RejectionReason | null,
): Descriptor =>
  SDK.validationTraceDescriptorDataFromCore(
    placeholderValidationTraceDescriptorV1(
      {
        eventKeyCbor: Buffer.from(eventKeyHex, "hex"),
        transactionCommitment: Buffer.from(commitmentHex, "hex"),
      },
      verdictName,
      rejection,
    ),
  );

/** Accepted, one step, arbitrary trace root and state hashes. */
const arbitraryAccepted = (eventKeyHex: string): Descriptor => ({
  ...committeeDescriptor(eventKeyHex, "00".repeat(32), "accepted", null),
  trace_root: "c1".repeat(32),
  step_count: 1n,
  initial_state_hash: "c2".repeat(32),
  terminal_state_hash: "c3".repeat(32),
});

const withTrace = (payload: SDK.DaPayload, valueHex: string) => {
  const [[key]] = payload.block_body.validation_traces as [SDK.DaPayloadEntry];
  return rebind({
    ...payload,
    block_body: { ...payload.block_body, validation_traces: [[key, valueHex]] },
  });
};

const normalWith = async (
  descriptor: (eventKeyHex: string, txId: string) => Descriptor | string,
) => {
  const f = await normalFixture();
  const [[key]] = f.payload.block_body.validation_traces as [
    SDK.DaPayloadEntry,
  ];
  const value = descriptor(key, f.transactionId);
  const payload = await withTrace(
    f.payload,
    typeof value === "string" ? value : encoded(value),
  );
  return { payload, imported: () => f.imported(payload) };
};

/** Lucid writes a constructor's fields as an indefinite list; the same
 * fields as a definite list decode to the same value but are not canonical. */
const definiteFields = (valueHex: string) => {
  expect(valueHex.startsWith("d8799f") && valueHex.endsWith("ff")).toBe(true);
  return `d87988${valueHex.slice(6, -2)}`;
};

type ImportResult = Awaited<ReturnType<typeof verdict>>;

const refusal = (result: ImportResult) =>
  result._tag === "Right"
    ? "imported"
    : result.left instanceof ForeignBlockVerificationError
      ? `${result.left.reason}: ${result.left.detail}`
      : String(result.left);

/** The imported ledger root; the refusal is named on failure. */
const importedRoot = (result: ImportResult) => {
  expect(refusal(result)).toBe("imported");
  return result._tag === "Right" ? result.right.root : undefined;
};

/** Refused as the block's fault, over its validation descriptors. */
const refusedOverDescriptor = (result: ImportResult) => {
  expect(refusal(result)).toMatch(/^invalid: .*descriptor/u);
};

describe("foreign import of derived validation descriptors", () => {
  it("imports a normal transaction under the committee fixture's descriptor", async () => {
    const f = await normalWith((key, txId) =>
      committeeDescriptor(key, txId, "accepted", null),
    );
    const result = await f.imported();
    expect(importedRoot(result)).toBe(f.payload.block_body.header.utxosRoot);
  });
  it("imports a normal transaction under an accepted one-step descriptor with arbitrary state hashes", async () => {
    const f = await normalWith((key) => arbitraryAccepted(key));
    const result = await f.imported();
    expect(importedRoot(result)).toBe(f.payload.block_body.header.utxosRoot);
  });
  it("imports a rejected forced transaction under a descriptor whose commitment is its order id", async () => {
    const payload = await fixture();
    const [[key]] = payload.block_body.validation_traces as [
      SDK.DaPayloadEntry,
    ];
    const result = await verdict(
      await withTrace(
        payload,
        encoded(
          committeeDescriptor(key, "5a".repeat(32), "rejected", "EmptyInputs"),
        ),
      ),
    );
    expect(importedRoot(result)).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  });
  it("imports a rejected forced transaction whose descriptor names another rejection", async () => {
    const payload = await fixture();
    const [[key]] = payload.block_body.validation_traces as [
      SDK.DaPayloadEntry,
    ];
    const result = await verdict(
      await withTrace(
        payload,
        encoded(
          committeeDescriptor(
            key,
            "5a".repeat(32),
            "rejected",
            "NetworkIdMismatch",
          ),
        ),
      ),
    );
    importedRoot(result);
  });

  it("refuses a descriptor that is not canonical for its schema", async () => {
    const f = await normalWith((key, txId) =>
      definiteFields(encoded(committeeDescriptor(key, txId, "accepted", null))),
    );
    refusedOverDescriptor(await f.imported());
  });
  it("refuses a descriptor that writes an inner integer non-minimally", async () => {
    const f = await normalWith((key) => {
      const minimal = `5820${"c1".repeat(32)}05`;
      const valueHex = encoded({ ...arbitraryAccepted(key), step_count: 5n });
      expect(valueHex.split(minimal)).toHaveLength(2);
      // The step count 5 as a one-byte-argument integer (`18 05`).
      return valueHex.replace(minimal, `5820${"c1".repeat(32)}1805`);
    });
    refusedOverDescriptor(await f.imported());
  });
  it("imports a normal transaction whose descriptor's step count is the bound", async () => {
    const f = await normalWith((key) => ({
      ...arbitraryAccepted(key),
      step_count: MAX_STEP_COUNT,
    }));
    importedRoot(await f.imported());
  });
  it("imports a rejected forced transaction whose descriptor's step count is the bound", async () => {
    const payload = await fixture();
    const [[key]] = payload.block_body.validation_traces as [
      SDK.DaPayloadEntry,
    ];
    const descriptor = {
      ...committeeDescriptor(key, "5a".repeat(32), "rejected", "EmptyInputs"),
      step_count: MAX_STEP_COUNT,
    };
    importedRoot(await verdict(await withTrace(payload, encoded(descriptor))));
  });
  it("refuses a descriptor whose step count exceeds the bound", async () => {
    const f = await normalWith((key) => ({
      ...arbitraryAccepted(key),
      step_count:
        BigInt(MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount) + 1n,
    }));
    refusedOverDescriptor(await f.imported());
  });
  it("refuses a descriptor whose verdict is not terminal", async () => {
    const f = await normalWith((key) => ({
      ...arbitraryAccepted(key),
      verdict: "Pending",
    }));
    refusedOverDescriptor(await f.imported());
  });
  it("refuses an accepted descriptor that carries a rejection code", async () => {
    const f = await normalWith((key) => ({
      ...arbitraryAccepted(key),
      rejection_code_hash: "c4".repeat(32),
    }));
    refusedOverDescriptor(await f.imported());
  });
  it("refuses a rejected descriptor for an accepted normal transaction", async () => {
    const f = await normalWith((key, txId) =>
      committeeDescriptor(key, txId, "rejected", "EmptyInputs"),
    );
    refusedOverDescriptor(await f.imported());
  });
  it("refuses an accepted descriptor for a rejected forced transaction", async () => {
    const payload = await fixture();
    const [[key]] = payload.block_body.validation_traces as [
      SDK.DaPayloadEntry,
    ];
    refusedOverDescriptor(
      await verdict(
        await withTrace(
          payload,
          encoded(committeeDescriptor(key, "5a".repeat(32), "accepted", null)),
        ),
      ),
    );
  });
  it("refuses a block that leaves out a transaction's descriptor", async () => {
    const f = await normalFixture();
    const body = f.payload.block_body;
    const payload = await rebind({
      ...f.payload,
      block_body: {
        ...body,
        validation_traces: [],
        counts: { ...body.counts, validationTraceCount: 0n },
      },
    });
    // Named as absent, not as an undecodable descriptor.
    expect(refusal(await f.imported(payload))).toMatch(
      /^invalid: .*lacks its validation descriptor/u,
    );
  });
  it("refuses a descriptor for an event the block does not carry", async () => {
    const f = await normalFixture();
    const body = f.payload.block_body;
    const stray = Data.to(
      { L2TransactionEventKey: { tx_id: "ee".repeat(32) } } as SDK.EventKey,
      SDK.EventKey,
    );
    const payload = await rebind({
      ...f.payload,
      block_body: {
        ...body,
        validation_traces: [
          ...body.validation_traces,
          [stray, encoded(arbitraryAccepted(stray))],
        ],
        counts: { ...body.counts, validationTraceCount: 2n },
      },
    });
    refusedOverDescriptor(await f.imported(payload));
  });
});

// The funded ledger the native normal fixture spends from.
const parentEntries = () => {
  const signer = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x17));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signer.to_public().hash()),
  ).to_address();
  return [
    {
      outRef: makeOutRefCbor(0x11),
      output: Buffer.from(
        makeMidgardTxOutput(
          address,
          CML.Value.from_coin(3_000_000n),
        ).to_cbor_bytes(),
      ),
    },
  ];
};

/** An empty block on top of `parent`. */
const emptyChild = (parent: SDK.DaPayload) => {
  const header = parent.block_body.header;
  const counts = countsFromLengths({});
  return rebind({
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header: {
        ...header,
        ...counts,
        prevHeaderHash: parent.block_body.header_hash,
        prevUtxosRoot: header.utxosRoot,
      },
      header_hash: "00".repeat(28),
      deposits: [],
      withdrawals: [],
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  });
};

const watcherLedger = async (block: SDK.DaPayload) => {
  const header = block.block_body.header;
  let checkpoint: LedgerCheckpoint | undefined = {
    headerHash: header.prevHeaderHash,
    entries: parentEntries(),
  };
  const refusals: ReplayedLedgerRefusal[] = [];
  const ledger = createReplayedLedger({
    checkpoints: {
      read: async () => checkpoint,
      write: async (next) => {
        checkpoint = next;
      },
    },
    onRefusal: (refusal) => refusals.push(refusal),
  });
  expect(
    await ledger.restore({
      headerHash: header.prevHeaderHash,
      utxosRoot: header.prevUtxosRoot,
    }),
  ).toBe("restored");
  const child = await emptyChild(block);
  const source: ReplayedLedgerSource = {
    payloadAt: async (headerHash) =>
      [block, child].find(
        (payload) => payload.block_body.header_hash === headerHash,
      ),
    materialFor: async (payload) => ({
      blockTimeMs: Number(payload.block_body.header.endTime),
      replayUserEvent: () => Effect.die("fixture has no user event"),
      verifyForcedSource: () => Effect.die("fixture has no forced order"),
    }),
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  };
  return {
    refusals,
    child,
    at: (payload: SDK.DaPayload) =>
      ledger.ledgerAt(source)(payload.block_body.header_hash),
  };
};

describe("the replayed ledger over derived validation descriptors", () => {
  it("derives the ledger of a block under the committee fixture's descriptor, and above it", async () => {
    const { payload } = await normalWith((key, txId) =>
      committeeDescriptor(key, txId, "accepted", null),
    );
    const w = await watcherLedger(payload);
    const after = await w.at(payload);
    expect(after).toBeDefined();
    expect(await ledgerEntriesRoot(after!)).toBe(
      payload.block_body.header.utxosRoot,
    );
    const above = await w.at(w.child);
    expect(above).toBeDefined();
    expect(await ledgerEntriesRoot(above!)).toBe(
      payload.block_body.header.utxosRoot,
    );
    expect(w.refusals).toEqual([]);
  });
  it("refuses a block whose descriptor verdict differs from the replay, and derives nothing above it", async () => {
    const { payload } = await normalWith((key, txId) =>
      committeeDescriptor(key, txId, "rejected", "EmptyInputs"),
    );
    const w = await watcherLedger(payload);
    expect(await w.at(payload)).toBeUndefined();
    expect(await w.at(w.child)).toBeUndefined();
    expect(w.refusals.map(({ kind }) => kind)).toContain("replay-refused");
  });
});
