import type { AvailabilityOperationRecord } from "@al-ft/midgard-core/availability-operation-journal";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityChallengeAssetName,
  daAvailabilityResponseDeadline,
  encodeDaAvailabilityChallengeRecord,
} from "../src/availability-challenge.js";
import {
  DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS,
  type DaAvailabilityForeignSpendReaders,
  DaAvailabilityWorkflowReleaseHopCapError,
  resolveDaAvailabilityWorkflowRelease,
} from "../src/availability-challenge-operation.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "../src/linked-list.js";

/**
 * P20: a watcher's challenge workflow row is released only on a finalized,
 * verified transaction that burns the header's queue node or closes its
 * challenge, found by walking the node chain from the watcher's own Open.
 * The transactions here are synthetic; each carries only what the walk reads
 * (inputs, outputs by asset, the record datum and the mint).
 */
const QUEUE = "a1".repeat(28);
const AVAILABILITY = "b2".repeat(28);
const HEADER = "22".repeat(28);
const DESCENDANT = "23".repeat(28);
const SECOND_DESCENDANT = "24".repeat(28);
const APPENDED = "25".repeat(28);
const MIN_DEPTH = 30;
const BOUNDARY = 10_000;
const nodeUnit = (header: string) =>
  QUEUE + STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;
const CHALLENGE_ASSET = daAvailabilityChallengeAssetName({
  transactionId: "99".repeat(32),
  outputIndex: 0n,
});
const RECORD_DATUM = encodeDaAvailabilityChallengeRecord({
  commitment: buildDaAvailabilityCommitment({
    deploymentIdentity: "11".repeat(28),
    headerHash: HEADER,
    payload: Uint8Array.from({ length: 1024 }, (_, i) => i % 256),
    responseGeometry: availabilityResponseGeometry(
      DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
  }),
  challenge_asset_name: CHALLENGE_ASSET,
  challenger: "33".repeat(28),
  opened_at: 1_000n,
  response_deadline: daAvailabilityResponseDeadline({
    payloadByteLength: 1024,
    openedAt: 1_000n,
  }),
});
const ADDRESS = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("44".repeat(28))),
).to_address();

type Output = Readonly<{
  assets?: Readonly<Record<string, bigint>>;
  datum?: string;
}>;
type Synthetic = Readonly<{ id: string; cbor: string }>;

const split = (unit: string) =>
  [
    CML.ScriptHash.from_hex(unit.slice(0, 56)),
    CML.AssetName.from_hex(unit.slice(56)),
  ] as const;

let nonce = 0;
const transaction = (input: {
  inputs: readonly string[];
  outputs: readonly Output[];
  mint?: Readonly<Record<string, bigint>>;
  valid?: boolean;
}): Synthetic => {
  const inputs = CML.TransactionInputList.new();
  for (const ref of input.inputs) {
    const [txHash, index] = ref.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(index!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  for (const output of input.outputs) {
    const multiAsset = CML.MultiAsset.new();
    for (const [unit, quantity] of Object.entries(output.assets ?? {}))
      multiAsset.set(...split(unit), quantity);
    outputs.add(
      CML.TransactionOutput.new(
        ADDRESS,
        CML.Value.new(2_000_000n, multiAsset),
        output.datum === undefined
          ? undefined
          : CML.DatumOption.new_datum(
              CML.PlutusData.from_cbor_hex(output.datum),
            ),
      ),
    );
  }
  // A distinct fee keeps otherwise identical bodies distinct.
  const body = CML.TransactionBody.new(inputs, outputs, BigInt(++nonce));
  if (input.mint !== undefined) {
    const mint = CML.Mint.new();
    for (const [unit, quantity] of Object.entries(input.mint))
      mint.set(...split(unit), quantity);
    body.set_mint(mint);
  }
  return {
    id: CML.hash_transaction(body).to_hex(),
    cbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      input.valid ?? true,
      undefined,
    ).to_cbor_hex(),
  };
};

const ref = (tx: Synthetic, index: number) => `${tx.id}#${index.toString()}`;

// The Open: the record (with the challenge asset it mints) and the header's
// node continuing as Challenged. Real Opens put the record first; the walk
// finds both by asset.
const openTx = (
  outputs: readonly Output[] = [
    { assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n }, datum: RECORD_DATUM },
    { assets: { [nodeUnit(HEADER)]: 1n } },
  ],
  mint: Readonly<Record<string, bigint>> = {
    [AVAILABILITY + CHALLENGE_ASSET]: 1n,
  },
) =>
  transaction({
    inputs: [`${"f0".repeat(32)}#0`, `${"f1".repeat(32)}#0`],
    outputs,
    mint,
  });
const OPEN = openTx();
const Q0 = ref(OPEN, 1);
const R0 = ref(OPEN, 0);

const openRecord = (
  open: Synthetic = OPEN,
  state: AvailabilityOperationRecord["state"] = "confirmed",
  overrides: Partial<AvailabilityOperationRecord["intent"]> = {},
): AvailabilityOperationRecord => ({
  intent: {
    id: "open-intent",
    deploymentIdentity: "d1".repeat(32),
    actor: "a0".repeat(28),
    headerHash: HEADER,
    action: "open",
    signedCbor: open.cbor,
    txHash: open.id,
    spentOutRefs: [`${"f0".repeat(32)}#0`, `${"f1".repeat(32)}#0`],
    collateralOutRefs: [],
    expectedOutRefs: [],
    validUntilSlot: 0,
    completesWorkflow: false,
    ...overrides,
  },
  state,
  inclusionPoint: state === "confirmed" ? "1:ab" : null,
  detail: null,
});

// The canonical chain as the readers report it: which transaction spends each
// outRef, at which block. Kupo may also name a spender whose bytes disagree.
type Spend = Readonly<{ tx: Synthetic; blockNo: number }>;
const chain = (
  spends: Readonly<Record<string, Spend>>,
  boundary = BOUNDARY,
): DaAvailabilityForeignSpendReaders => {
  const byId = new Map(
    Object.values(spends).map((spend) => [spend.tx.id, spend]),
  );
  return {
    readBoundary: async () => ({ pointId: "boundary", blockNo: boundary }),
    fetchSpend: async ({ txHash, outputIndex }) => {
      const spend = spends[`${txHash}#${outputIndex.toString()}`];
      return spend === undefined
        ? undefined
        : {
            transactionId: spend.tx.id,
            point: { slot: spend.blockNo * 20, blockHash: spend.tx.id },
          };
    },
    fetchAncestor: async (slot) => ({ slot: slot - 1, blockHash: "00" }),
    readTransaction: async ({ point, txHash }) => {
      const spend = byId.get(txHash);
      return spend === undefined
        ? undefined
        : {
            txHash,
            point: { ...point, blockNo: spend.blockNo },
            cbor: spend.tx.cbor,
          };
    },
  };
};

const release = (
  readers: DaAvailabilityForeignSpendReaders,
  record: AvailabilityOperationRecord = openRecord(),
) => resolveDaAvailabilityWorkflowRelease(readers, record, HEADER, MIN_DEPTH);

const DEEP = BOUNDARY - MIN_DEPTH;

// A Close: spends the node and the record, burns the record's assets, mints
// nothing under the queue policy; the node continues as Published.
const closeOf = (node: string, valid = true) =>
  transaction({
    inputs: [R0, node],
    outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
    mint: { [AVAILABILITY + CHALLENGE_ASSET]: -1n },
    valid,
  });
// A Timeout with no descendant: spends the record and burns the header's node.
const timeoutOf = (node: string) =>
  transaction({
    inputs: [R0, node],
    outputs: [{}],
    mint: {
      [AVAILABILITY + CHALLENGE_ASSET]: -1n,
      [nodeUnit(HEADER)]: -1n,
    },
  });
// A Timeout with a descendant: spends the record, burns the descendant's node,
// and the header's node continues.
const timeoutWithDescendantOf = (node: string) =>
  transaction({
    inputs: [R0, node, `${"d0".repeat(32)}#0`],
    outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }, {}],
    mint: {
      [AVAILABILITY + CHALLENGE_ASSET]: -1n,
      [nodeUnit(DESCENDANT)]: -1n,
    },
  });

describe("availability workflow release (P20)", () => {
  it("releases on a verified Close by anyone at the finality depth", async () => {
    const close = closeOf(Q0);
    await expect(
      release(chain({ [Q0]: { tx: close, blockNo: DEEP } })),
    ).resolves.toStrictEqual({
      reason: "challenge-closed",
      txHash: close.id,
      spendPoint: `${(DEEP * 20).toString()}:${close.id}`,
      confirmationDepth: MIN_DEPTH,
    });
  });

  it("releases on a rival Timeout that burns the header's node", async () => {
    const timeout = timeoutOf(Q0);
    await expect(
      release(chain({ [Q0]: { tx: timeout, blockNo: DEEP } })),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: timeout.id,
    });
  });

  it("releases when the header is pruned as another challenge's descendant", async () => {
    // The ancestor's Timeout removes this header's node; our record is never
    // consumed.
    const prune = transaction({
      inputs: [`${"c0".repeat(32)}#0`, Q0, `${"c1".repeat(32)}#0`],
      outputs: [{}, {}],
      mint: {
        [AVAILABILITY + "c2".repeat(32)]: -1n,
        [nodeUnit(HEADER)]: -1n,
      },
    });
    await expect(
      release(chain({ [Q0]: { tx: prune, blockNo: DEEP } })),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: prune.id,
    });
  });

  it("keeps the row through a rival Timeout with a descendant and its prunes, and releases on the remove", async () => {
    const timeout = timeoutWithDescendantOf(Q0);
    const prune = transaction({
      inputs: [ref(timeout, 0), `${"d1".repeat(32)}#0`],
      outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }, {}],
      mint: { [nodeUnit(SECOND_DESCENDANT)]: -1n },
    });
    const remove = transaction({
      inputs: [ref(prune, 0), `${"d2".repeat(32)}#0`],
      outputs: [{}],
      mint: { [nodeUnit(HEADER)]: -1n },
    });
    const walked = {
      [Q0]: { tx: timeout, blockNo: DEEP - 20 },
      [ref(timeout, 0)]: { tx: prune, blockNo: DEEP - 10 },
    };
    // The record is consumed, yet the header's removal chain still holds it.
    await expect(release(chain({ [Q0]: walked[Q0] }))).resolves.toBeUndefined();
    await expect(release(chain(walked))).resolves.toBeUndefined();
    await expect(
      release(
        chain({ ...walked, [ref(prune, 0)]: { tx: remove, blockNo: DEEP } }),
      ),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: remove.id,
    });
  });

  it("walks through a commit that appends after the header", async () => {
    const append = transaction({
      inputs: [Q0, `${"e0".repeat(32)}#0`],
      outputs: [
        { assets: { [nodeUnit(APPENDED)]: 1n } },
        { assets: { [nodeUnit(HEADER)]: 1n } },
      ],
      mint: { [nodeUnit(APPENDED)]: 1n },
    });
    const close = closeOf(ref(append, 1));
    await expect(
      release(
        chain({
          [Q0]: { tx: append, blockNo: DEEP - 5 },
          [ref(append, 1)]: { tx: close, blockNo: DEEP },
        }),
      ),
    ).resolves.toMatchObject({ reason: "challenge-closed", txHash: close.id });
  });

  it("finds the node and record by asset, not by output index", async () => {
    const open = openTx([
      { assets: { [nodeUnit(HEADER)]: 1n } },
      { assets: { ["c3".repeat(28) + "00"]: 1n } },
      { assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n }, datum: RECORD_DATUM },
    ]);
    const close = transaction({
      inputs: [ref(open, 2), ref(open, 0)],
      outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
      mint: { [AVAILABILITY + CHALLENGE_ASSET]: -1n },
    });
    await expect(
      release(
        chain({ [ref(open, 0)]: { tx: close, blockNo: DEEP } }),
        openRecord(open),
      ),
    ).resolves.toMatchObject({ reason: "challenge-closed" });
  });

  describe("keeps the row", () => {
    it("on a terminal spend one block short of the finality depth", async () => {
      await expect(
        release(chain({ [Q0]: { tx: closeOf(Q0), blockNo: DEEP + 1 } })),
      ).resolves.toBeUndefined();
    });

    it("on an intermediate hop short of the finality depth", async () => {
      const append = transaction({
        inputs: [Q0],
        outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
      });
      await expect(
        release(
          chain({
            [Q0]: { tx: append, blockNo: DEEP + 1 },
            [ref(append, 0)]: { tx: closeOf(ref(append, 0)), blockNo: DEEP },
          }),
        ),
      ).resolves.toBeUndefined();
    });

    it("and refuses a spend above the canonical boundary", async () => {
      await expect(
        release(
          chain({ [Q0]: { tx: closeOf(Q0), blockNo: BOUNDARY + 1 } }),
          openRecord(),
        ),
      ).rejects.toThrow(
        "Availability input spend lies above the canonical boundary",
      );
    });

    it("when the named spender does not list the node", async () => {
      const unrelated = transaction({
        inputs: [R0],
        outputs: [{}],
        mint: { [nodeUnit(HEADER)]: -1n },
      });
      await expect(
        release(chain({ [Q0]: { tx: unrelated, blockNo: DEEP } })),
      ).resolves.toBeUndefined();
    });

    it("when the spender failed phase 2", async () => {
      await expect(
        release(chain({ [Q0]: { tx: closeOf(Q0, false), blockNo: DEEP } })),
      ).resolves.toBeUndefined();
    });

    it("when no spend is reported", async () => {
      await expect(release(chain({}))).resolves.toBeUndefined();
    });

    it("unless the Open is confirmed", async () => {
      const readers = chain({ [Q0]: { tx: closeOf(Q0), blockNo: DEEP } });
      for (const state of [
        "pending",
        "included",
        "conflict",
        "expired",
      ] as const)
        await expect(
          release(readers, openRecord(OPEN, state)),
        ).resolves.toBeUndefined();
      await expect(
        release(readers, openRecord(OPEN, "confirmed", { action: "settle" })),
      ).resolves.toBeUndefined();
      await expect(
        release(
          readers,
          openRecord(OPEN, "confirmed", { headerHash: DESCENDANT }),
        ),
      ).resolves.toBeUndefined();
      await expect(
        release(
          readers,
          openRecord(OPEN, "confirmed", { txHash: "ee".repeat(32) }),
        ),
      ).resolves.toBeUndefined();
    });

    it("when a transaction spends the record but burns a descendant's node", async () => {
      await expect(
        release(
          chain({ [Q0]: { tx: timeoutWithDescendantOf(Q0), blockNo: DEEP } }),
        ),
      ).resolves.toBeUndefined();
    });

    it("when the Open's node or record is missing or ambiguous", async () => {
      const record = {
        assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n },
        datum: RECORD_DATUM,
      };
      const node = { assets: { [nodeUnit(HEADER)]: 1n } };
      const minted = { [AVAILABILITY + CHALLENGE_ASSET]: 1n };
      for (const open of [
        openTx([record], minted),
        openTx([record, node, node], minted),
        openTx(
          [
            record,
            { assets: { ["c4".repeat(28) + nodeUnit(HEADER).slice(56)]: 1n } },
            node,
          ],
          minted,
        ),
        openTx([{ assets: record.assets }, node], minted),
        openTx([record, record, node], minted),
        // A record whose challenge asset the Open did not mint.
        openTx([record, node], {}),
      ]) {
        const close = transaction({
          inputs: [ref(open, 0), ref(open, 1)],
          outputs: [{}],
          mint: { [nodeUnit(HEADER)]: -1n },
        });
        await expect(
          release(
            chain({
              [ref(open, 0)]: { tx: close, blockNo: DEEP },
              [ref(open, 1)]: { tx: close, blockNo: DEEP },
            }),
            openRecord(open),
          ),
        ).resolves.toBeUndefined();
      }
    });

    it("when a hop leaves the header's node in several outputs", async () => {
      const split = transaction({
        inputs: [Q0],
        outputs: [
          { assets: { [nodeUnit(HEADER)]: 1n } },
          { assets: { [nodeUnit(HEADER)]: 1n } },
        ],
      });
      await expect(
        release(
          chain({
            [Q0]: { tx: split, blockNo: DEEP - 1 },
            [ref(split, 0)]: { tx: closeOf(ref(split, 0)), blockNo: DEEP },
          }),
        ),
      ).resolves.toBeUndefined();
    });

    it("and reports a node chain longer than the hop cap", async () => {
      const hops: Record<string, Spend> = {};
      let anchor = Q0;
      for (
        let hop = 0;
        hop < DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS;
        hop++
      ) {
        const next = transaction({
          inputs: [anchor],
          outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
        });
        hops[anchor] = { tx: next, blockNo: DEEP - 1 };
        anchor = ref(next, 0);
      }
      await expect(release(chain(hops))).rejects.toBeInstanceOf(
        DaAvailabilityWorkflowReleaseHopCapError,
      );
      // The terminal step one hop inside the cap still releases.
      const last = Object.values(hops).at(-1)!.tx;
      const trimmed = Object.fromEntries(
        Object.entries(hops).filter(([, spend]) => spend.tx.id !== last.id),
      );
      const lastAnchor = Object.keys(hops).at(-1)!;
      await expect(
        release(
          chain({
            ...trimmed,
            [lastAnchor]: { tx: closeOf(lastAnchor), blockNo: DEEP },
          }),
        ),
      ).resolves.toMatchObject({ reason: "challenge-closed" });
    });
  });
});
