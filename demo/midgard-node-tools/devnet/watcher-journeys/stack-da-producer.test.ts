import { existsSync } from "node:fs";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { createDaLibp2pProducerTransport } from "midgard-node/da/libp2p-producer";
import {
  manifestFixture,
  PEER_A,
  PEER_B,
} from "midgard-node/tests/da-payload-libp2p-producer.manifest-fixture";
import {
  fraudProofPolicyId,
  header,
  headerHash,
  nodeOutput,
  scenario,
  stateQueueAddress,
  stateQueuePolicyId,
  transaction,
} from "midgard-watcher/tests/published-da-target-consumption.scenario";
import { NativeTransactionNotIncludedError } from "midgard-watcher/tests/support/published-da-target-consumption";
import { describe, expect, it } from "vitest";

import { readStackDaProducerManifest } from "./stack-da-producer.js";
import { classifyStackDaRemoval } from "./stack-da-publication.js";
import {
  archiveAgedOutCheckpoints,
  awaitAttestedAtOrAfterStackQueueTail,
  awaitAttestedStackQueueTail,
  type StackQueueNode,
  type StackQueuePorts,
} from "./stack-journey-da.js";

const TIMEOUT_MS = SDK.DA_ATTESTATION_TIMEOUT_MS;

const removalScenario = (removal: { validFromSlot?: bigint }) => {
  const creating = transaction({ outputs: [nodeOutput()] });
  const removing = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
    burn: -1n,
    ...removal,
  });
  const chain = new Map([
    [creating.txHash, creating.tx.to_cbor_hex()],
    [removing.txHash, removing.tx.to_cbor_hex()],
  ]);
  return {
    removalTxHash: removing.txHash,
    input: {
      headerHash,
      stateQueueAddress,
      stateQueuePolicyId,
      fraudProofPolicyId,
      consumptions: [
        {
          txHash: creating.txHash,
          outputIndex: 0,
          spentByTxHash: removing.txHash,
          spentAtSlot: 50,
        },
      ],
      readConfirmedTransaction: async (txHash: string) => {
        const cbor = chain.get(txHash);
        if (cbor === undefined)
          throw new NativeTransactionNotIncludedError(txHash);
        return { cbor };
      },
      // One-second slots from time zero.
      slotToUnixTime: (slot: number) => slot * 1_000,
      attestationTimeoutMs: TIMEOUT_MS,
    },
  };
};

describe("an attached stack's removal of a published block", () => {
  // The node's block time is its header's end time.
  const timeoutSlot = (header.endTime + TIMEOUT_MS + 999n) / 1_000n;

  it("is the block ageing out when it removed the unattested node no earlier than its attestation timeout", async () => {
    const { input, removalTxHash } = removalScenario({
      validFromSlot: timeoutSlot,
    });
    await expect(classifyStackDaRemoval(input)).resolves.toEqual({
      kind: "agedOut",
      headerHash,
      removalTxHash,
      removedStateQueueOutRef: `${input.consumptions[0]!.txHash}#0`,
    });
  });

  it.each([
    ["before the timeout", { validFromSlot: timeoutSlot - 1n }],
    ["with no validity lower bound", {}],
  ])("refuses a removal without a fraud proof %s", async (_, removal) => {
    await expect(
      classifyStackDaRemoval(removalScenario(removal).input),
    ).rejects.toThrow(
      /neither its fraud correction nor its attestation timeout/u,
    );
  });

  it("is the fault's correction when the removal references its fraud proof", async () => {
    const { input, removal, proof } = scenario();
    await expect(
      classifyStackDaRemoval({
        ...input,
        slotToUnixTime: (slot: number) => slot * 1_000,
        attestationTimeoutMs: TIMEOUT_MS,
      }),
    ).resolves.toMatchObject({
      kind: "corrected",
      removalTxHash: removal.txHash,
      fraudProofOutRef: `${proof.txHash}#0`,
    });
  });
});

describe("the drill's producer identity", () => {
  const OPERATOR_KEY = `seed:${"00".repeat(31)}02`;
  const OUR_KEY = `seed:${"00".repeat(31)}01`;
  // Our stack's own host producer manifest, as the e2e-stack writes it: it
  // announces our node's producer, on the host, and bootstraps from the
  // committee's host ports.
  const ourStackManifest = async () => {
    const fixture = manifestFixture() as {
      runtime_topology: Record<string, unknown>;
      da_transport: Record<string, unknown>;
    };
    const ourPeerId = (await loadDaLibp2pIdentity(OUR_KEY)).peerId;
    fixture.runtime_topology.profile = "host";
    fixture.runtime_topology.producer_peer_id = ourPeerId;
    fixture.da_transport.listen_multiaddrs = ["/ip4/127.0.0.1/tcp/26202"];
    fixture.da_transport.announce_multiaddrs = [
      `/ip4/127.0.0.1/tcp/26202/p2p/${ourPeerId}`,
    ];
    fixture.da_transport.bootstrap_multiaddrs = [
      `/ip4/127.0.0.1/tcp/9/p2p/${PEER_A}`,
    ];
    return fixture;
  };
  const withManifest = async (
    test: (manifestPath: string) => Promise<void>,
  ) => {
    const directory = await mkdtemp(join(tmpdir(), "stack-da-producer-"));
    try {
      const manifestPath = join(directory, "producer-host.json");
      await writeFile(manifestPath, JSON.stringify(await ourStackManifest()));
      await test(manifestPath);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  };

  it("publishes to the stack's committee peers as operator-2", async () => {
    await withManifest(async (manifestPath) => {
      const manifest = await readStackDaProducerManifest({
        manifestPath,
        operatorKey: { name: "OPERATOR2_KEY", source: OPERATOR_KEY },
        honestProducerKey: { name: "OUR_KEY", source: OUR_KEY },
      });
      expect(manifest.localPrivateKeySource).toBe(OPERATOR_KEY);
      expect(manifest.committeePeers.map(({ peerId }) => peerId)).toEqual([
        PEER_A,
        PEER_B,
      ]);
    });
  });

  it("opens operator-2's producer transport from our stack's own producer manifest", async () => {
    await withManifest(async (manifestPath) => {
      const manifest = await readStackDaProducerManifest({
        manifestPath,
        operatorKey: { name: "OPERATOR2_KEY", source: OPERATOR_KEY },
        honestProducerKey: { name: "OUR_KEY", source: OUR_KEY },
      });
      const transport = await createDaLibp2pProducerTransport(manifest, {
        mode: "dial-only",
      });
      try {
        expect(transport.localPeerId()).toBe(
          (await loadDaLibp2pIdentity(OPERATOR_KEY)).peerId,
        );
      } finally {
        await transport.close?.();
      }
    });
  });

  it("refuses our node's own producer identity, and a malformed key, by name only", async () => {
    await withManifest(async (manifestPath) => {
      await expect(
        readStackDaProducerManifest({
          manifestPath,
          operatorKey: { name: "OPERATOR2_KEY", source: OUR_KEY },
          honestProducerKey: { name: "OUR_KEY", source: OUR_KEY },
        }),
      ).rejects.toThrow(
        /OPERATOR2_KEY is our node's own DA producer identity/u,
      );
      const malformed = "seed:not-a-secret-value";
      const refusal = readStackDaProducerManifest({
        manifestPath,
        operatorKey: { name: "OPERATOR2_KEY", source: malformed },
        honestProducerKey: { name: "OUR_KEY", source: OUR_KEY },
      });
      await expect(refusal).rejects.toThrow(
        "OPERATOR2_KEY is not a libp2p private key source",
      );
      await expect(refusal).rejects.not.toThrow(/not-a-secret-value/u);
    });
  });
});

it("keeps an aged-out block's checkpoints aside instead of discarding them", async () => {
  const directory = await mkdtemp(join(tmpdir(), "stack-da-aged-out-"));
  try {
    const staged = join(directory, "staged.json");
    const history = join(directory, "history-deposit.json");
    await writeFile(staged, '{"staged":true}');
    await writeFile(history, '{"history":true}');
    const stages: string[] = [];
    archiveAgedOutCheckpoints(
      [history, staged, join(directory, "never-written.json")],
      new Date("2026-10-29T12:00:00.000Z"),
      (name) => stages.push(name),
    );
    expect(existsSync(staged) || existsSync(history)).toBe(false);
    expect((await readdir(join(directory, "archive"))).sort()).toEqual([
      "history-deposit-aged-out-2026-10-29T12-00-00-000Z.json",
      "staged-aged-out-2026-10-29T12-00-00-000Z.json",
    ]);
    expect(stages).toHaveLength(2);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

/** How often a tail wait polls the fake queue. */
const POLL_MS = 10_000;
/** A tail's attestation bound: twice the DA attestation timeout. */
const TAIL_BOUND_MS = 2 * Number(TIMEOUT_MS);

/**
 * The stack's state queue on a clock that moves only while a wait pauses.
 * Before each poll after the first, `beforePoll` may append a block, at the
 * clock's time, attest one, or remove one, as the stack does meanwhile.
 */
const stateQueue = (
  initial: readonly { attested: boolean }[],
  beforePoll: (queue: { blocks: StackQueueNode[]; append(): void }) => void,
) => {
  let clock = 1_000_000_000;
  let reads = 0;
  const blocks: StackQueueNode[] = [];
  const append = (attested = false) => {
    blocks.push({
      headerHash: (blocks.length + 1).toString(16).padStart(64, "0"),
      blockTime: BigInt(clock),
      attested,
    });
  };
  for (const { attested } of initial) append(attested);
  const read = () => {
    // A wait that never stops polling would otherwise hang the test.
    if (++reads > 100_000)
      throw new Error("The wait read the queue without bound");
  };
  const ports: StackQueuePorts<StackQueueNode> = {
    readTail: async () => {
      read();
      return blocks.at(-1)!;
    },
    readFrom: async (headerHash) => {
      read();
      const index = blocks.findIndex(
        (block) => block.headerHash === headerHash,
      );
      return index === -1 ? undefined : blocks.slice(index);
    },
    now: () => clock,
    pause: async () => {
      clock += POLL_MS;
      beforePoll({ blocks, append: () => append() });
    },
    attestationTimeoutMs: TIMEOUT_MS,
    onStage: () => undefined,
  };
  return { blocks, ports, now: () => clock };
};

const attest = (blocks: StackQueueNode[], index: number) => {
  blocks[index] = { ...blocks[index]!, attested: true };
};

const failure = (wait: Promise<unknown>) =>
  wait.then(
    () => undefined,
    (error: unknown) => error as Error,
  );

describe("the fault's predecessor: the stack's current queue tail, attested", () => {
  it("refuses by name, naming the first and latest tails, once the stack has appended unattested tails past the first one's bound", async () => {
    const queue = stateQueue([{ attested: false }], ({ append }) => append());
    const [first] = queue.blocks;
    const error = await failure(awaitAttestedStackQueueTail(queue.ports));
    expect(error?.name).toBe("StackQueueTailsOutpaceAttestationError");
    expect(error?.message).toContain(first!.headerHash);
    expect(error?.message).toContain(queue.blocks.at(-1)!.headerHash);
    expect(queue.now()).toBeGreaterThan(
      Number(first!.blockTime) + TAIL_BOUND_MS,
    );
    expect(queue.now()).toBeLessThanOrEqual(
      Number(first!.blockTime) + TAIL_BOUND_MS + POLL_MS,
    );
  });

  it("returns the tail once the stack pauses appending and the tail is attested", async () => {
    const queue = stateQueue([{ attested: false }], ({ blocks, append }) => {
      if (blocks.length < 4) append();
      else attest(blocks, 3);
    });
    const tail = await awaitAttestedStackQueueTail(queue.ports);
    expect(tail.headerHash).toBe(queue.blocks[3]!.headerHash);
  });

  it("still refuses a lone tail that ages out unattested, as before", async () => {
    const queue = stateQueue([{ attested: false }], () => undefined);
    const [tail] = queue.blocks;
    const error = await failure(awaitAttestedStackQueueTail(queue.ports));
    expect(error?.message).toBe(
      `Stack queue tail ${tail!.headerHash} was neither attested nor removed after its DA attestation timeout`,
    );
    expect(queue.now()).toBeGreaterThan(
      Number(tail!.blockTime) + TAIL_BOUND_MS,
    );
    expect(queue.now()).toBeLessThanOrEqual(
      Number(tail!.blockTime) + TAIL_BOUND_MS + POLL_MS,
    );
  });
});

describe("a funded check: the newest attested block at or after the tail first read", () => {
  it("returns the first tail once attested, while newer unattested tails keep arriving", async () => {
    const queue = stateQueue([{ attested: false }], ({ blocks, append }) => {
      append();
      if (blocks.length === 6) attest(blocks, 0);
    });
    const block = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    expect(block.headerHash).toBe(queue.blocks[0]!.headerHash);
  });

  it("returns a newer block attested after the first tail", async () => {
    const queue = stateQueue([{ attested: false }], ({ blocks, append }) => {
      append();
      if (blocks.length === 6) attest(blocks, 2);
    });
    const block = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    expect(block.headerHash).toBe(queue.blocks[2]!.headerHash);
  });

  it("refuses by name at the first tail's bound when nothing at or after it is attested", async () => {
    const queue = stateQueue([{ attested: false }], ({ append }) => append());
    const [first] = queue.blocks;
    const error = await failure(
      awaitAttestedAtOrAfterStackQueueTail(queue.ports),
    );
    expect(error?.name).toBe("StackQueueTailUnattestedError");
    expect(error?.message).toContain(first!.headerHash);
    expect(queue.now()).toBeGreaterThan(
      Number(first!.blockTime) + TAIL_BOUND_MS,
    );
    expect(queue.now()).toBeLessThanOrEqual(
      Number(first!.blockTime) + TAIL_BOUND_MS + POLL_MS,
    );
  });

  it("refuses by name when the first tail leaves the queue unattested", async () => {
    // Its attested parent becomes the tail again.
    const queue = stateQueue(
      [{ attested: true }, { attested: false }],
      ({ blocks }) => {
        blocks.splice(1);
      },
    );
    const removed = queue.blocks[1]!;
    const error = await failure(
      awaitAttestedAtOrAfterStackQueueTail(queue.ports),
    );
    expect(error?.name).toBe("StackQueueNodeRemovedError");
    expect(error?.message).toContain(removed.headerHash);
  });
});
