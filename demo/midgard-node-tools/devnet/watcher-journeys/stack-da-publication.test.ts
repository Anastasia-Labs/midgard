import { existsSync } from "node:fs";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type DaPayloadSubmitStatus,
  decodeDaPayloadSubmitRequestCbor,
  encodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import type { DaProducerTransport } from "midgard-node/da/libp2p-producer";
import {
  manifestFixture,
  parseManifestFixture,
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

import type { JourneyBlock } from "./fixture.js";
import {
  classifyStackDaRemoval,
  publishStackDaPayload,
  readStackDaProducerManifest,
  type StackDaAgedOut,
  type StackDaExpectation,
  StackDaPayloadAgedOutError,
  stackDaProducerPublisher,
  type StackDaTarget,
} from "./stack-da-publication.js";
import { archiveAgedOutCheckpoints } from "./stack-journey-da.js";

const TIMEOUT_MS = SDK.DA_ATTESTATION_TIMEOUT_MS;
const BLOCK_TIME = 1_000_000n;

const block: JourneyBlock = {
  header,
  headerHash,
  payloadEnvelopeCbor: Buffer.from("d8798182030405", "hex"),
};

type CommitteeAnswer =
  | DaPayloadSubmitStatus
  | { status: DaPayloadSubmitStatus; reasonCode: string }
  | "unreachable";

/**
 * A committee reached over the producer protocol. Each publication round
 * takes the next set of answers, one per committee peer; the last repeats.
 */
const committee = (rounds: readonly Record<string, CommitteeAnswer>[]) => {
  const submitted: { peerId: string; headerHash: string; bytes: string }[] = [];
  let round = -1;
  const transport: DaProducerTransport = {
    localPeerId: () => "operator-2-producer",
    sign: () => Buffer.alloc(64, 0x0b),
    request: async (peer, _protocolId, payload) => {
      const request = decodeDaPayloadSubmitRequestCbor(payload);
      submitted.push({
        peerId: peer.peerId,
        headerHash: request.headerHash.toString("hex"),
        bytes: Buffer.from(request.payloadBytes ?? []).toString("hex"),
      });
      const answer =
        rounds[Math.min(round, rounds.length - 1)]![peer.peerId] ?? "accepted";
      if (answer === "unreachable") throw new Error("dial refused");
      const { status, reasonCode } =
        typeof answer === "string"
          ? { status: answer, reasonCode: null }
          : answer;
      return encodeDaPayloadSubmitResponseCbor({
        status,
        headerHash: request.headerHash,
        payloadHash: request.payloadHash,
        reasonCode,
        retryAfterMs: null,
      });
    },
    publish: async () => ({ recipients: [PEER_A, PEER_B] }),
  };
  const manifest = parseManifestFixture();
  const publish = stackDaProducerPublisher(manifest, async () => {
    round += 1;
    return transport;
  });
  return { publish, submitted, rounds: () => round + 1 };
};

/**
 * The stack's L1 as the journey reads it: the header's node follows the
 * script of states, one per poll with the last repeating, or a function of
 * ledger time, which advances by a minute per pause.
 */
const stack = (
  states: readonly StackDaTarget[] | ((now: number) => StackDaTarget),
  removal: () => Promise<StackDaAgedOut> = async () => {
    throw new Error("no removal expected");
  },
) => {
  let clock = Number(BLOCK_TIME);
  let poll = 0;
  const stages: string[] = [];
  return {
    stages,
    ports: (publish: ReturnType<typeof committee>["publish"]) => ({
      readTarget: async () =>
        typeof states === "function"
          ? states(clock)
          : states[Math.min(poll++, states.length - 1)]!,
      publish,
      readRemoval: removal,
      now: () => clock,
      pause: async () => {
        clock += 60_000;
      },
      attestationTimeoutMs: TIMEOUT_MS,
      onStage: (name: string) => stages.push(name),
    }),
  };
};

const unattested: StackDaTarget = {
  kind: "unattested",
  blockTime: BLOCK_TIME,
};
const attested: StackDaTarget = { kind: "attested", txHash: "ab".repeat(32) };
const agedOut: StackDaAgedOut = {
  kind: "agedOut",
  headerHash,
  removalTxHash: "cd".repeat(32),
  removedStateQueueOutRef: `${"ef".repeat(32)}#0`,
};

const publishAs = (
  expectation: StackDaExpectation,
  answers: readonly Record<string, CommitteeAnswer>[],
  states: Parameters<typeof stack>[0],
  removal?: () => Promise<StackDaAgedOut>,
) => {
  const reached = committee(answers);
  const l1 = stack(states, removal);
  return {
    reached,
    l1,
    outcome: publishStackDaPayload(
      block,
      expectation,
      l1.ports(reached.publish),
    ),
  };
};

describe("producer-path DA publication on an attached stack", () => {
  it("submits the exact committed payload to every committee peer and follows the committee's attestation", async () => {
    const { reached, outcome } = publishAs(
      "attestation",
      [{}],
      [unattested, unattested, unattested, attested],
    );
    await expect(outcome).resolves.toEqual(attested);
    expect(reached.rounds()).toBe(1);
    expect(reached.submitted).toEqual(
      [PEER_A, PEER_B].map((peerId) => ({
        peerId,
        headerHash,
        bytes: block.payloadEnvelopeCbor.toString("hex"),
      })),
    );
  });

  it("publishes again while too few peers took the payload, and stops once enough have", async () => {
    const { reached, l1, outcome } = publishAs(
      "attestation",
      [
        { [PEER_A]: "unreachable", [PEER_B]: "unreachable" },
        { [PEER_A]: "deferred" },
        {},
      ],
      [unattested, unattested, unattested, unattested, attested],
    );
    await expect(outcome).resolves.toEqual(attested);
    expect(reached.rounds()).toBe(3);
    expect(
      l1.stages.filter((stage) => stage.includes("retrying")),
    ).toHaveLength(2);
  });

  it("asserts the refusal of a payload the committee must refuse, and the block's removal at its timeout", async () => {
    const refused = {
      status: "rejected",
      reasonCode: "malformed_transaction",
    } as const;
    const { reached, l1, outcome } = publishAs(
      "refusal",
      [{ [PEER_A]: refused, [PEER_B]: refused }],
      [unattested, unattested, { kind: "absent" }],
      async () => agedOut,
    );
    await expect(outcome).resolves.toEqual(agedOut);
    expect(reached.rounds()).toBe(1);
    expect(
      l1.stages.some((stage) => stage.includes("malformed_transaction")),
    ).toBe(true);
  });

  it("fails a refusal drill whose payload the committee attests", async () => {
    const refused = {
      status: "rejected",
      reasonCode: "unsupported_feature",
    } as const;
    await expect(
      publishAs(
        "refusal",
        [{ [PEER_A]: "accepted", [PEER_B]: refused }],
        [unattested, attested],
      ).outcome,
    ).rejects.toThrow(/attested header .* must refuse/u);
  });

  it("fails at once when the committee refuses a payload it must attest", async () => {
    const refused = {
      status: "rejected",
      reasonCode: "malformed_transaction",
    } as const;
    await expect(
      publishAs(
        "attestation",
        [{ [PEER_A]: refused, [PEER_B]: refused }],
        [unattested],
      ).outcome,
    ).rejects.toThrow(/rejected payload .*malformed_transaction/u);
  });

  it("fails at once when a committee peer holds another payload for the header", async () => {
    await expect(
      publishAs("attestation", [{ [PEER_B]: "conflict" }], [unattested])
        .outcome,
    ).rejects.toThrow(new RegExp(`peer ${PEER_B} holds a different payload`));
  });

  it("stops publishing once the block can no longer be attested, and reports that it aged out instead", async () => {
    const timedOutAt = Number(BLOCK_TIME + TIMEOUT_MS);
    const { reached, outcome } = publishAs(
      "attestation",
      [{ [PEER_A]: "unreachable", [PEER_B]: "unreachable" }],
      (now) =>
        now < timedOutAt + 5 * 60_000 ? unattested : { kind: "absent" },
      async () => agedOut,
    );
    await expect(outcome).rejects.toThrow(StackDaPayloadAgedOutError);
    expect(reached.rounds()).toBe(Number(TIMEOUT_MS / 60_000n));
  });

  it("fails when an unattested block outlives its timeout by another timeout", async () => {
    // Removed a minute after the bound: too late.
    const bound = Number(BLOCK_TIME + 2n * TIMEOUT_MS);
    await expect(
      publishAs(
        "refusal",
        [{}],
        (now) => (now <= bound + 60_000 ? unattested : { kind: "absent" }),
        async () => agedOut,
      ).outcome,
    ).rejects.toThrow(/was not removed within an attestation timeout/u);
  });
});

/** A commit that created the unattested node and a removal that burned it. */
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
  const withManifest = async (
    test: (manifestPath: string) => Promise<void>,
  ) => {
    const directory = await mkdtemp(join(tmpdir(), "stack-da-producer-"));
    try {
      const manifestPath = join(directory, "producer-host.json");
      await writeFile(manifestPath, JSON.stringify(manifestFixture()));
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
