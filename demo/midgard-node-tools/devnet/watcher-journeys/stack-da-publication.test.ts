import {
  type DaPayloadSubmitStatus,
  decodeDaPayloadSubmitRequestCbor,
  encodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import type { DaProducerTransport } from "midgard-node/da/libp2p-producer";
import {
  parseManifestFixture,
  PEER_A,
  PEER_B,
} from "midgard-node/tests/da-payload-libp2p-producer.manifest-fixture";
import {
  header,
  headerHash,
} from "midgard-watcher/tests/published-da-target-consumption.scenario";
import { describe, expect, it } from "vitest";

import type { JourneyBlock } from "./fixture.js";
import { stackDaProducerPublisher } from "./stack-da-producer.js";
import {
  awaitStackDaAgedOut,
  publishStackDaPayload,
  type StackDaAgedOut,
  type StackDaDelivery,
  StackDaDeliveryRejected,
  type StackDaExpectation,
  StackDaPayloadAgedOutError,
  StackDaRefusalNotObserved,
  type StackDaTarget,
} from "./stack-da-publication.js";

const TIMEOUT_MS = SDK.DA_ATTESTATION_TIMEOUT_MS;
const BLOCK_TIME = 1_000_000n;
const DEADLINE = Number(BLOCK_TIME + TIMEOUT_MS);
const THRESHOLD = parseManifestFixture().threshold;

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
  options: { startAt?: number; savedDelivery?: StackDaDelivery } = {},
) => {
  let clock = options.startAt ?? Number(BLOCK_TIME);
  let poll = 0;
  const stages: string[] = [];
  const saved: StackDaDelivery[] = [];
  return {
    stages,
    saved,
    now: () => clock,
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
      committeeThreshold: THRESHOLD,
      delivery: {
        saved: options.savedDelivery,
        save: async (delivery: StackDaDelivery) => {
          saved.push(delivery);
        },
      },
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
  options?: Parameters<typeof stack>[2],
) => {
  const reached = committee(answers);
  const l1 = stack(states, removal, options);
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

  it("observes the refusal of a payload the committee accepts at submit and never attests, once its attestation deadline passes", async () => {
    // The honest committee's handler takes canonicalDecodability and
    // l2TxMistag payloads at submit and withholds its signature.
    const { reached, l1, outcome } = publishAs(
      "refusal",
      [{}],
      () => unattested,
    );
    await expect(outcome).resolves.toEqual({
      kind: "refused",
      headerHash,
      attestationDeadlineMs: DEADLINE,
      delivery: {
        threshold: THRESHOLD,
        peers: { [PEER_A]: "accepted", [PEER_B]: "accepted" },
      },
    });
    expect(reached.rounds()).toBe(1);
    expect(l1.now()).toBeGreaterThanOrEqual(DEADLINE);
    expect(l1.now()).toBeLessThan(DEADLINE + 60_000);
    expect(l1.saved.at(-1)?.peers).toEqual({
      [PEER_A]: "accepted",
      [PEER_B]: "accepted",
    });
  });

  it("observes the refusal of a payload the committee rejects at submit, before the block ages out", async () => {
    const refused = {
      status: "rejected",
      reasonCode: "malformed_transaction",
    } as const;
    const { reached, l1, outcome } = publishAs(
      "refusal",
      [{ [PEER_A]: refused, [PEER_B]: refused }],
      () => unattested,
    );
    await expect(outcome).resolves.toMatchObject({
      kind: "refused",
      delivery: {
        peers: {
          [PEER_A]: expect.stringContaining("malformed_transaction"),
          [PEER_B]: expect.stringContaining("malformed_transaction"),
        },
      },
    });
    expect(reached.rounds()).toBe(1);
    expect(
      l1.stages.some((stage) => stage.includes("malformed_transaction")),
    ).toBe(true);
  });

  it.each([
    "deployment_fingerprint_mismatch",
    "payload_schema_version_mismatch",
    "payload_too_large",
  ])(
    "fails a refusal drill a peer rejects for how it was delivered (%s), keeping every peer's code",
    async (reasonCode) => {
      const { l1, outcome } = publishAs(
        "refusal",
        [
          {
            [PEER_A]: { status: "rejected", reasonCode },
            [PEER_B]: {
              status: "rejected",
              reasonCode: "malformed_transaction",
            },
          },
        ],
        () => unattested,
      );
      await expect(outcome).rejects.toThrow(StackDaDeliveryRejected);
      await expect(outcome).rejects.toThrow(`${PEER_A} rejected`);
      await expect(outcome).rejects.toThrow(reasonCode);
      expect(l1.saved.at(-1)?.peers).toEqual({
        [PEER_A]: `rejected: ${reasonCode}`,
        [PEER_B]: "rejected: malformed_transaction",
      });
    },
  );

  it("refuses to call it a refusal when no committee peer could be reached before the deadline", async () => {
    const { reached, outcome } = publishAs(
      "refusal",
      [{ [PEER_A]: "unreachable", [PEER_B]: "unreachable" }],
      (now) => (now < DEADLINE + 5 * 60_000 ? unattested : { kind: "absent" }),
      async () => agedOut,
    );
    await expect(outcome).rejects.toThrow(StackDaRefusalNotObserved);
    await expect(outcome).rejects.toThrow(/committee unreachable/u);
    expect(reached.rounds()).toBe(Number(TIMEOUT_MS / 60_000n));
  });

  it("refuses to call it a refusal when fewer than the threshold of peers took the payload", async () => {
    const { outcome } = publishAs(
      "refusal",
      [{ [PEER_B]: "unreachable" }],
      () => unattested,
    );
    await expect(outcome).rejects.toThrow(StackDaRefusalNotObserved);
    await expect(outcome).rejects.toThrow(/committee never received payload/u);
  });

  it("refuses a refused block that aged out before enough peers took its payload", async () => {
    await expect(
      publishAs(
        "refusal",
        [{ [PEER_A]: "unreachable", [PEER_B]: "unreachable" }],
        [{ kind: "absent" }],
        async () => agedOut,
      ).outcome,
    ).rejects.toThrow(/committee unreachable/u);
  });

  it("counts a delivery saved before a restart, and keeps the removal it finds", async () => {
    const savedDelivery = {
      threshold: THRESHOLD,
      peers: { [PEER_A]: "accepted", [PEER_B]: "duplicate" },
    };
    const { reached, outcome } = publishAs(
      "refusal",
      [{ [PEER_A]: "unreachable", [PEER_B]: "unreachable" }],
      [{ kind: "absent" }],
      async () => agedOut,
      { startAt: DEADLINE + 120_000, savedDelivery },
    );
    await expect(outcome).resolves.toEqual({
      kind: "refused",
      headerHash,
      // The removed node's block time is the header's end time.
      attestationDeadlineMs: Number(header.endTime + TIMEOUT_MS),
      delivery: savedDelivery,
      removal: agedOut,
    });
    expect(reached.rounds()).toBe(0);
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
        "attestation",
        [{}],
        (now) => (now <= bound + 60_000 ? unattested : { kind: "absent" }),
        async () => agedOut,
      ).outcome,
    ).rejects.toThrow(/was not removed within an attestation timeout/u);
  });
});

describe("a refused block's removal on an attached stack", () => {
  const follow = (
    states: Parameters<typeof stack>[0],
    removal: () => Promise<StackDaAgedOut>,
  ) => {
    const l1 = stack(states, removal, { startAt: DEADLINE });
    return awaitStackDaAgedOut(
      headerHash,
      DEADLINE,
      l1.ports(async () => {
        throw new Error("a removal wait publishes nothing");
      }),
    );
  };

  it("is its authenticated timeout removal", async () => {
    await expect(
      follow([unattested, unattested, { kind: "absent" }], async () => agedOut),
    ).resolves.toEqual(agedOut);
  });

  it("fails when the committee attests it after all", async () => {
    await expect(
      follow([unattested, attested], async () => agedOut),
    ).rejects.toThrow(/attested header .* must refuse/u);
  });

  it("fails when it outlives its deadline by another attestation timeout", async () => {
    await expect(
      follow(
        (now) =>
          now <= DEADLINE + Number(TIMEOUT_MS) + 60_000
            ? unattested
            : { kind: "absent" },
        async () => agedOut,
      ),
    ).rejects.toThrow(/was not removed within an attestation timeout/u);
  });
});

/** A commit that created the unattested node and a removal that burned it. */
