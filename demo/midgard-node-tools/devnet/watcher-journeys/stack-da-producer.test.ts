import { existsSync } from "node:fs";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
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
import { archiveAgedOutCheckpoints } from "./stack-journey-da.js";

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
