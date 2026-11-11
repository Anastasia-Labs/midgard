import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, expect, it } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock, JourneyFixtureStage } from "./fixture.js";
import { fixtureJourneyLedgerAt } from "./journey-fixture-ledger.test-support.js";
import type { JourneyBlockActor } from "./staging.journey-da-path.js";
import { createTransactionJourneyFixture } from "./staging.js";
import { buildRecoveryStageBlocks } from "./staging-recovery.funded-block.test-support.js";
import { stateQueueOutputs } from "./staging-recovery.queue-output.test-support.js";

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

/** Staging of an uncommitted fault, with only chain and actor I/O scripted. */
const openStage = async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-fault-commit-hook-"));
  directories.push(directory);
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const publisher = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account, publisher]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const publisherLucid = await Lucid(emulator, "Custom");
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  const operatorVkey = paymentCredentialOf(account.address).hash;
  const { predecessor, current } = await buildRecoveryStageBlocks({
    operatorVkey,
    address: account.address,
    funded: false,
  });
  const policyId = "ab".repeat(28);
  const commitTxHash = "22".repeat(32);
  const { unit, queueOutput } = stateQueueOutputs({
    policyId,
    address: account.address,
    commitTxHash,
  });
  const attested = { Attested: { commitment_hash: "cd".repeat(32) } };
  const outputs = new Map<string, UTxO>([
    [unit(predecessor.headerHash), queueOutput(predecessor, attested)],
  ]);
  const rootUnit = toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);
  outputs.set(rootUnit, {
    txHash: "33".repeat(32),
    outputIndex: 0,
    address: account.address,
    assets: { lovelace: 5_000_000n, [rootUnit]: 1n },
    datum: SDK.encodeLinkedListNodeView({
      key: "Empty",
      next: { Key: { key: predecessor.headerHash } },
      data: 0n,
    }),
  });
  const readOutputs = async (_address: string, requestedUnit: string) => {
    const output = outputs.get(requestedUnit);
    return output === undefined ? [] : [output];
  };
  const actions: string[] = [];
  // The block actor staging creates: L1 commits and attestations, scripted.
  const actor = {
    operatorVkey,
    operatorActive: async () => false,
    onboardOperator: async () => {
      actions.push("onboard");
    },
    commit: async (block: JourneyBlock) => {
      actions.push("commit");
      outputs.set(unit(block.headerHash), queueOutput(block, "Unattested"));
      return commitTxHash;
    },
    // Attests a fault committed unattested; an attested one is left as is.
    attest: async (block: JourneyBlock) => {
      const [output] = await readOutputs(
        account.address,
        unit(block.headerHash),
      );
      const node = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(output!).pipe(
          Effect.flatMap(SDK.getStateQueueNodeFromStateQueueDatum),
        ),
      );
      if (node.da_attestation === SDK.NO_DA_ATTESTATION) {
        actions.push("attest");
        outputs.set(unit(block.headerHash), queueOutput(block, attested));
      }
      return { kind: "attested", txHash: commitTxHash };
    },
  } as unknown as JourneyBlockActor;
  const { commitTxHash: _, ...draft } = {
    deploymentFingerprint: "ef".repeat(32),
    predecessor,
    current,
    commitTxHash,
  };
  await writeJourneyArtifact(join(directory, "staged.json"), draft);
  // Staging uses only these deployment ports; no launcher or live configuration is involved.
  const context = {
    runDirectory: directory,
    accounts: {
      operator: { seedPhrase: account.seedPhrase },
      publisher: { seedPhrase: publisher.seedPhrase },
      cosigner: { seedPhrase: publisher.seedPhrase },
    },
    provider: {
      getUtxosWithUnit: readOutputs,
      getUtxosByOutRef: async () => [],
    },
    deployment: {
      manifest: { manifestId: draft.deploymentFingerprint },
      operatorLucid: lucid,
      publisherLucid,
      chain: {
        now: () => 61_000,
        delaySlots: async () => {},
        awaitLedgerTime: async () => {},
      },
      contracts: {
        stateQueue: { policyId, spendingScriptAddress: account.address },
      },
    },
  } as unknown as JourneyFixtureStage["context"];
  const fixture = createTransactionJourneyFixture(
    "networkId",
    async () => current,
  );
  return {
    predecessor,
    actions,
    stage: (beforeFaultCommit: JourneyFixtureStage["beforeFaultCommit"]) =>
      fixture.stage({
        context,
        ledgerAt: fixtureJourneyLedgerAt([predecessor, current]),
        directory,
        retain: async () => {},
        readSignedCommitRecovery: async () => {
          throw new Error("No signed commit is recorded");
        },
        onStage: () => {},
        createBlockActor: async () => actor,
        ...(beforeFaultCommit === undefined ? {} : { beforeFaultCommit }),
      }),
  };
};

it("readies our node's work once before the fault's commit, and not again when a resumed staging finds the fault committed", async () => {
  const fixture = await openStage();
  const readied: string[] = [];
  const beforeFaultCommit: JourneyFixtureStage["beforeFaultCommit"] = async ({
    headerHash,
    header,
    ledger,
  }) => {
    fixture.actions.push("ready");
    readied.push(headerHash);
    // The predecessor comes replayed: the ledger our node must show.
    expect(ledger.utxosRoot).toBe(header.utxosRoot);
    return async () => {
      fixture.actions.push("hand over");
    };
  };

  await fixture.stage(beforeFaultCommit);
  expect(readied).toEqual([fixture.predecessor.headerHash]);
  expect(fixture.actions).toEqual([
    "onboard",
    "ready",
    "hand over",
    "commit",
    "attest",
  ]);

  await fixture.stage(beforeFaultCommit);
  expect(readied).toEqual([fixture.predecessor.headerHash]);
  expect(fixture.actions).toEqual([
    "onboard",
    "ready",
    "hand over",
    "commit",
    "attest",
  ]);
});
