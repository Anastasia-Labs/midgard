import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccountFromPrivateKey,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import {
  AvailabilityResponder,
  type AvailabilityResponderDeps,
  buildAvailabilityResponderTransaction,
  discoverAvailabilityResponderChallenges,
} from "da-committee-node/availability";
import { describe, expect, it } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  attestAvailability,
  availabilityDeployment,
  createAvailabilityFixture,
  openAvailability,
} from "./helpers/availability-challenge-emulator.js";

describe("committee availability responder through the durable production path", () => {
  it("answers public withholding, resumes after partial publication and closes back to the challenger through signed SDK transactions", async () => {
    const dir = await mkdtemp(
      join(tmpdir(), "midgard-availability-responder-"),
    );
    let journal = openAvailabilityOperationJournal(
      join(dir, "operations.sqlite"),
    );
    try {
      const f = await createAvailabilityFixture(14_021);
      // The SDK deployment view, including the pool's spending reference
      // script and the pool policy the snapshot reads.
      const deployment: SDK.DaAvailabilityDeployment =
        availabilityDeployment(f);
      const attested = await attestAvailability(f);
      const responderWallet = generateEmulatorAccountFromPrivateKey({
        lovelace: 0n,
      });
      // Fund collateral before the challenge opens: every emulator block
      // spends 20 s of the selected profile's response window.
      await f.submit(
        "fund independent accountable responder collateral",
        f.lucid
          .newTx()
          .pay.ToAddress(responderWallet.address, { lovelace: 10_000_000n }),
        true,
      );
      const opened = await openAvailability(f, attested);
      await opened.submit();
      // Close settles against the challenge record and never touches the
      // pooled committee bond.
      const beforeResponse = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        deployment,
        f.target.headerHash,
      );
      expect(beforeResponse.record).toBeDefined();
      expect(beforeResponse.pool).toBeDefined();
      f.lucid.selectWallet.fromPrivateKey(responderWallet.privateKey);
      const deploymentFingerprint = "ab".repeat(32);
      const actor = paymentCredentialOf(responderWallet.address).hash;
      expect(actor).not.toBe(f.responderKey);
      const submitted: string[] = [];
      const submittedCbor: string[] = [];
      const offsets: bigint[] = [];
      const readBoundary = async () => ({
        pointId: `emulator:${f.emulator.slot}`,
        slot: f.emulator.slot,
      });
      const context = (): SDK.DaAvailabilityOperationContext => ({
        deploymentIdentity: deploymentFingerprint,
        actor,
        journal,
        stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
        minimumConfirmationDepth: 100,
        transactionLimits: SDK.daAvailabilityOperationLimits(
          f.lucid,
          deployment.parameters,
        ),
        nowMs: () => f.emulator.now(),
        assertActuationCurrent: async () => undefined,
        observe: SDK.createDaAvailabilityOperationObserver({
          // Emulator has block heights but no block hashes. Give its simulated
          // blocks deterministic identities at this test-only provider boundary.
          lucid: {
            ...f.lucid,
            transactionStatus: async (txHash) => {
              const status = await f.lucid.transactionStatus(txHash);
              return status.status === "confirmed"
                ? {
                    ...status,
                    confirmation: {
                      ...status.confirmation,
                      blockHash: createHash("sha256")
                        .update(`emulator:${status.confirmation.blockHeight}`)
                        .digest("hex"),
                    },
                  }
                : status;
            },
          },
          readBoundary,
        }),
        submit: async (cbor) => {
          const hash = await f.emulator.submitTx(cbor);
          submitted.push(hash);
          submittedCbor.push(cbor);
          f.emulator.awaitBlock(1);
          return hash;
        },
      });
      const deps: AvailabilityResponderDeps = {
        deploymentIdentity: deployment.hubOraclePolicyId,
        deploymentFingerprint,
        now: () => f.emulator.now(),
        store: {
          getDaPayload: async () => ({
            deploymentFingerprint,
            headerHash: f.target.headerHash,
            payloadSchemaVersion: 1,
            payloadCborHex: Buffer.from(f.payload).toString("hex"),
            payloadSha256: computeDaSha256Hash(f.payload).toString("hex"),
            sourcePeerId: "withholding-public-peer",
            fetchedAt: new Date(f.emulator.now()).toISOString(),
            validationStatus: "verified",
          }),
        },
        discover: () =>
          discoverAvailabilityResponderChallenges(f.lucid, deployment),
        reconcile: async () => {
          const results =
            await SDK.reconcileDaAvailabilityOperations(context());
          return results.some(
            (result) =>
              !["included", "confirmed", "expired"].includes(result.status),
          )
            ? "pending"
            : "ready";
        },
        execute: async (action) => {
          if (action.kind === "publish")
            offsets.push(action.publication.chunk_offset);
          const result = await SDK.runDaAvailabilityOperation(context(), {
            headerHash: f.target.headerHash,
            action: action.kind,
            build: async () =>
              (
                await buildAvailabilityResponderTransaction(
                  f.lucid,
                  deployment,
                  action,
                  f.emulator.now(),
                )
              ).tx,
          });
          return result.status === "included" || result.status === "confirmed"
            ? result.status
            : "pending";
        },
      };
      let responder = new AvailabilityResponder(deps);
      expect(await responder.tick()).toMatchObject({
        action: "publish",
        status: "pending",
      });
      const firstObservation = await context().observe(
        journal.pending(deploymentFingerprint, actor)[0]!.intent,
      );
      expect(
        firstObservation.status,
        JSON.stringify({
          observation: firstObservation,
          status: await f.lucid.transactionStatus(submitted[0]!),
        }),
      ).toBe("included");
      journal.close();
      journal = openAvailabilityOperationJournal(
        join(dir, "operations.sqlite"),
      );
      responder = new AvailabilityResponder(deps);
      expect(await responder.tick()).toMatchObject({
        action: "publish",
        status: "pending",
      });
      expect(await responder.tick()).toMatchObject({
        action: "settle",
        status: "pending",
      });
      const beforeClose = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        deployment,
        f.target.headerHash,
      );
      const record = beforeClose.recordDatum!;
      const remaining =
        beforeClose.terminalDatum!.remaining_challenger_lovelace;
      expect(await responder.tick()).toMatchObject({
        action: "close",
        status: "pending",
      });
      expect(await responder.tick()).toMatchObject({ status: "idle" });
      expect(offsets).toEqual([0n, 14_020n]);
      expect(new Set(submitted).size).toBe(4);
      const closed = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        deployment,
        f.target.headerHash,
      );
      expect(closed.record).toBeUndefined();
      expect(closed.recordDatum).toBeUndefined();
      expect(closed.pool?.txHash).toBe(beforeResponse.pool!.txHash);
      expect(closed.pool?.outputIndex).toBe(beforeResponse.pool!.outputIndex);
      const queue = await f.lucid.utxosByOutRef([
        { txHash: submitted[3]!, outputIndex: 0 },
      ]);
      const updated = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        deployment,
        f.target.headerHash,
      );
      expect(updated.queue?.utxo.txHash).toBe(queue[0]?.txHash);
      // One challenger output: the terminal's remaining lovelace less the
      // exact Close fee, plus the challenge record lovelace.
      const closeFee = CML.Transaction.from_cbor_hex(submittedCbor[3]!)
        .body()
        .fee();
      const refund = await f.lucid.utxosByOutRef([
        { txHash: submitted[3]!, outputIndex: 1 },
      ]);
      expect(paymentCredentialOf(refund[0]!.address).hash).toBe(
        record.challenger,
      );
      expect(refund[0]!.assets).toEqual({
        lovelace: remaining - closeFee + parameters.challenge_record_lovelace,
      });
    } finally {
      journal.close();
      await rm(dir, { recursive: true, force: true });
    }
  }, 180_000);
});
import { createHash } from "node:crypto";
