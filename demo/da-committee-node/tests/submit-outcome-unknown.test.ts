/**
 * A submission whose outcome is unknown (the follower provider's
 * `L1SubmitOutcomeUnknownError`: the transport stopped waiting, or the
 * sidecar exited, while the node may have taken it) is remembered under its
 * transaction id and resolved by the follower's view of that id. Nothing is
 * re-planned while it may still land; a landed one is never rebuilt, and one
 * that never landed is released and planned again.
 */
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Cause, Runtime } from "effect";
import { describe, expect, it } from "vitest";

import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import type { DaSignatureRecord } from "../src/domain.js";
import type { InFlightSubmissionStatus } from "../src/l1/submitter.js";
import {
  inFlightSubmissionStatus,
  refreshL1SubmitterPlainAdaUtxos,
  selectL1SubmitterWallet,
  signSubmitAndConfirm,
} from "../src/l1/submitter.js";
import { candidateRecord, submitted } from "./coordinator.candidate-record.js";

/** An emulator whose submitter wallet holds one 100 ADA UTxO. */
const emulatorSubmitter = async () => {
  const account = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000n,
  });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  await selectL1SubmitterWallet(lucid, `private-key:${account.privateKey}`);
  const [funding] = await emulator.getUtxos(account.address);
  return {
    emulator,
    lucid,
    address: account.address,
    fundingOutRef: `${funding!.txHash}#${funding!.outputIndex.toString()}`,
  };
};

/**
 * Makes the emulator's next submission end with an unknown outcome, as the
 * follower provider reports a transport timeout: `taken` decides whether the
 * node took the transaction first. The provider's error names no id here,
 * so the id the submitter reports is the one it computed before submitting.
 */
const nextSubmitOutcomeUnknown = (emulator: Emulator, taken: boolean) => {
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (tx) => {
    emulator.submitTx = submit;
    if (taken) await submit(tx);
    throw new L1SubmitOutcomeUnknownError(null, "request_timeout");
  };
};

const payToSelf = async (
  lucid: LucidEvolution,
  address: string,
  validTo?: number,
) => {
  const tx = lucid.newTx().pay.ToAddress(address, { lovelace: 5_000_000n });
  return (validTo === undefined ? tx : tx.validTo(validTo)).complete();
};

const ttlSlotOf = (tx: TxSignBuilder): number => {
  const ttl = CML.Transaction.from_cbor_hex(tx.toCBOR()).body().ttl();
  if (ttl === undefined) throw new Error("transaction has no TTL");
  return Number(ttl);
};

describe("a submission whose outcome is unknown, on the emulator", () => {
  it("keeps its inputs out of selection while it is pending, and never rebuilds it once landed", async () => {
    const { emulator, lucid, address, fundingOutRef } =
      await emulatorSubmitter();
    const tx = await payToSelf(lucid, address);
    nextSubmitOutcomeUnknown(emulator, true);

    const failure = await signSubmitAndConfirm(lucid, tx).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(failure).toBeInstanceOf(L1SubmitOutcomeUnknownError);
    const txHash = (failure as L1SubmitOutcomeUnknownError).txHash!;
    expect(txHash).toBe(tx.toHash());

    expect(await inFlightSubmissionStatus(lucid, txHash)).toBe("pending");
    const whilePending = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(whilePending?.spendableOutRefs).not.toContain(fundingOutRef);

    emulator.awaitBlock();
    expect(await inFlightSubmissionStatus(lucid, txHash)).toBe("landed");
    const afterLanding = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(afterLanding?.spendableOutRefs).not.toContain(fundingOutRef);
    expect(
      afterLanding?.spendableOutRefs.every((outRef) =>
        outRef.startsWith(txHash),
      ),
    ).toBe(true);
    expect(emulator.transactionHistory[txHash]).toMatchObject({
      status: "confirmed",
    });
  });

  it("releases its inputs once it never landed by its TTL, so a replacement spends them", async () => {
    const { emulator, lucid, address, fundingOutRef } =
      await emulatorSubmitter();
    const dropped = await payToSelf(lucid, address, emulator.now() + 60_000);
    nextSubmitOutcomeUnknown(emulator, false);

    const failure = await signSubmitAndConfirm(lucid, dropped).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(failure).toBeInstanceOf(L1SubmitOutcomeUnknownError);
    const txHash = (failure as L1SubmitOutcomeUnknownError).txHash!;
    expect(txHash).toBe(dropped.toHash());

    emulator.awaitSlot(ttlSlotOf(dropped) - emulator.slot);
    expect(await inFlightSubmissionStatus(lucid, txHash)).toBe("released");
    const released = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(released?.spendableOutRefs).toEqual([fundingOutRef]);

    const replacement = await payToSelf(lucid, address);
    const replacementHash = await signSubmitAndConfirm(lucid, replacement, {
      awaitConfirmation: false,
    });
    emulator.awaitBlock();
    expect(emulator.transactionHistory[replacementHash]).toMatchObject({
      status: "confirmed",
    });
    expect(emulator.transactionHistory[txHash]).toBeUndefined();
  });
});

describe("the coordinator after a submission whose outcome is unknown", () => {
  const initTxHash = "aa".repeat(32);

  /**
   * What Lucid's `submit()` rejects with: a FiberFailure around its
   * `TxSubmitError`, whose cause is the provider's outcome-unknown error.
   */
  const lucidSubmitOutcomeUnknown = (txHash: string): Error =>
    Runtime.makeFiberFailure(
      Cause.fail(
        Object.assign(new Error("TxSubmitError"), {
          _tag: "TxSubmitError",
          cause: new L1SubmitOutcomeUnknownError(txHash, "sidecar_exited"),
        }),
      ),
    );

  /**
   * A coordinator whose first init ends with an unknown outcome, and whose
   * follower answers `statuses` in turn for that transaction id. The chain
   * shows the candidate once `initLanded()` is set.
   */
  const fixture = (statuses: InFlightSubmissionStatus[]) => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const threshold = candidateRecord({
      attestationCount: 2,
      status: "threshold",
    });
    let landed = false;
    const calls: string[] = [];
    const submissions: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      raceRecoveryRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          !landed ? [] : calls.includes("add") ? [threshold] : [initialized],
      },
      recordSubmission: async (record) => {
        submissions.push(
          `${record.txKind}:${record.txHash}:${record.resultStatus}`,
        );
      },
      submitter: {
        initAttestation: async () => {
          calls.push("init");
          if (calls.filter((call) => call === "init").length === 1)
            throw lucidSubmitOutcomeUnknown(initTxHash);
          landed = true;
          return submitted("replacementInitTx");
        },
        addSignatures: async () => {
          calls.push("add");
          return submitted("addTx");
        },
        applyAttestation: async () => {
          calls.push("apply");
          return submitted("applyTx");
        },
        submissionStatus: async (txHash) => {
          calls.push(`status:${txHash.slice(0, 4)}`);
          return statuses.shift() ?? "pending";
        },
      },
    });
    return {
      coordinator,
      calls,
      submissions,
      initLanded: () => {
        landed = true;
      },
      lastError: () => coordinator.lastPublishError(signatureRecord()),
    };
  };

  it("does not re-plan while the transaction is pending, then records it confirmed once landed and continues without a second init", async () => {
    const f = fixture(["pending", "landed"]);

    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("post_failed");
    expect(f.lastError()).toBe("TxSubmitError");
    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("post_failed");
    expect(f.lastError()).toMatch(
      new RegExp(
        `init transaction ${initTxHash} .* has an unknown submit outcome`,
        "u",
      ),
    );
    expect(f.calls).toEqual(["init", "status:aaaa"]);

    f.initLanded();
    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("posted");
    expect(f.calls).toEqual([
      "init",
      "status:aaaa",
      "status:aaaa",
      "add",
      "apply",
    ]);
    expect(f.submissions).toEqual([
      `init:${initTxHash}:confirmed`,
      "add_signatures:addTx:confirmed",
      "apply:applyTx:confirmed",
    ]);
  });

  it("releases a transaction that never landed and plans the init again", async () => {
    const f = fixture(["pending", "released"]);

    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("post_failed");
    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("post_failed");
    expect(f.calls).toEqual(["init", "status:aaaa"]);

    await expect(
      f.coordinator.publishSignature(signatureRecord()),
    ).resolves.toBe("posted");
    expect(f.calls).toEqual([
      "init",
      "status:aaaa",
      "status:aaaa",
      "init",
      "add",
      "apply",
    ]);
    expect(f.submissions).toEqual([
      "init:replacementInitTx:confirmed",
      "add_signatures:addTx:confirmed",
      "apply:applyTx:confirmed",
    ]);
  });
});

const availabilityCommitmentCbor = SDK.encodeDaAvailabilityCommitment(
  SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "99".repeat(28),
    headerHash: "01".repeat(28),
    payload: Buffer.from("public retained DA"),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  }),
);

const signatureRecord = (): DaSignatureRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  signerIndex: 0,
  signatureWitness: "00" + "11".repeat(64),
  availabilityCommitmentCbor,
  availabilityCommitmentDigest: computeDaSha256Hash(
    Buffer.from(availabilityCommitmentCbor, "hex"),
  ).toString("hex"),
  payloadHash: "03".repeat(32),
  committeeSignersHash: "02".repeat(32),
  signedAt: "2026-01-01T00:00:00.000Z",
  broadcastStatus: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
    rootsMatch: true,
    stateQueueOutRef: "state#0",
    headerHash: "01".repeat(28),
    rootSummary: {
      utxosRoot: "00".repeat(32),
      transactionsRoot: "00".repeat(32),
      depositsRoot: "00".repeat(32),
      withdrawalsRoot: "00".repeat(32),
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    },
    countSummary: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "04".repeat(28),
      prevHeaderHash: "05".repeat(28),
      protocolVersion: "1",
    },
  },
});
