import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  type PublishedDaAttestationOutcome,
  type PublishedDaAttestOptions,
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";
import type {
  PublishedDaTargetCorrection,
  PublishedDaTransactionRecord,
} from "midgard-watcher/tests/support/published-da-target-consumption";
import { afterEach, expect, it, vi } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock, JourneyFixtureStage } from "./fixture.js";
import {
  createTransactionJourneyFixture,
  prepareJourneyHistory,
} from "./staging.js";

const actorFactory = vi.hoisted(() => vi.fn());
vi.mock(
  "midgard-watcher/tests/support/published-block-actor",
  async (original) => ({
    ...(await original<
      typeof import("midgard-watcher/tests/support/published-block-actor")
    >()),
    createPublishedWatcherBlockActor: actorFactory,
  }),
);

const directories: string[] = [];
afterEach(async () => {
  actorFactory.mockReset();
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

/** Real checkpoints, retained payloads and SDK datums; only chain/actor I/O is controlled. */
const openStage = async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-staging-recovery-"));
  directories.push(directory);
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const publisher = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account, publisher]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const publisherLucid = await Lucid(emulator, "Custom");
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  const operatorVkey = paymentCredentialOf(account.address).hash;
  const predecessor = {
    ...(await depositEventsRetainedBlock({
      operatorVkey,
      startTime: 0n,
      endTime: 60_000n,
      blockSlot: 60n,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      priorLedger: [],
      events: [],
    })),
    commitTxHash: "11".repeat(32),
  };
  const current = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: predecessor.header.endTime,
    endTime: 120_000n,
    blockSlot: 120n,
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    priorLedger: [],
    events: [],
  });
  const policyId = "ab".repeat(28);
  const commitTxHash = "22".repeat(32);
  const unit = (headerHash: string) =>
    toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash);
  const queueOutput = (
    block: JourneyBlock,
    daAttestation: SDK.StateQueueNode["da_attestation"],
    txHash = commitTxHash,
  ): UTxO => ({
    txHash,
    outputIndex: 0,
    address: account.address,
    assets: { lovelace: 5_000_000n, [unit(block.headerHash)]: 1n },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: block.headerHash } },
      next: "Empty",
      data: Data.castTo(
        {
          proven_fraud: null,
          header: block.header,
          da_attestation: daAttestation,
        },
        SDK.StateQueueNode,
      ),
    }),
  });
  const attested = {
    Attested: { da_bond_asset_name: "cd".repeat(32) },
  };
  const removalTxHash = "55".repeat(32);
  const initRecord: PublishedDaTransactionRecord = {
    step: "init",
    txHash: "44".repeat(32),
    signedCbor: "80",
  };
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
  let removeAfterTargetRead = false;
  let removeWhenRetained = false;
  let inputsLive = false;
  const readOutputs = vi.fn(async (_address: string, requestedUnit: string) => {
    const output = outputs.get(requestedUnit);
    if (requestedUnit === unit(current.headerHash) && removeAfterTargetRead) {
      outputs.delete(requestedUnit);
      removeAfterTargetRead = false;
    }
    return output === undefined ? [] : [output];
  });
  const retain = vi.fn(async (block: JourneyBlock) => {
    if (removeWhenRetained && block.headerHash === current.headerHash)
      removeAfterTargetRead = true;
  });
  const actions: string[] = [];
  const actor = {
    operatorVkey,
    operatorActive: vi.fn(async () => false),
    onboardOperator: vi.fn(async () => {
      actions.push("onboard");
    }),
    commit: vi.fn(
      async (
        block: JourneyBlock,
        _anchor?: UTxO,
        _head?: UTxO,
        _onSigned?: (signed: {
          txHash: string;
          signedCbor: string;
        }) => Promise<void>,
      ) => {
        actions.push("commit");
        outputs.set(unit(block.headerHash), queueOutput(block, "Unattested"));
        return commitTxHash;
      },
    ),
    // Mirrors the real actor's shape: an initial target lookup, DA
    // transactions recorded before submission, and a final target lookup
    // before apply that reconciles an authenticated correction when the
    // running watcher consumed the header in between.
    attest: vi.fn(
      async (
        block: JourneyBlock,
        options: PublishedDaAttestOptions = {},
      ): Promise<PublishedDaAttestationOutcome> => {
        const submitted = [...(options.submitted ?? [])];
        const corrected = (
          removedTxHash: string,
        ): PublishedDaTargetCorrection => ({
          kind: "corrected",
          headerHash: block.headerHash,
          removalTxHash,
          removedStateQueueOutRef: `${removedTxHash}#0`,
          fraudProofOutRef: `${"66".repeat(32)}#0`,
          submittedDaTransactions: submitted.map((record) => ({
            ...record,
            disposition: "absent",
            reason: `removal ${removalTxHash} spent its state queue input`,
          })),
          orphanedAttestationOutRef: null,
        });
        const [initial] = await readOutputs(
          account.address,
          unit(block.headerHash),
        );
        if (initial === undefined) return corrected(commitTxHash);
        const node = await Effect.runPromise(
          SDK.getLinkedListNodeViewFromUTxO(initial).pipe(
            Effect.flatMap(SDK.getStateQueueNodeFromStateQueueDatum),
          ),
        );
        if (node.da_attestation !== SDK.NO_DA_ATTESTATION)
          return { kind: "attested", txHash: initial.txHash };
        actions.push("da-init");
        submitted.push(initRecord);
        await options.onSubmitted?.(initRecord);
        const [final] = await readOutputs(
          account.address,
          unit(block.headerHash),
        );
        if (final === undefined) return corrected(initial.txHash);
        actions.push("da-apply");
        outputs.set(unit(block.headerHash), queueOutput(block, attested));
        return { kind: "attested", txHash: commitTxHash };
      },
    ),
  };
  actorFactory.mockResolvedValue(actor);
  const checkpointPath = join(directory, "staged.json");
  const checkpoint = {
    deploymentFingerprint: "ef".repeat(32),
    predecessor,
    current,
    commitTxHash,
  };
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
      // A signed commit's inputs: unspent while `keepInputsLive` holds.
      getUtxosByOutRef: vi.fn(
        async (outRefs: { txHash: string; outputIndex: number }[]) =>
          inputsLive ? outRefs : [],
      ),
    },
    deployment: {
      manifest: { manifestId: checkpoint.deploymentFingerprint },
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
  const onStage = vi.fn();
  let recoveryStatus:
    | "expired"
    | "invalidated"
    | "included"
    | "conflict"
    | "pending"
    | "unknown" = "expired";
  const recoveryReads = vi.fn();
  const stageInput = (
    onHealthyPredecessor?: JourneyFixtureStage["onHealthyPredecessor"],
  ): JourneyFixtureStage => ({
    context,
    directory,
    retain,
    onHealthyPredecessor,
    onStage,
    historicalNativeScriptProviders: [],
    readSignedCommitRecovery: async (attempt) => {
      recoveryReads(attempt);
      return {
        transactionHash: attempt.txHash,
        signedTransactionCborHex: attempt.signedCbor,
        status: recoveryStatus,
        canonicalPoint: {
          pointId: "aa".repeat(32),
          blockHash: "aa".repeat(32),
          blockNo: "100",
          slot: "100",
        },
        releaseFinalPoint: {
          pointId: "bb".repeat(32),
          blockHash: "bb".repeat(32),
          blockNo: "70",
          slot: "70",
        },
        inputs: [],
        reason:
          recoveryStatus === "expired"
            ? "its inputs are unspent past validity slot " +
              CML.Transaction.from_cbor_hex(attempt.signedCbor).body().ttl()
            : `authenticated ${recoveryStatus}`,
      };
    },
  });
  const stage = (
    onHealthyPredecessor?: JourneyFixtureStage["onHealthyPredecessor"],
  ) => fixture.stage(stageInput(onHealthyPredecessor));
  return {
    directory,
    stageHistory: () =>
      prepareJourneyHistory(stageInput(), async ({ commitHistoryBlock }) => {
        await commitHistoryBlock("recovery", async () => current);
      }),
    actor,
    recoveryReads,
    refreshWallet: vi.spyOn(lucid, "overrideUTxOs"),
    refreshSuccessorWallet: vi.spyOn(publisherLucid, "overrideUTxOs"),
    actions,
    emulator,
    onStage,
    setRecoveryStatus: (status: typeof recoveryStatus) => {
      recoveryStatus = status;
    },
    keepInputsLive: () => {
      inputsLive = true;
    },
    checkpoint,
    checkpointPath,
    current,
    context,
    stage,
    removalTxHash,
    initRecord,
    publishTarget: (
      txHash = commitTxHash,
      daAttestation: SDK.StateQueueNode["da_attestation"] = attested,
    ) =>
      outputs.set(
        unit(current.headerHash),
        queueOutput(current, daAttestation, txHash),
      ),
    removeDuringLastTargetRead: () => {
      removeWhenRetained = true;
    },
  };
};

it("resumes an attested committed target removed during staging without rebonding", async () => {
  const fixture = await openStage();
  await writeJourneyArtifact(fixture.checkpointPath, fixture.checkpoint);
  fixture.publishTarget();
  fixture.removeDuringLastTargetRead();

  const staged = await fixture.stage();
  expect(staged.current.headerHash).toBe(fixture.current.headerHash);
  expect(staged.target).toEqual({
    kind: "attested",
    txHash: fixture.checkpoint.commitTxHash,
  });
  expect(fixture.actor.onboardOperator).not.toHaveBeenCalled();
  expect(fixture.actor.attest).toHaveBeenCalledTimes(1);
  expect(fixture.actor.commit).not.toHaveBeenCalled();
  expect(fixture.actions).toEqual([]);
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual(
    fixture.checkpoint,
  );
});

it("starts observation after retaining healthy history and before preparing the fault", async () => {
  const fixture = await openStage();
  await writeJourneyArtifact(fixture.checkpointPath, fixture.checkpoint);
  const stop = new Error("observer startup reached before fault preparation");
  const onHealthyPredecessor = vi.fn(async (headerHash: string) => {
    expect(headerHash).toBe(fixture.checkpoint.predecessor.headerHash);
    expect(fixture.actor.onboardOperator).not.toHaveBeenCalled();
    expect(fixture.actor.commit).not.toHaveBeenCalled();
    expect(fixture.actor.attest).not.toHaveBeenCalled();
    throw stop;
  });
  await expect(fixture.stage(onHealthyPredecessor)).rejects.toBe(stop);
  expect(onHealthyPredecessor).toHaveBeenCalledOnce();
});

it("finishes staging through the corrected target when correction consumes the header between the initial lookup and DA's final target lookup", async () => {
  const fixture = await openStage();
  await writeJourneyArtifact(fixture.checkpointPath, fixture.checkpoint);
  fixture.publishTarget(fixture.checkpoint.commitTxHash, "Unattested");
  fixture.removeDuringLastTargetRead();

  const staged = await fixture.stage();
  expect(staged.current.headerHash).toBe(fixture.current.headerHash);
  // The journey continues with an explicit outcome instead of throwing on
  // the missing header; the init already sent is preserved and reconciled.
  expect(staged.target).toEqual({
    kind: "corrected",
    headerHash: fixture.current.headerHash,
    removalTxHash: fixture.removalTxHash,
    removedStateQueueOutRef: `${fixture.checkpoint.commitTxHash}#0`,
    fraudProofOutRef: `${"66".repeat(32)}#0`,
    submittedDaTransactions: [
      {
        ...fixture.initRecord,
        disposition: "absent",
        reason: `removal ${fixture.removalTxHash} spent its state queue input`,
      },
    ],
    orphanedAttestationOutRef: null,
  });
  expect(fixture.actions).toEqual(["da-init"]);
  expect(fixture.actor.onboardOperator).not.toHaveBeenCalled();
  expect(fixture.actor.commit).not.toHaveBeenCalled();
  expect(fixture.actor.attest).toHaveBeenCalledWith(
    expect.objectContaining({ headerHash: fixture.current.headerHash }),
    expect.objectContaining({ submitted: [] }),
  );
  const persisted = {
    ...fixture.checkpoint,
    daTransactions: [fixture.initRecord],
    target: staged.target,
  };
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual(persisted);

  // A restart reauthenticates the persisted correction without rebonding.
  const resumed = await fixture.stage();
  expect(resumed.target).toEqual(staged.target);
  expect(fixture.actor.attest).toHaveBeenCalledTimes(2);
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual(persisted);
});

it("hands persisted DA transactions of an interrupted attempt to the reconciliation of an already corrected target", async () => {
  const fixture = await openStage();
  const interrupted = {
    ...fixture.checkpoint,
    daTransactions: [fixture.initRecord],
  };
  await writeJourneyArtifact(fixture.checkpointPath, interrupted);

  const staged = await fixture.stage();
  expect(fixture.actor.attest).toHaveBeenCalledWith(
    expect.objectContaining({ headerHash: fixture.current.headerHash }),
    expect.objectContaining({ submitted: [fixture.initRecord] }),
  );
  expect(staged.target).toMatchObject({
    kind: "corrected",
    removalTxHash: fixture.removalTxHash,
    submittedDaTransactions: [
      expect.objectContaining({ ...fixture.initRecord, disposition: "absent" }),
    ],
  });
  expect(fixture.actions).toEqual([]);
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
    ...interrupted,
    target: staged.target,
  });
});

it("onboards an unsigned uncommitted target before committing and attesting it", async () => {
  const fixture = await openStage();
  const { commitTxHash: _, ...unsigned } = fixture.checkpoint;
  await writeJourneyArtifact(fixture.checkpointPath, unsigned);

  const staged = await fixture.stage();
  expect(staged.current.headerHash).toBe(fixture.current.headerHash);
  expect(staged.target).toEqual({
    kind: "attested",
    txHash: fixture.checkpoint.commitTxHash,
  });
  expect(fixture.actions).toEqual(["onboard", "commit", "da-init", "da-apply"]);
  expect(fixture.actor.attest).toHaveBeenCalledTimes(1);
  // Every DA transaction is recorded before submission and kept afterwards.
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
    ...fixture.checkpoint,
    daTransactions: [fixture.initRecord],
  });
});

it.each([true, false])(
  "reconciles a signed commit before any registration, original output live=%s",
  async (live) => {
    const fixture = await openStage();
    const lucid = fixture.context.deployment.operatorLucid;
    const signed = await (
      await lucid
        .newTx()
        .pay.ToAddress(await lucid.wallet().address(), { lovelace: 5_000_000n })
        .complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const signedCommit = {
      txHash: signed.toHash(),
      signedCbor: signed.toCBOR(),
    };
    expect(
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signedCommit.signedCbor).body(),
      ).to_hex(),
    ).toBe(signedCommit.txHash);
    const { commitTxHash: _, ...unsigned } = fixture.checkpoint;
    await writeJourneyArtifact(fixture.checkpointPath, {
      ...unsigned,
      signedCommit,
    });
    if (live) {
      fixture.setRecoveryStatus("included");
      fixture.publishTarget(signedCommit.txHash);
      await fixture.stage();
      expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
        ...unsigned,
        signedCommit,
        commitTxHash: signedCommit.txHash,
      });
    } else {
      fixture.setRecoveryStatus("conflict");
      await expect(fixture.stage()).rejects.toThrow(
        "Header transaction recovery conflict: authenticated conflict",
      );
      expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
        ...unsigned,
        signedCommit,
      });
    }
    expect(fixture.actor.onboardOperator).not.toHaveBeenCalled();
    expect(fixture.actor.commit).not.toHaveBeenCalled();
  },
);

it("retires a persisted signed commit that expired unsubmitted and completes the journey from the current state", async () => {
  const fixture = await openStage();
  const lucid = fixture.context.deployment.operatorLucid;
  // Interrupted after persisting the signed bytes, before submission; the
  // validity bound then passes with every input still unspent.
  const signed = await (
    await lucid
      .newTx()
      .pay.ToAddress(await lucid.wallet().address(), { lovelace: 5_000_000n })
      .validTo(fixture.emulator.now() + 5_000)
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  const signedCommit = {
    txHash: signed.toHash(),
    signedCbor: signed.toCBOR(),
  };
  const expiresAtSlot = CML.Transaction.from_cbor_hex(signedCommit.signedCbor)
    .body()
    .ttl();
  expect(expiresAtSlot).toBeDefined();
  fixture.emulator.awaitSlot(Number(expiresAtSlot) + 100);
  expect(lucid.currentSlot()).toBeGreaterThan(Number(expiresAtSlot) + 60);
  fixture.keepInputsLive();
  const { commitTxHash: _, ...unsigned } = fixture.checkpoint;
  await writeJourneyArtifact(fixture.checkpointPath, {
    ...unsigned,
    signedCommit,
  });

  // Resuming completes automatically: the impossible attempt is retired and
  // a fresh commit is built from the current protocol state.
  const staged = await fixture.stage();
  expect(staged.current.headerHash).toBe(fixture.current.headerHash);
  expect(staged.target).toEqual({
    kind: "attested",
    txHash: fixture.checkpoint.commitTxHash,
  });
  expect(fixture.actions).toEqual(["onboard", "commit", "da-init", "da-apply"]);
  expect(fixture.onStage).toHaveBeenCalledWith(
    expect.stringContaining(
      `fault commit ${signedCommit.txHash} retired: its inputs are unspent past validity slot ${expiresAtSlot}`,
    ),
  );
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
    ...unsigned,
    commitTxHash: fixture.checkpoint.commitTxHash,
    daTransactions: [fixture.initRecord],
  });

  // A further restart finds the completed commit and never sees the retired bytes.
  await fixture.stage();
  expect(fixture.actor.commit).toHaveBeenCalledTimes(1);
  expect(fixture.actor.onboardOperator).toHaveBeenCalledTimes(1);
});

it("reauthenticates a saved correction after rollback and preserves the signed DA records", async () => {
  const fixture = await openStage();
  const saved = {
    ...fixture.checkpoint,
    daTransactions: [fixture.initRecord],
    target: { kind: "corrected", removalTxHash: fixture.removalTxHash },
  };
  await writeJourneyArtifact(fixture.checkpointPath, saved);
  fixture.publishTarget();
  const staged = await fixture.stage();
  expect(staged.target.kind).toBe("attested");
  expect(fixture.actor.attest).toHaveBeenCalledWith(
    expect.anything(),
    expect.objectContaining({ submitted: [fixture.initRecord] }),
  );
  expect(await readJourneyArtifact(fixture.checkpointPath)).toEqual({
    ...fixture.checkpoint,
    daTransactions: [fixture.initRecord],
  });
  expect(fixture.actor.commit).not.toHaveBeenCalled();
  expect(fixture.actions).toEqual([]);
});

const signedAttempt = () => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("77".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_ttl(30n);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedCbor: transaction.to_cbor_hex(),
  };
};

it.each(["included", "expired", "invalidated"] as const)(
  "reconciles an initial submission error as %s and completes staging without restart",
  async (status) => {
    const fixture = await openStage();
    const { commitTxHash: _, ...draft } = fixture.checkpoint;
    await writeJourneyArtifact(fixture.checkpointPath, draft);
    const attempt = signedAttempt();
    fixture.setRecoveryStatus(status);
    fixture.actor.commit.mockImplementationOnce(
      async (_block, _anchor, _head, onSigned) => {
        await onSigned!(attempt);
        expect(await readJourneyArtifact(fixture.checkpointPath)).toMatchObject(
          { signedCommit: attempt },
        );
        fixture.refreshWallet.mockClear();
        if (status === "included") fixture.publishTarget(attempt.txHash);
        throw new PublishedTransactionSubmissionError(
          attempt.txHash,
          new Error(
            "All inputs are spent. Transaction has probably already been included",
          ),
        );
      },
    );
    const staged = await fixture.stage();
    expect(staged.target.kind).toBe("attested");
    expect(fixture.recoveryReads).toHaveBeenCalledExactlyOnceWith(attempt);
    expect(fixture.refreshWallet).toHaveBeenCalledOnce();
    expect(fixture.actor.commit).toHaveBeenCalledTimes(
      status === "included" ? 1 : 2,
    );
    const saved = await readJourneyArtifact(fixture.checkpointPath);
    expect(saved).toMatchObject({
      commitTxHash:
        status === "included"
          ? attempt.txHash
          : fixture.checkpoint.commitTxHash,
    });
    if (status !== "included") expect(saved).not.toHaveProperty("signedCommit");
  },
);

it("reconciles a missing-output timeout before replacing a commitment already included", async () => {
  const fixture = await openStage();
  const { commitTxHash: _, ...draft } = fixture.checkpoint;
  await writeJourneyArtifact(fixture.checkpointPath, draft);
  const attempt = signedAttempt();
  fixture.setRecoveryStatus("included");
  fixture.actor.commit.mockImplementationOnce(
    async (_block, _anchor, _head, onSigned) => {
      await onSigned!(attempt);
      fixture.publishTarget(attempt.txHash);
      throw new PublishedTransactionExpiredError(
        "header commit",
        attempt.txHash,
        120_000,
      );
    },
  );
  await fixture.stage();
  expect(fixture.recoveryReads).toHaveBeenCalledExactlyOnceWith(attempt);
  expect(fixture.actor.commit).toHaveBeenCalledOnce();
  expect(await readJourneyArtifact(fixture.checkpointPath)).toMatchObject({
    commitTxHash: attempt.txHash,
  });
});

it("keeps an ambiguous initial attempt through pending observations without rebuilding", async () => {
  const fixture = await openStage();
  const { commitTxHash: _, ...draft } = fixture.checkpoint;
  await writeJourneyArtifact(fixture.checkpointPath, draft);
  const attempt = signedAttempt();
  fixture.recoveryReads
    .mockImplementationOnce(() => fixture.setRecoveryStatus("unknown"))
    .mockImplementationOnce(() => fixture.setRecoveryStatus("pending"))
    .mockImplementationOnce(() => {
      fixture.setRecoveryStatus("included");
      fixture.publishTarget(attempt.txHash);
    });
  fixture.actor.commit.mockImplementationOnce(
    async (_block, _anchor, _head, onSigned) => {
      await onSigned!(attempt);
      throw new PublishedTransactionSubmissionError(
        attempt.txHash,
        new Error("response lost"),
      );
    },
  );
  await fixture.stage();
  expect(fixture.recoveryReads).toHaveBeenCalledTimes(3);
  expect(fixture.actor.commit).toHaveBeenCalledOnce();
  expect(await readJourneyArtifact(fixture.checkpointPath)).toMatchObject({
    signedCommit: attempt,
    commitTxHash: attempt.txHash,
  });
});

it.each(["construction", "identity", "conflict"] as const)(
  "does not convert %s failure into permission to rebuild",
  async (failure) => {
    const fixture = await openStage();
    const { commitTxHash: _, ...draft } = fixture.checkpoint;
    await writeJourneyArtifact(fixture.checkpointPath, draft);
    const attempt = signedAttempt();
    fixture.setRecoveryStatus("conflict");
    fixture.actor.commit.mockImplementationOnce(
      async (_block, _anchor, _head, onSigned) => {
        if (failure === "construction") throw new Error("cannot construct");
        await onSigned!(attempt);
        if (failure === "identity")
          throw new Error(
            "Submitted header hash differs from its signed transaction",
          );
        throw new PublishedTransactionSubmissionError(
          attempt.txHash,
          new Error("RPC rejected"),
        );
      },
    );
    await expect(fixture.stage()).rejects.toThrow(
      failure === "construction"
        ? "cannot construct"
        : failure === "identity"
          ? "differs"
          : "recovery conflict",
    );
    expect(fixture.actor.commit).toHaveBeenCalledOnce();
    expect(fixture.recoveryReads).toHaveBeenCalledTimes(
      failure === "conflict" ? 1 : 0,
    );
    if (failure !== "construction")
      expect(await readJourneyArtifact(fixture.checkpointPath)).toMatchObject({
        signedCommit: attempt,
      });
  },
);

it("reconciles a history submission failure and refreshes before its replacement", async () => {
  const fixture = await openStage();
  await mkdir(join(fixture.directory, "work/journeys"), { recursive: true });
  await writeJourneyArtifact(
    join(fixture.directory, "work/journeys/head.json"),
    {
      deploymentFingerprint: fixture.checkpoint.deploymentFingerprint,
      block: fixture.checkpoint.predecessor,
    },
  );
  const attempt = signedAttempt();
  fixture.setRecoveryStatus("invalidated");
  fixture.actor.commit.mockImplementationOnce(
    async (_block, _anchor, _head, onSigned) => {
      await onSigned!(attempt);
      fixture.refreshWallet.mockClear();
      throw new PublishedTransactionSubmissionError(
        attempt.txHash,
        new Error("inputs spent"),
      );
    },
  );
  const history = await fixture.stageHistory();
  expect(history.commitTxHash).toBe(fixture.checkpoint.commitTxHash);
  expect(fixture.recoveryReads).toHaveBeenCalledExactlyOnceWith(attempt);
  expect(fixture.refreshWallet).toHaveBeenCalledOnce();
  expect(fixture.actor.commit).toHaveBeenCalledTimes(2);
});

it("reconciles a successor submission failure and refreshes before its replacement", async () => {
  const fixture = await openStage();
  await writeJourneyArtifact(fixture.checkpointPath, fixture.checkpoint);
  const staged = await fixture.stage();
  const attempt = signedAttempt();
  fixture.setRecoveryStatus("invalidated");
  fixture.actor.commit.mockImplementationOnce(
    async (_block, _anchor, _head, onSigned) => {
      await onSigned!(attempt);
      fixture.refreshSuccessorWallet.mockClear();
      throw new PublishedTransactionSubmissionError(
        attempt.txHash,
        new Error("inputs spent"),
      );
    },
  );
  const successor = await staged.commitHonestSuccessor({
    beforeCommit: async () => {},
  });
  expect(successor.commitTxHash).toBe(fixture.checkpoint.commitTxHash);
  expect(fixture.recoveryReads).toHaveBeenCalledExactlyOnceWith(attempt);
  expect(fixture.refreshSuccessorWallet).toHaveBeenCalledOnce();
  expect(fixture.actor.commit).toHaveBeenCalledTimes(2);
});

it("does not replace a locally timed-out commit while canonical recovery still sees it pending", async () => {
  const fixture = await openStage();
  const { commitTxHash: _, ...draft } = fixture.checkpoint;
  await writeJourneyArtifact(fixture.checkpointPath, draft);
  const attempt = signedAttempt();
  const commit = fixture.actor.commit.getMockImplementation()!;
  fixture.recoveryReads
    .mockImplementationOnce(() => fixture.setRecoveryStatus("unknown"))
    .mockImplementationOnce(() => fixture.setRecoveryStatus("pending"))
    .mockImplementationOnce(() => fixture.setRecoveryStatus("expired"));
  fixture.actor.commit
    .mockImplementationOnce(async (_block, _anchor, _head, onSigned) => {
      await onSigned!(attempt);
      fixture.refreshWallet.mockClear();
      // Local TTL + grace has passed, but the node has not yet advanced its
      // canonical boundary and the original still reserves the mempool inputs.
      throw new PublishedTransactionExpiredError(
        "header commit",
        attempt.txHash,
        120_000,
      );
    })
    .mockImplementationOnce(async (...args) => {
      expect(fixture.recoveryReads).toHaveBeenCalledTimes(3);
      expect(fixture.refreshWallet).toHaveBeenCalledOnce();
      return commit(...args);
    });
  await fixture.stage();
  expect(fixture.actor.commit).toHaveBeenCalledTimes(2);
  expect(await readJourneyArtifact(fixture.checkpointPath)).not.toHaveProperty(
    "signedCommit",
  );
});
