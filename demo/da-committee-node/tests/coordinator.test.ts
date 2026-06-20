import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import {
  planDaAttestationLifecycle,
  usableCandidatesFor,
} from "../src/coordinator/planner.js";
import {
  isSignerBitSet,
  packSortedSignatureWitnesses,
  setSignerBit,
} from "../src/coordinator/witnesses.js";
import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
} from "../src/domain.js";

describe("coordinator witness and candidate planning", () => {
  it("packs AddSignatures witnesses sorted by strictly increasing signer index", () => {
    const witness2 = "02" + "22".repeat(64);
    const witness0 = "00" + "00".repeat(64);
    expect(packSortedSignatureWitnesses([witness2, witness0])).toBe(
      witness0 + witness2,
    );
    expect(() =>
      packSortedSignatureWitnesses([witness0, "00" + "11".repeat(64)]),
    ).toThrow(/duplicate/);
  });

  it("uses MSB-first signer bitmaps", () => {
    let bitmap = "00".repeat(32);
    bitmap = setSignerBit(bitmap, 0);
    bitmap = setSignerBit(bitmap, 9);
    expect(bitmap.startsWith("8040")).toBe(true);
    expect(isSignerBitSet(bitmap, 0)).toBe(true);
    expect(isSignerBitSet(bitmap, 9)).toBe(true);
    expect(isSignerBitSet(bitmap, 1)).toBe(false);
  });

  it("does not mint duplicate init when a usable candidate exists", () => {
    const candidate = candidateRecord({ attestationCount: 0 });
    const action = planDaAttestationLifecycle({
      headerHash: candidate.headerHash,
      threshold: candidate.threshold,
      committeeSignersHash: candidate.committeeSignersHash,
      candidates: [candidate],
      ownWitnessHex: "00" + "11".repeat(64),
    });
    expect(action.kind).toBe("add_signatures");
  });

  it("waits in threshold-gated mode until witnesses can reach threshold", () => {
    const witness0 = "00" + "11".repeat(64);
    const initialized = candidateRecord({ attestationCount: 0 });
    expect(
      planDaAttestationLifecycle({
        headerHash: initialized.headerHash,
        threshold: 2,
        committeeSignersHash: initialized.committeeSignersHash,
        candidates: [],
        witnessHexes: [witness0],
        requireThresholdWitnesses: true,
      }),
    ).toMatchObject({
      kind: "wait",
      reason: "insufficient witnesses for threshold",
    });
    expect(
      planDaAttestationLifecycle({
        headerHash: initialized.headerHash,
        threshold: 2,
        committeeSignersHash: initialized.committeeSignersHash,
        candidates: [initialized],
        witnessHexes: [witness0],
        requireThresholdWitnesses: true,
      }),
    ).toMatchObject({
      kind: "wait",
      reason: "insufficient witnesses for threshold",
    });
    expect(
      planDaAttestationLifecycle({
        headerHash: initialized.headerHash,
        threshold: 2,
        committeeSignersHash: initialized.committeeSignersHash,
        candidates: [],
        witnessHexes: [witness0, "01" + "22".repeat(64)],
        requireThresholdWitnesses: true,
      }),
    ).toMatchObject({ kind: "init" });
  });

  it("selects threshold candidates for apply and filters stale/foreign candidates", () => {
    const usable = candidateRecord({
      attestationCount: 2,
      status: "threshold",
    });
    const stale = candidateRecord({
      attestationCount: 100,
      status: "stale",
      outRef: "stale#0",
    });
    expect(
      usableCandidatesFor({
        headerHash: usable.headerHash,
        threshold: usable.threshold,
        committeeSignersHash: usable.committeeSignersHash,
        candidates: [stale, usable],
      }),
    ).toEqual([usable]);
    const action = planDaAttestationLifecycle({
      headerHash: usable.headerHash,
      threshold: usable.threshold,
      committeeSignersHash: usable.committeeSignersHash,
      candidates: [stale, usable],
      ownWitnessHex: "00" + "11".repeat(64),
    });
    expect(action).toMatchObject({
      kind: "apply",
      candidateOutRef: usable.outRef,
    });
  });

  it("runs init, add-signatures, and apply after refetching candidates", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const threshold = candidateRecord({
      attestationCount: 2,
      status: "threshold",
    });
    const candidateResponses = [[], [initialized], [threshold]];
    const calls: string[] = [];
    const submissions: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [],
      },
      recordSubmission: async (record) => {
        submissions.push(
          `${record.txKind}:${record.txHash}:${record.inputsUsed.join(",")}`,
        );
      },
      submitter: {
        initAttestation: async () => {
          calls.push("init");
          return "initTx";
        },
        addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
          calls.push(
            `add:${signerIndexes.join(",")}:${packedWitnessesHex.slice(0, 2)}`,
          );
          return "addTx";
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "posted",
    );
    expect(calls).toEqual(["init", "add:0:00", "apply:tx#0"]);
    expect(submissions).toEqual([
      "init:initTx:state#0",
      "add_signatures:addTx:tx#0",
      "apply:applyTx:tx#0,state#0",
    ]);
  });

  it("adds stored peer witnesses with the local witness", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const threshold = candidateRecord({
      attestationCount: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const peerWitness = "01" + "22".repeat(64);
    const duplicateOwnWitness = "00" + "ff".repeat(64);
    const candidateResponses = [[initialized], [threshold]];
    const calls: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      peerWitnessesFor: async () => [
        duplicateOwnWitness,
        peerWitness,
        peerWitness,
      ],
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [threshold],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
          calls.push(`add:${signerIndexes.join(",")}:${packedWitnessesHex}`);
          return "addTx";
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "posted",
    );
    expect(calls).toEqual([
      `add:0,1:${"00" + "11".repeat(64)}${peerWitness}`,
      "apply:tx#0",
    ]);
  });

  it("filters stale stored peer signature records before planning", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const threshold = candidateRecord({
      attestationCount: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const candidateResponses = [[initialized], [threshold]];
    const calls: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      peerSignaturesFor: async () => [
        peerSignatureRecord({
          signerIndex: 1,
          signatureWitness: "01" + "22".repeat(64),
        }),
        peerSignatureRecord({
          signerIndex: 2,
          signatureWitness: "02" + "33".repeat(64),
          payloadHash: "ff".repeat(32),
        }),
        peerSignatureRecord({
          signerIndex: 3,
          signatureWitness: "03" + "44".repeat(64),
          deploymentFingerprint: "other-deployment",
        }),
      ],
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [threshold],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async ({ signerIndexes }) => {
          calls.push(`add:${signerIndexes.join(",")}`);
          return "addTx";
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "posted",
    );
    expect(calls).toEqual(["add:0,1", "apply:tx#0"]);
  });

  it("waits when the selected candidate already has the local signer", async () => {
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      chainReader: {
        fetchDaAttestationCandidates: async () => [
          candidateRecord({
            attestationCount: 1,
            status: "signed",
            bitmap: setSignerBit("00".repeat(32), 0),
          }),
        ],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          throw new Error("unexpected add");
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply");
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "posted",
    );
  });

  it("serializes concurrent publishes for the same header", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const signedByThisNode = candidateRecord({
      attestationCount: 1,
      status: "signed",
      bitmap: setSignerBit("00".repeat(32), 0),
    });
    let addCalls = 0;
    let added = false;
    let releaseAdd!: () => void;
    const addGate = new Promise<void>((resolve) => {
      releaseAdd = resolve;
    });
    let addStarted!: () => void;
    const addStartedSignal = new Promise<void>((resolve) => {
      addStarted = resolve;
    });
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          added ? [signedByThisNode] : [initialized],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          addCalls += 1;
          addStarted();
          await addGate;
          added = true;
          return "addTx";
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply");
        },
      },
    });

    const first = coordinator.publishSignature(signatureRecord());
    const second = coordinator.publishSignature(signatureRecord());
    await addStartedSignal;
    expect(addCalls).toBe(1);
    releaseAdd();
    await expect(Promise.all([first, second])).resolves.toEqual([
      "posted",
      "posted",
    ]);
    expect(addCalls).toBe(1);
  });

  it("recovers from a stale add-signatures input by refetching and applying a threshold candidate", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    const threshold = candidateRecord({
      attestationCount: 2,
      status: "threshold",
    });
    const candidateResponses = [[initialized], [threshold]];
    const calls: string[] = [];
    const submissions: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      raceRecoveryRetryCount: 1,
      raceRecoveryRetryDelayMs: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [threshold],
      },
      recordSubmission: async (record) => {
        submissions.push(
          `${record.txKind}:${record.txHash}:${record.inputsUsed.join(",")}`,
        );
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          calls.push("add");
          throw new Error("input not found: selected UTxO was spent");
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "posted",
    );
    expect(calls).toEqual(["add", "apply:tx#0"]);
    expect(submissions).toEqual(["apply:applyTx:tx#0,state#0"]);
  });

  it("does not hide non-race submitter failures", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    let addCalls = 0;
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      raceRecoveryRetryCount: 2,
      chainReader: {
        fetchDaAttestationCandidates: async () => [initialized],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          addCalls += 1;
          throw new Error("invalid redeemer");
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply");
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "post_failed",
    );
    expect(addCalls).toBe(1);
  });

  it("fails publish when init never becomes visible", async () => {
    let initCalls = 0;
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () => [],
      },
      submitter: {
        initAttestation: async () => {
          initCalls += 1;
          return "initTx";
        },
        addSignatures: async () => {
          throw new Error("unexpected add");
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply");
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "post_failed",
    );
    expect(initCalls).toBe(1);
  });

  it("fails publish when add-signatures is not visible after confirmation", async () => {
    const initialized = candidateRecord({ attestationCount: 0 });
    let addCalls = 0;
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () => [initialized],
      },
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          addCalls += 1;
          return "addTx";
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply");
        },
      },
    });

    await expect(coordinator.publishSignature(signatureRecord())).resolves.toBe(
      "post_failed",
    );
    expect(addCalls).toBe(1);
  });
});

const candidateRecord = ({
  attestationCount,
  status = "initialized",
  outRef = "tx#0",
  bitmap = "00".repeat(32),
}: {
  readonly attestationCount: number;
  readonly status?: DaAttestationCandidateRecord["status"];
  readonly outRef?: string;
  readonly bitmap?: string;
}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  outRef,
  datumCbor: "d87980",
  attestationCount,
  threshold: 2,
  committeeSignersHash: "02".repeat(32),
  bitmap,
  observedChainPoint: {},
  status,
});

const signatureRecord = (): DaSignatureRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  signerIndex: 0,
  signatureWitness: "00" + "11".repeat(64),
  payloadHash: "03".repeat(32),
  committeeSignersHash: "02".repeat(32),
  signedAt: "2026-01-01T00:00:00.000Z",
  broadcastStatus: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_V2_VERSION),
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

const peerSignatureRecord = ({
  signerIndex,
  signatureWitness,
  deploymentFingerprint = "dep",
  payloadHash = "03".repeat(32),
  committeeSignersHash = "02".repeat(32),
}: {
  readonly signerIndex: number;
  readonly signatureWitness: string;
  readonly deploymentFingerprint?: string;
  readonly payloadHash?: string;
  readonly committeeSignersHash?: string;
}): DaSignatureRecord => ({
  ...signatureRecord(),
  deploymentFingerprint,
  signerIndex,
  signatureWitness,
  payloadHash,
  committeeSignersHash,
});
