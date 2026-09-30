import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

import {
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  type WatcherDurableAtomicBackend,
  type WatcherDurableRecords,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  WatcherDurableStoreError,
  type WatcherDurableStoreErrorCode,
} from "../../src/storage/durable-store.js";

export const hex32 = (byte: string): string => byte.repeat(32);

export const marker = makeDeploymentMarker(hex32("aa"));

const payload = (cborHex = "80") => makeWatcherDurablePayload(cborHex);

export const recordsFixture = (): WatcherDurableRecords => ({
  l1Observations: [
    {
      observationId: hex32("02"),
      providerId: "provider-a",
      chainPointId: hex32("01"),
      payload: payload("820001"),
    },
  ],
  chainPoints: [
    {
      chainPointId: hex32("01"),
      providerId: "provider-a",
      blockHash: hex32("10"),
      slot: "100",
      blockNo: "50",
      depth: "8",
    },
  ],
  protocolUtxos: [
    {
      outRef: `${hex32("11")}#0`,
      role: "state_queue",
      chainPointId: hex32("01"),
      output: payload("d87980"),
    },
  ],
  spentProtocolUtxos: [
    {
      outRef: `${hex32("12")}#0`,
      role: "deposit",
      chainPointId: hex32("01"),
      output: payload("d87981"),
      spentAtChainPointId: hex32("01"),
    },
  ],
  daProofInputs: [
    {
      inputId: hex32("03"),
      kind: "da_payload",
      payload: payload("4401020304"),
    },
  ],
  reconstructedStates: [
    {
      blockHash: hex32("10"),
      chainPointId: hex32("01"),
      priorStateRoot: hex32("12"),
      postStateRoot: hex32("13"),
      inputIds: [hex32("03")],
      state: payload("82190100190101"),
    },
  ],
  decisions: [
    {
      blockHash: hex32("10"),
      decision: "fault_detected",
      reconstructionDigest: hex32("14"),
      evidenceDigest: hex32("15"),
    },
  ],
  faults: [
    {
      faultId: hex32("04"),
      blockHash: hex32("10"),
      familyId: "transition-trace",
      evidence: payload("a10001"),
    },
  ],
  submissions: [
    {
      submissionId: hex32("05"),
      faultId: hex32("04"),
      txBodyHash: hex32("16"),
      status: "submitted",
    },
  ],
  confirmations: [
    {
      confirmationId: hex32("06"),
      submissionId: hex32("05"),
      txHash: hex32("17"),
      chainPointId: hex32("01"),
      depth: "8",
      status: "confirmed",
    },
  ],
  retries: [
    {
      retryId: hex32("07"),
      submissionId: hex32("05"),
      attempt: "1",
      nextEligibleSlot: "101",
      reason: "submission_ambiguous",
    },
  ],
  deadlines: [
    {
      deadlineId: hex32("08"),
      subjectKind: "submission",
      subjectId: hex32("05"),
      kind: "confirmation",
      expiresAtSlot: "120",
    },
  ],
  correctionResults: [
    {
      correctionId: hex32("09"),
      faultId: hex32("04"),
      confirmationId: hex32("06"),
      outcome: "removed_slashed_and_rewarded",
      finalStateRoot: hex32("18"),
      slashLovelace: "5000000",
      rewardLovelace: "1000000",
    },
  ],
});

export const populatedStore = (): WatcherDurableStore =>
  makeWatcherDurableStore({
    deploymentMarker: marker,
    revision: "1",
    records: recordsFixture(),
  });

type MutableRecord = Record<string, any>;

export const mutateStore = (
  store: WatcherDurableStore,
  mutation: (mutable: MutableRecord) => void,
): MutableRecord => {
  const mutable = JSON.parse(JSON.stringify(store)) as MutableRecord;
  mutation(mutable);
  return mutable;
};

export const expectStoreError = (
  operation: () => unknown,
  code: WatcherDurableStoreErrorCode,
): void => {
  try {
    operation();
    throw new Error("Expected watcher durable store rejection");
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDurableStoreError);
    expect((error as WatcherDurableStoreError).code).toBe(code);
  }
};

export class MemoryAtomicBackend implements WatcherDurableAtomicBackend {
  bytes: Uint8Array | null;
  writes = 0;
  failBeforeCommit = false;
  failAfterCommit = false;
  alwaysConflict = false;

  constructor(bytes: Uint8Array | null = null) {
    this.bytes = bytes;
  }

  async read(): Promise<Uint8Array | null> {
    return this.bytes === null ? null : Uint8Array.from(this.bytes);
  }

  async compareAndSwap(
    expectedSha256: string | null,
    next: Uint8Array,
  ): Promise<boolean> {
    if (this.alwaysConflict) {
      return false;
    }
    const actualSha256 =
      this.bytes === null ? null : watcherDurableStoreBytesSha256(this.bytes);
    if (actualSha256 !== expectedSha256) {
      return false;
    }
    if (this.failBeforeCommit) {
      this.failBeforeCommit = false;
      throw new Error("simulated crash before atomic commit");
    }
    this.bytes = Uint8Array.from(next);
    this.writes += 1;
    if (this.failAfterCommit) {
      this.failAfterCommit = false;
      throw new Error("simulated process loss after atomic commit");
    }
    return true;
  }
}
