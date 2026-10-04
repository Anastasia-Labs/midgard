import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Emulator, Lucid as makeLucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { vi } from "vitest";

import * as Adoptions from "../../src/database/foreignNativeAdoptions.js";
import type { HistoryRecoveryPreparation } from "../../src/services/event-history-recovery.js";
import * as ConfirmedRecovery from "../../src/services/foreign-confirmed-ledger.js";
import { recoverForeignNativeAdoptions } from "../../src/services/foreign-native-adoption.js";
import { assertForeignVerificationSource } from "../../src/services/foreign-verification-source.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../../src/services/midgard-contracts.js";
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "../../src/services/mpf-native-owner/protocol.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "../../src/services/mpf-native-owner/service.normalize-owner-options.js";
import * as Topology from "../../src/services/state-queue-topology.js";
import * as Verification from "../../src/workers/commit-block-header.verify-foreign-base.js";
import { countsFromLengths, headerFor } from "../da-payload.record.js";
import { hash, run } from "../event-history-recovery-plans.registration.js";
import { loadRealMidgardContractsForTest } from "../helpers/real-midgard-contracts.js";

/** Recovery component: actual source SQL authority and coordinator; complete
 * L1 semantic verification and native RPC are explicit controlled boundaries.
 */
export const adoptionRecoveryFixture = async (
  initial: Verification.VerifiedForeignCommitBase,
  replay: PersistedNativeMpfReplay,
) => {
  let base = initial;
  let durableRoot = replay.baseRoot;
  let crashAfterPromotion = false;
  const phases: string[] = [];
  const handle = {
    ownerEpoch: Buffer.alloc(16, 1),
    generationId: Buffer.alloc(16, 2),
    baseRoot: replay.baseRoot,
  };
  const owner: NativeMpfOwnerService = {
    createWorkerPort: () => {
      throw new Error("Recovery cannot start a worker");
    },
    terminalFailure: () => undefined,
    close: async () => undefined,
    fork: vi.fn(async () => {
      phases.push("fork");
      return handle;
    }),
    applyEvents: vi.fn(async (_handle, log) => ({
      handle,
      candidateRoot: base.root,
      eventRoots: Array.from({ length: replay.eventCount }, () => base.root),
      eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex"),
      proofArenaDurationNs: 0,
      mutationDurationNs: 0,
    })),
    discard: vi.fn(async () => {
      phases.push("discard");
    }),
    promote: vi.fn(async () => {
      phases.push(
        `promote:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      durableRoot = base.root;
      if (crashAfterPromotion) {
        crashAfterPromotion = false;
        throw new Error("Modeled process crash after native promotion");
      }
    }),
    recover: vi.fn(async (log) => {
      phases.push(
        `recover:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      durableRoot = log.candidateRoot;
    }),
    restoreCanonicalRoot: vi.fn(async (plan) => {
      phases.push(
        `restore:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      if (durableRoot !== plan.expectedRoot && durableRoot !== plan.targetRoot)
        throw new Error("Native recovery CAS mismatch");
      durableRoot = plan.targetRoot;
    }),
    diagnostics: vi.fn(async () => ({
      ownerEpoch: handle.ownerEpoch,
      durableRoot,
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    })),
  };
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  const api = await makeLucid(new Emulator([]), "Custom");
  const service = Lucid.make({
    api,
    referenceScriptsApi: api,
    operatorMainAddress: contracts.stateQueue.spendingScriptAddress,
    operatorMergeAddress: contracts.stateQueue.spendingScriptAddress,
    referenceScriptsWalletAddress: contracts.stateQueue.spendingScriptAddress,
    referenceScriptsAddress: contracts.stateQueue.spendingScriptAddress,
    submitSlotSnapshot: () =>
      Effect.fail(new Error("Recovery fixture cannot submit")),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    switchToReferenceScriptWallet: Effect.void,
  });
  const header = headerFor(
    {
      utxosRoot: base.root,
      transactionsRoot: hash(0),
      depositsRoot: hash(0),
      withdrawalsRoot: hash(0),
      forcedTransactionsRoot: hash(0),
      transitionTraceRoot: hash(0),
      eventToStepRoot: hash(0),
      validationTracesRoot: hash(0),
    },
    countsFromLengths({}),
  );
  const node: SDK.StateQueueUTxO = {
    utxo: {
      txHash: hash(99),
      outputIndex: 0,
      address: contracts.stateQueue.spendingScriptAddress,
      assets: { lovelace: 2_000_000n },
    },
    datum: {
      key: { Key: { key: base.headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header, da_attestation: SDK.NO_DA_ATTESTATION, proven_fraud: null },
        SDK.StateQueueNode,
      ),
    },
    assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + base.headerHash,
  };
  vi.spyOn(Topology, "fetchCanonicalStateQueueNodesProgram").mockReturnValue(
    Effect.succeed([node]),
  );
  const verify = vi
    .spyOn(Verification, "verifyForeignCommitBase")
    .mockImplementation(() => Effect.succeed(base));
  vi.spyOn(
    ConfirmedRecovery,
    "reconcileForeignConfirmedLedger",
  ).mockReturnValue(Effect.succeed(undefined));
  vi.spyOn(Verification, "revalidateForeignCommitBase").mockImplementation(
    (accepted) =>
      assertForeignVerificationSource({
        kind: "recovery",
        binding: accepted.history,
      }),
  );
  const recover = (preparation: HistoryRecoveryPreparation) =>
    run(
      recoverForeignNativeAdoptions({
        owner,
        ownerBinarySha256: replay.ownerBinarySha256,
        coverage: base.history.coverage,
        preparation,
      }).pipe(
        Effect.provideService(Lucid, service),
        Effect.provideService(
          MidgardContracts,
          MidgardContracts.make({
            ...contracts,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "manifest",
            manifestId: base.history.token.deploymentIdentity,
            deploymentMarker: makeDeploymentMarker(
              base.history.token.deploymentIdentity,
            ),
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
      ),
    );
  return {
    owner,
    phases,
    verify,
    recover,
    root: () => durableRoot,
    crashAfterPromotion: () => {
      crashAfterPromotion = true;
    },
    rebind: (current: Verification.VerifiedForeignCommitBase) => {
      base = current;
    },
  };
};
