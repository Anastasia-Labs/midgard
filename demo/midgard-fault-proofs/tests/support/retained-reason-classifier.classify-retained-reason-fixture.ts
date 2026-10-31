import {
  type AuthenticatedStateQueueHeaderObservation,
  EMPTY_MERKLE_TREE_ROOT,
} from "@al-ft/midgard-sdk";

import type { LedgerAt } from "../../src/ledger-at.js";
import { retainedPayloadReplaySource } from "../../src/retained-payload-replay-source.js";
import type { RetainedDaPayloadSource } from "../../src/transition-trace/fetch.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "../../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import type { FraudProofReleaseFinalityAuthority } from "../../src/workflow/release-finality-policy.js";
import { fixtureLedgerAt } from "./fixture-ledger.js";

/** Runs the production classifier against retained bytes and an L1 observation. */
export const classifyRetainedReasonFixture = async ({
  observation,
  payloadEnvelopeCbor,
  deploymentFingerprint,
  releaseFinalityAuthority,
  replayer,
  predecessor,
  history = [],
  replayContext,
  transitionTraceEventAuthority,
  predecessorLedgerAt,
}: {
  readonly predecessorLedgerAt?: LedgerAt;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Buffer;
  readonly deploymentFingerprint: string;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly replayer: CompleteCanonicalReplay;
  readonly history?: readonly {
    readonly headerHash: string;
    readonly payloadEnvelopeCbor: Buffer;
  }[];
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly transitionTraceEventAuthority?: Parameters<
    typeof createHeaderClassifier
  >[0]["transitionTraceEventAuthority"];
  readonly predecessor?: {
    readonly observation: AuthenticatedStateQueueHeaderObservation;
    readonly payloadEnvelopeCbor: Buffer;
  };
}) => {
  const sources: readonly RetainedDaPayloadSource[] = [
    {
      sourceId: "retained-fixture",
      fetchPayloadByHeaderHash: async (headerHash) => {
        const bytes =
          headerHash === observation.headerHash
            ? payloadEnvelopeCbor
            : headerHash === predecessor?.observation.headerHash
              ? predecessor.payloadEnvelopeCbor
              : history.find((block) => block.headerHash === headerHash)
                  ?.payloadEnvelopeCbor;
        if (bytes === undefined)
          throw new Error("Retained fixture requested another header");
        return {
          ok: true,
          sourceId: "retained-fixture",
          sourcePeerId: "emulator",
          attempts: [],
          payloadEnvelopeCbor: bytes,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "retained-fixture/emulator",
            grade: "security",
          },
        };
      },
    },
  ];
  const classifier = await createHeaderClassifier({
    deploymentFingerprint,
    replayer,
    releaseFinalityAuthority,
    transitionTraceEventAuthority,
  });
  const policy = await releaseFinalityAuthority.verifyForWorkflow({
    deploymentFingerprint,
  });
  const replaySource = retainedPayloadReplaySource({ sources });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.policy.confirmationDepth,
      }),
    sources: replaySource.sources,
    ...(replayContext === undefined ? {} : { replayContext }),
    // The fixture's empty predecessor is supplied directly; the fixture
    // ledger recorded for each retained header supplies the others.
    ...(replayContext === undefined &&
    (predecessorLedgerAt !== undefined ||
      predecessor !== undefined ||
      observation.header.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT)
      ? {
          predecessorLedgerAt:
            predecessorLedgerAt ??
            (async (headerHash: string) =>
              predecessor === undefined &&
              headerHash === observation.header.prevHeaderHash &&
              observation.header.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT
                ? []
                : await fixtureLedgerAt({ sources: replaySource.sources })(
                    headerHash,
                  )),
        }
      : {}),
  });
  return { decision, sources };
};
