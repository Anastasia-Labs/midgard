import * as SDK from "@al-ft/midgard-sdk";

import { DaLibp2pProofProtocolHandlers } from "../src/da/libp2p/proof-protocols.js";
import type {
  DaStoredPayloadRootSet,
  Header,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { type PostgresCommitteeStore } from "../src/store/postgres.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

export const deploymentFingerprint = "01".repeat(32);

export const deploymentFingerprintBytes = Buffer.from(
  deploymentFingerprint,
  "hex",
);

export const makeHandlers = async (): Promise<{
  readonly handlers: DaLibp2pProofProtocolHandlers;
  readonly store: PostgresCommitteeStore;
}> => {
  const store = await openTestCommitteeStore();
  const handlers = new DaLibp2pProofProtocolHandlers({
    deploymentFingerprint,
    store,
    accessPolicy: { kind: "any_noise_authenticated_peer" },
  });
  return { handlers, store };
};

export const saveVerifiedPayload = async ({
  store,
  payloadCbor,
  payloadHash,
  header,
  headerHash,
  rootSummary = rootSummaryFromHeader(header),
}: {
  readonly store: PostgresCommitteeStore;
  readonly payloadCbor: Buffer;
  readonly payloadHash: Buffer;
  readonly header: Header;
  readonly headerHash: string;
  readonly rootSummary?: DaStoredPayloadRootSet;
}): Promise<void> => {
  await store.saveDaPayload({
    deploymentFingerprint,
    headerHash,
    payloadSchemaVersion: 1,
    payloadCborHex: payloadCbor.toString("hex"),
    payloadSha256: payloadHash.toString("hex"),
    sourcePeerId: "libp2p-fixture",
    fetchedAt: "2026-06-21T00:00:00.000Z",
    verifiedAt: "2026-06-21T00:00:01.000Z",
    rootSummary,
    validationStatus: "verified",
  });
  await store.upsertStateQueueHeader(
    stateQueueHeaderRecord({ header, headerHash }),
  );
};

export const stateQueueHeaderRecord = ({
  header,
  headerHash,
}: {
  readonly header: Header;
  readonly headerHash: string;
}): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: "aa".repeat(32) + "#0",
  blockAssetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  header,
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "bb".repeat(32),
    depth: 10,
    providerSource: "fixture",
  },
  finalized: true,
  status: "attested",
  validationErrors: [],
  updatedAt: "2026-06-21T00:00:02.000Z",
});

export const rootSummaryFromHeader = (
  header: Header,
): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});
