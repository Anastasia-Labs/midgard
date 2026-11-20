import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeDaSha256Hash,
  type DaEventToStepByEventRequestV1,
  type DaProofBundleByHeaderRequestV1,
  type DaTraceStepByIndexRequestV1,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { computeDaPayloadRoots, daPayloadSha256 } from "../src/da/payload.js";
import {
  DaProofArtifactDeriver,
  type DaProofArtifactStore,
} from "../src/da/proof-artifacts.js";
import type { DaPayloadRecord, StateQueueHeaderRecord } from "../src/domain.js";
import { IDENTITY_TX_PROJECTOR, makePayloadFixture } from "./helpers.js";

const deploymentFingerprint = "01".repeat(32);
const deploymentFingerprintBytes = Buffer.from(deploymentFingerprint, "hex");

describe("DA proof artifact derivation", () => {
  it("derives trace-step bytes and indexed membership proof bytes", async () => {
    const context = await makeVerifiedContext();
    const deriver = makeDeriver(context.store);
    const request = traceStepRequest(context.headerHash, 2);

    const result = await deriver.traceStepByIndex(request);

    expect(result.reasonCode).toBeNull();
    expect(result.response.status).toBe("found");
    expect(result.response.transitionStepBytes?.toString("hex")).toBe(
      context.payload.block_body.transition_trace[2]![1],
    );
    expect(result.response.membershipProofBytes).not.toBeNull();

    const proof = LucidData.from(
      result.response.membershipProofBytes!.toString("hex"),
      SDK.TransitionTraceMembershipProofSchema as never,
    ) as SDK.IndexedTraceProof;
    expect(proof.root).toBe(context.header.transitionTraceRoot);
    expect(proof.count).toBe(5n);
    expect(proof.key).toBe(2n);
    expect(proof.value.step_index).toBe(2n);
  });

  it("derives event-to-step membership entry and proof bytes", async () => {
    const context = await makeVerifiedContext();
    const deriver = makeDeriver(context.store);
    const [eventKeyHex, valueHex] =
      context.payload.block_body.event_to_step[0]!;

    const result = await deriver.eventToStepByEvent(
      eventToStepRequest(context.headerHash, Buffer.from(eventKeyHex, "hex")),
    );

    expect(result.reasonCode).toBeNull();
    expect(result.response.status).toBe("found");
    expect(result.response.eventToStepEntryBytes?.toString("hex")).toBe(
      valueHex,
    );

    const proof = LucidData.from(
      result.response.membershipOrNonmembershipProofBytes!.toString("hex"),
      SDK.EventToStepProofSchema as never,
    ) as SDK.EventToStepProof;
    expect("EventToStepMembership" in proof).toBe(true);
    if ("EventToStepMembership" in proof) {
      expect(proof.EventToStepMembership.membership.root).toBe(
        context.header.eventToStepRoot,
      );
      expect(proof.EventToStepMembership.membership.count).toBe(5n);
    }
  });

  it("returns event-to-step non-membership proof bytes for absent events", async () => {
    const context = await makeVerifiedContext();
    const deriver = makeDeriver(context.store);
    const absentEventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: "99".repeat(32) },
    };
    const absentEventKeyBytes = Buffer.from(
      LucidData.to(absentEventKey as never, SDK.EventKeySchema as never),
      "hex",
    );

    const result = await deriver.eventToStepByEvent(
      eventToStepRequest(context.headerHash, absentEventKeyBytes),
    );

    expect(result.response.status).toBe("found");
    expect(result.response.eventToStepEntryBytes).toBeNull();
    expect(result.response.membershipOrNonmembershipProofBytes).not.toBeNull();

    const proof = LucidData.from(
      result.response.membershipOrNonmembershipProofBytes!.toString("hex"),
      SDK.EventToStepProofSchema as never,
    ) as SDK.EventToStepProof;
    expect("EventToStepNonMembership" in proof).toBe(true);
    if ("EventToStepNonMembership" in proof) {
      expect(proof.EventToStepNonMembership.non_membership.root).toBe(
        context.header.eventToStepRoot,
      );
      expect(proof.EventToStepNonMembership.non_membership.key).toEqual(
        absentEventKey,
      );
    }
  });

  it("fails closed for unverified stored payload records", async () => {
    const context = await makeVerifiedContext();
    const store = makeStore({
      payload: {
        ...context.payloadRecord,
        validationStatus: "fetched",
        verifiedAt: undefined,
      },
      header: context.headerRecord,
    });
    const deriver = makeDeriver(store);

    const result = await deriver.traceStepByIndex(
      traceStepRequest(context.headerHash, 0),
    );

    expect(result.reasonCode).toBe("stored_payload_not_verified");
    expect(result.response.status).toBe("rejected");
    expect(result.response.transitionStepBytes).toBeNull();
    expect(result.response.membershipProofBytes).toBeNull();
  });

  it("fails closed when the stored root summary no longer matches payload bytes", async () => {
    const context = await makeVerifiedContext();
    const store = makeStore({
      payload: {
        ...context.payloadRecord,
        rootSummary: {
          ...context.payloadRecord.rootSummary!,
          transitionTraceRoot: "00".repeat(32),
        },
      },
      header: context.headerRecord,
    });
    const deriver = makeDeriver(store);

    const result = await deriver.traceStepByIndex(
      traceStepRequest(context.headerHash, 0),
    );

    expect(result.reasonCode).toBe("stored_root_summary_mismatch");
    expect(result.response.status).toBe("rejected");
  });

  it("fails closed when the committed L1 header identity diverges", async () => {
    const context = await makeVerifiedContext();
    const store = makeStore({
      payload: context.payloadRecord,
      header: {
        ...context.headerRecord,
        header: {
          ...context.headerRecord.header,
          eventToStepRoot: "00".repeat(32),
        },
      },
    });
    const deriver = makeDeriver(store);

    const result = await deriver.eventToStepByEvent(
      eventToStepRequest(
        context.headerHash,
        Buffer.from(context.payload.block_body.event_to_step[0]![0], "hex"),
      ),
    );

    expect(result.reasonCode).toBe("record_header_hash_mismatch");
    expect(result.response.status).toBe("rejected");
  });

  it("rejects malformed event keys", async () => {
    const context = await makeVerifiedContext();
    const deriver = makeDeriver(context.store);

    const result = await deriver.eventToStepByEvent(
      eventToStepRequest(context.headerHash, Buffer.from("not-cbor")),
    );

    expect(result.reasonCode).toBe("event_key_malformed");
    expect(result.response.status).toBe("rejected");
  });

  it("derives deterministic proof-bundle bytes from committed roots and counts", async () => {
    const context = await makeVerifiedContext();
    const deriver = makeDeriver(context.store);

    const result = await deriver.proofBundleByHeader(
      proofBundleRequest(context.headerHash),
    );

    expect(result.reasonCode).toBeNull();
    expect(result.response.status).toBe("found_inline");
    expect(result.response.reasonCode).toBeNull();
    expect(result.response.proofBundleBytes).not.toBeNull();
    expect(
      result.response.proofBundleHash?.equals(
        computeDaSha256Hash(result.response.proofBundleBytes!),
      ),
    ).toBe(true);

    const bundle = decodeSingleCbor(
      result.response.proofBundleBytes!,
    ) as unknown[];
    expect(bundle).toHaveLength(6);
    expect(BigInt(bundle[0] as number | bigint)).toBe(1n);
    expect(Buffer.from(bundle[1] as Uint8Array).toString("hex")).toBe(
      context.headerHash,
    );
    expect(Buffer.from(bundle[2] as Uint8Array).toString("hex")).toBe(
      context.payloadRecord.payloadSha256,
    );
    expect(Buffer.from(bundle[3] as Uint8Array)).toHaveLength(32);
    expect(bundle[4]).toHaveLength(7);
    expect(bundle[5]).toHaveLength(6);
  });
});

const makeVerifiedContext = async () => {
  const fixture = await makePayloadFixture();
  const rootSummary = await computeDaPayloadRoots(
    fixture.payload,
    IDENTITY_TX_PROJECTOR,
  );
  const payloadRecord: DaPayloadRecord = {
    deploymentFingerprint,
    headerHash: fixture.headerHash,
    payloadCborHex: fixture.payloadCbor.toString("hex"),
    payloadSha256: daPayloadSha256(fixture.payloadCbor),
    sourcePeerId: "fixture-peer",
    fetchedAt: "2026-06-21T00:00:00.000Z",
    verifiedAt: "2026-06-21T00:00:01.000Z",
    rootSummary,
    validationStatus: "verified",
  };
  const headerRecord: StateQueueHeaderRecord = {
    deploymentFingerprint,
    headerHash: fixture.headerHash,
    stateQueueOutRef: "aa".repeat(32) + "#0",
    blockAssetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${fixture.headerHash}`,
    header: fixture.header,
    computedHeaderHash: fixture.headerHash,
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
  };
  const store = makeStore({ payload: payloadRecord, header: headerRecord });
  return {
    ...fixture,
    payloadRecord,
    headerRecord,
    store,
  };
};

const makeDeriver = (store: DaProofArtifactStore): DaProofArtifactDeriver =>
  new DaProofArtifactDeriver({
    deploymentFingerprint,
    store,
    transactionProjector: IDENTITY_TX_PROJECTOR,
  });

const makeStore = ({
  payload,
  header,
}: {
  readonly payload?: DaPayloadRecord;
  readonly header?: StateQueueHeaderRecord;
}): DaProofArtifactStore => ({
  getDaPayload: async (headerHash) =>
    payload?.headerHash === headerHash ? payload : undefined,
  getStateQueueHeader: async (headerHash) =>
    header?.headerHash === headerHash ? header : undefined,
});

const traceStepRequest = (
  headerHash: string,
  stepIndex: number,
): DaTraceStepByIndexRequestV1 => ({
  deploymentFingerprint: deploymentFingerprintBytes,
  headerHash: Buffer.from(headerHash, "hex"),
  stepIndex,
});

const eventToStepRequest = (
  headerHash: string,
  eventKey: Buffer,
): DaEventToStepByEventRequestV1 => ({
  deploymentFingerprint: deploymentFingerprintBytes,
  headerHash: Buffer.from(headerHash, "hex"),
  eventKey,
});

const proofBundleRequest = (
  headerHash: string,
): DaProofBundleByHeaderRequestV1 => ({
  deploymentFingerprint: deploymentFingerprintBytes,
  headerHash: Buffer.from(headerHash, "hex"),
  maxInlineBytes: 1024,
});
