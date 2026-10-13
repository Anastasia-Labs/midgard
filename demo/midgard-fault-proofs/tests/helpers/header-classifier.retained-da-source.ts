import type { RetainedDaPayloadSource } from "../../src/transition-trace/fetch.js";

export const retainedDaSource = ({
  payloadEnvelopeCbor,
  fetched,
}: {
  readonly payloadEnvelopeCbor: Buffer;
  readonly fetched: { count: number };
}): RetainedDaPayloadSource => ({
  sourceId: "libp2p",
  fetchPayloadByHeaderHash: async () => {
    fetched.count += 1;
    return {
      ok: true,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
      sourceId: "libp2p",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    };
  },
});
