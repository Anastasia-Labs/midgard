import { Data } from "@lucid-evolution/lucid";

/**
 * Twin of `midgard/canonical_plutus_data_stream_v1.StreamFrameV1`: one open
 * container of the resumable canonical Data read. A step redeemer reveals the
 * stack top first; `@al-ft/midgard-core`'s `MidgardDataStreamFrame` is the
 * same frame as the off-chain reader keeps it.
 */
export const StreamFrameSchema = Data.Enum([
  Data.Object({ ListFrame: Data.Object({ started: Data.Boolean() }) }),
  Data.Object({ MapFrame: Data.Object({ remaining: Data.Integer() }) }),
  Data.Object({
    ChunksFrame: Data.Object({
      seen: Data.Integer(),
      leading: Data.Boolean(),
    }),
  }),
]);
