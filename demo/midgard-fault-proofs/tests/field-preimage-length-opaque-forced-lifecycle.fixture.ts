import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { opaqueForcedLengthMismatchFixture } from "./field-preimage-length-evidence.fixture.js";
import type { SetupOptions } from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";

/** Preserve exact retained commitments while binding admission to this deployment. */
export const opaqueForcedLifecycleDaFixture: NonNullable<
  SetupOptions["forcedDaFixture"]
> = async ({ operatorVkey, now }) => {
  const raw = await opaqueForcedLengthMismatchFixture(
    Buffer.from("8180", "hex"),
  );
  const header = {
    ...raw.header,
    operatorVkey,
    startTime: BigInt(now),
    endTime: BigInt(now + 300_000),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload = {
    ...raw.payload,
    block_body: { ...raw.payload.block_body, header, header_hash: headerHash },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};
