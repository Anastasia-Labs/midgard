import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { opaqueForcedLengthMismatchFixture } from "./field-preimage-length-evidence.fixture.js";
import type { SetupOptions } from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";

/** Preserve exact retained commitments while binding admission to this deployment. */
export const opaqueForcedLifecycleDaFixture: NonNullable<
  SetupOptions["forcedDaFixture"]
> = async ({ operatorVkey, now }) => {
  const raw = await opaqueForcedLengthMismatchFixture(
    Buffer.from("8180", "hex"),
  );
  // The raw evidence helper uses an arbitrary parent hash, whereas this queue
  // starts at genesis. Share the actual lifecycle admission coordinates; retain
  // every DA root/count so the opaque source remains exactly L1 committed.
  const admission = makeHeader(operatorVkey, now);
  const header = {
    ...raw.header,
    prevHeaderHash: admission.prevHeaderHash,
    operatorVkey: admission.operatorVkey,
    protocolVersion: admission.protocolVersion,
    startTime: admission.startTime,
    endTime: admission.endTime,
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
