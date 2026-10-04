import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { detectSourceMembershipMismatches } from "../src/transition-trace/detect.detect-source-membership-mismatches.js";
import {
  commitCountedRoot,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { validationRunBytesAreWellFormed } from "../src/transition-trace/witnesses.build-validation-run-faults.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const honest: SDK.ValidationTraceDescriptor = {
  schema_version: 1n,
  machine_version: 1n,
  trace_root: "11".repeat(32),
  step_count: 1n,
  initial_state_hash: "22".repeat(32),
  terminal_state_hash: "33".repeat(32),
  verdict: "Accepted",
  rejection_code_hash: "00".repeat(32),
};
const bytes = (descriptor: SDK.ValidationTraceDescriptor) =>
  Buffer.from(Data.to(descriptor, SDK.ValidationTraceDescriptor), "hex");

describe("validation-run proof primitives", () => {
  it("keeps successive absence openings and later membership on the same committed root", async () => {
    const key = Buffer.from("one");
    const value = Buffer.from("committed value");
    const root = await keyValuePhasRootWithCount([{ key, value }]);
    const expectedMembership = await keyValuePhasProof(root, key, value);
    await keyValuePhasNonMembershipProof(root, Buffer.from("missing one"));
    await keyValuePhasNonMembershipProof(root, Buffer.from("missing two"));
    expect(await keyValuePhasProof(root, key, value)).toEqual(
      expectedMembership,
    );
  });
  it("refuses malformed run bytes while accepting the exact honest descriptor", () => {
    expect(validationRunBytesAreWellFormed(bytes(honest))).toBe(true);
    expect(
      validationRunBytesAreWellFormed(bytes({ ...honest, step_count: 0n })),
    ).toBe(false);
    expect(
      validationRunBytesAreWellFormed(
        bytes({ ...honest, step_count: 4294967296n }),
      ),
    ).toBe(false);
    expect(
      validationRunBytesAreWellFormed(
        bytes({ ...honest, machine_version: 2n }),
      ),
    ).toBe(false);
    expect(
      validationRunBytesAreWellFormed(bytes({ ...honest, verdict: "Pending" })),
    ).toBe(false);
    expect(
      validationRunBytesAreWellFormed(
        bytes({ ...honest, verdict: "Rejected" }),
      ),
    ).toBe(false);
    for (const raw of ["ff", "00", "40", "80", "a0", "d87980", "d87a80"])
      expect(validationRunBytesAreWellFormed(Buffer.from(raw, "hex"))).toBe(
        false,
      );
  });
  it("proves an extra garbled run key even when all legitimate runs are present and the count label agrees", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(0x11, 0n)],
          fee: 0n,
        }),
      ],
    });
    const body = fixture.payload.block_body;
    const runs: SDK.DaPayloadEntry[] = [
      ["00", bytes(honest).toString("hex")],
      ...body.validation_traces,
    ];
    const phas = await keyValuePhasRootWithCount(
      runs.map(([k, v]) => ({
        key: Buffer.from(k, "hex"),
        value: Buffer.from(v, "hex"),
      })),
    );
    const header = {
      ...body.header,
      validationTracesRoot: await commitCountedRoot({
        domain: SDK.ROOT_DOMAINS.validationTraces,
        phasRoot: phas.root,
        count: body.header.validationTraceCount,
      }),
    };
    const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
    const payload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...body,
        header,
        header_hash: headerHash,
        validation_traces: runs,
      },
    };
    const reconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
        mode: "identity",
      }),
      committedHeader: header,
    });
    const found = await detectSourceMembershipMismatches(reconstruction);
    expect(found).toHaveLength(1);
    const detection = found[0]!;
    expect(detection).toMatchObject({
      buildable: true,
      invariant: "validation_run_event_key_canonical",
      fault: {
        SourceMembershipMismatch: {
          witness: { ForeignValidationRun: { event_key: "00" } },
        },
      },
    });
  });
});
