/** The validation-trace dispute runner at the native scripts step after
 * state 959 of our trace, over a native script past one chunk, whose
 * execution descriptor carries a whole first chunk: driven through the
 * phase resolver and the native scripts native semantic resolver to the
 * award, each move within the transaction size limit less its reserve
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { MIDGARD_BOUNDED_ITEM_CHUNK_BYTES } from "@al-ft/midgard-core";
import { beforeAll, describe, expect, it } from "vitest";

import { ownerAmongManyNativeScript } from "./helpers/native-script-shapes.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

/** 130 signatures beside the owner's take the script past one chunk. */
const SCRIPT = ownerAmongManyNativeScript(130);

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

const LABELS = [
  "EnterResolution",
  "PrepareResolution",
  "PrepareSelected nativeScripts",
  "Verify nativeScripts 1",
] as const;

const drive = phaseDrive({
  from: 959,
  phase: "nativeScripts",
  semantic: 1,
  evidence: { auxiliary: 33, fixed: { 1: 0n }, unwrapped: [15] },
  at: "nativeScriptsNativeSemantic",
  failingNative: { script: SCRIPT },
  failingNativeMint: true,
});

describe("validation-trace dispute runner: driving a thread at the native scripts native step after state 959, over a native script past one chunk", () => {
  beforeAll(drive.setUp, 1_800_000);

  it("witnesses a native script longer than one chunk", () => {
    expect(SCRIPT.scriptBytes.length).toBeGreaterThan(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    );
  });

  it(
    "routes the step through the phase resolver to the native scripts native semantic resolver, committing the transition beside the evidence it takes",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the native scripts native semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/native_scripts_native_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("moves the thread through the step's resolution to the award, each move within the transaction size limit less its reserve, and the semantic move carrying the whole first chunk", async () => {
    await drive.awards();
    for (const label of LABELS)
      expect(drive.bytesOf(label)).toBeLessThanOrEqual(
        MAX_TX_SIZE - SIZE_RESERVE,
      );
    expect(drive.bytesOf("Verify nativeScripts 1")).toBeGreaterThan(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    );
  }, 1_200_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
