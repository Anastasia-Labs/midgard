/**
 * A `da-bond` send whose outcome is unknown (the follower provider's
 * `L1SubmitOutcomeUnknownError`) names the transaction like a failed
 * confirmation wait does, so the operator looks for it before running the
 * command again; a send the node refused passes through as is.
 */
import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import { CML } from "@lucid-evolution/lucid";
import { Cause, Runtime } from "effect";
import { describe, expect, it } from "vitest";

import { daBondSubmitAndConfirm } from "../src/commands/da-bond.js";

/** A transaction with no inputs or outputs: enough to carry an id. */
const TX_CBOR = "84a3008001800200a0f5f6";
const TX_ID = CML.hash_transaction(
  CML.Transaction.from_cbor_hex(TX_CBOR).body(),
).to_hex();

const afterSubmit = (txHash: string, reason: string) =>
  `Transaction ${txHash} was submitted, but waiting for its confirmation failed: ${reason}. Check whether ${txHash} landed before retrying; do not submit it again`;

describe("da-bond production submit, when the send's outcome is unknown", () => {
  it("names the transaction the provider names, and waits for nothing", async () => {
    const outcomeUnknown = new L1SubmitOutcomeUnknownError(
      "ef".repeat(32),
      "request_timeout",
    );
    const waited: string[] = [];
    const failure = await daBondSubmitAndConfirm(
      async () => {
        throw outcomeUnknown;
      },
      async (txHash) => waited.push(txHash),
    )(TX_CBOR).catch((error: unknown) => error);

    expect((failure as Error).message).toBe(
      afterSubmit("ef".repeat(32), outcomeUnknown.message),
    );
    expect((failure as Error).cause).toBe(outcomeUnknown);
    expect(waited).toEqual([]);
  });

  it("names the transaction by its bytes when the provider names none, also inside a FiberFailure", async () => {
    const outcomeUnknown = new L1SubmitOutcomeUnknownError(
      null,
      "sidecar_exited",
    );
    const failure = await daBondSubmitAndConfirm(
      async () => {
        throw Runtime.makeFiberFailure(Cause.fail(outcomeUnknown));
      },
      async () => undefined,
    )(TX_CBOR).catch((error: unknown) => error);

    expect((failure as Error).message).toBe(
      afterSubmit(TX_ID, outcomeUnknown.message),
    );
    expect((failure as Error).cause).toBe(outcomeUnknown);
  });

  it("passes a send the node refused through as is: nothing was taken", async () => {
    const refused = new Error("submit refused: BadInputsUTxO");
    await expect(
      daBondSubmitAndConfirm(
        async () => {
          throw refused;
        },
        async () => undefined,
      )(TX_CBOR),
    ).rejects.toBe(refused);
  });
});
